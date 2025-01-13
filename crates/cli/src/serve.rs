use std::convert::Infallible;
use std::fmt::Display;
use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use futures_lite::{future, stream, Stream, StreamExt};
use http_body_util::{Either, StreamBody};
use hyper::body::{Bytes, Frame};
use hyper::header::{self, HeaderValue};
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response};
use hyper_util::rt::TokioIo;
use notify::{Event, EventKind, RecursiveMode, Watcher};
use serde::Serialize;
use tokio::net::TcpListener;
use tokio::sync::{broadcast, mpsc, oneshot};
use tower_async::{Service, ServiceBuilder};
use tower_async_http::compression::CompressionLayer;
use tower_async_http::services::ServeDir;

use crate::build::build;
use crate::log;
use crate::report::{Error, ErrorExt, Report};
use crate::Serve;

pub fn serve(s: &Serve) -> Report<()> {
    build(&s.build)?;

    let (notifier, builder, debounce_routine) = debounce(Timings {
        delay_after_update: Duration::from_millis(250),
        cooldown: Duration::from_secs(1),
    });

    let _watch = watch(&s.watch, notifier)?;

    let updates = Updates::new();

    thread::scope(|spawn| {
        spawn.spawn({
            let updates = updates.clone();
            || rebuild(s, builder, updates)
        });

        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .message("failed to create tokio runtime")?
            .block_on(async {
                future::try_zip(start_server(s, updates), async {
                    debounce_routine.await;
                    Ok(())
                })
                .await?;

                Ok(())
            })
    })
}

fn rebuild(s: &Serve, mut builder: Builder, updates: Updates) {
    loop {
        let Some(BuildEvent { paths, done }) = builder.wait() else {
            break;
        };

        for path in &paths {
            log::info!("updated {}", path.display());
        }

        let res = build(&s.build);
        if let Err(err) = &res {
            log::error!("rebuild failed: {err}");
        }

        _ = done.send(());
        updates.produce(res);
    }
}

fn watch(paths: &[PathBuf], notifier: Notifier) -> Report<impl Drop> {
    let handler = move |res| {
        let Ok(Event { kind, paths, .. }) = res else {
            return;
        };

        if let EventKind::Create(_) | EventKind::Modify(_) | EventKind::Remove(_) = kind {
            notifier.notify(UpdateEvent { paths });
        }
    };

    let mut watch = notify::recommended_watcher(handler)
        .map_err_into_io()
        .message("failed to create file watcher")?;

    for path in paths {
        watch
            .watch(path, RecursiveMode::Recursive)
            .map_err_into_io()
            .with_message(|| format!("failed to watch path {}", path.display()))?;
    }

    Ok(watch)
}

fn debounce(time: Timings) -> (Notifier, Builder, impl Future) {
    let (nofity, mut notified) = mpsc::channel(1);
    let (send_build, recv_build) = mpsc::channel(1);

    let notifier = Notifier(nofity);
    let builder = Builder(recv_build);

    let routine = async move {
        while let Some(UpdateEvent { mut paths }) = notified.recv().await {
            loop {
                let updates = async {
                    if let Some(UpdateEvent { paths: mut more }) = notified.recv().await {
                        more.retain(|path| !paths.contains(path));
                        paths.append(&mut more);
                    }

                    true
                };

                let delay = async {
                    tokio::time::sleep(time.delay_after_update).await;
                    false
                };

                if future::or(updates, delay).await {
                    break;
                }
            }

            let build = async {
                let (done, wait) = oneshot::channel();
                _ = send_build.send(BuildEvent { paths, done }).await;
                _ = wait.await;
                tokio::time::sleep(time.cooldown).await;
            };

            let skip_new_updates = async {
                loop {
                    _ = notified.recv().await;
                }
            };

            future::or(build, skip_new_updates).await;
        }
    };

    (notifier, builder, routine)
}

struct Timings {
    delay_after_update: Duration,
    cooldown: Duration,
}

struct Notifier(mpsc::Sender<UpdateEvent>);

impl Notifier {
    fn notify(&self, event: UpdateEvent) {
        _ = self.0.blocking_send(event);
    }
}

struct Builder(mpsc::Receiver<BuildEvent>);

impl Builder {
    fn wait(&mut self) -> Option<BuildEvent> {
        self.0.blocking_recv()
    }
}

struct UpdateEvent {
    paths: Vec<PathBuf>,
}

struct BuildEvent {
    paths: Vec<PathBuf>,
    done: oneshot::Sender<()>,
}

async fn start_server(s: &Serve, updates: Updates) -> Report<Infallible> {
    let listener = TcpListener::bind((s.address.as_str(), s.port))
        .await
        .with_message(|| format!("failed to bind tcp listener to {}:{}", s.address, s.port))?;

    log::starting!("development server at http://{}:{}", s.address, s.port);

    let serve = {
        let files = ServiceBuilder::new()
            .layer(CompressionLayer::new())
            .service(ServeDir::new(&s.build.dist));

        let serve = ServiceBuilder::new().layer_fn(Logger).service(Events {
            path: "/events",
            updates,
            inner: files,
        });

        Arc::new(serve)
    };

    loop {
        let (tcp, _) = listener
            .accept()
            .await
            .message("failed to accept tcp connection")?;

        let serve = serve.clone();
        tokio::spawn(async move {
            let io = TokioIo::new(tcp);
            if let Err(err) = http1::Builder::new()
                .serve_connection(io, service_fn(|req| serve.call(req)))
                .await
            {
                if err.is_incomplete_message() {
                    return;
                }

                log::error!("serving connection: {err}");
            }
        });
    }
}

#[derive(Clone)]
struct Update(Bytes);

impl Update {
    fn new(res: Result<(), Error>) -> Self {
        #[derive(Serialize)]
        #[serde(rename_all = "lowercase")]
        enum Repr {
            Reload,
            Error(String),
        }

        let repr = match res {
            Ok(()) => Repr::Reload,
            Err(err) => Repr::Error(err.to_string()),
        };

        let mut buf = b"event: update\ndata: ".to_vec();
        serde_json::to_writer(&mut buf, &repr).expect("serialize json");
        buf.extend(b"\n\n");

        Self(Bytes::from(buf))
    }
}

#[derive(Clone)]
struct Updates(broadcast::Sender<Update>);

impl Updates {
    fn new() -> Self {
        let (events, _) = broadcast::channel(1);
        Self(events)
    }

    fn produce(&self, res: Result<(), Error>) {
        _ = self.0.send(Update::new(res));
    }

    fn subscribe(&self) -> Subscriber {
        let events = self.0.subscribe();
        Subscriber(events)
    }
}

struct Subscriber(broadcast::Receiver<Update>);

impl Subscriber {
    async fn wait(&mut self) -> Option<Update> {
        self.0.recv().await.ok()
    }
}

type EventStream = StreamBody<Pin<Box<dyn Stream<Item = Result<Frame<Bytes>, Infallible>> + Send>>>;

struct Events<S> {
    path: &'static str,
    updates: Updates,
    inner: S,
}

impl<S> Events<S> {
    fn event_stream(&self) -> EventStream {
        let subscriber = self.updates.subscribe();
        let stream = stream::unfold(subscriber, |mut subscriber| async {
            let recv = async {
                let Update(bytes) = subscriber.wait().await?;
                Some(bytes)
            };

            let ping = async {
                tokio::time::sleep(Duration::from_secs(15)).await;
                const { Some(Bytes::from_static(b":\n\n")) }
            };

            let chunk = future::or(recv, ping).await?;
            Some((chunk, subscriber))
        })
        .map(Frame::data)
        .map(Ok)
        .boxed();

        StreamBody::new(stream)
    }
}

impl<S, I, O> Service<Request<I>> for Events<S>
where
    S: Service<Request<I>, Response = Response<O>>,
{
    type Response = Response<Either<EventStream, O>>;
    type Error = S::Error;

    async fn call(&self, req: Request<I>) -> Result<Self::Response, Self::Error> {
        let path = req.uri().path();
        let path = path.strip_suffix('/').unwrap_or(path);

        if path == self.path {
            let mut res = Response::new(Either::Left(self.event_stream()));
            let headers = res.headers_mut();

            headers.insert(
                header::CONTENT_TYPE,
                const { HeaderValue::from_static("text/event-stream") },
            );

            headers.insert(
                header::CACHE_CONTROL,
                const { HeaderValue::from_static("no-cache") },
            );

            Ok(res)
        } else {
            let res = self.inner.call(req).await?;
            Ok(res.map(Either::Right))
        }
    }
}

struct Logger<S>(S);

impl<S, I, O> Service<Request<I>> for Logger<S>
where
    S: Service<Request<I>, Response = Response<O>, Error: Display>,
{
    type Response = S::Response;
    type Error = S::Error;

    async fn call(&self, req: Request<I>) -> Result<Self::Response, Self::Error> {
        let method = req.method().to_owned();
        let uri = req.uri().to_owned();

        let out = self.0.call(req).await;

        match &out {
            Ok(res) => log::info!("{method} {uri} -> {}", res.status()),
            Err(err) => log::error!("{method} {uri} -> {err}"),
        }

        out
    }
}

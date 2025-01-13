use kobold::prelude::*;

#[component]
fn app() -> impl View {
    let count = state!(0_i32);

    view! {
        <p>
            <h3>"Counter is at "{ count }</h3>
            <button onclick={do *count -= 1}>"Decrement"</button>
            <button onclick={do *count += 1}>"Increment"</button>
    }
}

kobold::start!(app);

(() => {
    const es = new EventSource('events');
    window.addEventListener('beforeunload', () => es.close());

    es.addEventListener('update', ev => {
        const msg = JSON.parse(ev.data);

        if (msg == 'reload') {
            window.location.reload();
        } else if (msg.error) {
            console.error('rebuild failed:', msg.error);
        }
    });
})()
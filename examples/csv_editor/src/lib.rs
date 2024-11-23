use kobold::prelude::*;
use web_sys::HtmlInputElement as InputElement;

mod csv;
mod state;

use state::{Editing, State, Text};

#[component]
fn editor() -> impl View {
    let state = state!(State::mock);

    let onload = state.bind_async(|state, event: Event<InputElement>| async move {
        let file = match event.current_target().files().and_then(|list| list.get(0)) {
            Some(file) => file,
            None => return,
        };

        state.update(|state| state.name = file.name());

        if let Ok(table) = csv::read_file(file).await {
            state.update(move |state| state.table = table);
        }
    });

    let onkeydown = event!(move |state, e: &KeyboardEvent<_>| {
        if matches!(e.key().as_str(), "Esc" | "Escape") {
            state.editing = Editing::None;

            Then::Render
        } else {
            Then::Stop
        }
    });

    view! {
        <input type="file" accept="text/csv" onchange={onload}>
        <h1>{ &state.name }</h1>
        <table {onkeydown}>
            <thead>
                <tr>
                {
                    for state.columns().map(|col| head(col, state))
                }
            <tbody>
            {
                for state.rows().map(move |row| view! {
                    <tr>
                    {
                        for state.columns().map(move |col| cell(col, row, state))
                    }
                })
            }
    }
}

#[component(auto_branch)]
fn head(col: usize, state: &Hook<State>) -> impl View + '_ {
    let value = state.source.get_text(&state.columns[col]);

    if state.editing == (Editing::Column { col }) {
        let onchange = state.bind(move |state, e: &Event<InputElement>| {
            state.columns[col] = Text::Owned(e.current_target().value().into());
            state.editing = Editing::None;
        });

        view! {
            <th.edit>
                { value }
                <input.edit.edit-head {onchange} {value}>
        }
    } else {
        let ondblclick = state.bind(move |s, _| s.editing = Editing::Column { col });

        view! { <th {ondblclick}>{ value } }
    }
}

#[component(auto_branch)]
fn cell(col: usize, row: usize, state: &Hook<State>) -> impl View + '_ {
    let value = state.source.get_text(&state.rows[row][col]);

    if state.editing == (Editing::Cell { row, col }) {
        let onchange = event!(move |state, e: &Event<InputElement>| {
            state.rows[row][col] = Text::Owned(e.current_target().value().into());
            state.editing = Editing::None;
        });

        let mut selected = false;

        let onmouseenter = move |e: &MouseEvent<InputElement>| {
            if !selected {
                let input = e.current_target();
                input.focus();
                input.select();
                selected = true;
            }
        };

        view! {
            <td.edit>
                { value }
                <input.edit {onchange} {onmouseenter} {value}>
        }
    } else {
        let ondblclick = state.bind(move |s, _| s.editing = Editing::Cell { row, col });

        view! { <td {ondblclick}>{ value } }
    }
}

kobold::start!(editor);

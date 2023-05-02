// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use log::debug;
use logos::{Lexer, Logos};
use std::str::FromStr;
use take_mut::take;
use wasm_bindgen_futures::JsFuture;
use web_sys::{File, Url};

use crate::helpers::csv_helpers;
use crate::state::{Content, Table, TableVariant, Text, TextSource};

#[derive(Logos)]
enum Token {
    #[error]
    Err,
    #[token(",")]
    Comma,
    #[regex(r"[\n\r]+")]
    Newline,
    #[regex(r#"[^"\n\r,]+"#)]
    Value,
    #[regex(r#""[^"]+""#, priority = 6)]
    QuotedValue,
    #[regex(r#""([^"]|"")+""#)]
    EscapedValue,
}

#[derive(Debug)]
pub enum Error {
    NoData,
    FailedToBufferFile,
    FailedToReadFile,
    FailedToWriteFile,
    FailedToLoadMetadata,
    InvalidRowLength,
    MustBeAtLeastOneRowData,
    MustBeAtLeastOneColumnData,
    MustBeSameColumnLengthOnAllRows,
    MustBeThreeRowsIncludingLabelsRowDataRowVariablesRow,
}

fn parse_row(lex: &mut Lexer<Token>, columns: usize) -> Result<Option<Vec<Text>>, Error> {
    let mut row = Vec::with_capacity(columns);
    let mut value = None;

    while let Some(token) = lex.next() {
        value = match token {
            Token::Value => Some(Text::Insitu(lex.span())),
            Token::QuotedValue => {
                let mut span = lex.span();

                span.start += 1;
                span.end -= 1;

                Some(Text::Insitu(span))
            }
            Token::EscapedValue => {
                let mut slice = lex.slice();

                slice = &slice[1..slice.len() - 1];

                Some(Text::Owned(slice.replace("\"\"", "\"").into()))
            }
            Token::Comma => {
                row.push(value.take().unwrap_or_default());
                continue;
            }
            Token::Newline => {
                row.push(value.take().unwrap_or_default());
                break;
            }
            Token::Err => break,
        }
    }

    if let Some(value) = value {
        row.push(value);
    }

    match (columns, row.len()) {
        (_, 0) => Ok(None),
        (0, _) => Ok(Some(row)),
        (n, r) => {
            if n > r {
                row.resize_with(n, Text::default);
            }

            if r > n {
                Err(Error::InvalidRowLength)
            } else {
                Ok(Some(row))
            }
        }
    }
}

impl TryFrom<String> for Table {
    type Error = Error;

    fn try_from(source: String) -> Result<Self, Error> {
        let mut lex = Token::lexer(&source);

        let columns = parse_row(&mut lex, 0)?.ok_or(Error::NoData)?;

        let mut rows = Vec::new();

        while let Some(row) = parse_row(&mut lex, columns.len())? {
            rows.push(row);
        }

        Ok(Table {
            source: TextSource::from(source),
            columns,
            rows,
        })
    }
}

impl FromStr for Table {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Error> {
        s.to_owned().try_into()
    }
}

pub async fn read_file(file: File) -> Result<Table, Error> {
    let text = JsFuture::from(file.text())
        .await
        .map_err(|_| Error::FailedToReadFile)?
        .as_string()
        .ok_or(Error::FailedToReadFile)?;

    text.parse()
}

fn validate_same_columns_length_all_rows(
    new_csv: &Vec<Vec<&str>>,
    new_csv_lens: &Vec<usize>,
) -> Result<(), Error> {
    let is_not_all_same =
        |new_csv: &[usize]| -> bool { new_csv.iter().min() != new_csv.iter().max() };
    debug!("is_not_all_same {:?}", is_not_all_same(&new_csv_lens));
    if is_not_all_same(&new_csv_lens) == true {
        return Err(Error::MustBeSameColumnLengthOnAllRows);
    }
    Ok(())
}

// the TableVariant::Main has a single label row, and then multiple data rows under it in the CSV file. it
// does not have a label (variables) row.
//
// it needs to be processed differently from TableVariant::Details that has only a single label row,
// a single data row, and a single label (variables) row.
pub fn generate_csv_data_for_download(
    table_variant: TableVariant,
    content: &Content,
) -> Result<String, Error> {
    // generate CSV file format from object Url in state
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=f911a069c22a7f4cf4b5e8a9aa05e65e

    match table_variant {
        TableVariant::Main => {
            let binding_source = &content.table.source.source;
            let original_csv: Vec<&str> = binding_source.split(&['\n'][..]).collect();
            debug!("original_csv {:?}", original_csv);

            let mut new_csv: Vec<Vec<&str>> = vec![];
            let mut new_csv_lens: Vec<usize> = vec![];
            let padding = "".to_string();
            csv_helpers::pad_csv_data(&original_csv, &mut new_csv, &mut new_csv_lens, &padding);

            debug!(
                "content.table.columns.len() {:?}",
                content.table.columns.len()
            );
            // validate qty of columns
            if content.table.columns.len() < 1 {
                return Err(Error::MustBeAtLeastOneColumnData);
            }
            debug!("new_csv_lens {:?}", new_csv_lens);

            validate_same_columns_length_all_rows(&new_csv, &new_csv_lens)?;

            let mut arr = vec![];
            // only one column so we'll process that first before the rows
            let new_csv_labels_stringified: String =
                update_csv_row_for_modified_table_cells(&content.table.columns, &mut new_csv[0]); // labels
            arr.push(new_csv_labels_stringified);

            let content_table_rows = content.table.rows.clone();

            debug!("content_table_rows {:?}", content_table_rows);
            debug!("content_table_rows.len() {:?}", content_table_rows.len());
            // validate qty of rows
            if content_table_rows.len() < 1 {
                return Err(Error::MustBeAtLeastOneRowData);
            }

            // multiple rows so we'll push each of them now
            content_table_rows
                .into_iter()
                .enumerate()
                .for_each(|(i, row_data)| {
                    let new_csv_data_stringified: String = update_csv_row_for_modified_table_cells(
                        &content.table.rows[i],
                        &mut new_csv[i + 1],
                    ); // values row 1
                    arr.push(new_csv_data_stringified);
                });

            let content_serialized: String = arr.join("\n");
            debug!("content_serialized {:?}", content_serialized);

            return Ok(content_serialized);
        }
        TableVariant::Details => {
            let binding_source = &content.table.source.source;
            let original_csv: Vec<&str> = binding_source.split(&['\n'][..]).collect();
            debug!("original_csv {:?}", original_csv);

            let mut new_csv: Vec<Vec<&str>> = vec![];
            let mut new_csv_lens: Vec<usize> = vec![];
            let padding = "".to_string();
            csv_helpers::pad_csv_data(&original_csv, &mut new_csv, &mut new_csv_lens, &padding);

            debug!(
                "content cols rows {:?} {:?}",
                content.table.columns.len(),
                content.table.rows.len()
            );
            // validate qty of rows
            if content.table.columns.len() != (1 as usize)
                && content.table.rows.len() != (2 as usize)
            {
                return Err(Error::MustBeThreeRowsIncludingLabelsRowDataRowVariablesRow);
            }

            debug!("new_csv_lens {:?}", new_csv_lens);

            validate_same_columns_length_all_rows(&new_csv, &new_csv_lens)?;

            let new_csv_variables_stringified: String =
                update_csv_row_for_modified_table_cells(&content.table.columns, &mut new_csv[0]);
            let new_csv_values_stringified: String =
                update_csv_row_for_modified_table_cells(&content.table.rows[0], &mut new_csv[1]);
            let new_csv_labels_stringified: String =
                update_csv_row_for_modified_table_cells(&content.table.rows[1], &mut new_csv[2]);
            let arr = vec![
                new_csv_variables_stringified,
                new_csv_values_stringified,
                new_csv_labels_stringified,
            ];
            let content_serialized: String = arr.join("\n");
            debug!("content_serialized {:?}", content_serialized);

            return Ok(content_serialized);
        }
        _ => panic!("unknown variant name to generate csv data for download"),
    };
}

pub fn update_csv_row_for_modified_table_cells<'a>(
    cells: &'a Vec<Text>,
    csv_row: &mut Vec<&'a str>,
) -> String {
    let _ = &cells.into_iter().enumerate().for_each(|(i, el)| {
        match el {
            Text::Insitu(r) => {}
            Text::Owned(s) => {
                // let len = csv_row.len() - 1;
                // https://users.rust-lang.org/t/replacing-element-of-vector/57258/3
                // use `take` so we have a closure that must return a valid T otherwise
                // the closure panics and program aborts incase it panics before we've
                // finished the process of swapping for the new value
                take(csv_row, |mut cr| {
                    // Note: Do not need this lengthy approach. Possibly don't need
                    // `take_mut` either.
                    // // removes elem at index i and swaps last elem into old index i
                    // let old_cell_data = &cr.swap_remove(i);
                    // cr.push(s); // push new elem to end of vector
                    // cr.swap(i, len); // swap new elem into index i
                    // debug!("replaced {:?} with {:?}", old_cell_data, s);

                    // Note: This is a simpler approach to replacing the value
                    core::mem::replace(&mut cr[i], s);
                    cr // must return valid T or it panics
                });
            }
        }
    });
    // debug!("{:?}", csv_row);
    let mut c = 0;
    let new_csv_variables_stringified: String = csv_row
        .iter()
        .map(|text| {
            if c == csv_row.len() - 1 {
                c += 1;
                return text.to_string();
            }
            c += 1;
            return text.to_string() + ",";
        })
        .collect::<String>();
    new_csv_variables_stringified
}

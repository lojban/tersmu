use wasm_bindgen::prelude::*;

use crate::cli::Options;
use crate::run::{json_one_line, parse_line_to_result};

#[wasm_bindgen]
pub fn parse_lojban(input: &str) -> String {
    console_error_panic_hook::set_once();

    let opts = Options {
        json: true,
        utf8: true,
        ..Options::default()
    };
    let result = parse_line_to_result(input);
    json_one_line(&opts, input, &result)
}

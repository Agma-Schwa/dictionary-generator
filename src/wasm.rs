#![allow(unused)]
use std::sync::OnceLock;
use crate::generator::Generator;
use crate::Options;

#[cfg(target_arch = "wasm32")]
use wasm_minimal_protocol::*;

#[cfg(target_arch = "wasm32")]
initiate_protocol!();

#[cfg(target_arch = "wasm32")]
static GENERATOR_INSTANCE: OnceLock<Generator> = OnceLock::new();

#[cfg(target_arch = "wasm32")]
#[wasm_func]
pub fn wasm_parse_dictionary_file(opts: &[u8], content: &[u8]) -> Result<Vec<u8>, String> {
    if GENERATOR_INSTANCE.get().is_some() {
        return Err("wasm_parse_dictionary_file(): Generator already initialised".into());
    }

    if opts.len() != 1 {
        return Err("wasm_parse_dictionary_file(): First argument must be 1 byte".into());
    }

    let mut g = Generator::new(Options {
        pretty_json: false,
        populate_search_fields: false,
        always_include_ipa: opts[0] != 0,
    });

    let Ok(content) = str::from_utf8(content) else {
        return Err("wasm_parse_dictionary_file(): Input was invalid UTF-8".into());
    };

    g.parse(content)?;
    GENERATOR_INSTANCE.set(g).unwrap();
    Ok(vec![])
}

#[cfg(target_arch = "wasm32")]
#[wasm_func]
pub fn wasm_to_ipa(s: &[u8]) -> Result<Vec<u8>, String> {
    let Some(g) = GENERATOR_INSTANCE.get() else {
        return Err("wasm_to_ipa(): Generator not initialised".into());
    };

    let Ok(s) = str::from_utf8(s) else {
        return Err("wasm_to_ipa(): Input was invalid UTF-8".into());
    };

    let node = g.to_ipa(s)?;
    Ok(serde_json::to_string(&node).unwrap().into_bytes())
}

#[cfg(target_arch = "wasm32")]
#[wasm_func]
pub fn wasm_generate_dictionary() -> Result<Vec<u8>, String> {
    let Some(g) = GENERATOR_INSTANCE.get() else {
        return Err("wasm_generate_dictionary(): Generator not initialised".into());
    };

    Ok(g.json().into_bytes())
}

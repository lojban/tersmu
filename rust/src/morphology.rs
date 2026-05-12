//! Word segmentation for the pipeline, matching [Morph.hs](../Morph.hs) interface (`morph`).
//!
//! Haskell uses `Morphology.pappy` through `Morph.hs :: morph`. Rust reuses the camxes-rs word
//! grammar, with targeted grammar rewrites below so word acceptance and error positions match the
//! Haskell morphology parser while keeping the camxes integration self-contained.

use crate::camxes::peg::grammar::Peg;
use std::cell::OnceCell;

thread_local! {
    static WORD_PEG: OnceCell<Peg> = const { OnceCell::new() };
}

// Integration glue for `Morphology.pappy`: adapt camxes-rs surface grammar into the word-level
// morphology entry used by `Morph.hs :: morph`.
fn morphology_grammar() -> String {
    let (_, grammar) = crate::camxes::LOJBAN_GRAMMAR;
    grammar
        // Match Morphology.pappy's brivla classes directly instead of syntax-level valsi.
        .replace(
            "BRIVLA <- !syntax_cmene !syntax_cmavo syntax_valsi",
            "BRIVLA <- gismu / lujvo / fuhivla",
        )
        // Morphology.pappy starts words from generic cmavo forms, so `y+` cmavo are accepted.
        .replace("lojban_word <- CMEVLA / CMAVO / BRIVLA", "lojban_word <- CMEVLA / cmavo / BRIVLA")
        // Match Morphology.pappy word boundaries: a following nucleus can start the next word.
        .replace("post_word <- spaces", "post_word <- pause / !nucleus lojban_word")
        // Preserve Morphology.pappy `cmavo_form = ss:y+` behavior, including uppercase input.
        .replace("cmavo_form <- !h !cluster onset (nucleus h)* (!stressed nucleus / nucleus !cluster) / y+ / digit", "cmavo_form <- !h !cluster onset (nucleus h)* (!stressed nucleus / nucleus !cluster) / [yY]+ / digit")
        // The following rafsi/fu'ivla rewrites mirror Morphology.pappy's word-level acceptance rather
        // than camxes syntax-token constraints.
        .replace(
            "initial_rafsi <- extended_rafsi / y_rafsi / !any_extended_rafsi y_less_rafsi !any_extended_rafsi",
            "initial_rafsi <- extended_rafsi / y_rafsi / !any_extended_rafsi y_less_rafsi",
        )
        .replace(
            "stressed_fuhivla_rafsi <- fuhivla_head stressed_syllable consonantal_syllable* !h onset y",
            "stressed_fuhivla_rafsi <- fuhivla_head stressed_syllable &consonant onset y",
        )
        .replace(
            "fuhivla_rafsi <- &unstressed_syllable fuhivla_head !h onset y h?",
            "fuhivla_rafsi <- &unstressed_syllable fuhivla_head &consonant onset y h?",
        )
        .replace(
            "brivla_head <- !cmavo !slinkuhi !h &onset (!fuhivla_tail unstressed_syllable)*",
            "brivla_head <- !cmavo !slinkuhi !h &onset unstressed_syllable*",
        )
        .replace(
            "slinkuhi <- !rafsi_string consonant rafsi_string",
            "slinkuhi <- consonant rafsi_string",
        )
        .replace(
            "rafsi_string <- y_less_rafsi* (gismu / CVV_final_rafsi / stressed_y_less_rafsi short_final_rafsi / y_rafsi / stressed_y_rafsi / stressed_y_less_rafsi? initial_pair y / hy_rafsi / stressed_hy_rafsi)",
            "rafsi_string <- y_less_rafsi* (gismu / CVV_final_rafsi / stressed_y_less_rafsi short_final_rafsi / y_rafsi / stressed_y_rafsi / stressed_y_less_rafsi? initial_pair y)",
        )
        .replace(
            "y_less_rafsi <- !y_rafsi !stressed_y_rafsi !hy_rafsi !stressed_hy_rafsi (CVC_rafsi / CCV_rafsi / CVV_rafsi) !h",
            "y_less_rafsi <- !y_rafsi (CVC_rafsi !y / CCV_rafsi / CVV_rafsi) !any_extended_rafsi",
        )
        .replace("onset <- h / glide / initial", "onset <- h / consonant? glide / initial")
}

// Integration glue for `Morphology.pappy :: morphologyParse`: build and cache the adapted word PEG.
fn with_word_peg<R>(f: impl FnOnce(&Peg) -> Result<R, usize>) -> Result<R, usize> {
    WORD_PEG.with(|cell| {
        if cell.get().is_none() {
            let grammar = morphology_grammar();
            let peg = Peg::new("lojban_word", &grammar).map_err(|_| 0usize)?;
            let _ = cell.set(peg);
        }
        let peg = cell.get().ok_or(0usize)?;
        f(peg)
    })
}

// Ports the `morphologywords` word-prefix consumption used by `Morph.hs :: morph`.
fn parse_word_prefix(peg: &Peg, input: &str) -> Option<usize> {
    let trimmed_end = input.trim_end().len();
    if trimmed_end > 0 {
        let full_result = peg.parse(&input[..trimmed_end]);
        if full_result.3.as_ref().is_ok() && full_result.1 == trimmed_end {
            return Some(trimmed_end);
        }
    }

    let result = peg.parse(input);
    match result.3.as_ref() {
        Ok(_) if result.1 > 0 => Some(result.1),
        _ => None,
    }
}

// Matches the `nonLojbanWord = ss:"."+` branch in `Morphology.pappy`.
fn non_lojban_word_prefix(input: &str) -> Option<usize> {
    input
        .char_indices()
        .take_while(|(_, ch)| *ch == '.')
        .last()
        .map(|(i, ch)| i + ch.len_utf8())
}

// Mirrors the `nucleus` terminals from `Morphology.pappy` for comma error positioning.
fn is_nucleus_start(ch: char) -> bool {
    matches!(ch, 'a' | 'e' | 'i' | 'o' | 'u' | 'y' | 'A' | 'E' | 'I' | 'O' | 'U' | 'Y')
}

// Preserves `Morphology.pappy` comma/nucleus boundary failure positions reported through `Morph.hs`.
fn comma_boundary_error(input: &str) -> bool {
    let mut chars = input.chars();
    let mut saw_comma = false;

    while matches!(chars.clone().next(), Some(',')) {
        saw_comma = true;
        chars.next();
    }

    saw_comma && matches!(chars.next(), Some(ch) if is_nucleus_start(ch))
}

/// Strip punctuation like [Morph.stripPunc](https://hackage.haskell.org/package/tersmu).
///
/// Ports the Haskell logic:
/// ```haskell
/// stripPunc :: String -> String
/// stripPunc = map $ \c -> if isAlphaNum c || isSpace c || c `elem` ",'" then c else ' '
/// ```
pub fn strip_punc(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_alphanumeric() || c.is_whitespace() || c == ',' || c == '\'' {
                c
            } else {
                ' '
            }
        })
        .collect()
}

/// `morph :: String -> Either Int String` — see Morph.hs.
///
/// Ports the Haskell implementation:
/// ```haskell
/// morph :: String -> Either Int String
/// morph s = let
///         Parsed words d _ = morphologywords $ morphologyParse "words" $ stripPunc s ++ " "
///         p = posCol (dvPos d) - 1
///     in if p < length s
///         then Left p
///         else Right $ map toLower $ unwords $ words
/// ```
///
/// Now uses camxes-rs word-level parsing to validate each word, matching the Haskell
/// Morphology.pappy behavior.
pub fn morph(input: &str) -> Result<String, usize> {
    let stripped = strip_punc(input);
    let parse_input = format!("{stripped} ");
    let mut words = Vec::new();
    let mut pos = 0usize;

    with_word_peg(|peg| {
        while pos < parse_input.len() {
            while pos < parse_input.len() {
                let ch = parse_input[pos..].chars().next().ok_or(pos)?;
                if comma_boundary_error(&parse_input[pos..]) {
                    return Err(pos.saturating_sub(1).min(input.len()));
                } else if ch.is_whitespace() {
                    pos += ch.len_utf8();
                } else {
                    break;
                }
            }

            if pos >= parse_input.len() {
                break;
            }

            let len = parse_word_prefix(peg, &parse_input[pos..])
                .or_else(|| non_lojban_word_prefix(&parse_input[pos..]));

            match len {
                Some(len) => {
                    let word = parse_input[pos..pos + len].replace(',', "");
                    words.push(word);
                    pos += len;
                }
                None => return Err(pos.min(input.len())),
            }
        }
        Ok(())
    })?;

    Ok(words.join(" ").to_lowercase())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_punc_basic() {
        assert_eq!(strip_punc("hello, world!"), "hello, world ");
        assert_eq!(strip_punc("mi'a"), "mi'a");
    }

    #[test]
    fn test_strip_punc_preserves_comma_and_apostrophe() {
        assert_eq!(strip_punc("ki'e,do"), "ki'e,do");
    }

    #[test]
    fn test_morph_simple_sentence() {
        let result = morph("mi klama le zarci");
        if let Err(pos) = &result {
            log::debug!("morph failed at position: {}", pos);
        }
        assert!(result.is_ok(), "morph failed: {:?}", result);
        assert_eq!(result.unwrap(), "mi klama le zarci");
    }

    #[test]
    fn test_morph_with_punctuation() {
        let result = morph(".i coi do!");
        if let Err(pos) = &result {
            log::debug!("morph failed at position: {}", pos);
        }
        assert!(result.is_ok(), "morph failed: {:?}", result);
        // Punctuation should be stripped
        let output = result.unwrap();
        assert!(output.contains("i"));
        assert!(output.contains("coi"));
        assert!(output.contains("do"));
    }

    #[test]
    fn test_morph_empty_input() {
        let result = morph("");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "");
    }

    #[test]
    fn test_morph_whitespace_only() {
        let result = morph("   ");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "");
    }

    #[test]
    fn test_morph_adjacent_cmavo() {
        assert_eq!(morph("pujenaicajeba"), Ok("pu je nai ca je ba".to_string()));
        assert_eq!(morph("pu,je,nai,ca,je,ba"), Ok("pu je nai ca je ba".to_string()));
    }

    #[test]
    fn test_morph_lujvo() {
        assert_eq!(morph("bongnanba"), Ok("bongnanba".to_string()));
    }
}

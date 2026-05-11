use tersmu::{morphology, parse_lojban::parse_text};

fn main() {
    let input = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    let morphed = morphology::morph(&input).expect("morph");
    eprintln!("morphed: {morphed}");
    match parse_text(&morphed) {
        Ok(text) => println!("{:#?}", text),
        Err(pos) => eprintln!("parse error at {pos}"),
    }
}

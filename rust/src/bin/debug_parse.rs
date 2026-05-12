use tersmu::{morphology, parse_lojban::parse_text};

fn main() {
    env_logger::init();
    let input = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    let morphed = morphology::morph(&input).expect("morph");
    log::debug!("morphed: {morphed}");
    match parse_text(&morphed) {
        Ok(text) => println!("{:#?}", text),
        Err(pos) => log::error!("parse error at {pos}"),
    }
}

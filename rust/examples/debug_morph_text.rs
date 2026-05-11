fn main() {
    let input = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    let morphed = tersmu::morphology::morph(&input).unwrap();
    eprintln!("{morphed}");
    let text = tersmu::parse_lojban::parse_text(&format!("{morphed} %%%END%%%")).unwrap();
    println!("{:#?}", text);
}

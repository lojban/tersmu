fn main() {
    let input = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    let morphed = camxes_rs::morphology::morph(&input).unwrap();
    eprintln!("{morphed}");
    let text = camxes_rs::parse_lojban::parse_text(&format!("{morphed} %%%END%%%")).unwrap();
    println!("{:#?}", text);
}

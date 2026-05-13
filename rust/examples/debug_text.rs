fn main() {
    let input = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    let text = camxes_rs::parse_lojban::parse_text(&input).unwrap();
    println!("{:#?}", text);
}

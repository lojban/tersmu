fn main() {
    let input = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    let text = tersmu::parse_lojban::parse_text(&input).unwrap();
    println!("{:#?}", text);
}

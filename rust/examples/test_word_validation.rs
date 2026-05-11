fn main() {
    let words = vec!["nin,mu", "ninmu", "bongnanba", "mi", "klama"];
    for word in words {
        let result = tersmu::morphology::morph(word);
        println!("{}: {:?}", word, result);
    }
}

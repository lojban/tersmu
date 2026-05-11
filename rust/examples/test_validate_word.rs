fn main() {
    // Access the private validate_word through morph
    let test_words = vec![
        "nin,mu",
        "ninmu", 
        "mi",
        "i",
        "ai",
        ".i.ai.i.ai.o",
    ];
    
    for word in test_words {
        let result = tersmu::morphology::morph(word);
        println!("{:20} -> {:?}", word, result);
    }
}

fn main() {
    let inputs = vec!["nin,mu", "fit,pri", ".i,ai,i,ai,on."];
    for input in inputs {
        let stripped = tersmu::morphology::strip_punc(input);
        let result = tersmu::morphology::morph(input);
        println!("{:20} -> stripped: {:?}, morph: {:?}", input, stripped, result);
    }
}

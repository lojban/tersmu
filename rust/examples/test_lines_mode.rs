use std::time::Instant;

fn main() {
    let inputs = vec!["mi klama", "do tavla", "ti zdani"];
    
    let start = Instant::now();
    for (i, input) in inputs.iter().enumerate() {
        let t = Instant::now();
        let _ = tersmu::parse_lojban::parse_text(input);
        println!("Parse {}: {:?}", i+1, t.elapsed());
    }
    println!("Total: {:?}", start.elapsed());
}

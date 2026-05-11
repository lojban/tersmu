use std::time::Instant;

fn main() {
    let start = Instant::now();
    
    let t0 = Instant::now();
    let input = "mi klama";
    println!("Input prep: {:?}", t0.elapsed());
    
    let t1 = Instant::now();
    let result = tersmu::parse_lojban::parse_text(input);
    println!("parse_text: {:?}", t1.elapsed());
    
    let t2 = Instant::now();
    if let Ok(text) = result {
        let _ = tersmu::jbo_parse::eval_text(&text);
    }
    println!("eval_text: {:?}", t2.elapsed());
    
    println!("Total: {:?}", start.elapsed());
}

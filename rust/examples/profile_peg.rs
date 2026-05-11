use std::time::Instant;

fn main() {
    let start = Instant::now();
    
    // First call - will compile PEG
    let t1 = Instant::now();
    let _ = tersmu::parse_lojban::parse_text("mi klama");
    println!("First parse (with PEG compilation): {:?}", t1.elapsed());
    
    // Second call - PEG should be cached in thread-local
    let t2 = Instant::now();
    let _ = tersmu::parse_lojban::parse_text("do tavla");
    println!("Second parse (PEG cached): {:?}", t2.elapsed());
    
    // Third call
    let t3 = Instant::now();
    let _ = tersmu::parse_lojban::parse_text("ti zdani");
    println!("Third parse (PEG cached): {:?}", t3.elapsed());
    
    println!("Total: {:?}", start.elapsed());
}

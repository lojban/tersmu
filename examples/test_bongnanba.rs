use std::io::Write;

fn main() {
    let input = "bongnanba";
    
    // Test morphology
    match tersmu::morphology::morph(input) {
        Ok(morphed) => {
            println!("Morphology OK: {}", morphed);
            
            // Test parse
            let with_end = format!("{} %%%END%%%", morphed);
            match tersmu::parse_lojban::parse_text(&with_end) {
                Ok(parsed) => {
                    println!("Parse OK: {:?}", parsed);
                    
                    // Test eval
                    let (logical, canonical, graph) = tersmu::eval_show::eval_text_to_outputs(&parsed);
                    println!("Logical: {}", logical);
                    println!("Canonical: {}", canonical);
                    println!("Graph: {}", graph);
                }
                Err(pos) => println!("Parse error at position {}", pos),
            }
        }
        Err(pos) => println!("Morphology error at position {}", pos),
    }
}

use std::time::Instant;

fn main() {
    let start = Instant::now();
    let input = "mi klama le zarci .i do prami mi";

    let t0 = Instant::now();
    let morphed = camxes_rs::morphology::morph(input).expect("morphology");
    println!("morphology: {:?}", t0.elapsed());

    let t1 = Instant::now();
    let parsed = camxes_rs::parse_lojban::parse_text(&morphed).expect("parse_text");
    println!("parse_text: {:?}", t1.elapsed());

    let t2 = Instant::now();
    let eval = camxes_rs::jbo_parse::eval_text(&parsed);
    println!("eval_text: {:?}", t2.elapsed());

    let t3 = Instant::now();
    let _ = camxes_rs::eval_show::eval_text_to_outputs_with_options(&parsed, true);
    println!("render_outputs: {:?}", t3.elapsed());

    println!("semantic_results: {}", eval.len());
    println!("total: {:?}", start.elapsed());
}

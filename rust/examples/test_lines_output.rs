fn main() {
    let opts = camxes_rs::cli::Options {
        input: camxes_rs::cli::InputType::Lines,
        ..Default::default()
    };
    
    println!("Input type: {:?}", opts.input);
    println!("Is Lines: {}", matches!(opts.input, camxes_rs::cli::InputType::Lines));
}

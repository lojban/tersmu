fn main() {
    let opts = tersmu::cli::Options {
        input: tersmu::cli::InputType::Lines,
        ..Default::default()
    };
    
    println!("Input type: {:?}", opts.input);
    println!("Is Lines: {}", matches!(opts.input, tersmu::cli::InputType::Lines));
}

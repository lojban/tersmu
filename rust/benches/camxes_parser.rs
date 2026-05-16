//! Benchmarks: grammar startup and parse for the embedded Lojban PEG.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use camxes_rs::camxes::peg::grammar::Peg;
use camxes_rs::camxes::LOJBAN_GRAMMAR;

fn bench_grammar_startup(c: &mut Criterion) {
    c.bench_function("peg_new_lojban_grammar", |b| {
        b.iter(|| {
            let (start, grammar) = LOJBAN_GRAMMAR;
            black_box(Peg::new(start, grammar).unwrap())
        })
    });
}

fn bench_parse(c: &mut Criterion) {
    let (start, grammar) = LOJBAN_GRAMMAR;
    let peg = Peg::new(start, grammar).unwrap();

    c.bench_function("parse_short", |b| {
        b.iter(|| black_box(peg.parse(black_box("mi prami do"))))
    });

    c.bench_function("parse_medium", |b| {
        let input = "mi prami do .i do prami mi ".repeat(20);
        b.iter(|| black_box(peg.parse(black_box(input.as_str()))))
    });
}

fn bench_full_pipeline(c: &mut Criterion) {
    c.bench_function("parse_text_short", |b| {
        b.iter(|| black_box(camxes_rs::parse_lojban::parse_text(black_box("mi prami do"))))
    });

    c.bench_function("parse_text_medium", |b| {
        let input = "mi prami do .i do prami mi ".repeat(20);
        b.iter(|| black_box(camxes_rs::parse_lojban::parse_text(black_box(input.as_str()))))
    });

    c.bench_function("full_pipeline_short", |b| {
        b.iter(|| {
            let input = black_box("mi prami do");
            let morphed = camxes_rs::morphology::morph(input).unwrap();
            let parsed = camxes_rs::parse_lojban::parse_text(&morphed).unwrap();
            black_box(camxes_rs::eval_show::eval_text_to_outputs_with_options(&parsed, true))
        })
    });
}

criterion_group!(benches, bench_grammar_startup, bench_parse, bench_full_pipeline);
criterion_main!(benches);

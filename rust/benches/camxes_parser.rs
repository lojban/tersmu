//! Benchmarks: grammar startup and parse for the embedded Lojban PEG.

use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::LOJBAN_GRAMMAR;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

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

criterion_group!(benches, bench_grammar_startup, bench_parse);
criterion_main!(benches);

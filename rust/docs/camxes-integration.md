# Camxes integration

`camxes` is integrated into the Rust `tersmu` crate as `tersmu::camxes`. It is no longer a separate `camxes-rs` Cargo package or path dependency in this repository.

## Location

```text
rust/src/camxes/
  mod.rs
  grammar/lojban.peg
  peg/
    grammar/
    rule/
    parsing.rs
    semantic.rs
    transformer/
```

The full Lojban PEG grammar is embedded by `tersmu::camxes::LOJBAN_GRAMMAR` from `src/camxes/grammar/lojban.peg`.

## Runtime role

The Rust parser path uses camxes as its PEG engine:

```text
morphology/preparse → camxes PEG parse → semantic reducers → JboSyntax/JboParse → logic/show/graph output
```

The Haskell grammar and semantic modules remain the reference for behavior. Rust camxes integration code should either mirror named Haskell behavior directly or document the minimal integration difference required by the PEG engine.

## Validation

From `rust/`:

```bash
cargo test --test camxes_lojban_grammar
cargo test --test camxes_semantic_actions
cargo build --examples
cargo bench --bench camxes_parser --no-run
```

After changing camxes grammar or parser behavior, rebuild `tersmu` and run the golden examples:

```bash
cargo build --release --bin tersmu
./test_all_examples.sh
```

The golden examples live in `../examples` and must remain read-only.

# Rust port plan: Haskell `tersmu` → full Rust stack

This document is the roadmap for replacing the Haskell implementation with a **self-contained Rust** toolchain using the integrated `tersmu::camxes` parser instead of porting Pappy as the runtime parser. It records what is **already implemented**, what remains, and how to **validate** parity using the existing example suite.

**Last updated:** 2026-05-11 00:00 UTC

## Core Principle: Haskell is the Source of Truth

**The Haskell implementation is the authoritative specification.** All Rust code must be ported directly from Haskell, not rewritten or "improved."

**Always check the Haskell code carefully.** Do not infer behavior only from current Rust output; verify the original Haskell logic and source code first, then mirror it in Rust.

**After any camxes parser or grammar change, rebuild before testing.** Parser changes are not valid until the affected Rust binary has been rebuilt.

**Remove Rust-only hacks and deviations from Haskell.** Every Rust implementation choice must either be a direct port of a named Haskell source location or a minimal integration adaptation required by Rust/camxes. Existing Rust behavior without a Haskell reference is technical debt: either document the Haskell original it ports, or replace it with a direct Haskell-backed port. Hardcoding is only acceptable when the Haskell code hardcodes the same behavior.

---

## Current Status (2026-05-11)

**Current validated state:**
- ✅ Rust CLI, morphology wrapper, integrated camxes parsing pipeline, semantic passes, show pipeline, JSON graph path, and release build path are in place.
- ✅ `./test_all_examples.sh` from `rust/` passes **20/20** against the checked-in golden `.loj` files.
- ✅ The Rust binary is the active self-contained validation target; Haskell remains the source of truth for behavior, not a runtime dependency for normal Rust validation.
- ✅ `jbo_tree.rs` has a functional `JboTree.hs`-style proposition-to-graph implementation for JSON output instead of the old placeholder.
- ✅ Recent audit work removed or documented the actionable `TODO`, `simplified`, `placeholder`, `for now`, and `hack` markers in parser, semantic, show, morphology, and graph code. Remaining matches should be rechecked before acting because some are benign words, URLs, or formatter placeholder terminology.

**Completed foundation:**
- ✅ `ParseText.hs` / `Morph.hs` behavior is represented by Rust morphology/preparse glue and integrated camxes parser entry points.
- ✅ `ParseM.hs` infrastructure is substantially ported through `parse_m.rs`, including `BridiParseState`, argument threading, sumbasti/bribasti bindings, side texticules, incidentals, assignment helpers, finite domains, and proposition transforms.
- ✅ `JboSyntax.hs` AST shapes are represented in `jbo_syntax.rs` for the validated example-suite surface forms.
- ✅ Large parts of `JboParse.hs`, `JboProp.hs`, `Logic.hs`, and `JboShow.hs` are mirrored in Rust and should continue to be maintained as direct Haskell ports.
- ✅ Integrated camxes is the chosen parser path; do not revive Pappy generation for the Rust implementation.
- ✅ Pappy-style error tracking is implemented in the integrated camxes code, including furthest-error propagation through combinators and rule-specific parity fixes observed in the golden suite.

**Recently validated parity areas:**
- ✅ Morphology, Lojbanize/name handling, indicators/frees, comma-internal rafsi/fu'ivla behavior, invalid gloss-tail morphology, and parse-error caret cases.
- ✅ JOI connected sumti, `ek`/`joik_ek` reducer preservation, `Equal`/`Among` show special cases, `NAhE` scalar negation with sub-bridi scoped `parsedSelbriToNewSelbri` / `closeBridi` behavior, side predicate output, omitted x1 formatting, `TUMe`, and `TUZei`.
- ✅ BE/linkarg scoping, non-veridical type handling, side proposition ordering, quantified `me`, scalar-negated place gaps, `na` modal ordering, and tail-term / VI-tag behavior.
- ✅ Assignment semantics, anaphora / `go'i`, relation-variable display (`bu'a` vs `broda`), relative-variable shunting, prenex propagation/isolation, relative-clause attachment, and GOI/prenex assignment behavior.
- ✅ Free-only continuations, `sei`/`se'u` sentence frees, `xu` truth questions after discursive clauses, and Pappy-compatible parse-error windows.
- ✅ Connected bridi/term continuation semantics, forethought/afterthought branch argument isolation, interval JOIK endpoint preservation, connected mex quantifiers/operators, tagged logical connectives, bare GIhA/JOI fragments, and `li'o` fragment display.
- ✅ Parser/semantic cleanup work for `mPredToVDom`, `stripForeRestrictives`, `TUXOhI`/`TagRel`, bridi-question `kau` depth, unparsed fragments, and JSON graph conversion.

**Known constraints / maintenance notes:**

1. **Haskell remains authoritative:** When a future diff appears, inspect the Haskell grammar/function path first and mirror that specific behavior. Do not add generic Rust heuristics.

2. **Parser/reducer changes require rebuild:** Always rebuild the Rust release binary after parser, reducer, morphology, semantic, or show changes before trusting example diffs.

3. **Test suite performance:** Full-suite runs can be slow or noisy during iteration. Prefer targeted temporary diffs while fixing one root cause at a time, then run the full suite before declaring completion. `test_fast.sh` ignores numeric arguments and starts the whole suite; do not use it for a single example unless the script is changed first.

4. **Stack overflow on large texts:** Parsing many lines as one text can stack overflow in integrated camxes. Use line mode (`-L`) or line-oriented scripts for validation.

5. **Examples are read-only:** Do not edit files under `examples/`; write actual outputs and diffs to temporary files outside `examples/`.

**Ongoing work:**
- Keep Rust code aligned with named Haskell source locations as new edge cases are found.
- Continue removing undocumented Rust-only behavior when audits find it: either add a verified Haskell reference or replace the code with a direct Haskell-backed port.
- Re-run `cargo build --release --bin tersmu` and `./test_all_examples.sh` after any meaningful parser, semantic, show, morphology, or graph change.

---

## 1. Goals


| Goal                               | Description                                                                                                                                               |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **No Haskell runtime for parsing** | Users can build and run `tersmu` with only Rust + Cargo (optional: keep Haskell for comparison during development).                                       |
| **Use integrated camxes parser**           | Leverage the integrated Lojban PEG parser instead of porting Pappy compiler infrastructure for the runtime path.                                                   |
| **Semantic parity**                | Port `JboParse`, `ParseText`, `ParseM`, `Logic`, `JboProp`, etc., so the meaning pipeline matches Haskell.                                                |
| **Direct Haskell port**            | **Always rely on Haskell logic as the source of truth.** Port Haskell code directly to Rust, documenting the source file and function in Rust comments.   |
| **Regression safety**              | `test_all_examples.sh` passes **20/20** against the Rust binary with the same golden `.loj` files as Haskell.                                             |


---

## Porting Philosophy

**Core principle:** The Haskell implementation is the **authoritative specification**. When porting:

1. **Port directly from Haskell** — Don't rewrite or "improve" the logic. Mirror the Haskell structure.
2. **Document the source** — Every ported function/module must include a comment referencing the Haskell source:
   ```rust
   // Ported from: JboParse.hs :: evalBridi
   fn eval_bridi(state: &mut ParseState, terms: &[Term], tail: &BridiTail) -> Option<SemanticResult> {
       // ...
   }
   ```
3. **Preserve semantics** — If Haskell uses a specific algorithm (e.g., nudgeFrees, binding resolution), port it exactly.
4. **Adapt only for integration** — The only changes should be:
   - Rust syntax/idioms (Result instead of Either, Vec instead of lists)
   - Integration with integrated camxes parser (instead of Pappy)
   - CLI interface differences
5. **Rebuild after parser changes** — After any camxes grammar, reducer, or parser change, rebuild the Rust binary before running validation.
6. **Remove undocumented Rust-only behavior** — If Rust code lacks a reference to the Haskell function/rule it ports, treat that as an issue to solve: add the Haskell reference after verification, or replace the code with a direct port. Rust-only hacks, heuristics, and deviations are not acceptable unless they are explicitly required integration glue and documented as such.

**Anti-patterns to avoid:**
- ❌ "Improving" the algorithm while porting
- ❌ Guessing what the Haskell code does without reading it
- ❌ Guessing from current Rust output instead of checking original Haskell logic and code
- ❌ Testing camxes parser changes without rebuilding first
- ❌ Hardcoding test outputs instead of implementing the logic
- ❌ Adding Rust-only hacks, heuristics, or deviations absent from Haskell
- ❌ Leaving Rust parser/semantic/show code without a verified Haskell source reference
- ❌ Skipping intermediate steps that seem unnecessary

---

## Reference Architecture

**Haskell flow:**
1. Morphology — `Morphology.pappy` → `Morphology.hs`
2. Lojban surface — `Lojban.pappy` → `Lojban.hs`
3. Semantics — `JboParse.hs` maps parse trees to logic (`Logic.hs`, `JboProp.hs`)
4. CLI — `Main.hs` (flags, REPL, files, JSON)

**Rust flow:**
integrated camxes parser → preparsing / morph → semantic passes → CLI

**Key change:** Replace Pappy-generated parsers with integrated camxes PEG parser. Port all semantic modules directly from Haskell.

---

## Haskell Modules to Port

| Module                   | Role                                                        | Rust File                | Status        |
| ------------------------ | ----------------------------------------------------------- | ------------------------ | ------------- |
| `ParseText.hs`           | Preparsing, morph, `%%%END%%%`, calls into `Lojban` parser. | `parse_lojban.rs`, `morphology.rs` | ✅ Functional |
| `JboParse.hs`            | Central semantic pass over surface syntax.                  | `jbo_parse.rs`           | ✅ Functional for golden parity; continue Haskell-reference audits |
| `ParseM.hs`              | Monadic context for parsing / name state.                   | `parse_m.rs`             | ✅ Substantially ported; verify edge cases |
| `Bindful.hs`             | Binding / scope.                                            | `bindful.rs`             | ❌ Not started |
| `JboSyntax.hs`           | AST types.                                                  | `jbo_syntax.rs`          | ✅ Functional |
| `JboProp.hs`, `Logic.hs` | Predicate logic IR.                                         | `jbo_prop.rs`, `logic.rs`| ✅ Functional for golden parity; verify new edge cases against Haskell |
| `JboShow.hs`             | Pretty-print / asciify.                                     | `jbo_show.rs`            | ✅ Functional for golden parity; maintain direct Haskell references |
| `JboTree.hs`             | JSON tree (used with `--json`).                             | `jbo_tree.rs`            | ✅ Functional graph conversion port; verify future JSON edge cases |
| `Morph.hs`               | Wrapper around morphology parser.                           | `morphology.rs`          | ✅ Functional with targeted parity fixes |
| `Util.hs`                | Shared helpers.                                             | `util.rs`                | ✅ Functional |

---

---

## Validation

**Test script:** `test_all_examples.sh` in `rust/`
- **Inputs:** `examples/1.jbo` … `examples/20.jbo`
- **Goldens:** `examples/1.loj` … `examples/20.loj`
- **Pass criteria:** 20/20 PASS with diff against `.loj` files
- no cheating: don't hardcode constructs just to pass tests. bee generic and rely on the Haskell code as the source of truth.
- **No test hangs:** All tests must complete within 5 seconds per example

**Correct Rust-port test workflow:**
1. The Rust crate root is `/home/user/lojban/tersmu/rust`; golden example inputs/outputs live in `../examples`.
2. After any Rust parser/semantic/show/morphology change, rebuild the release binary before validating:
   ```bash
   cargo build --release --bin tersmu
   ```
3. For a full checkpoint from the repository root:
   ```bash
   ./test_all_examples.sh
   ```
4. Treat `examples/` as read-only. Do not edit golden/input files under `../examples`, and do not write generated validation artifacts under `../examples`.
5. For one-example iteration, avoid `test_fast.sh N` because it ignores `N`. Use temporary files outside `examples/` for both actual output and diffs:
   ```bash
   cargo build --release --bin tersmu
   ACTUAL=$(mktemp)
   DIFF=$(mktemp)
   ./target/release/tersmu -L < ../examples/8.jbo 2>&1 | grep -v '^DEBUG' > "$ACTUAL" || true
   diff -u ../examples/8.loj "$ACTUAL" > "$DIFF" || true
   wc -l "$DIFF"
   rm -f "$ACTUAL" "$DIFF"
   ```
6. During iteration, inspect temporary diffs, then fix one Haskell-backed root cause at a time.

The test suite is the acceptance gate for declaring the Rust port complete.

**Muplis database corpus examples:**
- `examples/muplis-database/` contains additional `.jbo` / `.loj` golden pairs generated from the `Lojban` column of `/home/user/lojban/korpora2/korpora/muplis-database.tsv`.
- Generation rule: run each TSV sentence through the current Haskell WASM parser in `wasm-web-app/tersmu.wasm`, skip rows with parse, morphology, empty-result, or WASM runtime errors, and save only Haskell-parsing sentences.
- Current corpus size: 945 generated `.jbo` / `.loj` pairs. Treat the generation script output as authoritative for exact accepted/skipped TSV counts when regenerating.
- Treat this corpus as a secondary Rust-port validation set. The checked-in `.loj` files are current Haskell WASM goldens, so Rust diffs should be investigated against the Haskell source before changing behavior.

**Muplis Rust parity comparison (2026-05-11):**
- Command shape: build `rust/target/release/tersmu`, then run each `examples/muplis-database/N.jbo` through Rust with `-u -L` and compare against `examples/muplis-database/N.loj`.
- Current result: 945/945 exact matches; 0 mismatches; 0 Rust panics; 0 timeouts; 0 nonzero exits.
- Latest comparison used a 12-worker Python `ProcessPoolExecutor` harness with a 10-second per-example timeout. Prefer this parallel harness over the old serial loop for full-corpus checks.
- Comparison artifacts: `/tmp/muplis-rust-par-5kf0rfc6`; summary: `/tmp/muplis-rust-par-5kf0rfc6/summary.json`.
- Previously failing IDs `2,165,193,194,195,199,264,303,368,369,493,500,719,720,847,854` now pass.

The previous muplis mismatch categories are resolved in the current validation run. Continue treating future muplis diffs as Rust-port bugs to fix by checking the relevant Haskell grammar, semantic, or show path first, then porting that behavior directly.

**Current Haskell WASM vs legacy 19 goldens:**
- See `rust/docs/wasm-golden-examples-report.md`.
- Current `wasm-web-app/tersmu.wasm` matches all 19 checked-in upstream golden examples when validated through the ASCII compatibility export.
- The UTF-8 `parseLojban` export remains the default browser/API mode; use `parseLojbanAscii` for byte-for-byte validation against ASCII legacy goldens.

---

## Documentation Requirements

Every Rust parser, semantic, show, morphology, or logic implementation must identify its Haskell original. Lack of a Haskell source reference is an issue to resolve, not an acceptable final state. For each undocumented Rust function or behavior, either add the verified Haskell source reference or replace the implementation with a direct Haskell-backed port. Integration-only differences for Rust/camxes must be explicitly documented as integration glue.

When porting Haskell code, always document the source:

```rust
//! Module description
//!
//! Ported from: FileName.hs

// Ported from: FileName.hs :: functionName
// Original Haskell signature:
//   functionName :: Type1 -> Type2 -> Type3
fn function_name(arg1: Type1, arg2: Type2) -> Type3 {
    // Preserve the algorithm exactly from Haskell
}
```


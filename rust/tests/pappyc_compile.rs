//! End-to-end test: `pappyc -o OUT.rs INPUT.pappy` (`CARGO_BIN_EXE_pappyc` is set by Cargo).

const MIN_GRAMMAR: &str = r#"parser min:

{
}

top start

start :: () = "a"
"#;

#[test]
fn pappyc_writes_rust_from_minimal_grammar() {
    let dir = std::env::temp_dir().join(format!("tersmu_pappyc_e2e_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let input = dir.join("g.pappy");
    let output = dir.join("g_out.rs");
    std::fs::write(&input, MIN_GRAMMAR).expect("write grammar");

    let exe = env!("CARGO_BIN_EXE_pappyc");
    let status = std::process::Command::new(exe)
        .arg("-o")
        .arg(&output)
        .arg(&input)
        .status()
        .expect("spawn pappyc");

    assert!(status.success(), "pappyc exit {status:?}");

    let rust = std::fs::read_to_string(&output).expect("read emitted .rs");
    assert!(
        rust.contains("pub struct Min") && rust.contains("fn min_parse_start"),
        "expected emitted module shape"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn pappyc_default_output_replaces_pappy_extension() {
    let dir = std::env::temp_dir().join(format!("tersmu_pappyc_defo_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let input = dir.join("min.pappy");
    let expected_rs = dir.join("min.rs");
    std::fs::write(&input, MIN_GRAMMAR).expect("write grammar");

    let exe = env!("CARGO_BIN_EXE_pappyc");
    let status = std::process::Command::new(exe)
        .arg(&input)
        .status()
        .expect("spawn pappyc INPUT only");

    assert!(status.success(), "pappyc exit {status:?}");
    assert!(
        expected_rs.is_file(),
        "expected default output {}",
        expected_rs.display()
    );
    let rust = std::fs::read_to_string(&expected_rs).expect("read min.rs");
    assert!(
        rust.contains("pub struct Min") && rust.contains("fn min_parse_start"),
        "expected emitted module shape"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn pappyc_verbose_logs_input_and_output() {
    let dir = std::env::temp_dir().join(format!("tersmu_pappyc_v_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let input = dir.join("g.pappy");
    let output = dir.join("g_verbose.rs");
    std::fs::write(&input, MIN_GRAMMAR).expect("write grammar");

    let exe = env!("CARGO_BIN_EXE_pappyc");
    let out = std::process::Command::new(exe)
        .arg("-v")
        .arg("-o")
        .arg(&output)
        .arg(&input)
        .output()
        .expect("spawn pappyc -v");

    assert!(
        out.status.success(),
        "stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("input=")
            && stderr.contains("output=")
            && stderr.contains("wrote"),
        "stderr={stderr}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn pappyc_verbose_short_matches_long() {
    let dir = std::env::temp_dir().join(format!("tersmu_pappyc_v_eq_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let input = dir.join("g.pappy");
    let output = dir.join("g_eq.rs");
    std::fs::write(&input, MIN_GRAMMAR).expect("write grammar");

    let exe = env!("CARGO_BIN_EXE_pappyc");
    let short = std::process::Command::new(exe)
        .arg("-v")
        .arg("-o")
        .arg(&output)
        .arg(&input)
        .output()
        .expect("spawn -v");
    let long = std::process::Command::new(exe)
        .arg("--verbose")
        .arg("-o")
        .arg(&output)
        .arg(&input)
        .output()
        .expect("spawn --verbose");

    assert!(short.status.success(), "short stderr={}", String::from_utf8_lossy(&short.stderr));
    assert!(long.status.success(), "long stderr={}", String::from_utf8_lossy(&long.stderr));
    assert_eq!(
        String::from_utf8_lossy(&short.stderr),
        String::from_utf8_lossy(&long.stderr)
    );

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn pappyc_output_short_matches_long() {
    let dir = std::env::temp_dir().join(format!("tersmu_pappyc_o_eq_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let input = dir.join("g.pappy");
    let out_short = dir.join("via_dash_o.rs");
    let out_long = dir.join("via_output.rs");
    std::fs::write(&input, MIN_GRAMMAR).expect("write grammar");

    let exe = env!("CARGO_BIN_EXE_pappyc");
    let s = std::process::Command::new(exe)
        .arg("-o")
        .arg(&out_short)
        .arg(&input)
        .status()
        .expect("spawn -o");
    let l = std::process::Command::new(exe)
        .arg("--output")
        .arg(&out_long)
        .arg(&input)
        .status()
        .expect("spawn --output");

    assert!(s.success(), "pappyc -o exit {s:?}");
    assert!(l.success(), "pappyc --output exit {l:?}");
    assert_eq!(
        std::fs::read_to_string(&out_short).expect("read -o"),
        std::fs::read_to_string(&out_long).expect("read --output")
    );

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn pappyc_exits_nonzero_on_invalid_grammar() {
    let dir = std::env::temp_dir().join(format!("tersmu_pappyc_bad_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let input = dir.join("bad.pappy");
    let output = dir.join("out.rs");
    std::fs::write(&input, "!!! not pappy !!!\n").expect("write");

    let exe = env!("CARGO_BIN_EXE_pappyc");
    let out = std::process::Command::new(exe)
        .arg("-o")
        .arg(&output)
        .arg(&input)
        .output()
        .expect("spawn pappyc");

    assert!(!out.status.success(), "expected nonzero exit");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("pappyc:"), "stderr={stderr:?}");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn pappyc_exits_nonzero_on_missing_input_file() {
    let ghost = std::env::temp_dir().join(format!(
        "tersmu_pappyc_ghost_{}.pappy",
        std::process::id()
    ));
    assert!(
        !ghost.exists(),
        "precondition: path must not exist: {}",
        ghost.display()
    );

    let exe = env!("CARGO_BIN_EXE_pappyc");
    let out = std::process::Command::new(exe)
        .arg(&ghost)
        .output()
        .expect("spawn pappyc");

    assert!(!out.status.success(), "expected nonzero exit");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("pappyc:"), "stderr={stderr:?}");
}

#[test]
fn pappyc_exits_usage_on_unknown_flag() {
    let exe = env!("CARGO_BIN_EXE_pappyc");
    let out = std::process::Command::new(exe)
        .arg("--not-a-real-flag")
        .output()
        .expect("spawn pappyc");

    assert_eq!(out.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("Usage:") && stderr.contains("pappyc"),
        "stderr={stderr:?}"
    );
}

#[test]
fn pappyc_exits_usage_when_o_has_no_value() {
    let exe = env!("CARGO_BIN_EXE_pappyc");
    let out = std::process::Command::new(exe).arg("-o").output().expect("spawn pappyc -o");

    assert_eq!(out.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("Usage:") && stderr.contains("pappyc"),
        "stderr={stderr:?}"
    );
}

#[test]
fn pappyc_exits_usage_on_extra_positional() {
    let exe = env!("CARGO_BIN_EXE_pappyc");
    let out = std::process::Command::new(exe)
        .arg("/tmp/tersmu_pappyc_first_dummy.pappy")
        .arg("/tmp/tersmu_pappyc_second_dummy.pappy")
        .output()
        .expect("spawn pappyc two inputs");

    assert_eq!(out.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("Usage:") && stderr.contains("pappyc"),
        "stderr={stderr:?}"
    );
}

//! Integration tests for the `pappyc` binary (`CARGO_BIN_EXE_pappyc` is set by Cargo).

fn help_output(flag: &str) -> String {
    let exe = env!("CARGO_BIN_EXE_pappyc");
    let out = std::process::Command::new(exe)
        .arg(flag)
        .output()
        .unwrap_or_else(|e| panic!("spawn pappyc {flag}: {e}"));
    assert!(
        out.status.success(),
        "flag={flag} status={:?} stderr={}",
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    let stdout = String::from_utf8_lossy(&out.stdout);
    format!("{stdout}{stderr}")
}

#[test]
fn pappyc_help_exits_zero() {
    let combined = help_output("--help");
    assert!(
        combined.contains("Usage:") && combined.contains("pappyc"),
        "{combined}"
    );
}

#[test]
fn pappyc_short_help_matches_long() {
    let long = help_output("--help");
    let short = help_output("-h");
    assert_eq!(long, short);
}

use std::{
    fs::File, io::{prelude::*, BufReader, Write}, path::{Path, PathBuf}, process::{Command, Stdio}, sync::mpsc, thread, time::Duration
};

#[macro_export]
macro_rules! success_tests {
    ($($name:ident: $expected:literal),* $(,)?) => {
        $(
        #[test]
        fn $name() {
            $crate::infra::run_success_test(stringify!($name), $expected);
        }
        )*
    }
}
#[macro_export]
macro_rules! failure_tests {
    ($($name:ident: $expected:literal),* $(,)?) => {
        $(
        #[test]
        fn $name() {
            $crate::infra::run_failure_test(stringify!($name), $expected);
        }
        )*
    }
}

#[macro_export]
macro_rules! repl_tests {
    ($($name:ident: [$($command:literal),*] => [$($expected:literal),*]),* $(,)?) => {
        $(
        #[test]
        fn $name() {
            let commands = vec![$($command),*];
            let expected_outputs = vec![$($expected),*];
            $crate::infra::run_repl_sequence_test(stringify!($name), &commands, &expected_outputs);
        }
        )*
    }
}

fn build() {
    let status = if cfg!(target_os = "macos") {
        Command::new("cargo")
            .arg("build")
            .arg("--target")
            .arg("x86_64-apple-darwin")
            .status()
            .expect("could not run argo")
    } else {
        Command::new("cargo")
            .arg("build")
            .status()
            .expect("could not run cargo")
    };
    assert!(status.success(), "could not build the project");
}

fn compile(name: &str) -> Result<(String, String), String> {
    build();

    // Run the compiler
    let boa_path = if cfg!(target_os = "macos") {
        PathBuf::from("target/x86_64-apple-darwin/debug/boa")
    } else {
        PathBuf::from("target/debug/boa")
    };
    let output_c = Command::new(&boa_path)
        .arg("-c")
        .arg(&mk_path(name, Ext::Snek))
        .arg(&mk_path(name, Ext::Asm))
        .output()
        .expect("could not run the compiler");
    if !output_c.status.success() {
        return Err(String::from_utf8(output_c.stderr).unwrap());
    }

    let output_e = Command::new(&boa_path)
        .arg("-e")
        .arg(&mk_path(name, Ext::Snek))
        .output()
        .expect("could not run the compiler");
    if !output_e.status.success() {
        return Err(String::from_utf8(output_e.stderr).unwrap());
    }
    let jit_stdout = String::from_utf8(output_e.stdout).unwrap();

    eprintln!("JIT result: {}", jit_stdout);

    // Assemble and link
    let output = Command::new("make")
        .arg(&mk_path(name, Ext::Run))
        .output()
        .expect("could not run make");
    assert!(output.status.success(), "linking failed");

    // Run produced program and capture stdout
    let run_path = mk_path(name, Ext::Run);
    let output_run = Command::new(&run_path)
        .output()
        .expect("could not run compiled program");
    if !output_run.status.success() {
        return Err(String::from_utf8(output_run.stderr).unwrap());
    }
    let run_stdout = String::from_utf8(output_run.stdout).unwrap();

    Ok((jit_stdout, run_stdout))
}

pub(crate) fn run_success_test(name: &str, expected: &str) {
    let (jit_out, run_out) = match compile(name) {
        Ok((jit, run)) => (jit, run),
        Err(err) => panic!("expected a successful compilation, but got an error: `{}`", err),
    };

    let expected_trim = expected.trim();

    let jit_trim = jit_out.trim();
    let run_trim = run_out.trim();

    let mut failed_flags = Vec::new();

    if expected_trim != jit_trim {
        failed_flags.push(("-e", jit_trim.to_string(), jit_out));
    }
    if expected_trim != run_trim {
        failed_flags.push(("-c", run_trim.to_string(), run_out));
    }

    if !failed_flags.is_empty() {
        for (flag, actual_trim, raw) in &failed_flags {
            eprintln!("Flag {} unexpected output:\n{}", flag, prettydiff::diff_lines(raw, expected_trim));
        }
        panic!("test failed: outputs did not match expected value for flags: {:?}", failed_flags.iter().map(|(f,_,_)| *f).collect::<Vec<_>>());
    }
}

pub(crate) fn run_failure_test(name: &str, expected: &str) {
    let Err(actual_err) = compile(name) else {
        panic!("expected a failure, but compilation succeeded");
    };
    assert!(
        actual_err.contains(expected.trim()),
        "the reported error message does not match: {}", actual_err,
    );
}

pub(crate) fn run_repl_sequence_test(name: &str, commands: &[&str], expected_outputs: &[&str]) {
    build();

    let actual_outputs = run_repl_with_timeout(commands, 3000);

    // Parse outputs
    let actual_lines = parse_repl_output(&actual_outputs);
    let actual_vec: Vec<&str> = actual_lines.iter().map(|s| s.trim()).collect();

    // For each expected_outputs entry, allow comma-separated substrings, and pass if all are found in the corresponding actual output
    let mut mismatch = false;
    for (i, expected) in expected_outputs.iter().enumerate() {
        let expected_subs: Vec<&str> = expected.split(',').map(|s| s.trim()).collect();
        let actual = actual_vec.get(i).unwrap_or(&"");
        let all_found = expected_subs.iter().all(|sub| actual.contains(sub));
        if !all_found {
            eprintln!(
                "Mismatch at index {}: expected substrings {:?} not all found in actual '{}'.\nFull raw output:\n{}",
                i, expected_subs, actual, actual_outputs
            );
            mismatch = true;
        }
    }
    if mismatch {
        panic!(
            "Vector mismatch in test '{}'\nExpected substrings: {:?}\nActual vector:   {:?}\n\nFull raw output:\n{}",
            name, expected_outputs, actual_vec, actual_outputs
        );
    }
}

fn parse_repl_output(raw_output: &str) -> Vec<String> {
    let lines: Vec<&str> = raw_output.lines().collect();
    let mut actual_lines = Vec::new();
    
    for i in 0..lines.len() {
        let line = lines[i].trim();
        
        // Result on same line as > for some reason
        if line.starts_with("> ") && line.len() > 2 {
            let mut result = line[2..].trim();
            // Sometimes skip result (with define)
            while result.starts_with("> ") && result.len() > 2 {
                result = result[2..].trim();
            }
            if !result.is_empty() && result != ">" {
                actual_lines.push(result.to_string());
                continue;
            }
        }
        
    }
    
    actual_lines
}


fn run_repl_with_timeout(commands: &[&str], timeout_ms: u64) -> String {
    // Probably dont need this for autograder
    let boa_path = if cfg!(target_os = "macos") {
        "target/x86_64-apple-darwin/debug/boa"
    } else {
        "target/debug/boa"
    };

    let mut child = Command::new(boa_path)
        .arg("-i")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to start repl");

    {
        let stdin = child.stdin.as_mut().expect("failed to open stdin");
        
        for command in commands {
            writeln!(stdin, "{}", command).unwrap();
            stdin.flush().unwrap();
            thread::sleep(Duration::from_millis(50));
        }
        
        // kill REPL
        writeln!(stdin, "").unwrap();
        stdin.flush().unwrap();
    }
    
    // Processing
    thread::sleep(Duration::from_millis(timeout_ms));
    
    let _ = child.kill();
    
    let output = child.wait_with_output().expect("failed to read output");
    String::from_utf8_lossy(&output.stdout).to_string()
}


fn mk_path(name: &str, ext: Ext) -> PathBuf {
    Path::new("tests").join(format!("{name}.{ext}"))
}

#[derive(Copy, Clone)]
enum Ext {
    Snek,
    Asm,
    Run,
}

impl std::fmt::Display for Ext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ext::Snek => write!(f, "snek"),
            Ext::Asm => write!(f, "s"),
            Ext::Run => write!(f, "run"),
        }
    }
}
use super::Environment;
use crate::VERSION;
use std::fmt;

#[allow(clippy::all)]
const ARGS: &[(&[&str], &str, fn(&mut Environment))] = &[
    (&["--help", "-h"], "show this message", |env| {
        env.output.help = true
    }),
    (
        &["--dry", "--dry-run"],
        "compile to ir without running any of the result",
        |env| env.output.run = false,
    ),
    (
        &["--show-ir", "--ir"],
        "dump the low-level IR in an ascii representation",
        |env| env.output.ir = true,
    ),
    (
        &["--show-ast", "--ast"],
        "dump the AST headers of all used modules",
        |env| {
            env.output.ast = true;
        },
    ),
];

impl Environment {
    pub fn parse_flags<I: Iterator<Item = String>>(&mut self, mut args: I) {
        loop {
            let arg = match args.next() {
                None => return,
                Some(arg) => arg,
            };

            for (flag, _, then_do) in ARGS {
                if flag.contains(&arg.as_str()) {
                    then_do(self)
                }
            }
        }
    }

    pub fn help_message(&self) {
        println!("leaf programming language {}\n", VERSION);

        for (flag, message, _) in ARGS {
            let flags = flag
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join("  ");
            println!(
                "  {}{}{}",
                flags,
                std::iter::repeat(" ")
                    .take(32 - flags.len())
                    .collect::<String>(),
                message
            );
        }
    }
}

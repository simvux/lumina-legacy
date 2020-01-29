#![feature(box_patterns)]
#![feature(mem_take)]
#![feature(print_internals)]
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

mod macros;

#[macro_use]
extern crate smallvec;

mod parser;
use parser::{ParseError, Parser};
mod env;
use env::Environment;
use parser::{FileSource, IrBuilder};
mod interpreter;
pub mod ir;

pub const VERSION: &str = "alpha-1.0";

fn main() {
    let environment = match Environment::discover() {
        Ok(env) => Rc::new(env),
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    if environment.output.help {
        environment.help_message();
        return;
    }
    match run(environment.clone()) {
        Ok(_main_returns) => {}
        Err(e) => {
            println!("{}", e);
            if environment.panicky {
                panic!("leaf encountered an error");
            }
        }
    };
}

fn run(env: Rc<Environment>) -> Result<ir::Value, ParseError> {
    let (ir, entrypoint) = {
        // The parser is our main object up until our AST is both finished and type checked
        let mut parser = Parser::new(env.clone());

        if let Err(e) = parser.read_prelude_source() {
            return Err(e.with_parser(parser));
        }

        // Open entry-point sourcecode file
        let mut source_code = String::with_capacity(20);
        File::open(&env.entrypoint)
            .unwrap()
            .read_to_string(&mut source_code)
            .unwrap();

        // Construct our AST by streaming tokens directly from the file into
        // parser.modules.{functions,types} seperated only by headers such as {fn,type,operator}
        let fid = match parser.tokenize(
            FileSource::Project(vec![env.entrypoint_name.clone()]),
            source_code.chars(),
        ) {
            Ok(functions) => functions,
            Err(e) => {
                return Err(e.with_parser(parser).load_source_code());
            }
        };
        if env.output.ast_full {
            println!(
                "{}\n",
                &parser
                    .modules
                    .iter()
                    .map(|a| std::fmt::Display::to_string(a))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        } else if env.output.ast_entry {
            println!("{}\n", &parser.modules[fid]);
        }

        // Verify syntax, infer types and compile to low-level IR.
        match IrBuilder::new(parser, env.clone()).start_type_checker(fid, "main", &[]) {
            Err(e) => {
                return Err(e.load_source_code());
            }
            Ok(ir) => ir,
        }
    };

    let mut runtime = interpreter::Runtime::new(ir);
    if env.optimize {
        runtime.optimize();
    }

    if env.output.ir {
        for (i, entity) in runtime.instructions.iter().enumerate() {
            println!("{}: {}", i, entity);
        }
    }
    drop(env);

    let entry = &runtime.instructions[entrypoint];
    let final_value = interpreter::Runner::start(&runtime, &entry, vec![]);
    Ok(final_value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Output;
    use std::path::{Path, PathBuf};
    use std::str::FromStr;

    // Unfortunately we can't really verify the actual return value of main itself since io:puts
    // overwrites it with _.
    //
    // I tried using the `gag` crate however when using `cargo test` then there's a ton of other
    // STDOUT which gets captured by `gag`.
    //
    // But hey, atleast we can verify that the examples don't panic.
    fn run_example(path: &str, name: &str) {
        let environment = Environment {
            leafpath: std::env::var("LEAFPATH")
                .map(|s| Path::new(&s).to_owned())
                .unwrap_or_else(|_| {
                    std::env::current_dir()
                        .unwrap_or_else(|_| panic!("Could not find leafpath"))
                        .to_owned()
                }),
            entrypoint: PathBuf::from_str(path).unwrap(),
            entrypoint_name: name.into(),
            panicky: true,
            output: Output {
                ir: false,
                ast_full: false,
                ast_entry: false,
                run: true,
                help: false,
            },
            optimize: true,
        };

        let environment = Rc::new(environment);

        match run(environment) {
            Err(e) => {
                println!("{}", e);
                panic!("leaf encountered an error")
            }
            Ok(_) => {}
        }
    }

    #[test]
    fn example_lists() {
        run_example("examples/lists.lf", "lists.lf");
    }

    #[test]
    fn example_if_else() {
        run_example("examples/if-else.lf", "if-else.lf");
    }

    #[test]
    fn example_void_values() {
        run_example("examples/void-values.lf", "void-values.lf");
    }
}

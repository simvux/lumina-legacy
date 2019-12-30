#![feature(box_patterns)]
#![feature(mem_take)]
#![feature(print_internals)]
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

#[macro_use]
extern crate smallvec;

pub fn entrypoint() -> FileSource {
    FileSource::Project(vec!["main".to_owned()])
}

mod parser;
use parser::Parser;
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

    let mut parser = Parser::new(environment.clone());
    if let Err(e) = parser.read_prelude_source() {
        println!("{}", e.with_parser(parser));
        return;
    }

    let mut source_code = String::with_capacity(20);
    File::open(&environment.entrypoint)
        .unwrap()
        .read_to_string(&mut source_code)
        .unwrap();
    let file_path = entrypoint();

    // Construct a raw token representation of the code
    let fid = match parser.tokenize(file_path.clone(), source_code.chars()) {
        Ok(functions) => functions,
        Err(e) => {
            println!(
                "{}",
                e.with_source_code(source_code, &file_path)
                    .with_parser(parser)
            );
            return;
        }
    };
    if environment.output.ast {
        println!("{:#?}", &parser.modules[fid]);
    }

    // Verify syntax, infer types and compile to low-level IR.
    let (ir, entrypoint) =
        match IrBuilder::new(parser, environment.clone()).start_type_checker(fid, "main", &[]) {
            Err(e) => {
                println!("{}", e.with_source_code(source_code, &file_path));
                return;
            }
            Ok(ir) => ir,
        };

    if environment.output.ir {
        for entity in ir.iter() {
            println!("{}", entity);
        }
    }

    drop(file_path);
    drop(source_code);

    let runtime = interpreter::Runtime::new(ir);
    let entry = &runtime.instructions[entrypoint];
    let _final_value = interpreter::Runner::start(&runtime, &entry, vec![]);
}

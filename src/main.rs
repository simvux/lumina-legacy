#![feature(box_patterns)]
#![feature(print_internals)]
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

pub fn entrypoint() -> FileSource {
    FileSource::Project(vec!["main".to_owned()])
}

macro_rules! debug {
    ($($arg:tt)*) => (
        #[cfg(debug_assertions)]
        print!(" leaf -> ");
        #[cfg(debug_assertions)]
        println!($($arg)*);
    )
}

mod parser;
use parser::Parser;
mod env;
use env::Environment;
use parser::{FileSource, IrBuilder};
mod interpreter;
pub mod ir;

fn main() {
    let environment = match Environment::discover() {
        Ok(env) => Rc::new(env),
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    let mut parser = Parser::new(environment.clone());
    parser.read_prelude_source();

    let mut source_code = Vec::with_capacity(20);
    File::open(&environment.entrypoint)
        .unwrap()
        .read_to_end(&mut source_code)
        .unwrap();
    let file_path = entrypoint();

    // Construct a raw token representation of the code
    let fid = match parser.tokenize(file_path.clone(), &source_code) {
        Ok(functions) => functions,
        Err(e) => {
            println!(
                "{}",
                e.with_source_code(&source_code, &file_path)
                    .with_parser(parser)
            );
            return;
        }
    };
    debug!("{:#?}", parser);

    // Verify syntax, infer types and compile to low-level IR.
    let (ir, entrypoint) =
        match IrBuilder::new(parser, environment).start_type_checker(fid, "main", &[]) {
            Err(e) => {
                println!("{}", e.with_source_code(&source_code, &file_path));
                return;
            }
            Ok(ir) => ir,
        };
    debug!("Initializing runtime with entry {:?}", entrypoint);
    drop(file_path);
    drop(source_code);

    let runtime = interpreter::Runtime::new(ir);
    let entry = &runtime.instructions[entrypoint];
    let final_value = interpreter::Runner::start(&runtime, &entry, Vec::new().into());
    println!("{:?}", final_value);
}

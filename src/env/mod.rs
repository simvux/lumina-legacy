use std::env;
use std::path::{Path, PathBuf};

mod output;
pub use output::Output;
mod flags;

#[derive(Debug)]
pub struct Environment {
    pub leafpath: PathBuf,
    pub entrypoint: PathBuf,
    pub entrypoint_name: String,
    pub output: Output,
    pub optimize: bool,
}

impl Environment {
    pub fn from(entrypoint_name: String, entrypoint: PathBuf) -> Self {
        Self {
            entrypoint,
            entrypoint_name,
            leafpath: env::var("LEAFPATH")
                .map(|s| Path::new(&s).to_owned())
                .unwrap_or_else(|_| {
                    env::current_dir()
                        .unwrap_or_else(|_| panic!("Could not find leafpath"))
                        .to_owned()
                }),
            optimize: true,
            output: Output::default(),
        }
    }

    pub fn discover<'a>() -> Result<Self, &'a str> {
        let args = env::args();
        if args.len() < 2 {
            Err("you need to provide a filename")
        } else {
            let mut args = env::args().collect::<Vec<String>>();
            let name = args.pop().unwrap();
            let mut path = PathBuf::from(&name);
            if path.is_relative() {
                let current_dir = env::current_dir().expect("Could not get active directory");
                path = current_dir.join(path);
            }
            if !path.exists() {
                return Err("file does not exist");
            }
            let mut env = Environment::from(name.split('/').last().unwrap().into(), path);

            // Skipping first since that's binary path
            env.parse_flags(args.drain(1..));

            Ok(env)
        }
    }
}

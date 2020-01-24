use super::ParseFault;
use std::convert::TryFrom;
use std::fmt;

// Used for placing compile-time arguments to the compiler. Such as `linux` in
// `fn print<linux> (string -> int)`
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Attr {
    Windows,
    Macos,
    Linux,
    Unix,
}

impl Attr {
    #[cfg(target_os = "windows")]
    pub fn is_targeted_sys(self) -> bool {
        match self {
            Attr::Windows => true,
            Attr::Linux | Attr::Macos | Attr::Unix => false,
        }
    }

    #[cfg(target_os = "linux")]
    pub fn is_targeted_sys(self) -> bool {
        match self {
            Attr::Linux => true,
            Attr::Windows | Attr::Macos | Attr::Unix => false,
        }
    }

    #[cfg(target_os = "unix")]
    pub fn is_targeted_sys(self) -> bool {
        match self {
            Attr::Unix => true,
            Attr::Linux | Attr::Macos | Attr::Windows => false,
        }
    }

    #[cfg(target_os = "macos")]
    pub fn is_targeted_sys(self) -> bool {
        match self {
            Attr::Macos => true,
            Attr::Linux | Attr::Unix | Attr::Windows => false,
        }
    }

    #[cfg(not(any(
        target_os = "macos",
        target_os = "linux",
        target_os = "windows",
        target_os = "unix"
    )))]
    pub fn is_targeted_sys(self) -> bool {
        false
    }
}

impl TryFrom<&str> for Attr {
    type Error = ParseFault;

    fn try_from(s: &str) -> Result<Attr, Self::Error> {
        let attr = match s {
            "windows" => Attr::Windows,
            "linux" => Attr::Linux,
            "macos" | "darwin" => Attr::Macos,
            _ => return Err(ParseFault::UnrecognizedAttribute(s.to_owned())),
        };
        Ok(attr)
    }
}

impl fmt::Display for Attr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!("{:?}", self).to_lowercase())
    }
}

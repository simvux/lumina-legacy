use std::convert::TryFrom;
use std::fmt;

#[derive(PartialEq, Clone, Debug, Default, Hash, Eq)]
pub struct Operator {
    pub identifier: String,
}

impl From<String> for Operator {
    fn from(s: String) -> Self {
        Self { identifier: s }
    }
}
impl TryFrom<&str> for Operator {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        const ALLOWED_CHARACTERS: &str = "!$%&/=?^~@+-*/;<>";

        for c in s.chars() {
            if !ALLOWED_CHARACTERS.contains(|a| a == c) {
                return Err(());
            }
        }

        let valid = Self {
            identifier: s.to_owned(),
        };
        Ok(valid)
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.identifier)
    }
}

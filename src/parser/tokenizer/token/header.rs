use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Header {
    Function,
    Operator,
    Enum,
    Type,
    Use,
}

impl TryFrom<&str> for Header {
    type Error = ();

    fn try_from(bytes: &str) -> Result<Header, Self::Error> {
        let res = match bytes {
            "fn" => Header::Function,
            "type" => Header::Type,
            "use" => Header::Use,
            "enum" => Header::Enum,
            "operator" => Header::Operator,
            _ => return Err(()),
        };
        Ok(res)
    }
}

impl Header {
    pub fn as_str(&self) -> &str {
        match self {
            Header::Function => "fn",
            Header::Operator => "operator",
            Header::Enum => "enum",
            Header::Type => "type",
            Header::Use => "use",
        }
    }
}

impl fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

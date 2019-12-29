use super::Runner;
use crate::ir::Value;

macro_rules! calc {
    ($op:tt, $x:ident, $y:ident) => (
        match $x {
            Value::Int(x) => match $y {
                Value::Int(y) => return Value::Int(x $op y),
                _ => unreachable!(),
            }
            Value::Float(x) => match $y {
                Value::Float(y) => return Value::Float(x $op y),
                _ => unreachable!(),
            }
            _ => unreachable!(),
        }
    )
}

impl<'p> Runner<'p> {
    pub fn eval_bridged(self, index: u16) -> Value {
        let (x, y) = (self.params.clone_param(0), self.params.clone_param(1));
        match index {
            0 => calc!(+, x, y),
            1 => calc!(-, x, y),
            2 => calc!(*, x, y),
            3 => calc!(/, x, y),
            4 => {
                if let Value::List(mut list) = y {
                    list.push_back(x);
                    Value::List(list)
                } else {
                    unreachable!();
                }
            }
            5 => {
                if let Value::List(mut list) = y {
                    list.push_front(x);
                    Value::List(list)
                } else {
                    unreachable!();
                }
            }
            _ => unreachable!(),
        }
    }
}

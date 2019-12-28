use super::Runner;
use crate::ir::Value;

// We're gonna make bridged-functions instead be methods for Runner to give us more control of
// memory.

pub fn get_func_copy(id: u16) -> fn(Value, Value) -> Value {
    match id {
        0 => |x, y| {
            if let Value::Int(x) = x {
                if let Value::Int(y) = y {
                    return Value::Int(x + y);
                }
            }
            unreachable!();
        },
        1 => |x, y| {
            if let Value::Int(x) = x {
                if let Value::Int(y) = y {
                    return Value::Int(x - y);
                }
            }
            unreachable!();
        },
        2 => |x, y| {
            if let Value::Int(x) = x {
                if let Value::Int(y) = y {
                    return Value::Int(x * y);
                }
            }
            unreachable!();
        },
        3 => |x, y| {
            if let Value::Int(x) = x {
                if let Value::Int(y) = y {
                    return Value::Int(x / y);
                }
            }
            unreachable!();
        },
        _ => unreachable!(),
    }
}

pub fn get_func_write(id: u16) -> fn(&Value, Value) -> Value {
    unimplemented!();
}

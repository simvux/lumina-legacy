use super::ParamBuffer;
use super::Runner;
use crate::ir::Value;

macro_rules! calc {
    ($op:tt, $x:expr, $y:expr) => (
        match $x {
            Value::Int(x) => match $y {
                Value::Int(y) => return Value::Int(*x $op *y),
                _ => unreachable!(),
            }
            Value::Float(x) => match $y {
                Value::Float(y) => return Value::Float(*x $op *y),
                _ => unreachable!(),
            }
            _ => unreachable!(),
        }
    )
}

impl<'p> Runner<'p> {
    pub fn eval_bridged(self, index: u16) -> Value {
        // let (x, y) = (self.params.clone_param(0), self.params.clone_param(1));
        match index {
            0 => calc!(+, self.params.borrow_param(0), self.params.borrow_param(1)),
            1 => calc!(-, self.params.borrow_param(0), self.params.borrow_param(1)),
            2 => calc!(*, self.params.borrow_param(0), self.params.borrow_param(1)),
            3 => calc!(/, self.params.borrow_param(0), self.params.borrow_param(1)),
            4 => {
                if let Value::List(mut list) = self.params.clone_param(1) {
                    list.push_back(self.params.clone_param(0));
                    return Value::List(list);
                }
                unreachable!();
            }
            5 => {
                if let Value::List(mut list) = self.params.clone_param(1) {
                    list.push_front(self.params.clone_param(0));
                    return Value::List(list);
                }
                unreachable!();
            }
            6 => {
                if let Value::List(list) = self.params.borrow_param(1) {
                    if let Value::Int(i) = self.params.clone_param(0) {
                        return list[i as usize].clone();
                    }
                }
                unreachable!();
            }
            7 => {
                if let Value::List(list) = self.params.borrow_param(0) {
                    Value::Int(list.len() as i64)
                } else {
                    dbg!(&self.params);
                    unreachable!("{:?}", self.params.borrow_param(0))
                }
            }
            // TODO: This should be reimplemented in Leaf when we have a stronger type system
            // or should it?
            8 => Value::Bool(self.params.borrow_param(0) == self.params.borrow_param(1)),
            9 => Value::Bool(self.params.borrow_param(0) < self.params.borrow_param(1)),
            10 => {
                if let Value::List(mut list) = self.params.clone_param(1) {
                    if let Value::Int(i) = self.params.borrow_param(0) {
                        return list.remove(*i as usize).unwrap();
                    }
                }
                unreachable!();
            }
            11 => {
                if let Value::List(mut list) = self.params.clone_param(1) {
                    if let Value::Int(i) = self.params.borrow_param(0) {
                        list.remove(*i as usize);
                        return Value::List(list);
                    }
                }
                unreachable!();
            }
            12 => {
                println!("{}", self.params.borrow_param(0));
                Value::Nothing
            }
            13 => {
                // TODO: This implementation is temporary. The cloning here is obviously bad.
                if let Value::List(mut list) = self.params.clone_param(1) {
                    if let Value::Function(box (action, captured)) = self.params.clone_param(0) {
                        for previous in list.iter_mut() {
                            let new = self.spawn(
                                &action,
                                ParamBuffer::Owned(smallvec![previous.clone()]),
                                captured.clone(),
                            );
                            *previous = new;
                        }
                        return Value::List(list);
                    }
                }
                unreachable!();
            }
            14 => {
                if let Value::List(mut list) = self.params.clone_param(0) {
                    if let Value::List(mut addition) = self.params.clone_param(1) {
                        list.append(&mut addition);
                        return Value::List(list);
                    }
                }
                unreachable!();
            }
            _ => unreachable!(),
        }
    }
}

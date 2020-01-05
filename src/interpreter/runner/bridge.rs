use super::ParamBuffer;
use super::Runner;
use crate::ir::bridge::Bridged;
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
    pub fn eval_bridged(self, func: Bridged) -> Value {
        // let (x, y) = (self.params.clone_param(0), self.params.clone_param(1));
        match func {
            Bridged::add => calc!(+, self.params.borrow_param(0), self.params.borrow_param(1)),
            Bridged::sub => calc!(-, self.params.borrow_param(0), self.params.borrow_param(1)),
            Bridged::mul => calc!(*, self.params.borrow_param(0), self.params.borrow_param(1)),
            Bridged::div => calc!(/, self.params.borrow_param(0), self.params.borrow_param(1)),
            Bridged::push_back => {
                if let Value::List(mut list) = self.params.clone_param(1) {
                    list.push_back(self.params.clone_param(0));
                    return Value::List(list);
                }
                unreachable!();
            }
            Bridged::push_front => {
                if let Value::List(mut list) = self.params.clone_param(1) {
                    list.push_front(self.params.clone_param(0));
                    return Value::List(list);
                }
                unreachable!();
            }
            Bridged::get => {
                if let Value::List(list) = self.params.borrow_param(1) {
                    if let Value::Int(i) = self.params.clone_param(0) {
                        return list[i as usize].clone();
                    }
                }
                unreachable!();
            }
            Bridged::len => {
                if let Value::List(list) = self.params.borrow_param(0) {
                    Value::Int(list.len() as i64)
                } else {
                    dbg!(&self.params);
                    unreachable!("{:?}", self.params.borrow_param(0))
                }
            }
            // TODO: This should be reimplemented in Leaf when we have a stronger type system
            // or should it?
            Bridged::eq => Value::Bool(self.params.borrow_param(0) == self.params.borrow_param(1)),
            Bridged::lt => Value::Bool(self.params.borrow_param(0) < self.params.borrow_param(1)),
            Bridged::steal => {
                if let Value::List(mut list) = self.params.clone_param(1) {
                    if let Value::Int(i) = self.params.borrow_param(0) {
                        return list.remove(*i as usize).unwrap();
                    }
                }
                unreachable!();
            }
            Bridged::remove => {
                if let Value::List(mut list) = self.params.clone_param(1) {
                    if let Value::Int(i) = self.params.borrow_param(0) {
                        list.remove(*i as usize);
                        return Value::List(list);
                    }
                }
                unreachable!();
            }
            Bridged::print_any => {
                println!("{}", self.params.borrow_param(0));
                Value::Nothing
            }
            Bridged::map_overwrite => {
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
            Bridged::append => {
                if let Value::List(mut list) = self.params.clone_param(0) {
                    if let Value::List(mut addition) = self.params.clone_param(1) {
                        list.append(&mut addition);
                        return Value::List(list);
                    }
                }
                unreachable!();
            }
        }
    }
}

use crate::ir;
use crate::ir::Entity;

pub struct Runtime {
    pub instructions: Vec<ir::Entity>,
}

impl Runtime {
    pub fn new(instructions: Vec<ir::Entity>) -> Self {
        Self { instructions }
    }
}

impl Runtime {
    // TODO: The parameters of forking doesn't get optimized even though they sometimes can.
    // Suppose that's why I need to make this a bit recursive somehow.
    //
    // `4 + (3 + 2)` gets optimized `builtin:add 4 (3 + 2)`
    // instead of `builtin:add 4 (builtin:add 3 2)`
    pub fn optimize(&mut self) {
        let mut being_optimized_i = 0;
        while being_optimized_i < self.instructions.len() {
            match &self.instructions[being_optimized_i] {
                Entity::FunctionCall(findex, params) => {
                    if params.iter().all(|t| t.non_forking()) {
                        let child = &self.instructions[*findex as usize];

                        match child {
                            Entity::RustCall(n, child_params) => {
                                if child_params.iter().all(|p| p.non_forking()) {
                                    let new_params = child_params
                                        .iter()
                                        .cloned()
                                        .map(|p| p.inline_param(&params))
                                        .collect::<Vec<Entity>>();

                                    self.instructions[being_optimized_i] =
                                        Entity::RustCall(*n, new_params);

                                    // Try to optimize it again
                                    continue;
                                }
                            }
                            Entity::FunctionCall(child_findex, child_params) => {
                                let new_params = child_params
                                    .iter()
                                    .cloned()
                                    .map(|p| p.inline_param(&params))
                                    .collect::<Vec<Entity>>();

                                self.instructions[being_optimized_i] =
                                    Entity::FunctionCall(*child_findex, new_params);

                                // Try to optimize it again
                                continue;
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
            being_optimized_i += 1;
        }
    }
}

use std::rc::Rc;

use anyhow::{bail, Context, Result};
use fnv::FnvHashMap;
use inkwell::values::{BasicValue, InstructionOpcode};
use inkwell::AddressSpace;
use num_bigint::{BigInt, Sign};
use python_syntax::ast;
use python_syntax::visitor::{Accept, Visitor};

use crate::codegen::Codegen;
use crate::object::Object;

pub struct ExpressionVisitor<'c, 'l, 'ctx> {
    gen: &'c Codegen<'l, 'ctx>,
}

impl<'c, 'l, 'ctx> ExpressionVisitor<'c, 'l, 'ctx> {
    pub fn new(gen: &'c Codegen<'l, 'ctx>) -> ExpressionVisitor<'c, 'l, 'ctx> {
        ExpressionVisitor { gen }
    }
}

impl<'c, 'l, 'ctx> Visitor for ExpressionVisitor<'c, 'l, 'ctx> {
    type T = Result<Rc<Object<'ctx>>>;

    fn visit_module(&mut self, _node: &ast::Module) -> Self::T {
        unreachable!()
    }

    fn visit_interactive(&mut self, _node: &ast::Interactive) -> Self::T {
        unreachable!()
    }

    fn visit_eval(&mut self, _node: &ast::Eval) -> Self::T {
        unreachable!()
    }

    fn visit_function_def(&mut self, _node: &ast::FunctionDef) -> Self::T {
        unreachable!()
    }

    fn visit_async_function_def(&mut self, _node: &ast::AsyncFunctionDef) -> Self::T {
        unreachable!()
    }

    fn visit_class_def(&mut self, _node: &ast::ClassDef) -> Self::T {
        unreachable!()
    }

    fn visit_return(&mut self, _node: &ast::Return) -> Self::T {
        unreachable!()
    }

    fn visit_delete(&mut self, _node: &ast::Delete) -> Self::T {
        unreachable!()
    }

    fn visit_assign(&mut self, _node: &ast::Assign) -> Self::T {
        unreachable!()
    }

    fn visit_aug_assign(&mut self, _node: &ast::AugAssign) -> Self::T {
        unreachable!()
    }

    fn visit_ann_assign(&mut self, _node: &ast::AnnAssign) -> Self::T {
        unreachable!()
    }

    fn visit_for(&mut self, _node: &ast::For) -> Self::T {
        unreachable!()
    }

    fn visit_async_for(&mut self, _node: &ast::AsyncFor) -> Self::T {
        unreachable!()
    }

    fn visit_while(&mut self, _node: &ast::While) -> Self::T {
        unreachable!()
    }

    fn visit_if(&mut self, _node: &ast::If) -> Self::T {
        unreachable!()
    }

    fn visit_with(&mut self, _node: &ast::With) -> Self::T {
        unreachable!()
    }

    fn visit_async_with(&mut self, _node: &ast::AsyncWith) -> Self::T {
        unreachable!()
    }

    fn visit_raise(&mut self, _node: &ast::Raise) -> Self::T {
        unreachable!()
    }

    fn visit_try(&mut self, _node: &ast::Try) -> Self::T {
        unreachable!()
    }

    fn visit_assert(&mut self, _node: &ast::Assert) -> Self::T {
        unreachable!()
    }

    fn visit_import(&mut self, _node: &ast::Import) -> Self::T {
        unreachable!()
    }

    fn visit_import_from(&mut self, _node: &ast::ImportFrom) -> Self::T {
        unreachable!()
    }

    fn visit_global(&mut self, _node: &ast::Global) -> Self::T {
        unreachable!()
    }

    fn visit_nonlocal(&mut self, _node: &ast::Nonlocal) -> Self::T {
        unreachable!()
    }

    fn visit_expr(&mut self, _node: &ast::Expr) -> Self::T {
        unreachable!()
    }

    fn visit_pass(&mut self, _node: &ast::Pass) -> Self::T {
        unreachable!()
    }

    fn visit_break(&mut self, _node: &ast::Break) -> Self::T {
        unreachable!()
    }

    fn visit_continue(&mut self, _node: &ast::Continue) -> Self::T {
        unreachable!()
    }

    fn visit_bool_op(&mut self, _node: &ast::BoolOp) -> Self::T {
        unimplemented!()
    }

    fn visit_bin_op(&mut self, node: &ast::BinOp) -> Self::T {
        let left = node.left.accept(self)?;
        let right = node.right.accept(self)?;
        match node.op {
            ast::OpKind::Addition => {
                let args = [
                    left.ptr().as_basic_value_enum(),
                    right.ptr().as_basic_value_enum(),
                ];
                Ok(self.gen.build_builtin_call("py_object_add", &args))
            }
            _ => unimplemented!(),
        }
    }
    fn visit_unary_op(&mut self, _node: &ast::UnaryOp) -> Self::T {
        unimplemented!()
    }

    fn visit_lambda(&mut self, _node: &ast::Lambda) -> Self::T {
        unimplemented!()
    }

    fn visit_if_exp(&mut self, _node: &ast::IfExp) -> Self::T {
        unimplemented!()
    }

    fn visit_dict(&mut self, _node: &ast::Dict) -> Self::T {
        unimplemented!()
    }

    fn visit_set(&mut self, _node: &ast::Set) -> Self::T {
        unimplemented!()
    }

    fn visit_list_comp(&mut self, _node: &ast::ListComp) -> Self::T {
        unimplemented!()
    }

    fn visit_set_comp(&mut self, _node: &ast::SetComp) -> Self::T {
        unimplemented!()
    }

    fn visit_dict_comp(&mut self, _node: &ast::DictComp) -> Self::T {
        unimplemented!()
    }

    fn visit_generator_exp(&mut self, _node: &ast::GeneratorExp) -> Self::T {
        unimplemented!()
    }

    fn visit_await(&mut self, _node: &ast::Await) -> Self::T {
        unimplemented!()
    }

    fn visit_yield(&mut self, _node: &ast::Yield) -> Self::T {
        unimplemented!()
    }

    fn visit_yield_from(&mut self, _node: &ast::YieldFrom) -> Self::T {
        unimplemented!()
    }

    fn visit_compare(&mut self, _node: &ast::Compare) -> Self::T {
        unimplemented!()
    }

    fn visit_call(&mut self, node: &ast::Call) -> Self::T {
        let args = node
            .args
            .iter()
            .map(|x| Ok(x.accept(self)?))
            .collect::<Result<Vec<_>>>()?;
        let keywords = node
            .keywords
            .iter()
            .map(|x| Ok((x.arg.as_ref(), x.value.accept(self)?)))
            .collect::<Result<Vec<_>>>()?;
        match &*node.func {
            ast::Expression::Name(ast::Name { id, .. }) => {
                let fun_obj = node.func.accept(self)?;
                match &*fun_obj {
                    Object::Function { ptr, signature, .. } => {
                        let mut slots =
                            vec![None; signature.args.len() + signature.kwonlyargs.len()];
                        if args.len() > signature.args.len() && signature.vararg.is_none() {
                            bail!(
                                "{}() takes {} positional arguments but {} were given (line {} column {})",
                                id,
                                signature.args.len(),
                                args.len(),
                                node.span.start.line,
                                node.span.start.column,
                                )
                        }

                        let mut vararg = vec![];
                        for (idx, arg) in args.into_iter().enumerate() {
                            if idx < slots.len() {
                                slots[idx] = Some(arg);
                            } else {
                                vararg.push(arg);
                            }
                        }

                        let mut kwarg = FnvHashMap::default();
                        for (kwarg_name, kwarg_value) in keywords {
                            match kwarg_name {
                                Some(kwarg_name) => {
                                    match signature.position(kwarg_name) {
                                        Some(idx) => slots[idx] = Some(kwarg_value),
                                        None if signature.kwarg.is_some() => {kwarg.insert(kwarg_name, kwarg_value);},
                                        _ => bail!(
                                            "{}() got an unexpected keyword argument '{}' (line {} column {})",
                                            id,
                                            kwarg_name,
                                            node.span.start.line,
                                            node.span.start.column,
                                        ),
                                    }
                                }
                                None => unimplemented!(), // **kwarg
                            }
                        }

                        let mut call_args = slots.into_iter().enumerate().map(|(idx, slot)| {
                            match slot {
                                Some(arg) => Ok(arg.ptr().as_basic_value_enum()),
                                None => {
                                    let param = signature.nth(idx).unwrap();
                                    param.default.as_ref().map(|x| x.ptr().as_basic_value_enum()).with_context(|| {
                                        format!(
                                            "{}() missing required argument '{}' (line {} column {})",
                                            id,
                                            param.name,
                                            node.span.start.line,
                                            node.span.start.column,
                                        )
                                    })
                                }
                            }
                        }).collect::<Result<Vec<_>>>()?;

                        let vararg_obj = signature.vararg.is_some().then(|| {
                            let list = self.gen.build_list(&vararg).ptr().as_basic_value_enum();
                            call_args.push(list);
                            list
                        });
                        if signature.kwarg.is_some() {
                            // TODO: kwarg
                            call_args.push(self.gen.ref_type().const_null().as_basic_value_enum());
                        }
                        let obj = self
                            .gen
                            .builder
                            .build_call(*ptr, &call_args, "")
                            .try_as_basic_value()
                            .left()
                            .map(|x| Object::Instance {
                                ptr: x.into_pointer_value(),
                                typ: None,
                            })
                            .unwrap();
                        if let Some(vararg_obj) = vararg_obj {
                            self.gen.builder.build_call(
                                self.gen.module.get_function("py_list_decref").unwrap(),
                                &[vararg_obj],
                                "",
                            );
                        }
                        Ok(Rc::new(obj))
                    }
                    Object::Class { .. } | Object::Instance { .. } => unimplemented!(),
                    Object::Module { .. } => bail!("module object is not callable"),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn visit_num(&mut self, node: &ast::Num) -> Self::T {
        match &node.value {
            ast::NumKind::Integer(int) => {
                let bytes = BigInt::from_biguint(Sign::Plus, int.clone()).to_signed_bytes_le();
                let ptr = self
                    .gen
                    .builder
                    .build_alloca(self.gen.ctx.i8_type().array_type(bytes.len() as u32), "");
                self.gen
                    .builder
                    .build_store(ptr, self.gen.ctx.const_string(&bytes, false));
                let ptr = self.gen.builder.build_cast(
                    InstructionOpcode::BitCast,
                    ptr,
                    self.gen.ctx.i8_type().ptr_type(AddressSpace::Generic),
                    "",
                );
                Ok(self.gen.build_builtin_call(
                    "py_int_from_bytes",
                    &[
                        ptr.as_basic_value_enum(),
                        self.gen
                            .ptr_sized_int_type()
                            .const_int(bytes.len() as u64, false)
                            .as_basic_value_enum(),
                    ],
                ))
            }
            _ => unimplemented!(),
        }
    }

    fn visit_str(&mut self, node: &ast::Str) -> Self::T {
        let bytes = node.value.as_bytes();
        let ptr = self.gen.builder.build_alloca(
            self.gen.ctx.i8_type().array_type((bytes.len() + 1) as u32),
            "",
        );
        self.gen
            .builder
            .build_store(ptr, self.gen.ctx.const_string(bytes, true));
        let ptr = self.gen.builder.build_cast(
            InstructionOpcode::BitCast,
            ptr,
            self.gen.ctx.i8_type().ptr_type(AddressSpace::Generic),
            "",
        );
        Ok(self
            .gen
            .build_builtin_call("py_string_from_bytes", &[ptr.as_basic_value_enum()]))
    }

    fn visit_formatted_value(&mut self, _node: &ast::FormattedValue) -> Self::T {
        unimplemented!()
    }

    fn visit_joined_str(&mut self, _node: &ast::JoinedStr) -> Self::T {
        unimplemented!()
    }

    fn visit_bytes(&mut self, _node: &ast::Bytes) -> Self::T {
        unimplemented!()
    }

    fn visit_name_constant(&mut self, _node: &ast::NameConstant) -> Self::T {
        unimplemented!()
    }

    fn visit_ellipsis(&mut self, _node: &ast::Ellipsis) -> Self::T {
        unimplemented!()
    }

    fn visit_attribute(&mut self, _node: &ast::Attribute) -> Self::T {
        unimplemented!()
    }

    fn visit_subscript(&mut self, _node: &ast::Subscript) -> Self::T {
        unimplemented!()
    }

    fn visit_starred(&mut self, _node: &ast::Starred) -> Self::T {
        unimplemented!()
    }

    fn visit_name(&mut self, node: &ast::Name) -> Self::T {
        self.gen
            .stack
            .iter()
            .rev()
            .filter_map(|x| {
                let symtable = x.symtable();
                symtable.borrow().get(&node.id).cloned()
            })
            .next()
            .or_else(|| {
                let builtins = self.gen.modules.borrow().get("builtins").cloned().unwrap();
                match &*builtins {
                    Object::Module { symtable } => symtable.borrow().get(&node.id).cloned(),
                    _ => panic!("builtins is not a module"),
                }
            })
            .with_context(|| {
                format!(
                    "name '{}' is not defined (line {} column {})",
                    node.id, node.span.start.line, node.span.start.column
                )
            })
    }

    fn visit_list(&mut self, _node: &ast::List) -> Self::T {
        unimplemented!()
    }

    fn visit_tuple(&mut self, _node: &ast::Tuple) -> Self::T {
        unimplemented!()
    }
}

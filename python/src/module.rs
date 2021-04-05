use std::cell::RefCell;
use std::rc::Rc;

use anyhow::Result;
use inkwell::types::BasicType;
use inkwell::values::BasicValue;
use inkwell::IntPredicate;
use python_syntax::ast;
use python_syntax::visitor::{self, Accept, Visitor};

use crate::bail_with_node;
use crate::codegen::Codegen;
use crate::expression::ExpressionVisitor;
use crate::object::{Binding, Constant, Object, Parameter, Scope, Signature, SymbolTable, Type};

pub struct ModuleVisitor<'c, 'l, 'ctx> {
    gen: &'c mut Codegen<'l, 'ctx>,
    name: &'c str,
    seen_return: bool,
}

impl<'c, 'l, 'ctx> ModuleVisitor<'c, 'l, 'ctx> {
    pub fn new(gen: &'c mut Codegen<'l, 'ctx>, name: &'c str) -> ModuleVisitor<'c, 'l, 'ctx> {
        ModuleVisitor {
            gen,
            name,
            seen_return: false,
        }
    }

    fn visit_statements(&mut self, statements: &[ast::Statement]) -> Result<()> {
        self.seen_return = false;
        for e in statements {
            e.accept(self)?;
            if self.seen_return {
                break;
            };
        }
        Ok(())
    }
}

impl<'c, 'l, 'ctx> Visitor for ModuleVisitor<'c, 'l, 'ctx> {
    type T = Result<()>;

    fn visit_module(&mut self, node: &ast::Module) -> Self::T {
        let fun_type = self.gen.ctx.void_type().fn_type(&[], false);
        let fun = self.gen.module.add_function(self.name, fun_type, None);
        let block = self.gen.ctx.append_basic_block(fun, "entry");
        self.gen.builder.position_at_end(block);
        let scope = Rc::new(RefCell::new(Scope::Module {
            symtable: SymbolTable::default(),
        }));
        {
            self.gen
                .modules
                .insert(self.name.to_string(), scope.clone());
        }
        self.gen.stack.push(scope);
        for e in &node.body {
            e.accept(self)?;
        }
        self.gen.stack.pop().unwrap();
        self.gen.builder.build_return(None);
        Ok(())
    }

    fn visit_interactive(&mut self, node: &ast::Interactive) -> Self::T {
        visitor::walk_interactive(self, node);
        Ok(())
    }

    fn visit_eval(&mut self, node: &ast::Eval) -> Self::T {
        visitor::walk_eval(self, node);
        Ok(())
    }

    fn visit_function_def(&mut self, node: &ast::FunctionDef) -> Self::T {
        let signature = Signature {
            args: node
                .args
                .args
                .iter()
                .map(|x| Parameter::required(x.arg.clone()))
                .collect(),
            vararg: node.args.vararg.as_ref().cloned().map(|x| x.arg),
            kwonlyargs: node
                .args
                .kwonlyargs
                .iter()
                .map(|x| {
                    let default = match &x.kind {
                        ast::ArgKind::Optional(expr) => {
                            let mut expr_visitor = ExpressionVisitor::new(self.gen);
                            Some(expr.accept(&mut expr_visitor)?)
                        }
                        _ => None,
                    };
                    Ok(Parameter::new(x.arg.clone(), default))
                })
                .collect::<Result<Vec<_>>>()?,
            kwarg: node.args.kwarg.as_ref().cloned().map(|x| x.arg),
        };
        let llvm_args = vec![
            self.gen.ref_type().as_basic_type_enum();
            signature.slots()
                + if signature.vararg.is_some() { 1 } else { 0 }
                + if signature.kwarg.is_some() { 1 } else { 0 }
        ];
        let llvm_fun = self.gen.module.add_function(
            &format!("{}${}", self.name, node.name),
            self.gen.ref_type().fn_type(&llvm_args, false),
            None,
        );
        let prev_block = self.gen.builder.get_insert_block().unwrap();
        let block = self.gen.ctx.append_basic_block(llvm_fun, "entry");
        self.gen.builder.position_at_end(block);
        let mut symtable = SymbolTable::default();
        for (idx, param) in llvm_fun
            .get_param_iter()
            .enumerate()
            .take(signature.slots())
        {
            let ptr = self.gen.builder.build_alloca(self.gen.ref_type(), "");
            self.gen.builder.build_store(ptr, param);
            let binding = Binding {
                ptr,
                typ: Type::Instance,
            };
            let name = signature.nth_slot(idx).map(|x| x.name.clone()).unwrap();
            symtable.insert(name, binding);
        }
        if let Some(name) = &signature.vararg {
            let param = llvm_fun.get_nth_param(signature.slots() as u32).unwrap();
            let ptr = self.gen.builder.build_alloca(self.gen.ref_type(), "");
            self.gen.builder.build_store(ptr, param);
            let binding = Binding {
                ptr,
                typ: Type::Instance,
            };
            symtable.insert(name.clone(), binding);
        }
        if let Some(name) = &signature.kwarg {
            let param = llvm_fun
                .get_nth_param(signature.slots() as u32 + 1)
                .unwrap();
            let ptr = self.gen.builder.build_alloca(self.gen.ref_type(), "");
            self.gen.builder.build_store(ptr, param);
            let binding = Binding {
                ptr,
                typ: Type::Instance,
            };
            symtable.insert(name.clone(), binding);
        }
        let fun = Rc::new(Object::Function {
            ptr: llvm_fun.as_global_value().as_pointer_value(),
            signature: signature.clone(),
        });
        let binding = Binding {
            ptr: fun.ptr(),
            typ: Type::Function { signature },
        };
        {
            self.gen
                .scope()
                .borrow_mut()
                .symtable_mut()
                .insert(node.name.to_string(), binding);
        }
        let scope = Rc::new(RefCell::new(Scope::Function { symtable }));
        self.gen.stack.push(scope);
        self.visit_statements(&node.body)?;
        self.gen.stack.pop().unwrap();
        if !self.seen_return {
            self.gen
                .builder
                .build_return(Some(&self.gen.build_load_constant(Constant::None).ptr()));
        }
        self.gen.builder.position_at_end(prev_block);
        Ok(())
    }

    fn visit_async_function_def(&mut self, node: &ast::AsyncFunctionDef) -> Self::T {
        visitor::walk_async_function_def(self, node);
        Ok(())
    }

    fn visit_class_def(&mut self, node: &ast::ClassDef) -> Self::T {
        visitor::walk_class_def(self, node);
        Ok(())
    }

    fn visit_return(&mut self, node: &ast::Return) -> Self::T {
        if !self.gen.scope().borrow().is_function() {
            bail_with_node!(node, "'return' outside function")
        }

        let mut expr_visitor = ExpressionVisitor::new(self.gen);
        let obj = match &node.value {
            Some(value) => value.accept(&mut expr_visitor)?,
            _ => self.gen.build_load_constant(Constant::None),
        };
        self.gen.builder.build_return(Some(&obj.ptr()));
        self.seen_return = true;
        Ok(())
    }

    fn visit_delete(&mut self, node: &ast::Delete) -> Self::T {
        visitor::walk_delete(self, node);
        Ok(())
    }

    fn visit_assign(&mut self, node: &ast::Assign) -> Self::T {
        let mut expr_visitor = ExpressionVisitor::new(self.gen);
        let value = node.value.accept(&mut expr_visitor)?;
        for e in &node.targets {
            match e {
                ast::Expression::Name(ast::Name { id, .. }) => {
                    let scope = self.gen.scope();
                    let mut scope = scope.borrow_mut();
                    let is_module_scope = scope.is_module();
                    let binding = scope.symtable_mut().entry(id.clone()).or_insert_with(|| {
                        let ptr = match is_module_scope {
                            true => {
                                let global = self.gen.module.add_global(
                                    self.gen.ref_type(),
                                    None,
                                    &format!("{}${}", self.name, id),
                                );
                                global.set_initializer(&self.gen.ref_type().const_null());
                                global.as_pointer_value()
                            }
                            false => self.gen.builder.build_alloca(self.gen.ref_type(), ""),
                        };
                        Binding {
                            ptr,
                            typ: Type::Instance,
                        }
                    });
                    binding.typ = value.typ();
                    self.gen.builder.build_store(binding.ptr, value.ptr());
                }
                _ => unimplemented!(),
            }
        }
        Ok(())
    }

    fn visit_aug_assign(&mut self, node: &ast::AugAssign) -> Self::T {
        visitor::walk_aug_assign(self, node);
        Ok(())
    }

    fn visit_ann_assign(&mut self, node: &ast::AnnAssign) -> Self::T {
        visitor::walk_ann_assign(self, node);
        Ok(())
    }

    fn visit_for(&mut self, node: &ast::For) -> Self::T {
        visitor::walk_for(self, node);
        Ok(())
    }

    fn visit_async_for(&mut self, node: &ast::AsyncFor) -> Self::T {
        visitor::walk_async_for(self, node);
        Ok(())
    }

    fn visit_while(&mut self, node: &ast::While) -> Self::T {
        visitor::walk_while(self, node);
        Ok(())
    }

    fn visit_if(&mut self, node: &ast::If) -> Self::T {
        let mut expr_visitor = ExpressionVisitor::new(self.gen);
        let function = self
            .gen
            .builder
            .get_insert_block()
            .and_then(|x| x.get_parent())
            .unwrap();
        let body_block = self.gen.ctx.append_basic_block(function, "");
        let merge_block = self.gen.ctx.append_basic_block(function, "");
        let else_block = match node.orelse.is_empty() {
            true => merge_block,
            false => self.gen.ctx.insert_basic_block_after(body_block, ""),
        };

        let test = node.test.accept(&mut expr_visitor)?;
        let evaluated_test = self
            .gen
            .build_builtin_call("py_bool", &[test.ptr().as_basic_value_enum()]);
        let ptr_diff = self.gen.builder.build_ptr_diff(
            evaluated_test.ptr(),
            self.gen.build_load_constant(Constant::True).ptr(),
            "",
        );
        let is_true = self.gen.builder.build_int_compare(
            IntPredicate::EQ,
            ptr_diff,
            self.gen.ptr_sized_int_type().const_zero(),
            "",
        );
        self.gen
            .builder
            .build_conditional_branch(is_true, body_block, else_block);

        self.gen.builder.position_at_end(body_block);
        self.visit_statements(&node.body)?;
        if !self.seen_return {
            self.gen.builder.build_unconditional_branch(merge_block);
        }

        if !node.orelse.is_empty() {
            self.gen.builder.position_at_end(else_block);
            self.visit_statements(&node.orelse)?;
            if !self.seen_return {
                self.gen.builder.build_unconditional_branch(merge_block);
            }
        }

        self.gen.builder.position_at_end(merge_block);
        self.seen_return = false;

        Ok(())
    }

    fn visit_with(&mut self, node: &ast::With) -> Self::T {
        visitor::walk_with(self, node);
        Ok(())
    }

    fn visit_async_with(&mut self, node: &ast::AsyncWith) -> Self::T {
        visitor::walk_async_with(self, node);
        Ok(())
    }

    fn visit_raise(&mut self, node: &ast::Raise) -> Self::T {
        visitor::walk_raise(self, node);
        Ok(())
    }

    fn visit_try(&mut self, node: &ast::Try) -> Self::T {
        visitor::walk_try(self, node);
        Ok(())
    }

    fn visit_assert(&mut self, node: &ast::Assert) -> Self::T {
        visitor::walk_assert(self, node);
        let mut expr_visitor = ExpressionVisitor::new(self.gen);
        let test = node.test.accept(&mut expr_visitor)?;
        let msg = match &node.msg {
            Some(msg) => Some(msg.accept(&mut expr_visitor)?),
            _ => None,
        };
        msg.map(|x| {
            self.gen.build_builtin_call(
                "py_assert_msg",
                &[
                    test.ptr().as_basic_value_enum(),
                    x.ptr().as_basic_value_enum(),
                ],
            )
        })
        .unwrap_or_else(|| {
            self.gen
                .build_builtin_call("py_assert", &[test.ptr().as_basic_value_enum()])
        });
        Ok(())
    }

    fn visit_import(&mut self, node: &ast::Import) -> Self::T {
        visitor::walk_import(self, node);
        Ok(())
    }

    fn visit_import_from(&mut self, node: &ast::ImportFrom) -> Self::T {
        visitor::walk_import_from(self, node);
        Ok(())
    }

    fn visit_global(&mut self, node: &ast::Global) -> Self::T {
        visitor::walk_global(self, node);
        Ok(())
    }

    fn visit_nonlocal(&mut self, node: &ast::Nonlocal) -> Self::T {
        visitor::walk_nonlocal(self, node);
        Ok(())
    }

    fn visit_expr(&mut self, node: &ast::Expr) -> Self::T {
        let mut expr_visitor = ExpressionVisitor::new(self.gen);
        node.value.accept(&mut expr_visitor)?;
        Ok(())
    }

    fn visit_pass(&mut self, node: &ast::Pass) -> Self::T {
        visitor::walk_pass(self, node);
        Ok(())
    }

    fn visit_break(&mut self, node: &ast::Break) -> Self::T {
        visitor::walk_break(self, node);
        Ok(())
    }

    fn visit_continue(&mut self, node: &ast::Continue) -> Self::T {
        visitor::walk_continue(self, node);
        Ok(())
    }

    fn visit_bool_op(&mut self, node: &ast::BoolOp) -> Self::T {
        visitor::walk_bool_op(self, node);
        Ok(())
    }

    fn visit_bin_op(&mut self, node: &ast::BinOp) -> Self::T {
        visitor::walk_bin_op(self, node);
        Ok(())
    }

    fn visit_unary_op(&mut self, node: &ast::UnaryOp) -> Self::T {
        visitor::walk_unary_op(self, node);
        Ok(())
    }

    fn visit_lambda(&mut self, node: &ast::Lambda) -> Self::T {
        visitor::walk_lambda(self, node);
        Ok(())
    }

    fn visit_if_exp(&mut self, node: &ast::IfExp) -> Self::T {
        visitor::walk_if_exp(self, node);
        Ok(())
    }

    fn visit_dict(&mut self, node: &ast::Dict) -> Self::T {
        visitor::walk_dict(self, node);
        Ok(())
    }

    fn visit_set(&mut self, node: &ast::Set) -> Self::T {
        visitor::walk_set(self, node);
        Ok(())
    }

    fn visit_list_comp(&mut self, node: &ast::ListComp) -> Self::T {
        visitor::walk_list_comp(self, node);
        Ok(())
    }

    fn visit_set_comp(&mut self, node: &ast::SetComp) -> Self::T {
        visitor::walk_set_comp(self, node);
        Ok(())
    }

    fn visit_dict_comp(&mut self, node: &ast::DictComp) -> Self::T {
        visitor::walk_dict_comp(self, node);
        Ok(())
    }

    fn visit_generator_exp(&mut self, node: &ast::GeneratorExp) -> Self::T {
        visitor::walk_generator_exp(self, node);
        Ok(())
    }

    fn visit_await(&mut self, node: &ast::Await) -> Self::T {
        visitor::walk_await(self, node);
        Ok(())
    }

    fn visit_yield(&mut self, node: &ast::Yield) -> Self::T {
        visitor::walk_yield(self, node);
        Ok(())
    }

    fn visit_yield_from(&mut self, node: &ast::YieldFrom) -> Self::T {
        visitor::walk_yield_from(self, node);
        Ok(())
    }

    fn visit_compare(&mut self, node: &ast::Compare) -> Self::T {
        visitor::walk_compare(self, node);
        Ok(())
    }

    fn visit_call(&mut self, node: &ast::Call) -> Self::T {
        visitor::walk_call(self, node);
        Ok(())
    }

    fn visit_num(&mut self, node: &ast::Num) -> Self::T {
        visitor::walk_num(self, node);
        Ok(())
    }

    fn visit_str(&mut self, node: &ast::Str) -> Self::T {
        visitor::walk_str(self, node);
        Ok(())
    }

    fn visit_formatted_value(&mut self, node: &ast::FormattedValue) -> Self::T {
        visitor::walk_formatted_value(self, node);
        Ok(())
    }

    fn visit_joined_str(&mut self, node: &ast::JoinedStr) -> Self::T {
        visitor::walk_joined_str(self, node);
        Ok(())
    }

    fn visit_bytes(&mut self, node: &ast::Bytes) -> Self::T {
        visitor::walk_bytes(self, node);
        Ok(())
    }

    fn visit_name_constant(&mut self, node: &ast::NameConstant) -> Self::T {
        visitor::walk_name_constant(self, node);
        Ok(())
    }

    fn visit_ellipsis(&mut self, node: &ast::Ellipsis) -> Self::T {
        visitor::walk_ellipsis(self, node);
        Ok(())
    }

    fn visit_attribute(&mut self, node: &ast::Attribute) -> Self::T {
        visitor::walk_attribute(self, node);
        Ok(())
    }

    fn visit_subscript(&mut self, node: &ast::Subscript) -> Self::T {
        visitor::walk_subscript(self, node);
        Ok(())
    }

    fn visit_starred(&mut self, node: &ast::Starred) -> Self::T {
        visitor::walk_starred(self, node);
        Ok(())
    }

    fn visit_name(&mut self, node: &ast::Name) -> Self::T {
        visitor::walk_name(self, node);
        Ok(())
    }

    fn visit_list(&mut self, node: &ast::List) -> Self::T {
        visitor::walk_list(self, node);
        Ok(())
    }

    fn visit_tuple(&mut self, node: &ast::Tuple) -> Self::T {
        visitor::walk_tuple(self, node);
        Ok(())
    }
}

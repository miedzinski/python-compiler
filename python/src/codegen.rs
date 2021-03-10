use std::cell::RefCell;
use std::convert::AsRef;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

use anyhow::{Context as _, Result};
use fnv::FnvHashMap;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, IntType, PointerType};
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::AddressSpace;
use python_syntax::parser::parse_module;
use python_syntax::visitor::Visitor;

use crate::error::LlvmError;
use crate::module::ModuleVisitor;
use crate::object::{Object, Signature, SymbolTable};

fn read_file<P: AsRef<Path>>(path: P) -> Result<String> {
    let mut file = File::open(path.as_ref())?;
    let mut ret = String::new();
    file.read_to_string(&mut ret)
        .with_context(|| format!("Failed to read input file {:?}", path.as_ref()))?;
    Ok(ret)
}

pub struct Scope<'ctx> {
    pub block: BasicBlock<'ctx>,
    // Module, class or function
    pub object: Rc<Object<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn new(block: BasicBlock<'ctx>, object: Rc<Object<'ctx>>) -> Scope<'ctx> {
        Scope { block, object }
    }

    pub fn symtable(&self) -> &SymbolTable<'ctx> {
        match &*self.object {
            Object::Module { symtable, .. }
            | Object::Function { symtable, .. }
            | Object::Class { symtable, .. } => symtable,
            _ => panic!("object associated with this scope is not a module, class, or function"),
        }
    }
}

pub struct Codegen<'l, 'ctx> {
    pub ctx: &'ctx Context,
    pub builder: &'l Builder<'ctx>,
    pub module: &'l Module<'ctx>,
    pub target: &'l TargetData,
    pub modules: SymbolTable<'ctx>,
    pub stack: Vec<Scope<'ctx>>,
}

impl<'l, 'ctx> Codegen<'l, 'ctx> {
    pub fn new(
        ctx: &'ctx Context,
        builder: &'l Builder<'ctx>,
        module: &'l Module<'ctx>,
        target: &'l TargetData,
    ) -> Codegen<'l, 'ctx> {
        Codegen {
            ctx,
            builder,
            module,
            target,
            modules: SymbolTable::default(),
            stack: vec![],
        }
    }

    pub fn compile_main<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        self.add_runtime_decls();
        self.setup_builtins();
        self.compile_module(path, "__main__")?;
        self.add_entry();
        self.module
            .verify()
            .map_err(LlvmError::from)
            .context("Failed to verify module")?;
        Ok(())
    }

    fn compile_module<P: AsRef<Path>>(&mut self, path: P, name: &str) -> Result<()> {
        let ast = parse_module(&read_file(path.as_ref())?)?;
        let mut vis = ModuleVisitor::new(self, name);
        vis.visit_module(&ast)
            .with_context(|| format!("Failed to compile {:?}", path.as_ref()))?;
        Ok(())
    }

    fn add_runtime_decls(&mut self) {
        self.module.add_global(self.ref_type(), None, "py_none");
        self.module
            .add_function("py_init", self.ctx.void_type().fn_type(&[], false), None);
        self.module.add_function(
            "py_incref",
            self.ctx
                .void_type()
                .fn_type(&[self.ref_type().as_basic_type_enum()], false),
            None,
        );
        self.module
            .add_function("py_decref", self.ctx.void_type().fn_type(&[], false), None);
        self.module.add_function(
            "py_object_add",
            self.ref_type().fn_type(
                &[
                    self.ref_type().as_basic_type_enum(),
                    self.ref_type().as_basic_type_enum(),
                ],
                false,
            ),
            None,
        );
        self.module.add_function(
            "py_list_new",
            self.ref_type()
                .fn_type(&[self.ptr_sized_int_type().as_basic_type_enum()], false),
            None,
        );
        self.module.add_function(
            "py_list_decref",
            self.ctx
                .void_type()
                .fn_type(&[self.ref_type().as_basic_type_enum()], false),
            None,
        );
        self.module.add_function(
            "py_list_set",
            self.ctx.void_type().fn_type(
                &[
                    self.ref_type().as_basic_type_enum(),
                    self.ptr_sized_int_type().as_basic_type_enum(),
                    self.ref_type().as_basic_type_enum(),
                ],
                false,
            ),
            None,
        );
        self.module
            .add_function("py_dict_new", self.ref_type().fn_type(&[], false), None);
        self.module.add_function(
            "py_int_from_bytes",
            self.ref_type().fn_type(
                &[
                    self.ctx
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .as_basic_type_enum(),
                    self.ptr_sized_int_type().as_basic_type_enum(),
                ],
                false,
            ),
            None,
        );
        self.module.add_function(
            "py_float_from_f64",
            self.ref_type()
                .fn_type(&[self.ctx.f64_type().as_basic_type_enum()], false),
            None,
        );
        self.module.add_function(
            "py_complex_from_f64",
            self.ref_type().fn_type(
                &[
                    self.ctx.f64_type().as_basic_type_enum(),
                    self.ctx.f64_type().as_basic_type_enum(),
                ],
                false,
            ),
            None,
        );
        self.module.add_function(
            "py_string_from_bytes",
            self.ref_type().fn_type(
                &[self
                    .ctx
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum()],
                false,
            ),
            None,
        );
        self.module.add_function(
            "py_print",
            self.ref_type()
                .fn_type(&[self.ref_type().as_basic_type_enum()], false),
            None,
        );
    }

    fn setup_builtins(&self) {
        let mut symtable = FnvHashMap::default();
        symtable.insert(
            "print".to_string(),
            Rc::new(Object::Function {
                ptr: self
                    .module
                    .get_function("py_print")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value(),
                signature: Signature {
                    args: vec![],
                    vararg: Some("objects".to_string()),
                    kwonlyargs: vec![],
                    kwarg: None,
                },
                symtable: SymbolTable::default(),
            }),
        );
        let module = Rc::new(Object::Module {
            symtable: RefCell::new(symtable),
        });
        self.modules
            .borrow_mut()
            .insert("builtins".to_string(), module);
    }

    fn add_entry(&mut self) {
        let main_fn_type = self.ctx.i32_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let block = self.ctx.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(block);
        self.builder
            .build_call(self.module.get_function("py_init").unwrap(), &[], "");
        self.builder
            .build_call(self.module.get_function("__main__").unwrap(), &[], "");
        self.builder
            .build_return(Some(&self.ctx.i32_type().const_zero()));
    }

    pub fn emit_bc<P: AsRef<Path>>(&self, out: P) -> Result<()> {
        self.module.write_bitcode_to_path(out.as_ref());
        Ok(())
    }

    pub fn emit_ir<P: AsRef<Path>>(&self, out: P) -> Result<()> {
        Ok(self.module.print_to_file(out).map_err(LlvmError::from)?)
    }

    pub fn ptr_sized_int_type(&self) -> IntType<'ctx> {
        self.ctx.ptr_sized_int_type(self.target, None)
    }

    pub fn ref_type(&self) -> PointerType<'ctx> {
        self.ctx.i8_type().ptr_type(AddressSpace::Generic)
    }

    pub fn scope(&self) -> &Scope<'ctx> {
        self.stack.last().unwrap()
    }

    pub fn build_builtin_call(
        &self,
        name: &str,
        args: &[BasicValueEnum<'ctx>],
    ) -> Rc<Object<'ctx>> {
        let fun = self.module.get_function(name).unwrap();
        let obj = self
            .builder
            .build_call(fun, args, "")
            .try_as_basic_value()
            .left()
            .map(|x| Object::Instance {
                ptr: x.into_pointer_value(),
                typ: None,
            })
            .unwrap();
        Rc::new(obj)
    }

    pub fn build_list(&self, contents: &[Rc<Object<'ctx>>]) -> Rc<Object<'ctx>> {
        let list = self.build_builtin_call(
            "py_list_new",
            &[self
                .ptr_sized_int_type()
                .const_int(contents.len() as u64, false)
                .as_basic_value_enum()],
        );
        for (idx, arg) in contents.iter().enumerate() {
            self.builder.build_call(
                self.module.get_function("py_incref").unwrap(),
                &[arg.ptr().as_basic_value_enum()],
                "",
            );
            self.builder.build_call(
                self.module.get_function("py_list_set").unwrap(),
                &[
                    list.ptr().as_basic_value_enum(),
                    self.ptr_sized_int_type()
                        .const_int(idx as u64, false)
                        .as_basic_value_enum(),
                    arg.ptr().as_basic_value_enum(),
                ],
                "",
            );
        }
        list
    }
}

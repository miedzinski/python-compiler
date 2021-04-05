use std::cell::RefCell;
use std::convert::AsRef;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

use anyhow::{Context as _, Result};
use fnv::FnvHashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, IntType, PointerType};
use inkwell::values::{BasicValue, BasicValueEnum, InstructionOpcode};
use inkwell::AddressSpace;
use python_syntax::parser::parse_module;
use python_syntax::visitor::Visitor;

use crate::error::LlvmError;
use crate::module::ModuleVisitor;
use crate::object::{Binding, Constant, Object, Parameter, Scope, Signature, Type};

fn read_file<P: AsRef<Path>>(path: P) -> Result<String> {
    let mut file = File::open(path.as_ref())?;
    let mut ret = String::new();
    file.read_to_string(&mut ret)
        .with_context(|| format!("Failed to read input file {:?}", path.as_ref()))?;
    Ok(ret)
}

pub struct Codegen<'l, 'ctx> {
    pub ctx: &'ctx Context,
    pub builder: &'l Builder<'ctx>,
    pub module: &'l Module<'ctx>,
    pub target: &'l TargetData,
    pub modules: FnvHashMap<String, Rc<RefCell<Scope<'ctx>>>>,
    pub stack: Vec<Rc<RefCell<Scope<'ctx>>>>,
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
            modules: FnvHashMap::default(),
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
        self.module.add_global(self.ref_type(), None, "py_true");
        self.module.add_global(self.ref_type(), None, "py_false");
        self.module
            .add_function("py_init", self.ctx.void_type().fn_type(&[], false), None);
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
            "py_list_from_slice",
            self.ref_type().fn_type(
                &[
                    self.ref_type()
                        .ptr_type(AddressSpace::Generic)
                        .as_basic_type_enum(),
                    self.ptr_sized_int_type().as_basic_type_enum(),
                ],
                false,
            ),
            None,
        );
        self.add_runtime_fn("py_bool", 1);
        self.add_runtime_fn("py_not", 1);
        self.add_runtime_fn("py_minus", 1);
        self.add_runtime_fn("py_add", 2);
        self.add_runtime_fn("py_sub", 2);
        self.add_runtime_fn("py_mul", 2);
        self.add_runtime_fn("py_div", 2);
        self.add_runtime_fn("py_mod", 2);
        self.add_runtime_fn("py_eq", 2);
        self.add_runtime_fn("py_neq", 2);
        self.add_runtime_fn("py_lt", 2);
        self.add_runtime_fn("py_lte", 2);
        self.add_runtime_fn("py_gt", 2);
        self.add_runtime_fn("py_gte", 2);
        self.add_runtime_fn("py_and", 2);
        self.add_runtime_fn("py_or", 2);
        self.add_runtime_fn("py_assert", 1);
        self.add_runtime_fn("py_assert_msg", 2);
        self.module.add_function(
            "py_print",
            self.ref_type()
                .fn_type(&[self.ref_type().as_basic_type_enum()], false),
            None,
        );
    }

    fn add_runtime_fn(&self, name: &str, arity: usize) {
        self.module.add_function(
            name,
            self.ref_type()
                .fn_type(&vec![self.ref_type().as_basic_type_enum(); arity], false),
            None,
        );
    }

    fn setup_builtins(&mut self) {
        let mut symtable = FnvHashMap::default();
        symtable.insert(
            "print".to_string(),
            Binding {
                ptr: self
                    .module
                    .get_function("py_print")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value(),
                typ: Type::Function {
                    signature: Signature {
                        args: vec![],
                        vararg: Some("objects".to_string()),
                        kwonlyargs: vec![],
                        kwarg: None,
                    },
                },
            },
        );
        symtable.insert(
            "bool".to_string(),
            Binding {
                ptr: self
                    .module
                    .get_function("py_bool")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value(),
                typ: Type::Function {
                    signature: Signature {
                        args: vec![Parameter::required("x".to_string())],
                        vararg: None,
                        kwonlyargs: vec![],
                        kwarg: None,
                    },
                },
            },
        );
        let module = Rc::new(RefCell::new(Scope::Module { symtable }));
        self.modules.insert("builtins".to_string(), module);
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

    pub fn scope(&self) -> Rc<RefCell<Scope<'ctx>>> {
        self.stack.last().cloned().unwrap()
    }

    pub fn build_load_constant(&self, constant: Constant) -> Rc<Object<'ctx>> {
        let name = match constant {
            Constant::None => "py_none",
            Constant::True => "py_true",
            Constant::False => "py_false",
        };
        let ptr = self.module.get_global(name).unwrap();
        let obj = self.builder.build_load(ptr.as_pointer_value(), "");
        Rc::new(Object::Instance {
            ptr: obj.into_pointer_value(),
        })
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
            })
            .unwrap();
        Rc::new(obj)
    }

    pub fn build_list(&self, contents: &[Rc<Object<'ctx>>]) -> Rc<Object<'ctx>> {
        let array = self
            .builder
            .build_alloca(self.ref_type().array_type(contents.len() as u32), "");
        for (idx, item) in contents.iter().enumerate() {
            let indices = [
                self.ptr_sized_int_type().const_zero(),
                self.ptr_sized_int_type().const_int(idx as u64, false),
            ];
            let slot = unsafe { self.builder.build_gep(array, &indices, "") };
            self.builder.build_store(slot, item.ptr());
        }
        let ptr = self.builder.build_cast(
            InstructionOpcode::BitCast,
            array,
            self.ref_type().ptr_type(AddressSpace::Generic),
            "",
        );

        self.build_builtin_call(
            "py_list_from_slice",
            &[
                ptr.as_basic_value_enum(),
                self.ptr_sized_int_type()
                    .const_int(contents.len() as u64, false)
                    .as_basic_value_enum(),
            ],
        )
    }
}

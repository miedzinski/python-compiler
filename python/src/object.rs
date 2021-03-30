use std::cell::RefCell;
use std::rc::Rc;

use fnv::FnvHashMap;
use inkwell::values::PointerValue;
use python_syntax::ast;

pub type SymbolTable<'ctx> = FnvHashMap<String, Rc<Object<'ctx>>>;

pub enum Object<'ctx> {
    Module {
        symtable: RefCell<SymbolTable<'ctx>>,
    },
    Class {
        ptr: PointerValue<'ctx>,
        def: ast::ClassDef,
        symtable: RefCell<SymbolTable<'ctx>>,
    },
    Function {
        ptr: PointerValue<'ctx>,
        signature: Signature<'ctx>,
        symtable: RefCell<SymbolTable<'ctx>>,
    },
    Instance {
        ptr: PointerValue<'ctx>,
        typ: Option<Rc<Object<'ctx>>>,
    },
}

impl<'ctx> Object<'ctx> {
    pub fn ptr(&self) -> PointerValue<'ctx> {
        match *self {
            Object::Class { ptr, .. }
            | Object::Function { ptr, .. }
            | Object::Instance { ptr, .. } => ptr,
            Object::Module { .. } => panic!("attempted to access module's pointer"),
        }
    }
}

pub struct Signature<'ctx> {
    pub args: Vec<Parameter<'ctx>>,
    pub vararg: Option<String>,
    pub kwonlyargs: Vec<Parameter<'ctx>>,
    pub kwarg: Option<String>,
}

impl<'ctx> Signature<'ctx> {
    pub fn position(&self, arg: &str) -> Option<usize> {
        self.args
            .binary_search_by(|x| x.name.as_str().cmp(arg))
            .or_else(|_| {
                self.kwonlyargs
                    .binary_search_by(|x| x.name.as_str().cmp(arg))
                    .map(|x| x + self.args.len())
            })
            .ok()
    }

    pub fn nth_slot(&self, n: usize) -> Option<&Parameter<'ctx>> {
        self.args
            .get(n)
            .or_else(|| self.kwonlyargs.get(n - self.args.len()))
    }

    pub fn slots(&self) -> usize {
        self.args.len() + self.kwonlyargs.len()
    }
}

pub struct Parameter<'ctx> {
    pub name: String,
    pub default: Option<Rc<Object<'ctx>>>,
}

impl<'ctx> Parameter<'ctx> {
    pub fn new(name: String, default: Option<Rc<Object<'ctx>>>) -> Parameter<'ctx> {
        Parameter { name, default }
    }

    pub fn required(name: String) -> Parameter<'ctx> {
        Parameter {
            name,
            default: None,
        }
    }
}

pub enum Constant {
    None,
    True,
    False,
}

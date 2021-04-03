use std::rc::Rc;

use fnv::FnvHashMap;
use inkwell::values::PointerValue;

pub type SymbolTable<'ctx> = FnvHashMap<String, Binding<'ctx>>;

pub enum Scope<'ctx> {
    Module { symtable: SymbolTable<'ctx> },
    Function { symtable: SymbolTable<'ctx> },
}

impl<'ctx> Scope<'ctx> {
    pub fn symtable(&self) -> &SymbolTable<'ctx> {
        match &*self {
            Scope::Module { symtable, .. } | Scope::Function { symtable, .. } => symtable,
        }
    }

    pub fn symtable_mut(&mut self) -> &mut SymbolTable<'ctx> {
        match &mut *self {
            Scope::Module { symtable, .. } | Scope::Function { symtable, .. } => symtable,
        }
    }
}

#[derive(Clone)]
pub struct Binding<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub typ: Type<'ctx>,
}

#[derive(Clone)]
pub enum Type<'ctx> {
    Function { signature: Signature<'ctx> },
    Instance,
}

pub enum Object<'ctx> {
    Function {
        ptr: PointerValue<'ctx>,
        signature: Signature<'ctx>,
    },
    Instance {
        ptr: PointerValue<'ctx>,
    },
}

impl<'ctx> Object<'ctx> {
    pub fn ptr(&self) -> PointerValue<'ctx> {
        match *self {
            Object::Function { ptr, .. } | Object::Instance { ptr, .. } => ptr,
        }
    }

    pub fn typ(&self) -> Type<'ctx> {
        match self {
            Object::Function { signature, .. } => Type::Function {
                signature: signature.clone(),
            },
            Object::Instance { .. } => Type::Instance,
        }
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
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

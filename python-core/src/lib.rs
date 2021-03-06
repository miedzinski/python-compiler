mod ops;

use std::ffi::CStr;
use std::fmt::{Debug, Display, Formatter, Write};
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::os::raw::c_char;
use std::{fmt, ptr, slice};

use boehm::BoehmAllocator;
use num_bigint::BigInt;

#[global_allocator]
static A: BoehmAllocator = BoehmAllocator;

pub struct Reference {
    inner: ManuallyDrop<Box<Object>>,
}

impl Reference {
    unsafe fn from_raw(ptr: *mut Object) -> Reference {
        Reference {
            inner: ManuallyDrop::new(Box::from_raw(ptr)),
        }
    }
}

impl Deref for Reference {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

impl DerefMut for Reference {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.inner
    }
}

impl Debug for Reference {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&**self, f)
    }
}

impl PartialEq for Reference {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&**self, &*other)
    }
}

#[derive(Debug)]
pub enum Object {
    None,
    Integer(BigInt),
    Float(f64),
    Bool(bool),
    String(String),
    List(Vec<Reference>),
}

impl Object {
    pub fn is_none(&self) -> bool {
        matches!(self, Object::None)
    }

    pub fn to_int(&self) -> &BigInt {
        if let Object::Integer(int) = self {
            int
        } else {
            panic!("{:?} is not an integer", self)
        }
    }

    pub fn to_float(&self) -> f64 {
        if let Object::Float(float) = self {
            *float
        } else {
            panic!("{:?} is not a float", self)
        }
    }

    pub fn to_bool(&self) -> bool {
        if let Object::Bool(bool) = self {
            *bool
        } else {
            panic!("{:?} is not a bool", self)
        }
    }

    pub fn to_string(&self) -> &str {
        if let Object::String(str) = self {
            str
        } else {
            panic!("{:?} is not a string", self)
        }
    }

    pub fn to_list(&self) -> &Vec<Reference> {
        if let Object::List(list) = self {
            list
        } else {
            panic!("{:?} is not a list", self)
        }
    }

    pub unsafe fn into_heap(self) -> *const Object {
        match self {
            x @ Object::None if NONE.is_null() => Box::into_raw(Box::new(x)),
            Object::None => NONE,
            x @ Object::Bool(true) if TRUE.is_null() => Box::into_raw(Box::new(x)),
            Object::Bool(true) => TRUE,
            x @ Object::Bool(false) if FALSE.is_null() => Box::into_raw(Box::new(x)),
            Object::Bool(false) => FALSE,
            x => Box::into_raw(Box::new(x)),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Object::None => f.write_str("None"),
            Object::Integer(int) => write!(f, "{}", int),
            Object::Float(float) => write!(f, "{}", float),
            Object::Bool(bool) => match bool {
                true => f.write_str("True"),
                false => f.write_str("False"),
            },
            Object::String(str) => write!(f, "{}", str),
            Object::List(list) => {
                f.write_char('[')?;
                let mut iter = list.iter();
                if let Some(e) = iter.next() {
                    write!(f, "{}", e)?
                }
                for e in iter {
                    write!(f, ", {}", e)?
                }
                f.write_char(']')
            }
        }
    }
}

#[export_name = "py_none"]
pub static mut NONE: *const Object = ptr::null();
#[export_name = "py_true"]
pub static mut TRUE: *const Object = ptr::null();
#[export_name = "py_false"]
pub static mut FALSE: *const Object = ptr::null();

#[no_mangle]
pub unsafe extern "C" fn py_init() {
    BoehmAllocator::init();
    NONE = Object::None.into_heap();
    TRUE = Object::Bool(true).into_heap();
    FALSE = Object::Bool(false).into_heap();
}

#[no_mangle]
pub unsafe extern "C" fn py_int_from_bytes(bytes: *const u8, len: usize) -> *const Object {
    let int = BigInt::from_signed_bytes_le(slice::from_raw_parts(bytes, len));
    Object::Integer(int).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_float_from_f64(val: f64) -> *const Object {
    Object::Float(val).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_string_from_bytes(bytes: *const c_char) -> *const Object {
    let str = CStr::from_ptr(bytes).to_str().unwrap().to_string();
    Object::String(str).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_list_from_slice(
    objects: *const *mut Object,
    len: usize,
) -> *const Object {
    let pointers: &[*mut Object] = slice::from_raw_parts(objects, len);
    let mut list: Vec<Reference> = Vec::with_capacity(len);
    for ptr in pointers {
        list.push(Reference::from_raw(*ptr));
    }
    Object::List(list).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_bool(val: *const Object) -> *const Object {
    (*val).truth().into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_not(val: *const Object) -> *const Object {
    (!(&*val)).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_minus(val: *const Object) -> *const Object {
    (-(&*val)).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_add(lhs: *const Object, rhs: *const Object) -> *const Object {
    (&*lhs + &*rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_sub(lhs: *const Object, rhs: *const Object) -> *const Object {
    (&*lhs - &*rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_mul(lhs: *const Object, rhs: *const Object) -> *const Object {
    (&*lhs * &*rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_div(lhs: *const Object, rhs: *const Object) -> *const Object {
    (&*lhs / &*rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_mod(lhs: *const Object, rhs: *const Object) -> *const Object {
    (&*lhs % &*rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_eq(lhs: *const Object, rhs: *const Object) -> *const Object {
    Object::Bool(*lhs == *rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_neq(lhs: *const Object, rhs: *const Object) -> *const Object {
    Object::Bool(*lhs != *rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_lt(lhs: *const Object, rhs: *const Object) -> *const Object {
    Object::Bool(*lhs < *rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_lte(lhs: *const Object, rhs: *const Object) -> *const Object {
    Object::Bool(*lhs <= *rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_gt(lhs: *const Object, rhs: *const Object) -> *const Object {
    Object::Bool(*lhs > *rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_gte(lhs: *const Object, rhs: *const Object) -> *const Object {
    Object::Bool(*lhs >= *rhs).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_and(lhs: *const Object, rhs: *const Object) -> *const Object {
    Object::Bool((*lhs).truth().to_bool() && (*rhs).truth().to_bool()).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_or(lhs: *const Object, rhs: *const Object) -> *const Object {
    Object::Bool((*lhs).truth().to_bool() || (*rhs).truth().to_bool()).into_heap()
}

#[no_mangle]
pub unsafe extern "C" fn py_assert(test: *const Object) -> *const Object {
    if !(*test).to_bool() {
        eprintln!("Assertion error");
        std::process::exit(1)
    }
    NONE
}

#[no_mangle]
pub unsafe extern "C" fn py_assert_msg(test: *const Object, msg: *const Object) -> *const Object {
    if !(*test).to_bool() {
        eprintln!("Assertion error: {}", (*msg).to_string());
        std::process::exit(1)
    }
    NONE
}

#[no_mangle]
pub unsafe extern "C" fn py_print(args: *const Object) -> *const Object {
    if !args.is_null() {
        print!(
            "{}",
            (*args)
                .to_list()
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
    println!();
    NONE
}

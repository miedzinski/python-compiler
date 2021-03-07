use std::ffi::CStr;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops::{Deref, DerefMut};
use std::os::raw::c_char;
use std::rc::Rc;
use std::slice;

use fnv::FnvHashMap;
use num_bigint::BigInt;

#[repr(C)]
#[derive(Debug)]
pub struct ObjectRef(*mut Object);

impl ObjectRef {
    fn new(obj: Object) -> ObjectRef {
        ObjectRef(Rc::into_raw(Rc::new(obj)) as *mut Object)
    }

    unsafe fn inc(&self) -> ObjectRef {
        let obj = Rc::from_raw(self.0);
        mem::forget(obj.clone());
        mem::forget(obj);
        ObjectRef(self.0)
    }

    unsafe fn dec(&self) {
        Rc::from_raw(self.0);
    }

    unsafe fn deref(&self) -> &Object {
        &*self.0
    }

    unsafe fn deref_mut(&mut self) -> &mut Object {
        &mut *self.0
    }

    fn is_null(&self) -> bool {
        self.0.is_null()
    }
}

impl Clone for ObjectRef {
    fn clone(&self) -> Self {
        unsafe {
            self.inc();
        }
        ObjectRef(self.0)
    }
}

impl Hash for ObjectRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let obj = unsafe { Rc::from_raw(self.0) };
        obj.hash(state);
        mem::forget(obj);
    }
}

impl PartialEq for ObjectRef {
    fn eq(&self, other: &Self) -> bool {
        let this = unsafe { Rc::from_raw(self.0) };
        let other = unsafe { Rc::from_raw(other.0) };
        let eq = this.eq(&other);
        mem::forget(this);
        mem::forget(other);
        eq
    }
}

impl Eq for ObjectRef {}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Object {
    None,
    Integer(BigInt),
    Bool(bool),
    Float(Float),
    Complex(Complex),
    List(List),
    Dict(Dict),
    String(String),
    Function,
    Class,
    Instance,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Object::None => f.write_str("None"),
            Object::Integer(int) => write!(f, "{}", int),
            Object::Bool(bool) => write!(f, "{}", bool),
            Object::Float(float) => write!(f, "{}", float),
            Object::Complex(complex) => write!(f, "{}", complex),
            Object::List(list) => write!(f, "{}", list),
            Object::Dict(dict) => write!(f, "{}", dict),
            Object::String(str) => write!(f, "{}", str),
            Object::Function => write!(f, "<function>"),
            Object::Class => write!(f, "<class>"),
            Object::Instance => write!(f, "<instance>"),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Float(u64);

impl Float {
    fn new(mut val: f64) -> Float {
        if val.is_nan() {
            val = f64::NAN;
        }
        Float(val.to_bits())
    }

    fn get(&self) -> f64 {
        f64::from_bits(self.0)
    }
}

impl Display for Float {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Complex(u64, u64);

impl Complex {
    fn new(mut real: f64, mut imag: f64) -> Complex {
        if real.is_nan() {
            real = f64::NAN;
        }
        if imag.is_nan() {
            imag = f64::NAN;
        }
        Complex(real.to_bits(), imag.to_bits())
    }

    fn real(&self) -> f64 {
        f64::from_bits(self.0)
    }

    fn imag(&self) -> f64 {
        f64::from_bits(self.1)
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}+{}j)", self.real(), self.imag())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct List(Vec<ObjectRef>);

impl List {
    fn with_len(capacity: usize) -> List {
        List(vec![unsafe { NONE.inc() }; capacity])
    }
}

impl Deref for List {
    type Target = [ObjectRef];

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl DerefMut for List {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

impl From<&[ObjectRef]> for List {
    fn from(slice: &[ObjectRef]) -> List {
        List(slice.to_vec())
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for List {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("unhashable type")
    }
}

impl Display for List {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char('[')?;
        let mut iter = self.0.iter();
        unsafe {
            if let Some(item) = iter.next() {
                write!(f, "{}", item.deref())?;
            }
            for item in iter {
                write!(f, ", {}", item.deref())?;
            }
        }
        f.write_char(']')
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Dict(FnvHashMap<ObjectRef, ObjectRef>);

impl Dict {
    fn new() -> Dict {
        Dict(FnvHashMap::default())
    }

    fn get(&self, key: ObjectRef) -> Option<ObjectRef> {
        self.0.get(&key).map(|x| unsafe { x.inc() })
    }

    fn insert(&mut self, key: ObjectRef, value: ObjectRef) -> Option<ObjectRef> {
        self.0.insert(key, value)
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Dict {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("unhashable type")
    }
}

impl Display for Dict {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char('[')?;
        let mut iter = self.0.iter();
        unsafe {
            if let Some((key, value)) = iter.next() {
                write!(f, "{}: {}", key.deref(), value.deref())?;
            }
            for (key, value) in iter {
                write!(f, ", {}: {}", key.deref(), value.deref())?;
            }
        }
        f.write_char(']')
    }
}

#[export_name = "py_none"]
pub static mut NONE: ObjectRef = ObjectRef(0 as *mut Object);

#[no_mangle]
pub unsafe extern "C" fn py_init() {
    NONE = ObjectRef::new(Object::None);
}

#[no_mangle]
pub unsafe extern "C" fn py_incref(obj: ObjectRef) {
    obj.inc();
}

#[no_mangle]
pub unsafe extern "C" fn py_decref(obj: ObjectRef) {
    obj.dec();
}

#[no_mangle]
pub unsafe extern "C" fn py_object_add(a: ObjectRef, b: ObjectRef) -> ObjectRef {
    match (a.deref(), b.deref()) {
        (Object::Integer(a), Object::Integer(b)) => ObjectRef::new(Object::Integer(a + b)),
        _ => unimplemented!("py_object_add not implemented"),
    }
}

#[no_mangle]
pub extern "C" fn py_list_new(len: usize) -> ObjectRef {
    ObjectRef::new(Object::List(List::with_len(len)))
}

#[no_mangle]
pub unsafe extern "C" fn py_list_decref(mut list: ObjectRef) {
    if let Object::List(list) = list.deref_mut() {
        for e in list.iter() {
            e.dec();
        }
    }
    list.dec();
}

#[no_mangle]
pub unsafe extern "C" fn py_list_set(mut list: ObjectRef, idx: usize, item: ObjectRef) {
    if let Object::List(list) = list.deref_mut() {
        let slot = &mut list[idx];
        slot.dec();
        *slot = item;
    }
}

#[no_mangle]
pub extern "C" fn py_dict_new() -> ObjectRef {
    ObjectRef::new(Object::Dict(Dict::new()))
}

#[no_mangle]
pub unsafe extern "C" fn py_int_from_bytes(bytes: *const u8, len: usize) -> ObjectRef {
    ObjectRef::new(Object::Integer(BigInt::from_signed_bytes_le(
        slice::from_raw_parts(bytes, len),
    )))
}

#[no_mangle]
pub extern "C" fn py_float_from_f64(val: f64) -> ObjectRef {
    ObjectRef::new(Object::Float(Float::new(val)))
}

#[no_mangle]
pub extern "C" fn py_complex_from_f64(real: f64, imag: f64) -> ObjectRef {
    ObjectRef::new(Object::Complex(Complex::new(real, imag)))
}

#[no_mangle]
pub unsafe extern "C" fn py_string_from_bytes(bytes: *const c_char) -> ObjectRef {
    ObjectRef::new(Object::String(
        CStr::from_ptr(bytes).to_str().unwrap().to_string(),
    ))
}

#[no_mangle]
pub unsafe extern "C" fn py_print(
    _args: ObjectRef,
    vararg: ObjectRef,
    _kwarg: ObjectRef,
) -> ObjectRef {
    if !vararg.is_null() {
        if let Object::List(list) = vararg.deref() {
            print!(
                "{}",
                list.iter()
                    .map(|x| format!("{}", x.deref()))
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        }
    }
    println!();
    NONE.inc()
}

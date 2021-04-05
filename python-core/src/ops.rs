use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};

use num_bigint::ToBigInt;
use num_traits::ToPrimitive;

use crate::Object;

impl Object {
    pub fn truth(&self) -> Object {
        match self {
            Object::None => Object::Bool(false),
            Object::Integer(int) if *int == 0.to_bigint().unwrap() => Object::Bool(false),
            Object::Integer(_) => Object::Bool(true),
            Object::Float(float) if *float == 0f64 => Object::Bool(false),
            Object::Float(_) => Object::Bool(true),
            Object::Bool(bool) => Object::Bool(*bool),
            Object::String(str) if str.is_empty() => Object::Bool(false),
            Object::String(_) => Object::Bool(true),
            Object::List(list) if list.is_empty() => Object::Bool(false),
            Object::List(_) => Object::Bool(true),
        }
    }
}

impl Not for &Object {
    type Output = Object;

    fn not(self) -> Self::Output {
        match self.truth() {
            Object::Bool(bool) => Object::Bool(!bool),
            _ => panic!("Object::truth didn't return Bool"),
        }
    }
}

impl Neg for &Object {
    type Output = Object;

    fn neg(self) -> Self::Output {
        match self {
            Object::Integer(int) => Object::Integer(-int.clone()),
            Object::Float(float) => Object::Float(-*float),
            _ => unimplemented!(),
        }
    }
}

macro_rules! forward_unaryop {
    (impl $trait:ident, $method:ident for $t:ty) => {
        impl $trait for $t {
            type Output = $t;

            fn $method(self) -> Self::Output {
                $trait::$method(&self)
            }
        }
    };
}

forward_unaryop!(impl Not, not for Object);
forward_unaryop!(impl Neg, neg for Object);

impl Add for &Object {
    type Output = Object;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Integer(lhs), Object::Integer(rhs)) => Object::Integer(lhs + rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => {
                Object::Float(lhs.to_f64().unwrap() + rhs)
            }
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs + rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => {
                Object::Float(lhs + rhs.to_f64().unwrap())
            }
            (Object::String(lhs), Object::String(rhs)) => Object::String(format!("{}{}", lhs, rhs)),
            _ => unimplemented!(),
        }
    }
}

impl Sub for &Object {
    type Output = Object;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Integer(lhs), Object::Integer(rhs)) => Object::Integer(lhs - rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => {
                Object::Float(lhs.to_f64().unwrap() - rhs)
            }
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs - rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => {
                Object::Float(lhs - rhs.to_f64().unwrap())
            }
            _ => unimplemented!(),
        }
    }
}

impl Mul for &Object {
    type Output = Object;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Integer(lhs), Object::Integer(rhs)) => Object::Integer(lhs * rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => {
                Object::Float(lhs.to_f64().unwrap() * rhs)
            }
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs * rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => {
                Object::Float(lhs * rhs.to_f64().unwrap())
            }
            _ => unimplemented!(),
        }
    }
}

impl Div for &Object {
    type Output = Object;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Integer(lhs), Object::Integer(rhs)) => {
                Object::Float(lhs.to_f64().unwrap() / rhs.to_f64().unwrap())
            }
            (Object::Integer(lhs), Object::Float(rhs)) => {
                Object::Float(lhs.to_f64().unwrap() / rhs)
            }
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs / rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => {
                Object::Float(lhs * rhs.to_f64().unwrap())
            }
            _ => unimplemented!(),
        }
    }
}

impl Rem for &Object {
    type Output = Object;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Integer(lhs), Object::Integer(rhs)) => Object::Integer(lhs % rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => {
                Object::Float(lhs.to_f64().unwrap() % rhs)
            }
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs % rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => {
                Object::Float(lhs % rhs.to_f64().unwrap())
            }
            _ => unimplemented!(),
        }
    }
}

macro_rules! forward_binop {
    (impl $trait:ident, $method:ident) => {
        impl $trait for Object {
            type Output = Object;

            fn $method(self, other: Self) -> Self::Output {
                $trait::$method(&self, &other)
            }
        }
    };
}

forward_binop!(impl Add, add);
forward_binop!(impl Sub, sub);
forward_binop!(impl Mul, mul);
forward_binop!(impl Div, div);
forward_binop!(impl Rem, rem);

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::None, Object::None) => true,
            (Object::None, _) | (_, Object::None) => false,
            (Object::Integer(lhs), Object::Integer(rhs)) => lhs == rhs,
            (Object::Integer(lhs), Object::Float(rhs)) => lhs.to_f64().unwrap() == *rhs,
            (Object::Float(lhs), Object::Float(rhs)) => lhs == rhs,
            (Object::Float(lhs), Object::Integer(rhs)) => *lhs == rhs.to_f64().unwrap(),
            (Object::Bool(lhs), Object::Bool(rhs)) => lhs == rhs,
            (Object::String(lhs), Object::String(rhs)) => lhs == rhs,
            (Object::List(lhs), Object::List(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Object::Integer(lhs), Object::Integer(rhs)) => lhs.partial_cmp(rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => lhs.to_f64().unwrap().partial_cmp(rhs),
            (Object::Float(lhs), Object::Float(rhs)) => lhs.partial_cmp(rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => lhs.partial_cmp(&rhs.to_f64().unwrap()),
            _ => unimplemented!(),
        }
    }
}

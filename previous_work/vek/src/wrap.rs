extern crate num_integer;
extern crate num_traits;

use self::num_integer::Integer;
use self::num_traits::{Float, FloatConst};

pub trait WrapFloat: Float {
    fn wrapped(self, l:Self) -> Self {
        self - (self/l).floor() * l
    }
}
pub trait Wrap2PI: WrapFloat + FloatConst {
    fn wrapped_2pi(self) -> Self {
        self.wrapped(Self::PI()+Self::PI())
    }
}
pub trait WrapInteger: Integer + Clone {
    fn wrapped(self, l:Self) -> Self {
        let clone = self.clone();
        let l_clone = l.clone();
        self - (clone/l) * l_clone
    }
}

pub fn wrapf<T: WrapFloat>(x: T, l: T) -> T {
    x.wrapped(l)
}
pub fn wrap<T: WrapInteger>(x: T, l: T) -> T {
    x.wrapped(l)
}
pub fn wrap_2pi<T: Wrap2PI>(x: T) -> T {
    x.wrapped_2pi()
}

impl WrapFloat   for f32 {}
impl WrapFloat   for f64 {}
impl Wrap2PI     for f32 {}
impl Wrap2PI     for f64 {}
impl WrapInteger for i8  {}
impl WrapInteger for i16 {}
impl WrapInteger for i32 {}
impl WrapInteger for i64 {}
impl WrapInteger for isize {}
impl WrapInteger for u8  {}
impl WrapInteger for u16 {}
impl WrapInteger for u32 {}
impl WrapInteger for u64 {}
impl WrapInteger for usize {}

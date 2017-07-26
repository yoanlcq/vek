extern crate num_traits;

use self::num_traits::{Zero, One};

pub trait PartialMinMax: PartialOrd + Sized {
    fn partial_min(v1: Self, v2: Self) -> Self {
        if v1 < v2 { v1 } else { v2 }
    }
    fn partial_max(v1: Self, v2: Self) -> Self {
        if v1 > v2 { v1 } else { v2 }
    }
}

pub fn partial_max<T: PartialMinMax>(v1: T, v2: T) -> T {
    T::partial_max(v1, v2)
}
pub fn partial_min<T: PartialMinMax>(v1: T, v2: T) -> T {
    T::partial_min(v1, v2)
}

pub trait Clamp: PartialMinMax {
    fn clamped(self, lower: Self, upper: Self) -> Self {
        Self::partial_min(Self::partial_max(self, lower), upper)
    }
    fn is_between(self, lower: Self, upper: Self) -> bool {
        lower <= self && self <= upper
    }
}
pub trait Clamp01: Zero + One + Clamp {
    fn clamped01(self) -> Self {
        self.clamped(Self::zero(), Self::one())
    }
    fn is_between01(self) -> bool {
        self.is_between(Self::zero(), Self::one())
    }
}
pub fn clamp<T: Clamp>(x: T, lower: T, upper: T) -> T {
    x.clamped(lower, upper)
}
pub fn clamp01<T: Clamp01>(x: T) -> T {
    x.clamped01()
}

macro_rules! impl_clamp {
    ($($T:ty)+) => {
        $(
            impl PartialMinMax for $T {}
            impl Clamp for $T {}
            impl Clamp01 for $T {}
        )+
    }
}

impl_clamp!(f32 f64 i8 i16 i32 i64 u8 u16 u32 u64 isize usize);

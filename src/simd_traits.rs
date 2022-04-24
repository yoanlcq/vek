//! SIMD traits; they are useful when the features "repr_simd" and "platform_intrinsics" are enabled.

use std::num::Wrapping;
use num_traits;

/// This trait should be implemented by scalar types, vectors of which are supported by SIMD intrinsics.
/// For instance, i16 and f32 can implement this trait, as well as `#[repr(transparent)]` wrappers of these, but not hand-built numeric types.
pub trait SimdElement {
    /// The corresponding mask type for this element type: this is usually the unsigned integer type with the same size.
    type SimdMaskType: SimdMask;
}

/// Implemented by unsigned integer types that can represent the result of SIMD comparison elements: semantically, these are booleans, but in their representation, zero is false and any other value is true.
/// Typically, SIMD comparison of two vectors will yield a vector of mask elements, on which you can call `reduce_and()` or `reduce_or()`.
pub trait SimdMask: num_traits::sign::Unsigned + num_traits::bounds::Bounded + num_traits::cast::FromPrimitive {
    /// Used for fallback code.
    #[inline]
    fn from_bool(b: bool) -> Self {
        Self::from_u8(b as _).unwrap()
    }
}

macro_rules! impl_simd_mask {
    ($T:ty) => {
        impl SimdMask for $T {}
    }
}

macro_rules! impl_simd_element {
    ($T:ty, $M:ty) => {
        impl SimdElement for $T {
            type SimdMaskType = $M;
        }
    }
}

impl_simd_mask!{u8}
impl_simd_mask!{u16}
impl_simd_mask!{u32}
impl_simd_mask!{u64}

impl<T: SimdMask> SimdMask for Wrapping<T> where Wrapping<T>: num_traits::sign::Unsigned {}

impl_simd_element!{i8, u8}
impl_simd_element!{i16, u16}
impl_simd_element!{i32, u32}
impl_simd_element!{i64, u64}
impl_simd_element!{u8, u8}
impl_simd_element!{u16, u16}
impl_simd_element!{u32, u32}
impl_simd_element!{u64, u64}
impl_simd_element!{f32, u32}
impl_simd_element!{f64, u64}

impl<T: SimdElement> SimdElement for Wrapping<T> {
    type SimdMaskType = T::SimdMaskType; // Don't propagate the Wrapping<>, masks are only intended for reduction to booleans; it doesn't make sense to perform arithmetic on them.
}
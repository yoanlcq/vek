//! NOTE: If you want to lerp with integer types, convert them to floats first, lerp on them, then
//! convert them back to integers. This eases our implementation and gives you explicit control
//! over the conversion behavior.


// WISH: would be good to be able to directly Lerp on Rgb<T: ColorChannel> though

extern crate num_traits;

use self::num_traits::One;
use core::ops::*;
use vec::repr_simd::*;
use vec::repr_c_aliases::*;
use clamp::{Clamp01, clamp01};

pub trait Lerp<Progress=f32>: Sized + Add<Output=Self> + Mul<Progress, Output=Self>
{
    fn lerp_unclamped(from: Self, to: Self, progress: Progress) -> Self
        where Progress : Clone + One + Sub<Output=Progress>
    {
        let progress_dup = progress.clone();
        from*(Progress::one()-progress) + to*progress_dup
    }
    fn lerp(from: Self, to: Self, progress: Progress) -> Self
        where Progress : Clone + Clamp01 + Sub<Output=Progress>
    {
        Self::lerp_unclamped(from, to, clamp01(progress))
    }
}

pub fn lerp_unclamped<Progress, T>(from: T, to: T, progress: Progress) -> T 
    where T: Lerp<Progress>, Progress : Clone + One + Sub<Output=Progress>
{
    T::lerp_unclamped(from, to, progress)
}
pub fn lerp<Progress, T>(from: T, to: T, progress: Progress) -> T
    where T: Lerp<Progress>, Progress : Clone + Clamp01 + Sub<Output=Progress>
{
    T::lerp(from, to, progress)
}

impl Lerp<f32> for f32 {}
impl Lerp<f64> for f64 {}

macro_rules! lerp_impl_for_vecs {
    ($($Vec:ident)+) => {
        $(
            impl<T: Clone + Clamp01 + Sub<Output=T>> Lerp<     T > for $Vec <T> {}
            // WISH: Extend Clamp to vectors somehow
            // impl<T: Clone + Clamp01 + Sub<Output=T>> Lerp<$Vec<T>> for $Vec <T> {}
        )+
    }
}

lerp_impl_for_vecs!(Vec4 Vec3 Vec2 Xyzw Xyz Xy Rgba Rgb Uvw Uv Extent3 Extent2);
lerp_impl_for_vecs!(CVec4 CVec3 CVec2 CXyzw CXyz CXy CRgba CRgb CUvw CUv CExtent3 CExtent2);

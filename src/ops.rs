//! Operations defined by this crate, such as `MulAdd`, `Lerp`, `LerpUnclamped`, and `WrapFloat`.

use core::num::Wrapping;
use core::ops::*;
use num_traits::{Zero, One, Float, FloatConst};

pub trait ElementwisePartialMinMax {
    fn partial_min(a: Self, b: Self) -> Self;
    fn partial_max(a: Self, b: Self) -> Self;
}

/// Compares and returns the minimum of two values, using partial ordering.
pub fn partial_min<T: ElementwisePartialMinMax>(a: T, b: T) -> T {
    T::partial_min(a, b)
}
/// Compares and returns the maximum of two values, using partial ordering.
pub fn partial_max<T: ElementwisePartialMinMax>(a: T, b: T) -> T {
    T::partial_max(a, b)
}

/// A value that can be constrained to be between two values (inclusive).
pub trait Clamp<Bound=Self>: Broadcast<Bound> {
    /// `Self` for scalar, and a vector of `bool`s for vectors.
    type BoolVector;
    /// Constrains this value to be between `lower` and `upper` (inclusive).
    ///
    /// This would rather make use of inclusive ranges, but it's an unstable
    /// feature.
    ///
    /// ```
    /// assert_eq!(7.clamped(5, 10), 7);
    /// assert_eq!(4.clamped(5, 10), 4);
    /// assert_eq!(5.clamped(5, 10), 5);
    /// assert_eq!(10.clamped(5, 10), 10);
    /// assert_eq!(11.clamped(5, 10), 10);
    /// ```
    fn clamped(self, lower: Bound, upper: Bound) -> Self {
	partial_min(partial_max(self, Self::broadcast(lower)), Self::broadcast(upper))
    }

    /// Alias to `clamped`, which doesn't take `self`.
    fn clamp(val: Self, lower: Bound, upper: Bound) -> Self {
	val.clamped(lower, upper)
    }

    /// Returns whether this value is between `lower` and `upper` (inclusive).
    ///
    /// This would rather make use of inclusive ranges, but it's an unstable
    /// feature.
    /// ```
    /// assert!(5 .is_between(5, 10));
    /// assert!(7 .is_between(5, 10));
    /// assert!(10.is_between(5, 10));
    /// assert!(!(4 .is_between(5, 10)));
    /// assert!(!(11.is_between(5, 10)));
    /// ```
    fn is_between(self, lower: Bound, upper: Bound) -> Self::BoolVector;
}

/// A value that can be constrained to be between 0 and 1 (inclusive).
pub trait Clamp01: Zero + One + Clamp {
    /// Constrains this value to be between 0 and 1 (inclusive).
    fn clamped01(self) -> Self {
        self.clamped(Self::zero(), Self::one())
    }
    /// Constrains this value to be between 0 and 1 (inclusive).
    fn clamp01(val: Self) -> Self {
        Self::clamp(val, Self::zero(), Self::one())
    }
    /// Returns whether this value is between 0 and 1 (inclusive).
    fn is_between01(self) -> Self::BoolVector {
        self.is_between(Self::zero(), Self::one())
    }
}
/// Constrains a value to be between `lower` and `upper` (inclusive).
pub fn clamp<B, T: Clamp<B>>(val: T, lower: B, upper: B) -> T {
    T::clamp(val, lower, upper)
}
/// Constrains a value to be between 0 and 1 (inclusive).
pub fn clamp01<T: Clamp01>(x: T) -> T {
    x.clamped01()
}

macro_rules! impl_clamp {
    ($($T:ty)+) => {
        $(
            impl Clamp for $T {}
            impl Clamp01 for $T {}
        )+
    }
}

impl_clamp!(f32 f64 i8 i16 i32 i64 u8 u16 u32 u64 isize usize);



/// The fused multiply-add operation.
pub trait MulAdd<MulRhs=Self, AddRhs=Self> {
	/// The resulting type after applying the fused multiply-add operation.
    type Output;
	/// Returns `(self * mul) + add`.
    fn mul_add(self, mul: MulRhs, add: AddRhs) -> Self::Output;
}

/// Returns `(val * mul) + add`.
pub fn mul_add<Output,V,M,A>(val: V, mul: M, add: A) -> Output where V: MulAdd<M,A,Output=Output> {
	val.mul_add(mul, add)
}

macro_rules! impl_muladd_content {
    ($Out:ty, $MulRhs:ty, $AddRhs:ty) => {
	type Output = $Out;
	fn mul_add(self, mul: $MulRhs, add: $AddRhs) -> Self::Output {
	    self * mul + add
	}
    }
}
// PERF: Implement this more efficiently for floats (with intrinsics or something)
macro_rules! impl_muladd {
    ($($ty:ty)+) => {
	$(
	    impl             MulAdd<    $ty,     $ty> for     $ty { impl_muladd_content!{$ty,     $ty,     $ty} }
	    impl<        'c> MulAdd<    $ty, &'c $ty> for     $ty { impl_muladd_content!{$ty,     $ty, &'c $ty} }
	    impl<    'b    > MulAdd<&'b $ty,     $ty> for     $ty { impl_muladd_content!{$ty, &'b $ty,     $ty} }
	    impl<    'b, 'c> MulAdd<&'b $ty, &'c $ty> for     $ty { impl_muladd_content!{$ty, &'b $ty, &'c $ty} }
	    impl<'a,       > MulAdd<    $ty,     $ty> for &'a $ty { impl_muladd_content!{$ty,     $ty,     $ty} }
	    impl<'a,     'c> MulAdd<    $ty, &'c $ty> for &'a $ty { impl_muladd_content!{$ty,     $ty, &'c $ty} }
	    impl<'a, 'b,   > MulAdd<&'b $ty,     $ty> for &'a $ty { impl_muladd_content!{$ty, &'b $ty,     $ty} }
	    impl<'a, 'b, 'c> MulAdd<&'b $ty, &'c $ty> for &'a $ty { impl_muladd_content!{$ty, &'b $ty, &'c $ty} }
	)+
    }
}

impl_muladd!{
    i8 i16 i32 i64 isize u8 u16 u32 u64 usize f32 f64
    Wrapping<i8>
    Wrapping<i16>
    Wrapping<i32>
    Wrapping<i64>
    Wrapping<isize>
    Wrapping<u8>
    Wrapping<u16>
    Wrapping<u32>
    Wrapping<u64>
    Wrapping<usize>
}


// WISH: would be good to be able to directly Lerp on Rgb<T: ColorChannel> though

/// A value that can be linearly interpolated, without constraining the factor to the [0, 1] range.
///
/// Note: If you want to lerp with integer types, convert them to floats first, lerp on them, then
/// convert them back to integers. This eases our implementation and gives you explicit control
/// over the conversion behavior.
pub trait LerpUnclamped<Progress=f32>: Sized + Add<Output=Self> + Mul<Progress, Output=Self>
{
	/// Returns the linear interpolation of `from` to `to` with `progress` as the factor.  
	/// `progress` is NOT constrained to be between 0 and 1 (inclusive).
	///
	/// This would make use of inclusive ranges, but they aren't stable yet.
    fn lerp_unclamped(from: Self, to: Self, progress: Progress) -> Self
        where Progress : Clone + One + Sub<Output=Progress>
    {
        let progress_dup = progress.clone();
        from*(Progress::one()-progress) + to*progress_dup
    }
}
/// A value that can be linearly interpolated.
///
/// Note: If you want to lerp with integer types, convert them to floats first, lerp on them, then
/// convert them back to integers. This eases our implementation and gives you explicit control
/// over the conversion behavior.
pub trait Lerp<Progress=f32>: LerpUnclamped<Progress>
{
	/// Returns the linear interpolation of `from` to `to` with `progress` as the factor.  
	/// `progress` is implicitly constrained to be between 0 and 1 (inclusive).
	///
	/// This would make use of inclusive ranges, but they aren't stable yet.
    fn lerp(from: Self, to: Self, progress: Progress) -> Self
        where Progress : Clone + Clamp01 + Sub<Output=Progress>
    {
        Self::lerp_unclamped(from, to, clamp01(progress))
    }
}

/// Returns the linear interpolation of `from` to `to` with `progress` as the factor.  
/// `progress` is NOT constrained to be between 0 and 1 (inclusive).
pub fn lerp_unclamped<Progress, T>(from: T, to: T, progress: Progress) -> T 
    where T: LerpUnclamped<Progress>, Progress : Clone + One + Sub<Output=Progress>
{
    T::lerp_unclamped(from, to, progress)
}

/// Returns the linear interpolation of `from` to `to` with `progress` as the factor.  
/// `progress` is implicitly constrained to be between 0 and 1 (inclusive).
pub fn lerp<Progress, T>(from: T, to: T, progress: Progress) -> T
    where T: Lerp<Progress>, Progress : Clone + Clamp01 + Sub<Output=Progress>
{
    T::lerp(from, to, progress)
}

impl LerpUnclamped<f32> for f32 {}
impl LerpUnclamped<f64> for f64 {}
impl Lerp<f32> for f32 {}
impl Lerp<f64> for f64 {}



// NOTE to self: `Wrap` wraps between 0 and some value. NOT between some value and another.
// If you want this, you can do `lerp(a, b, wrap(progress, 1))` (which effectively performs cycling).

pub trait Wrap<Bound=Self> {
    /// Returns `self - (self/upper).floor() * upper``.
    fn wrapped(self, upper: Bound) -> Self;
    fn wrap(val: Self, upper: Bound) -> Self {
	val.wrapped(upper)
    }
    fn wrapped_between(self, lower: Bound, upper: Bound) -> Self {
	self -= lower;
	self = self.wrapped(upper);
	self + lower
    }
    fn wrap_between(val: Self, lower: Bound, upper: Bound) -> Self {
	val.wrapped_between(lower, upper)
    }
    fn pingpong(self, upper: Bound) -> Self {
	let t = self.wrapped(upper + upper);
	upper - (t - upper).abs()
    }
    fn delta_angle(self, target: Self) -> Self where Self: From<u16> {
	let mut num = Self::wrap(target - current, Self::from(360));
	if (num > Self::from(180)) {
	    num -= Self::from(360);
	}
	num
    }
}
pub trait Wrap2PI: WrapFloat + FloatConst {
    fn wrapped_2pi(self) -> Self {
        self.wrapped(Self::PI()+Self::PI())
    }
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

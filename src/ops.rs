//! Operations defined by this crate, such as `MulAdd`, `Lerp`, `Clamp`, and `Wrap`.

// TODO panic when lower > upper and document it.
// TODO fixed doc-tests for `Wrap`.

use core::num::Wrapping;
use core::ops::*;
use core::cmp;
use num_traits::{Zero, One, FloatConst, Signed};

/// Compares and returns the minimum of two values, using partial ordering.
pub fn partial_min<T: PartialOrd + Sized>(a: T, b: T) -> T {
	if a <= b { a } else { b }
}
/// Compares and returns the maximum of two values, using partial ordering.
pub fn partial_max<T: PartialOrd + Sized>(a: T, b: T) -> T {
	if a >= b { a } else { b }
}


/// A value that can be constrained to be between two values (inclusive).
pub trait Clamp<Bound=Self>: Sized {
	/// `bool` for scalars, or vector of `u32`s for vectors.
	type BoolVector;
	/// Constrains this value to be between `lower` and `upper` (inclusive).
	///
	/// This would rather make use of inclusive ranges, but it's an unstable
	/// feature.
	///
	/// ```
	/// use vek::ops::Clamp;
	///
	/// assert_eq!(7.clamped(5, 10), 7);
	/// assert_eq!(4.clamped(5, 10), 5);
	/// assert_eq!(5.clamped(5, 10), 5);
	/// assert_eq!(10.clamped(5, 10), 10);
	/// assert_eq!(11.clamped(5, 10), 10);
	/// ```
	fn clamped(self, lower: Bound, upper: Bound) -> Self;

	/// Alias to `clamped`, which doesn't take `self`.
	fn clamp(val: Self, lower: Bound, upper: Bound) -> Self {
		val.clamped(lower, upper)
	}

	/// Returns whether this value is between `lower` and `upper` (inclusive).
	///
	/// This would rather make use of inclusive ranges, but it's an unstable
	/// feature.
	///
	/// ```
	/// use vek::ops::Clamp;
	///
	/// assert!(5_i32 .is_between(5, 10));
	/// assert!(7_i32 .is_between(5, 10));
	/// assert!(10_i32.is_between(5, 10));
	/// assert!(!(4_i32 .is_between(5, 10)));
	/// assert!(!(11_i32.is_between(5, 10)));
	/// ```
	fn is_between(self, lower: Bound, upper: Bound) -> Self::BoolVector;

	/// Constrains this value to be between 0 and 1 (inclusive).
	fn clamped01(self) -> Self where Bound: Zero + One {
		self.clamped(Bound::zero(), Bound::one())
	}
	/// Alias to `clamped01`, which doesn't take `self`.
	fn clamp01(val: Self) -> Self where Bound: Zero + One {
		Self::clamp(val, Bound::zero(), Bound::one())
	}
	/// Returns whether this value is between 0 and 1 (inclusive).
	fn is_between01(self) -> Self::BoolVector where Bound: Zero + One {
		self.is_between(Bound::zero(), Bound::one())
	}
}

macro_rules! impl_clamp_float {
	($($T:ty)+) => {
		$(
			impl Clamp for $T {
				type BoolVector = bool;
				fn clamped(self, lower: Self, upper: Self) -> Self {
					partial_min(partial_max(self, lower), upper)
				}
				fn is_between(self, lower: Self, upper: Self) -> bool {
					lower <= self && self <= upper
				}
			}
		)+
	}
}
macro_rules! impl_clamp_integer {
	($($T:ty)+) => {
		$(
			impl Clamp for $T {
				type BoolVector = bool;
				fn clamped(self, lower: Self, upper: Self) -> Self {
					cmp::min(cmp::max(self, lower), upper)
				}
				fn is_between(self, lower: Self, upper: Self) -> bool {
					lower <= self && self <= upper
				}
			}
		)+
	}
}

impl_clamp_float!{
	f32 f64
}
impl_clamp_integer!{
	i8 i16 i32 i64 isize u8 u16 u32 u64 usize
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




/// The fused multiply-add operation.
pub trait MulAdd<MulRhs=Self, AddRhs=Self> {
	/// The resulting type after applying the fused multiply-add operation.
	type Output;
	/// Returns `(self * mul) + add` as a possibly faster and more precise single operation.
	fn mul_add(self, mul: MulRhs, add: AddRhs) -> Self::Output;
}

/// Returns `(val * mul) + add`.
pub fn mul_add<Output,V,M,A>(val: V, mul: M, add: A) -> Output where V: MulAdd<M,A,Output=Output> {
	val.mul_add(mul, add)
}

macro_rules! impl_muladd_content {
	(float, $Out:ty, $MulRhs:ty, $AddRhs:ty) => {
		type Output = $Out;
		fn mul_add(self, mul: $MulRhs, add: $AddRhs) -> Self::Output {
			// PERF: Find some way to write `self.mul_add(&mul, &add)` here instead
			self * mul + add
		}
	};
	(integer, $Out:ty, $MulRhs:ty, $AddRhs:ty) => {
		type Output = $Out;
		fn mul_add(self, mul: $MulRhs, add: $AddRhs) -> Self::Output {
			self * mul + add
		}
	};
}
macro_rules! impl_muladd {
	($kind:ident $($ty:ty)+) => {
	$(
		impl             MulAdd<    $ty,     $ty> for     $ty { impl_muladd_content!{$kind, $ty,     $ty,     $ty} }
		impl<        'c> MulAdd<    $ty, &'c $ty> for     $ty { impl_muladd_content!{$kind, $ty,     $ty, &'c $ty} }
		impl<    'b    > MulAdd<&'b $ty,     $ty> for     $ty { impl_muladd_content!{$kind, $ty, &'b $ty,     $ty} }
		impl<    'b, 'c> MulAdd<&'b $ty, &'c $ty> for     $ty { impl_muladd_content!{$kind, $ty, &'b $ty, &'c $ty} }
		impl<'a,       > MulAdd<    $ty,     $ty> for &'a $ty { impl_muladd_content!{$kind, $ty,     $ty,     $ty} }
		impl<'a,     'c> MulAdd<    $ty, &'c $ty> for &'a $ty { impl_muladd_content!{$kind, $ty,     $ty, &'c $ty} }
		impl<'a, 'b,   > MulAdd<&'b $ty,     $ty> for &'a $ty { impl_muladd_content!{$kind, $ty, &'b $ty,     $ty} }
		impl<'a, 'b, 'c> MulAdd<&'b $ty, &'c $ty> for &'a $ty { impl_muladd_content!{$kind, $ty, &'b $ty, &'c $ty} }
	)+
	}
}

impl_muladd!{float f32 f64}
impl_muladd!{integer
	i8 i16 i32 i64 isize u8 u16 u32 u64 usize
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


/// A value that can be linearly interpolated.
pub trait Lerp<Factor=f32>: Sized
{
	/// Returns the linear interpolation of `from` to `to` with `factor` unconstrained.  
	///
	/// This would make use of inclusive ranges, but they aren't stable yet.
	///
	/// A possible implementation is `from*(1-factor) + to*factor`.  
	///
	/// ```
	/// use vek::ops::Lerp;
	///
	/// assert_eq!(Lerp::lerp_unclamped_precise(10, 20, -1.0_f32),  0);
	/// assert_eq!(Lerp::lerp_unclamped_precise(10, 20, -0.5_f32),  5);
	/// assert_eq!(Lerp::lerp_unclamped_precise(10, 20,  0.0_f32), 10);
	/// assert_eq!(Lerp::lerp_unclamped_precise(10, 20,  0.5_f32), 15);
	/// assert_eq!(Lerp::lerp_unclamped_precise(10, 20,  1.0_f32), 20);
	/// assert_eq!(Lerp::lerp_unclamped_precise(10, 20,  1.5_f32), 25);
	/// ```
	fn lerp_unclamped_precise(from: Self, to: Self, factor: Factor) -> Self;
	/// Same as `lerp_unclamped_precise`, implemented as a possibly faster but less precise operation.
	///
	/// This would make use of inclusive ranges, but they aren't stable yet.
	///
	/// A possible implementation is `from + factor * (to - from)`, a.k.a 
	/// `factor.mul_add(to - from, from)`.
	///
	/// ```
	/// use vek::ops::Lerp;
	///
	/// assert_eq!(Lerp::lerp_unclamped(10, 20, -1.0_f32),  0);
	/// assert_eq!(Lerp::lerp_unclamped(10, 20, -0.5_f32),  5);
	/// assert_eq!(Lerp::lerp_unclamped(10, 20,  0.0_f32), 10);
	/// assert_eq!(Lerp::lerp_unclamped(10, 20,  0.5_f32), 15);
	/// assert_eq!(Lerp::lerp_unclamped(10, 20,  1.0_f32), 20);
	/// assert_eq!(Lerp::lerp_unclamped(10, 20,  1.5_f32), 25);
	/// ```
	fn lerp_unclamped(from: Self, to: Self, factor: Factor) -> Self;

	/// Alias to `lerp_unclamped_precise` which constrains `factor` to be between 0 and 1
	/// (inclusive).
	///
	/// ```
	/// use vek::ops::Lerp;
	///
	/// assert_eq!(Lerp::lerp_precise(10, 20, -1.0_f32), 10);
	/// assert_eq!(Lerp::lerp_precise(10, 20, -0.5_f32), 10);
	/// assert_eq!(Lerp::lerp_precise(10, 20,  0.0_f32), 10);
	/// assert_eq!(Lerp::lerp_precise(10, 20,  0.5_f32), 15);
	/// assert_eq!(Lerp::lerp_precise(10, 20,  1.0_f32), 20);
	/// assert_eq!(Lerp::lerp_precise(10, 20,  1.5_f32), 20);
	/// ```
	fn lerp_precise(from: Self, to: Self, factor: Factor) -> Self where Factor: Clamp + Zero + One
	{
		Self::lerp_unclamped_precise(from, to, factor.clamped01())
	}
	/// Alias to `lerp_unclamped` which constrains `factor` to be between 0 and 1
	/// (inclusive).
	///
	///	```
	/// use vek::ops::Lerp;
	///
	/// assert_eq!(Lerp::lerp(10, 20, -1.0_f32), 10);
	/// assert_eq!(Lerp::lerp(10, 20, -0.5_f32), 10);
	/// assert_eq!(Lerp::lerp(10, 20,  0.0_f32), 10);
	/// assert_eq!(Lerp::lerp(10, 20,  0.5_f32), 15);
	/// assert_eq!(Lerp::lerp(10, 20,  1.0_f32), 20);
	/// assert_eq!(Lerp::lerp(10, 20,  1.5_f32), 20);
	/// ```
	fn lerp(from: Self, to: Self, factor: Factor) -> Self where Factor: Clamp + Zero + One
	{
		Self::lerp_unclamped(from, to, factor.clamped01())
	}

}

macro_rules! lerp_impl_float {
	($($T:ty)+) => {
		$(
			impl Lerp<$T> for $T {
				fn lerp_unclamped_precise(from: Self, to: Self, factor: Self) -> Self {
					from*(Self::one()-factor) + to*factor
				}
				fn lerp_unclamped(from: Self, to: Self, factor: Self) -> Self {
					factor.mul_add(to - from, from)
				}
			}
		)+
	}
}
macro_rules! lerp_impl_integer {
	($($T:ty)+) => {
		$(
			impl Lerp<f32> for $T {
				fn lerp_unclamped_precise(from: Self, to: Self, factor: f32) -> Self {
					((from as f32)*((1f32)-factor) + (to as f32)*factor).round() as Self
				}
				fn lerp_unclamped(from: Self, to: Self, factor: f32) -> Self {
					factor.mul_add((to - from) as f32, from as f32).round() as Self
				}
			}

			impl Lerp<f64> for $T {
				fn lerp_unclamped_precise(from: Self, to: Self, factor: f64) -> Self {
					((from as f64)*((1f64)-factor) + (to as f64)*factor).round() as Self
				}
				fn lerp_unclamped(from: Self, to: Self, factor: f64) -> Self {
					factor.mul_add((to - from) as f64, from as f64).round() as Self
				}
			}
		)+
	}
}

lerp_impl_float!{f32 f64}
lerp_impl_integer!{
	i8 i16 i32 i64 isize u8 u16 u32 u64 usize
	/*
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
	*/
}

/// A value that can wrap itself around given bounds.
pub trait Wrap<Bound=Self>: Sized {
	/// Returns this value, wrapped between zero and some `upper` bound (both inclusive).
	///
	/// The computation is `self - (self/upper).floor() * upper`.
	///
	/// This might look like the remainder (`%`) operator, but behaves differently with negative
	/// values.
	///
	/// If you're familiar with Unity, this is the `Mathf.Repeat()` function.
	///
	/// ```
	/// use vek::ops::Wrap;
	///
	/// # // assert_eq!((-3_i32).wrapped(3), 0);
	/// # // assert_eq!((-2_i32).wrapped(3), 1);
	/// # // assert_eq!((-1_i32).wrapped(3), 2);
	/// assert_eq!(0_i32.wrapped(3), 0);
	/// assert_eq!(1_i32.wrapped(3), 1);
	/// assert_eq!(2_i32.wrapped(3), 2);
	/// assert_eq!(3_i32.wrapped(3), 0);
	/// assert_eq!(4_i32.wrapped(3), 1);
	/// assert_eq!(5_i32.wrapped(3), 2);
	/// ```
	fn wrapped(self, upper: Bound) -> Self;
	/// Alias to `wrapped()` which doesn't take `self`.
	fn wrap(val: Self, upper: Bound) -> Self {
		val.wrapped(upper)
	}
	/// Returns this value, wrapped between zero and two times 𝛑 (inclusive).
	///
	/// This ought to be named `wrapped_tau`, but I assume people are
	/// more familiar with 𝛑, and `2pi` is therefore more evocative.
	fn wrapped_2pi(self) -> Self where Bound: FloatConst + Add<Output=Bound> {
		self.wrapped(Bound::PI() + Bound::PI())
	}
	/// Alias to `wrapped_2pi` which doesn't take `self`.
	///
	/// This ought to be named `wrap_tau`, but I assume people are
	/// more familiar with 𝛑, and `2pi` is therefore more evocative.
	fn wrap_2pi(val: Self) -> Self where Bound: FloatConst + Add<Output=Bound> {
		val.wrapped_2pi()
	}

	/// Returns this value, wrapped between `lower` and `upper` (inclusive).
	///
	/// ```
	/// use vek::ops::Wrap;
	///
	/// # // assert_eq!((-4_i32).wrapped_between(2, 5), 2);
	/// # // assert_eq!((-3_i32).wrapped_between(2, 5), 3);
	/// # // assert_eq!((-2_i32).wrapped_between(2, 5), 4);
	/// # // assert_eq!((-1_i32).wrapped_between(2, 5), 2);
	/// // assert_eq!(0_i32.wrapped_between(2, 5), 3);
	/// // assert_eq!(1_i32.wrapped_between(2, 5), 4);
	/// assert_eq!(2_i32.wrapped_between(2, 5), 2);
	/// assert_eq!(3_i32.wrapped_between(2, 5), 3);
	/// assert_eq!(4_i32.wrapped_between(2, 5), 4);
	/// ```
	fn wrapped_between(self, lower: Bound, upper: Bound) -> Self 
		where Self: Sub<Output=Self> + Add<Output=Self> + From<Bound>,
			  Bound: Clone + Sub<Output=Bound>
	{
		let lower = || lower.clone();
		let out = self - Self::from(lower());
		let out = out.wrapped(upper - lower());
		out + Self::from(lower())
	}
	/// Alias to `wrapped_between` which doesn't take `self`.
	fn wrap_between(val: Self, lower: Bound, upper: Bound) -> Self 
		where Self: Sub<Output=Self> + Add<Output=Self> + From<Bound>,
			  Bound: Clone + Sub<Output=Bound>
	{
		val.wrapped_between(lower, upper)
	}
	/// Wraps a value such that it goes back and forth from zero to `upper` as it increases.
	///
	/// ```
	/// use vek::ops::Wrap;
	///
	/// # // assert_eq!((-4_i32).pingpong(3), 2);
	/// # // assert_eq!((-3_i32).pingpong(3), 0);
	/// # // assert_eq!((-2_i32).pingpong(3), 1);
	/// # // assert_eq!((-1_i32).pingpong(3), 2);
	/// assert_eq!(0_i32.pingpong(3), 0);
	/// assert_eq!(1_i32.pingpong(3), 1);
	/// assert_eq!(2_i32.pingpong(3), 2);
	/// assert_eq!(3_i32.pingpong(3), 3);
	/// assert_eq!(4_i32.pingpong(3), 2);
	/// assert_eq!(5_i32.pingpong(3), 1);
	/// assert_eq!(6_i32.pingpong(3), 0);
	/// assert_eq!(7_i32.pingpong(3), 1);
	/// ```
	fn pingpong(self, upper: Bound) -> Self 
		where Self: Signed + From<Bound> + Sub<Output=Self>,
			Bound: Add<Output=Bound> + Clone
	{
		let t = self.wrapped(upper.clone() + upper.clone());
		let upper = || Self::from(upper.clone());
		upper() - (t - upper()).abs()
	}
	/// Calculates the shortest difference between two given angles, in radians.
	fn delta_angle(self, target: Self) -> Self
		where Self: From<Bound> + Sub<Output=Self> + PartialOrd,
			Bound: FloatConst + Add<Output=Bound>
	{
		let num = Self::wrap(target - self, Bound::PI() + Bound::PI());
		if num > Self::from(Bound::PI()) {
			return num - Self::from(Bound::PI() + Bound::PI());
		}
		num
	}
	/// Calculates the shortest difference between two given angles, in degrees.
	///
	/// This exists because it's enough for `Bound` to implement `From<u16>`.
	fn delta_angle_degrees(self, target: Self) -> Self
		where Self: From<Bound> + Sub<Output=Self> + PartialOrd,
			Bound: From<u16>
	{
		let num = Self::wrap(target - self, Bound::from(360));
		if num > Self::from(Bound::from(180)) {
			return num - Self::from(Bound::from(360));
		}
		num
	}

}

macro_rules! wrap_impl_float {
	($($T:ty)+) => {
		$(
			impl Wrap for $T {
				fn wrapped(self, upper: Self) -> Self {
					self - (self/upper).floor() * upper
				}
			}
		)+
	}
}
macro_rules! wrap_impl_integer {
	($($T:ty)+) => {
		$(
			impl Wrap for $T {
				fn wrapped(self, upper: Self) -> Self {
					self - (self/upper) * upper
				}
			}
		)+
	}
}

wrap_impl_float!{f32 f64}
wrap_impl_integer!{
	i8 i16 i32 i64 isize u8 u16 u32 u64 usize
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

/// Trait for types that are suitable for representing a color component value.
pub trait ColorComponent : Zero {
    /// The minimum value such that the color is at its maximum.
    ///
    /// In pratice, it yields `T::MAX` for integers and `1` for real number types.
    fn full() -> Self;
}

impl ColorComponent for f32 { fn full() -> Self { 1f32 } }
impl ColorComponent for f64 { fn full() -> Self { 1f64 } }
impl ColorComponent for u8  { fn full() -> Self { ::core::u8  ::MAX } }
impl ColorComponent for u16 { fn full() -> Self { ::core::u16 ::MAX } }
impl ColorComponent for u32 { fn full() -> Self { ::core::u32 ::MAX } }
impl ColorComponent for u64 { fn full() -> Self { ::core::u64 ::MAX } }
//impl ColorComponent for u128{ fn full() -> Self { ::core::u128::MAX } }
impl ColorComponent for i8  { fn full() -> Self { ::core::i8  ::MAX } }
impl ColorComponent for i16 { fn full() -> Self { ::core::i16 ::MAX } }
impl ColorComponent for i32 { fn full() -> Self { ::core::i32 ::MAX } }
impl ColorComponent for i64 { fn full() -> Self { ::core::i64 ::MAX } }
//impl ColorComponent for i128{ fn full() -> Self { ::core::i128::MAX } }
impl ColorComponent for Wrapping<u8 >  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<u16>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<u32>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<u64>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
//impl ColorComponent for Wrapping<u128> { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<i8 >  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<i16>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<i32>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<i64>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
//impl ColorComponent for Wrapping<i128> { fn full() -> Self { Wrapping(ColorComponent::full()) } }


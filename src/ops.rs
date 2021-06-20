//! Operations defined by this crate, such as `MulAdd`, `Lerp`, `Clamp`, and `Wrap`.

use std::num::Wrapping;
use std::ops::*;
use std::cmp;
use num_traits::{Zero, One, FloatConst};

pub use num_traits::ops::mul_add::MulAdd;

// On no_std targets, we have to import the Real trait, but on std targets it will use the built-in primitive methods instead and warn that Real is unused.
#[allow(unused_imports)]
use num_traits::real::Real;

/// Compares and returns the minimum of two values, using partial ordering.
pub fn partial_min<T: PartialOrd + Sized>(a: T, b: T) -> T {
    if a <= b { a } else { b }
}
/// Compares and returns the maximum of two values, using partial ordering.
pub fn partial_max<T: PartialOrd + Sized>(a: T, b: T) -> T {
    if a >= b { a } else { b }
}

/// A value that can tell whether or not it is between two bounds (inclusive).
pub trait IsBetween<Bound=Self>: Sized {
    /// `bool` for scalars, or vector of `bool`s for vectors.
    type Output;
    /// Returns whether this value is between `lower` and `upper` (inclusive).
    ///
    /// # Panics
    /// Panics if `lower` is greater than `upper`. Swap the values yourself if necessary.
    ///
    /// ```
    /// use vek::ops::IsBetween;
    ///
    /// assert!(5_i32 .is_between(5, 10));
    /// assert!(7_i32 .is_between(5, 10));
    /// assert!(10_i32.is_between(5, 10));
    /// assert!(!(4_i32 .is_between(5, 10)));
    /// assert!(!(11_i32.is_between(5, 10)));
    /// ```
    fn is_between(self, lower: Bound, upper: Bound) -> Self::Output;
    /// Returns whether this value is between 0 and 1 (inclusive).
    fn is_between01(self) -> Self::Output where Bound: Zero + One {
        self.is_between(Bound::zero(), Bound::one())
    }
    /// Returns whether this value is between the lower and upper bounds of this inclusive range.
    /// This is redundant with `RangeInclusive::contains()`, but is still useful for generics that use the `IsBetween` trait.
    fn is_between_inclusive_range_bounds(self, range: RangeInclusive<Bound>) -> Self::Output {
        let (start, end) = range.into_inner();
        self.is_between(start, end)
    }
}

/// A value that can tell whether or not it is between 0 and 1 (inclusive).
pub trait IsBetween01: IsBetween + Zero + One {}
impl<T: IsBetween + Zero + One> IsBetween01 for T {}


/// A scalar or vector that can be constrained to be between two values (inclusive).
pub trait Clamp<Bound=Self>: Sized {
    /// Constrains this value to be between `lower` and `upper` (inclusive).
    ///
    /// # Panics
    /// Panics if `lower` is greater than `upper`. Swap the values yourself if necessary.
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
    /// Alias to `clamped`, which accepts a `RangeInclusive` parameter instead of two values.
    fn clamped_to_inclusive_range(self, range: RangeInclusive<Bound>) -> Self {
        let (start, end) = range.into_inner();
        self.clamped(start, end)
    }
    /// Alias to `clamped`, which doesn't take `self`.
    ///
    /// # Panics
    /// Panics if `lower` is greater than `upper`. Swap the values yourself if necessary.
    fn clamp(val: Self, lower: Bound, upper: Bound) -> Self {
        val.clamped(lower, upper)
    }
    /// Alias to `clamp`, which accepts a `RangeInclusive` parameter instead of two values.
    fn clamp_to_inclusive_range(val: Self, range: RangeInclusive<Bound>) -> Self {
        let (start, end) = range.into_inner();
        Self::clamp(val, start, end)
    }
    /// Constrains this value to be between 0 and 1 (inclusive).
    fn clamped01(self) -> Self where Bound: Zero + One {
        self.clamped(Bound::zero(), Bound::one())
    }
    /// Alias to `clamped01`, which doesn't take `self`.
    fn clamp01(val: Self) -> Self where Bound: Zero + One {
        Self::clamp(val, Bound::zero(), Bound::one())
    }
    /// Constrains this value to be between -1 and 1 (inclusive).
    fn clamped_minus1_1(self) -> Self where Bound: One + Neg<Output=Bound> {
        self.clamped(-Bound::one(), Bound::one())
    }
    /// Alias to `clamped_minus1_1`, which doesn't take `self`.
    fn clamp_minus1_1(val: Self) -> Self where Bound: One + Neg<Output=Bound> {
        Self::clamp(val, -Bound::one(), Bound::one())
    }
}

/// A scalar or vector that can be constrained to be between 0 and 1 (inclusive).
pub trait Clamp01: Clamp + Zero + One {}
impl<T: Clamp + Zero + One> Clamp01 for T {}

/// A scalar or vector that can be constrained to be between -1 and 1 (inclusive).
pub trait ClampMinus1: Clamp + One + Neg<Output=Self> {}
impl<T: Clamp + One + Neg<Output=T>> ClampMinus1 for T {}

macro_rules! impl_clamp_float {
    ($($T:ty)+) => {
        $(
            impl Clamp for $T {
                fn clamped(self, lower: Self, upper: Self) -> Self {
                    assert!(lower <= upper);
                    partial_min(partial_max(self, lower), upper)
                }
            }
            impl IsBetween for $T {
                type Output = bool;
                fn is_between(self, lower: Self, upper: Self) -> bool {
                    assert!(lower <= upper);
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
                fn clamped(self, lower: Self, upper: Self) -> Self {
                    assert!(lower <= upper);
                    cmp::min(cmp::max(self, lower), upper)
                }
            }
            impl IsBetween for $T {
                type Output = bool;
                fn is_between(self, lower: Self, upper: Self) -> bool {
                    assert!(lower <= upper);
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


/// Returns `(val * mul) + add`.
#[deprecated(since = "0.15.0", note = "This is redundant with num-trait's MulAdd trait and std's mul_add operation")]
pub fn mul_add<Output,V,M,A>(val: V, mul: M, add: A) -> Output where V: MulAdd<M,A,Output=Output> {
    val.mul_add(mul, add)
}


/// A value that can be linearly interpolated.
///
/// Note that, like standard operators, this can be implement for `T` and `&T`.
/// You would make the difference like so:
///
/// ```
/// use vek::ops::Lerp;
///
/// let a = Lerp::lerp(0, 10, 0.5_f32);
/// let b = Lerp::lerp(&0, &10, 0.5_f32);
/// let c = i32::lerp(0, 10, 0.5_f32);
/// let d = <&i32>::lerp(&0, &10, 0.5_f32);
/// assert_eq!(a, b);
/// assert_eq!(a, c);
/// assert_eq!(a, d);
/// ```
///
/// This is made possible thanks to the explicit `Output` type.
/// Therefore, it's also convenient for `GameState` structures, which you might
/// prefer to interpolate by reference instead of consuming them.
/// The interpolation of two `&GameState`s would produce a new `GameState` value.
///
/// ```
/// use vek::{Lerp, Vec3};
///
/// /// A data-heavy structure that represents a current game state.
/// /// It's neither Copy and nor even Clone!
/// struct GameState {
///     pub camera_position: Vec3<f32>,
///     // ... obviously a lot of other members following ...
/// }
/// // We can select the Progress type. I chose f64; the default is f32.
/// impl<'a> Lerp<f64> for &'a GameState {
///     type Output = GameState;
///     fn lerp_unclamped(a: Self, b: Self, t: f64) -> GameState {
///         GameState {
///             camera_position: Lerp::lerp(a.camera_position, b.camera_position, t as f32),
///             // ... etc for all relevant members...
///         }
///     }
/// }
/// let a = GameState { camera_position: Vec3::zero() };
/// let b = GameState { camera_position: Vec3::unit_x() };
/// let c = Lerp::lerp(&a, &b, 0.5);
/// // Hurray! We've got an interpolated state without consuming the two previous ones.
/// # let _ = c;
/// ```
pub trait Lerp<Factor=f32>: Sized
{
    /// The resulting type after performing the LERP operation.
    type Output;
    /// Returns the linear interpolation of `from` to `to` with `factor` unconstrained,
    /// using the supposedly fastest but less precise implementation.
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
    fn lerp_unclamped(from: Self, to: Self, factor: Factor) -> Self::Output;

    /// Version of `lerp_unclamped()` that used a single `RangeInclusive` parameter instead of two values.
    fn lerp_unclamped_inclusive_range(range: RangeInclusive<Self>, factor: Factor) -> Self::Output {
        let (from, to) = range.into_inner();
        Self::lerp_unclamped(from, to, factor)
    }

    /// Returns the linear interpolation of `from` to `to` with `factor` unconstrained,
    /// using a possibly slower but more precise operation.
    ///
    /// A possible implementation is `from*(1-factor) + to*factor`, a.k.a
    /// `from.mul_add(1-factor, to*factor)`.
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
    fn lerp_unclamped_precise(from: Self, to: Self, factor: Factor) -> Self::Output {
        Self::lerp_unclamped(from, to, factor)
    }

    /// Version of `lerp_unclamped_precise()` that used a single `RangeInclusive` parameter instead of two values.
    fn lerp_unclamped_precise_inclusive_range(range: RangeInclusive<Self>, factor: Factor) -> Self::Output {
        let (from, to) = range.into_inner();
        Self::lerp_unclamped_precise(from, to, factor)
    }

    /// Alias to `lerp_unclamped` which constrains `factor` to be between 0 and 1
    /// (inclusive).
    ///
    /// ```
    /// use vek::ops::Lerp;
    ///
    /// assert_eq!(Lerp::lerp(10, 20, -1.0_f32), 10);
    /// assert_eq!(Lerp::lerp(10, 20, -0.5_f32), 10);
    /// assert_eq!(Lerp::lerp(10, 20,  0.0_f32), 10);
    /// assert_eq!(Lerp::lerp(10, 20,  0.5_f32), 15);
    /// assert_eq!(Lerp::lerp(10, 20,  1.0_f32), 20);
    /// assert_eq!(Lerp::lerp(10, 20,  1.5_f32), 20);
    /// ```
    fn lerp(from: Self, to: Self, factor: Factor) -> Self::Output where Factor: Clamp + Zero + One {
        Self::lerp_unclamped(from, to, factor.clamped01())
    }

    /// Version of `lerp()` that used a single `RangeInclusive` parameter instead of two values.
    fn lerp_inclusive_range(range: RangeInclusive<Self>, factor: Factor) -> Self::Output where Factor: Clamp + Zero + One {
        let (from, to) = range.into_inner();
        Self::lerp(from, to, factor)
    }

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
    fn lerp_precise(from: Self, to: Self, factor: Factor) -> Self::Output where Factor: Clamp + Zero + One {
        Self::lerp_unclamped_precise(from, to, factor.clamped01())
    }

    /// Version of `lerp_precise()` that used a single `RangeInclusive` parameter instead of two values.
    fn lerp_precise_inclusive_range(range: RangeInclusive<Self>, factor: Factor) -> Self::Output where Factor: Clamp + Zero + One {
        let (from, to) = range.into_inner();
        Self::lerp_precise(from, to, factor)
    }
}

macro_rules! lerp_impl_float {
    ($($T:ty)+) => {
        $(
            impl Lerp<$T> for $T {
                type Output = $T;
                fn lerp_unclamped_precise(from: Self, to: Self, factor: Self) -> Self {
                    from*(Self::one()-factor) + to*factor
                }
                fn lerp_unclamped(from: Self, to: Self, factor: Self) -> Self {
                    self::MulAdd::mul_add(factor, to - from, from)
                }
            }
            impl<'a> Lerp<$T> for &'a $T {
                type Output = $T;
                fn lerp_unclamped_precise(from: Self, to: Self, factor: $T) -> $T {
                    Lerp::lerp_unclamped_precise(*from, *to, factor)
                }
                fn lerp_unclamped(from: Self, to: Self, factor: $T) -> $T {
                    Lerp::lerp_unclamped(*from, *to, factor)
                }
            }
        )+
    }
}
macro_rules! lerp_impl_integer {
    ($($T:ty)+) => {
        $(
            impl Lerp<f32> for $T {
                type Output = $T;
                fn lerp_unclamped_precise(from: Self, to: Self, factor: f32) -> Self {
                    num_traits::Float::round((from as f32)*((1f32)-factor) + (to as f32)*factor) as Self
                }
                fn lerp_unclamped(from: Self, to: Self, factor: f32) -> Self {
                    num_traits::Float::round(self::MulAdd::mul_add(factor, (to - from) as f32, from as f32)) as Self
                }
            }
            impl Lerp<f64> for $T {
                type Output = $T;
                fn lerp_unclamped_precise(from: Self, to: Self, factor: f64) -> Self {
                    num_traits::Float::round((from as f64)*((1f64)-factor) + (to as f64)*factor) as Self
                }
                fn lerp_unclamped(from: Self, to: Self, factor: f64) -> Self {
                    num_traits::Float::round(self::MulAdd::mul_add(factor, (to - from) as f64, from as f64)) as Self
                }
            }
            impl<'a> Lerp<f32> for &'a $T {
                type Output = $T;
                fn lerp_unclamped_precise(from: Self, to: Self, factor: f32) -> $T {
                    Lerp::lerp_unclamped_precise(*from, *to, factor)
                }
                fn lerp_unclamped(from: Self, to: Self, factor: f32) -> $T {
                    Lerp::lerp_unclamped(*from, *to, factor)
                }
            }
            impl<'a> Lerp<f64> for &'a $T {
                type Output = $T;
                fn lerp_unclamped_precise(from: Self, to: Self, factor: f64) -> $T {
                    Lerp::lerp_unclamped_precise(*from, *to, factor)
                }
                fn lerp_unclamped(from: Self, to: Self, factor: f64) -> $T {
                    Lerp::lerp_unclamped(*from, *to, factor)
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

/// A value that can be Spherically Linearly interpolated.
///
/// The `Output` type allows this trait to be meaningfully implemented for `&T` as well as `T`.
pub trait Slerp<Factor=f32>: Sized {
    /// The resulting type after performing the SLERP operation.
    type Output;
    /// Performs spherical linear interpolation without implictly constraining `factor` to
    /// be between 0 and 1.
    fn slerp_unclamped(from: Self, to: Self, factor: Factor) -> Self::Output;
    /// Performs spherical linear interpolation, constraining `factor` to
    /// be between 0 and 1.
    fn slerp(from: Self, to: Self, factor: Factor) -> Self::Output where Factor: Clamp + Zero + One {
        Self::slerp_unclamped(from, to, factor.clamped01())
    }
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
    /// # Panics
    /// Panics if `upper <= 0`. Reasons include :
    ///
    /// - Some types may implement it as `self.wrapped_between(zero, upper)`.
    ///   A negative `upper` would violate the `lower <= upper` requirement in this case;
    /// - On unsigned integers, this just resolves to `self % upper`,
    ///   and integer division by zero is forbidden. Testing for `i==0` incurs unnecessary overhead.
    /// - Handling negative `upper` values would double the number of test cases
    ///   and increases implementation complexity;
    ///
    /// ```
    /// use vek::ops::Wrap;
    ///
    /// assert_eq!((-5_i32).wrapped(3), 1);
    /// assert_eq!((-4_i32).wrapped(3), 2);
    /// assert_eq!((-3_i32).wrapped(3), 0);
    /// assert_eq!((-2_i32).wrapped(3), 1);
    /// assert_eq!((-1_i32).wrapped(3), 2);
    /// assert_eq!(0_i32.wrapped(3), 0);
    /// assert_eq!(1_i32.wrapped(3), 1);
    /// assert_eq!(2_i32.wrapped(3), 2);
    /// assert_eq!(3_i32.wrapped(3), 0);
    /// assert_eq!(4_i32.wrapped(3), 1);
    /// assert_eq!(5_i32.wrapped(3), 2);
    /// ```
    fn wrapped(self, upper: Bound) -> Self;
    /// Alias to `wrapped()` which doesn't take `self`.
    ///
    /// # Panics
    /// Panics if `upper <= 0`. See `wrapped()` for a rationale.
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

    /// Returns this value, wrapped between `lower` (inclusive) and `upper` (exclusive).
    ///
    /// # Panics
    /// Panics if `lower >= upper`. Swap the values yourself if necessary.
    ///
    /// Also panics if `lower < 0` or `upper <= 0`. See `wrapped()` for a rationale.
    /// Forcing `lower` and `upper` to be positive allows implementations to be simpler and faster.
    ///
    /// ```
    /// use vek::ops::Wrap;
    ///
    /// assert_eq!((-4_i32).wrapped_between(2, 5), 2);
    /// assert_eq!((-3_i32).wrapped_between(2, 5), 3);
    /// assert_eq!((-2_i32).wrapped_between(2, 5), 4);
    /// assert_eq!((-1_i32).wrapped_between(2, 5), 2);
    /// assert_eq!(  0_i32 .wrapped_between(2, 5), 3);
    /// assert_eq!(  1_i32 .wrapped_between(2, 5), 4);
    /// assert_eq!(  2_i32 .wrapped_between(2, 5), 2);
    /// assert_eq!(  3_i32 .wrapped_between(2, 5), 3);
    /// assert_eq!(  4_i32 .wrapped_between(2, 5), 4);
    /// assert_eq!(  5_i32 .wrapped_between(2, 5), 2);
    /// assert_eq!(  6_i32 .wrapped_between(2, 5), 3);
    /// ```
    fn wrapped_between(self, lower: Bound, upper: Bound) -> Self;
    /// Alias to `wrapped_between` which doesn't take `self`.
    ///
    /// # Panics
    /// Panics if `lower` is greater than `upper`. Swap the values yourself if necessary.
    fn wrap_between(val: Self, lower: Bound, upper: Bound) -> Self
        where Self: Sub<Output=Self> + Add<Output=Self> + From<Bound>,
              Bound: Copy + Sub<Output=Bound> + PartialOrd
    {
        val.wrapped_between(lower, upper)
    }
    /// Wraps a value such that it goes back and forth from zero to `upper` (inclusive) as it increases.
    ///
    /// # Panics
    /// Panics if `upper <= 0`. See `wrapped()` for a rationale.
    ///
    /// ```
    /// use vek::ops::Wrap;
    ///
    /// assert_eq!((-4_i32).pingpong(3), 2);
    /// assert_eq!((-3_i32).pingpong(3), 3);
    /// assert_eq!((-2_i32).pingpong(3), 2);
    /// assert_eq!((-1_i32).pingpong(3), 1);
    /// assert_eq!(  0_i32 .pingpong(3), 0);
    /// assert_eq!(  1_i32 .pingpong(3), 1);
    /// assert_eq!(  2_i32 .pingpong(3), 2);
    /// assert_eq!(  3_i32 .pingpong(3), 3);
    /// assert_eq!(  4_i32 .pingpong(3), 2);
    /// assert_eq!(  5_i32 .pingpong(3), 1);
    /// assert_eq!(  6_i32 .pingpong(3), 0);
    /// assert_eq!(  7_i32 .pingpong(3), 1);
    /// ```
    fn pingpong(self, upper: Bound) -> Self;

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
                    assert!(upper > Self::zero());
                    // assert_relative_ne!(upper, Self::zero());
                    self - num_traits::Float::floor(self/upper) * upper
                }
                fn wrapped_between(self, lower: Self, upper: Self) -> Self {
                    assert!(lower < upper);
                    assert!(lower >= Self::zero());
                    assert!(upper > Self::zero());
                    let out = self - lower;
                    let out = out.wrapped(upper - lower);
                    out + lower
                }
                fn pingpong(self, upper: Self) -> Self {
                    assert!(upper > Self::zero());
                    // assert_relative_ne!(upper, Self::zero());
                    let t = self.wrapped(upper + upper);
                    let upper = || Self::from(upper);
                    upper() - num_traits::Float::abs(t - upper())
                }
            }
        )+
    }
}
macro_rules! wrap_impl_uint {
    ($($T:ty)+) => {
        $(
            impl Wrap for $T {
                // https://stackoverflow.com/a/707426
                fn wrapped_between(mut self, lower: Self, upper: Self) -> Self {
                    assert!(lower < upper);
                    assert!(lower >= Self::zero());
                    assert!(upper > Self::zero());
                    let range_size = upper - lower /*+ Self::one()*/;
                    if self < lower {
                        self += range_size * ((lower-self)/range_size + Self::one());
                    }
                    lower + (self - lower) % range_size
                }
                fn wrapped(self, upper: Self) -> Self {
                    assert!(upper > Self::zero());
                    self % upper
                }
                fn pingpong(self, upper: Self) -> Self {
                    assert!(upper > Self::zero());
                    let r = self % (upper+upper);
                    if r < upper {
                        r
                    } else {
                        upper+upper-r
                    }
                }
            }
        )+
    }
}
macro_rules! wrap_impl_sint {
    ($($T:ty)+) => {
        $(
            impl Wrap for $T {
                // https://stackoverflow.com/a/707426
                fn wrapped_between(mut self, lower: Self, upper: Self) -> Self {
                    assert!(lower < upper);
                    assert!(lower >= Self::zero());
                    assert!(upper > Self::zero());
                    let range_size = upper - lower /*+ Self::one()*/;
                    if self < lower {
                        self += range_size * ((lower-self)/range_size + Self::one());
                    }
                    lower + (self - lower) % range_size
                }
                fn wrapped(self, upper: Self) -> Self {
                    assert!(upper > Self::zero());
                    self.wrapped_between(Self::zero(), upper)
                }
                fn pingpong(self, upper: Self) -> Self {
                    assert!(upper > Self::zero());
                    let r = self.wrapped(upper+upper);
                    if r <= upper {
                        r
                    } else {
                        upper+upper-r
                    }
                }
            }
        )+
    }
}

wrap_impl_float!{f32 f64}
wrap_impl_uint!{
    u8 u16 u32 u64 usize
    Wrapping<u8>
    Wrapping<u16>
    Wrapping<u32>
    Wrapping<u64>
    Wrapping<usize>
}
wrap_impl_sint!{
    i8 i16 i32 i64 isize
    Wrapping<i8>
    Wrapping<i16>
    Wrapping<i32>
    Wrapping<i64>
    Wrapping<isize>
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
impl ColorComponent for u8  { fn full() -> Self { std::u8  ::MAX } }
impl ColorComponent for u16 { fn full() -> Self { std::u16 ::MAX } }
impl ColorComponent for u32 { fn full() -> Self { std::u32 ::MAX } }
impl ColorComponent for u64 { fn full() -> Self { std::u64 ::MAX } }
//impl ColorComponent for u128{ fn full() -> Self { ::std::u128::MAX } }
impl ColorComponent for i8  { fn full() -> Self { std::i8  ::MAX } }
impl ColorComponent for i16 { fn full() -> Self { std::i16 ::MAX } }
impl ColorComponent for i32 { fn full() -> Self { std::i32 ::MAX } }
impl ColorComponent for i64 { fn full() -> Self { std::i64 ::MAX } }
//impl ColorComponent for i128{ fn full() -> Self { ::std::i128::MAX } }
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


#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! for_each_unsigned_type {
        ($($T:ident)+) => {
            $(mod $T {
                use super::Wrap;
                #[test]
                fn wrapped() {
                    assert_eq!((0 as $T).wrapped(3 as $T), 0 as $T);
                    assert_eq!((1 as $T).wrapped(3 as $T), 1 as $T);
                    assert_eq!((2 as $T).wrapped(3 as $T), 2 as $T);
                    assert_eq!((3 as $T).wrapped(3 as $T), 0 as $T);
                    assert_eq!((4 as $T).wrapped(3 as $T), 1 as $T);
                    assert_eq!((5 as $T).wrapped(3 as $T), 2 as $T);
                }
                #[test]
                fn wrapped_between() {
                    assert_eq!((0 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                    assert_eq!((1 as $T).wrapped_between(2 as $T, 5 as $T), 4 as $T);
                    assert_eq!((2 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_eq!((3 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                    assert_eq!((4 as $T).wrapped_between(2 as $T, 5 as $T), 4 as $T);
                    assert_eq!((5 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_eq!((6 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                }
                #[test]
                fn pingpong() {
                    assert_eq!((0 as $T).pingpong(3 as $T), 0 as $T);
                    assert_eq!((1 as $T).pingpong(3 as $T), 1 as $T);
                    assert_eq!((2 as $T).pingpong(3 as $T), 2 as $T);
                    assert_eq!((3 as $T).pingpong(3 as $T), 3 as $T);
                    assert_eq!((4 as $T).pingpong(3 as $T), 2 as $T);
                    assert_eq!((5 as $T).pingpong(3 as $T), 1 as $T);
                    assert_eq!((6 as $T).pingpong(3 as $T), 0 as $T);
                    assert_eq!((7 as $T).pingpong(3 as $T), 1 as $T);
                }
            })+
        };
    }

    macro_rules! for_each_signed_type {
        ($($T:ident)+) => {
            $(mod $T {
                use super::Wrap;
                #[test]
                fn wrapped() {
                    assert_eq!((-5 as $T).wrapped(3 as $T), 1 as $T);
                    assert_eq!((-4 as $T).wrapped(3 as $T), 2 as $T);
                    assert_eq!((-3 as $T).wrapped(3 as $T), 0 as $T);
                    assert_eq!((-2 as $T).wrapped(3 as $T), 1 as $T);
                    assert_eq!((-1 as $T).wrapped(3 as $T), 2 as $T);
                    assert_eq!(( 0 as $T).wrapped(3 as $T), 0 as $T);
                    assert_eq!(( 1 as $T).wrapped(3 as $T), 1 as $T);
                    assert_eq!(( 2 as $T).wrapped(3 as $T), 2 as $T);
                    assert_eq!(( 3 as $T).wrapped(3 as $T), 0 as $T);
                    assert_eq!(( 4 as $T).wrapped(3 as $T), 1 as $T);
                    assert_eq!(( 5 as $T).wrapped(3 as $T), 2 as $T);
                }
                #[test]
                fn wrapped_between() {
                    assert_eq!((-4 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_eq!((-3 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                    assert_eq!((-2 as $T).wrapped_between(2 as $T, 5 as $T), 4 as $T);
                    assert_eq!((-1 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_eq!(( 0 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                    assert_eq!(( 1 as $T).wrapped_between(2 as $T, 5 as $T), 4 as $T);
                    assert_eq!(( 2 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_eq!(( 3 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                    assert_eq!(( 4 as $T).wrapped_between(2 as $T, 5 as $T), 4 as $T);
                    assert_eq!(( 5 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_eq!(( 6 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                }
                #[test]
                fn pingpong() {
                    assert_eq!((-4 as $T).pingpong(3 as $T), 2 as $T);
                    assert_eq!((-3 as $T).pingpong(3 as $T), 3 as $T);
                    assert_eq!((-2 as $T).pingpong(3 as $T), 2 as $T);
                    assert_eq!((-1 as $T).pingpong(3 as $T), 1 as $T);
                    assert_eq!(( 0 as $T).pingpong(3 as $T), 0 as $T);
                    assert_eq!(( 1 as $T).pingpong(3 as $T), 1 as $T);
                    assert_eq!(( 2 as $T).pingpong(3 as $T), 2 as $T);
                    assert_eq!(( 3 as $T).pingpong(3 as $T), 3 as $T);
                    assert_eq!(( 4 as $T).pingpong(3 as $T), 2 as $T);
                    assert_eq!(( 5 as $T).pingpong(3 as $T), 1 as $T);
                    assert_eq!(( 6 as $T).pingpong(3 as $T), 0 as $T);
                    assert_eq!(( 7 as $T).pingpong(3 as $T), 1 as $T);
                }
            })+
        };
    }

    macro_rules! for_each_float_type {
        ($($T:ident)+) => {
            $(mod $T {
                use super::Wrap;
                #[test]
                fn wrapped() {
                    assert_relative_eq!((-5 as $T).wrapped(3 as $T), 1 as $T);
                    assert_relative_eq!((-4 as $T).wrapped(3 as $T), 2 as $T);
                    assert_relative_eq!((-3 as $T).wrapped(3 as $T), 0 as $T);
                    assert_relative_eq!((-2 as $T).wrapped(3 as $T), 1 as $T);
                    assert_relative_eq!((-1 as $T).wrapped(3 as $T), 2 as $T);
                    assert_relative_eq!(( 0 as $T).wrapped(3 as $T), 0 as $T);
                    assert_relative_eq!(( 1 as $T).wrapped(3 as $T), 1 as $T);
                    assert_relative_eq!(( 2 as $T).wrapped(3 as $T), 2 as $T);
                    assert_relative_eq!(( 3 as $T).wrapped(3 as $T), 0 as $T);
                    assert_relative_eq!(( 4 as $T).wrapped(3 as $T), 1 as $T);
                    assert_relative_eq!(( 5 as $T).wrapped(3 as $T), 2 as $T);
                }
                #[test]
                fn wrapped_between() {
                    assert_relative_eq!((-4 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_relative_eq!((-3 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                    assert_relative_eq!((-2 as $T).wrapped_between(2 as $T, 5 as $T), 4 as $T);
                    assert_relative_eq!((-1 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_relative_eq!(( 0 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                    assert_relative_eq!(( 1 as $T).wrapped_between(2 as $T, 5 as $T), 4 as $T);
                    assert_relative_eq!(( 2 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_relative_eq!(( 3 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                    assert_relative_eq!(( 4 as $T).wrapped_between(2 as $T, 5 as $T), 4 as $T);
                    assert_relative_eq!(( 5 as $T).wrapped_between(2 as $T, 5 as $T), 2 as $T);
                    assert_relative_eq!(( 6 as $T).wrapped_between(2 as $T, 5 as $T), 3 as $T);
                }
                #[test]
                fn pingpong() {
                    assert_relative_eq!((-4 as $T).pingpong(3 as $T), 2 as $T);
                    assert_relative_eq!((-3 as $T).pingpong(3 as $T), 3 as $T);
                    assert_relative_eq!((-2 as $T).pingpong(3 as $T), 2 as $T);
                    assert_relative_eq!((-1 as $T).pingpong(3 as $T), 1 as $T);
                    assert_relative_eq!(( 0 as $T).pingpong(3 as $T), 0 as $T);
                    assert_relative_eq!(( 1 as $T).pingpong(3 as $T), 1 as $T);
                    assert_relative_eq!(( 2 as $T).pingpong(3 as $T), 2 as $T);
                    assert_relative_eq!(( 3 as $T).pingpong(3 as $T), 3 as $T);
                    assert_relative_eq!(( 4 as $T).pingpong(3 as $T), 2 as $T);
                    assert_relative_eq!(( 5 as $T).pingpong(3 as $T), 1 as $T);
                    assert_relative_eq!(( 6 as $T).pingpong(3 as $T), 0 as $T);
                    assert_relative_eq!(( 7 as $T).pingpong(3 as $T), 1 as $T);
                }
            })+
        };
    }

    for_each_float_type!{f32 f64}
    for_each_signed_type!{i8 i16 i32 i64 isize}
    for_each_unsigned_type!{u8 u16 u32 u64 usize}
}

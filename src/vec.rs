//! Vector types.
//!
//! They do NOT derive `PartialOrd` and `Ord`, because it makes no sense for them, 
//! and functions such as `partial_min` and `partial_max` may give surprising results
//! because of this.  
//! They do have element-wise comparison functions though.

use core::borrow::{Borrow, BorrowMut};
use core::fmt::{self, Display, Formatter};
use core::iter::{FromIterator, Product, Sum};
use core::mem;
use core::ptr;
use core::cmp;
use core::ops::*;
use core::slice::{self, /*SliceIndex*/}; // NOTE: Will want to use SliceIndex once it's stabilized
use num_traits::{Zero, One, NumCast, Signed, Float};
use ops::*;

macro_rules! vec_impl_cmp {
	($(#[$attrs:meta])*, $Vec:ident, $cmp:ident, $op:tt, $Bounds:tt) => {
		$(#[$attrs])*
		pub fn $cmp<Rhs: AsRef<Self>>(&self, rhs: Rhs) -> $Vec<u8> where T: $Bounds {
			let mut out: $Vec<u8> = unsafe { mem::uninitialized() };
			let mut iter = self.iter().zip(rhs.as_ref().iter());
			for elem in &mut out {
				let (a, b) = iter.next().unwrap();
				*elem = (a $op b) as u8;
			}
			out
		}
	}
}

macro_rules! vec_impl_trinop_vec_vec {
	($op:ident, $Out:ty, $Rhs1:ty, $Rhs2:ty) => {
		type Output = $Out;
		fn $op(self, a: $Rhs1, b: $Rhs2) -> Self::Output {
			let mut out: $Out = unsafe { mem::uninitialized() };
			let mut iter = self.into_iter().zip(a.into_iter().zip(b.into_iter()));
			for elem in &mut out {
				let (val, (aa, bb)) = iter.next().unwrap();
				*elem = val.$op(aa, bb);
			}
			out
		}
	}
}
/*
macro_rules! vec_impl_trinop_vec_s {
	($op:ident, $Out:ty, $Rhs1:ty, $Rhs2:ty, $getb:expr) => {
		type Output = $Out;
		fn $op(self, a: $Rhs1, b: $Rhs2) -> Self::Output {
			let mut out: $Out = unsafe { mem::uninitialized() };
			let mut iter = self.into_iter().zip(a.into_iter());
			for elem in &mut out {
				let (val, aa) = iter.next().unwrap();
				*elem = val.$op(aa, $getb);
			}
			out
		}
	}
}
macro_rules! vec_impl_trinop_s_vec {
	($op:ident, $Out:ty, $Rhs1:ty, $Rhs2:ty, $geta:expr) => {
		type Output = $Out;
		fn $op(self, a: $Rhs1, b: $Rhs2) -> Self::Output {
			let mut out: $Out = unsafe { mem::uninitialized() };
			let mut iter = self.into_iter().zip(b.into_iter());
			for elem in &mut out {
				let (val, bb) = iter.next().unwrap();
				*elem = val.$op($geta, bb);
			}
			out
		}
	}
}
macro_rules! vec_impl_trinop_s_s {
	($op:ident, $Out:ty, $Rhs1:ty, $Rhs2:ty, $geta:expr, $getb:expr) => {
		type Output = $Out;
		fn $op(self, a: $Rhs1, b: $Rhs2) -> Self::Output {
			let mut out: $Out = unsafe { mem::uninitialized() };
			let mut iter = self.into_iter();
			for elem in &mut out {
				let val = iter.next().unwrap();
				*elem = val.$op($geta, $getb);
			}
			out
		}
	}
}
*/
macro_rules! vec_impl_trinop {
	(impl $Op:ident for $Vec:ident { $op:tt } ($($get:tt)+)) => {
		impl<			T> $Op<	$Vec<T>,	 $Vec<T>> for	 $Vec<T> where	 T: $Op<	T,	 T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>,	 $Vec<T>,	 $Vec<T>} }
		impl<		'c, T> $Op<	$Vec<T>,	 $Vec<T>> for &'c $Vec<T> where &'c T: $Op<	T,	 T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>,	 $Vec<T>,	 $Vec<T>} }
		impl<	'b,	 T> $Op<	$Vec<T>, &'b $Vec<T>> for	 $Vec<T> where	 T: $Op<	T, &'b T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>,	 $Vec<T>, &'b $Vec<T>} }
		impl<	'b, 'c, T> $Op<	$Vec<T>, &'b $Vec<T>> for &'c $Vec<T> where &'c T: $Op<	T, &'b T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>,	 $Vec<T>, &'b $Vec<T>} }
		impl<'a,		 T> $Op<&'a $Vec<T>,	 $Vec<T>> for	 $Vec<T> where	 T: $Op<&'a T,	 T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>, &'a $Vec<T>,	 $Vec<T>} }
		impl<'a,	 'c, T> $Op<&'a $Vec<T>,	 $Vec<T>> for &'c $Vec<T> where &'c T: $Op<&'a T,	 T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>, &'a $Vec<T>,	 $Vec<T>} }
		impl<'a, 'b,	 T> $Op<&'a $Vec<T>, &'b $Vec<T>> for	 $Vec<T> where	 T: $Op<&'a T, &'b T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>, &'a $Vec<T>, &'b $Vec<T>} }
		impl<'a, 'b, 'c, T> $Op<&'a $Vec<T>, &'b $Vec<T>> for &'c $Vec<T> where &'c T: $Op<&'a T, &'b T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>, &'a $Vec<T>, &'b $Vec<T>} }

		/* I give up, it's dumb.
		impl<			T> $Op<	T,	 T> for	 $Vec<T> where	 T: $Op<	T,	 T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>,	 T,	 T, a.clone(), b.clone()} }
		impl<		'c, T> $Op<	T,	 T> for &'c $Vec<T> where &'c T: $Op<	T,	 T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>,	 T,	 T, a.clone(), b.clone()} }
		impl<	'b,	 T> $Op<	T, &'b T> for	 $Vec<T> where	 T: $Op<	T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>,	 T, &'b T, a.clone(), b		} }
		impl<	'b, 'c, T> $Op<	T, &'b T> for &'c $Vec<T> where &'c T: $Op<	T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>,	 T, &'b T, a.clone(), b		} }
		impl<'a,		 T> $Op<&'a T,	 T> for	 $Vec<T> where	 T: $Op<&'a T,	 T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>, &'a T,	 T, a		, b.clone()} }
		impl<'a,	 'c, T> $Op<&'a T,	 T> for &'c $Vec<T> where &'c T: $Op<&'a T,	 T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>, &'a T,	 T, a		, b.clone()} }
		impl<'a, 'b,	 T> $Op<&'a T, &'b T> for	 $Vec<T> where	 T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>, &'a T, &'b T, a		, b		} }
		impl<'a, 'b, 'c, T> $Op<&'a T, &'b T> for &'c $Vec<T> where &'c T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>, &'a T, &'b T, a		, b		} }

		impl<			T> $Op<	$Vec<T>,	 T> for	 $Vec<T> where	 T: $Op<	T,	 T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>,	 $Vec<T>,	 T, b.clone() }
		impl<		'c, T> $Op<	$Vec<T>,	 T> for &'c $Vec<T> where &'c T: $Op<	T,	 T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>,	 $Vec<T>,	 T, b.clone() }
		impl<	'b,	 T> $Op<	$Vec<T>, &'b T> for	 $Vec<T> where	 T: $Op<	T, &'b T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>,	 $Vec<T>, &'b T, b.clone() }
		impl<	'b, 'c, T> $Op<	$Vec<T>, &'b T> for &'c $Vec<T> where &'c T: $Op<	T, &'b T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>,	 $Vec<T>, &'b T, b.clone() }
		impl<'a,		 T> $Op<&'a $Vec<T>,	 T> for	 $Vec<T> where	 T: $Op<&'a T,	 T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>, &'a $Vec<T>,	 T, b.clone() }
		impl<'a,	 'c, T> $Op<&'a $Vec<T>,	 T> for &'c $Vec<T> where &'c T: $Op<&'a T,	 T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>, &'a $Vec<T>,	 T, b.clone() }
		impl<'a, 'b,	 T> $Op<&'a $Vec<T>, &'b T> for	 $Vec<T> where	 T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>, &'a $Vec<T>, &'b T, b.clone() }
		impl<'a, 'b, 'c, T> $Op<&'a $Vec<T>, &'b T> for &'c $Vec<T> where &'c T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>, &'a $Vec<T>, &'b T, b.clone() }

		impl<			T> $Op<	T,	 $Vec<T>> for	 $Vec<T> where	 T: $Op<	T,	 T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>,	 T,	 $Vec<T>, a.clone() }
		impl<		'c, T> $Op<	T,	 $Vec<T>> for &'c $Vec<T> where &'c T: $Op<	T,	 T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>,	 T,	 $Vec<T>, a.clone() }
		impl<	'b,	 T> $Op<	T, &'b $Vec<T>> for	 $Vec<T> where	 T: $Op<	T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>,	 T, &'b $Vec<T>, a.clone() }
		impl<	'b, 'c, T> $Op<	T, &'b $Vec<T>> for &'c $Vec<T> where &'c T: $Op<	T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>,	 T, &'b $Vec<T>, a.clone() }
		impl<'a,		 T> $Op<&'a T,	 $Vec<T>> for	 $Vec<T> where	 T: $Op<&'a T,	 T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>, &'a T,	 $Vec<T>, a.clone() }
		impl<'a,	 'c, T> $Op<&'a T,	 $Vec<T>> for &'c $Vec<T> where &'c T: $Op<&'a T,	 T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>, &'a T,	 $Vec<T>, a.clone() }
		impl<'a, 'b,	 T> $Op<&'a T, &'b $Vec<T>> for	 $Vec<T> where	 T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>, &'a T, &'b $Vec<T>, a.clone() }
		impl<'a, 'b, 'c, T> $Op<&'a T, &'b $Vec<T>> for &'c $Vec<T> where &'c T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>, &'a T, &'b $Vec<T>, a.clone() }
		*/
	}
}

macro_rules! vec_impl_binop {
	(impl $Op:ident for $Vec:ident { $op:tt } ($($get:tt)+)) => {
		impl<T> $Op<$Vec<T>> for $Vec<T> where T: $Op<Output=T> {
			type Output = $Vec<T>;
			fn $op(self, rhs: $Vec<T>) -> Self::Output {
				$Vec::new($(self.$get.$op(rhs.$get)),+)
			}
		}
		impl<'a, T> $Op<&'a $Vec<T>> for $Vec<T> where T: $Op<&'a T, Output=T> {
			type Output = $Vec<T>;
			fn $op(self, rhs: &'a $Vec<T>) -> Self::Output {
				$Vec::new($(self.$get.$op(&rhs.$get)),+)
			}
		}
		impl<'a, T> $Op<$Vec<T>> for &'a $Vec<T> where &'a T: $Op<T, Output=T> {
			type Output = $Vec<T>;
			fn $op(self, rhs: $Vec<T>) -> Self::Output {
				$Vec::new($(self.$get.$op(rhs.$get)),+)
			}
		}
		impl<'a, 'b, T> $Op<&'a $Vec<T>> for &'b $Vec<T> where &'b T: $Op<&'a T, Output=T> {
			type Output = $Vec<T>;
			fn $op(self, rhs: &'a $Vec<T>) -> Self::Output {
				$Vec::new($(self.$get.$op(&rhs.$get)),+)
			}
		}

		// Implement on scalars too
		impl<T> $Op<T> for $Vec<T> where T: $Op<Output=T> + Clone {
			type Output = $Vec<T>;
			fn $op(self, rhs: T) -> Self::Output {
				$Vec::new($(self.$get.$op(rhs.clone())),+)
			}
		}
		impl<'a, T> $Op<&'a T> for $Vec<T> where T: $Op<&'a T, Output=T> {
			type Output = $Vec<T>;
			fn $op(self, rhs: &'a T) -> Self::Output {
				$Vec::new($(self.$get.$op(rhs)),+)
			}
		}
		impl<'a, T> $Op<T> for &'a $Vec<T> where &'a T: $Op<T, Output=T>, T: Clone {
			type Output = $Vec<T>;
			fn $op(self, rhs: T) -> Self::Output {
				$Vec::new($(self.$get.$op(rhs.clone())),+)
			}
		}
		impl<'a, 'b, T> $Op<&'a T> for &'b $Vec<T> where &'b T: $Op<&'a T, Output=T> {
			type Output = $Vec<T>;
			fn $op(self, rhs: &'a T) -> Self::Output {
				$Vec::new($(self.$get.$op(rhs)),+)
			}
		}

	}
}
macro_rules! vec_impl_unop {
	(impl $Op:ident for $Vec:ident { $op:tt } ($($get:tt)+)) => {
		impl<T> $Op<$Vec<T>> for $Vec<T> where T: $Op<T> {
			fn $op(&mut self, rhs: $Vec<T>) {
				$(self.$get.$op(rhs.$get);)+
			}
		}
		impl<'a, T> $Op<&'a $Vec<T>> for $Vec<T> where T: $Op<&'a T> {
			fn $op(&mut self, rhs: &'a $Vec<T>) {
				$(self.$get.$op(&rhs.$get);)+
			}
		}
		impl<T> $Op<T> for $Vec<T> where T: $Op<T> + Clone {
			fn $op(&mut self, rhs: T) {
				$(self.$get.$op(rhs.clone());)+
			}
		}
		impl<'a, T> $Op<&'a T> for $Vec<T> where T: $Op<&'a T> {
			fn $op(&mut self, rhs: &'a T) {
				$(self.$get.$op(rhs);)+
			}
		}
	}
}

macro_rules! vec_impl_index {
	($Vec:ident $((($I:ty) -> $Output:tt))+) => {
		$(
			impl<T> Index<$I> for $Vec<T> {
				type Output = $Output;
				fn index(&self, i: $I) -> &Self::Output {
					&self.as_slice()[i]
				}
			}
			impl<T> IndexMut<$I> for $Vec<T> {
				fn index_mut(&mut self, i: $I) -> &mut Self::Output {
					&mut self.as_mut_slice()[i]
				}
			}
		)+
	}
}


/// Generates implementations specific to the given vector type.
macro_rules! vec_impl_vec {

	(tuple $Vec:ident $vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

		impl<T> $Vec<T> {
			/// Creates a vector from each component.
			#[cfg_attr(feature = "clippy", allow(too_many_arguments))]
			pub fn new($($namedget:T),+) -> Self {
				$Vec($($namedget),+)
			}
		}

		vec_impl_vec!{common $Vec $vec ($dim) ($fmt) ($($get)+) ($($namedget)+) ($($tupleget)+) $Tuple}

	};

	(struct $Vec:ident $vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

		impl<T> $Vec<T> {
			/// Creates a vector from each component.
			#[cfg_attr(feature = "clippy", allow(too_many_arguments))]
			pub fn new($($namedget:T),+) -> Self {
				Self { $($namedget),+ }
			}
		}

		vec_impl_vec!{common $Vec $vec ($dim) ($fmt) ($($get)+) ($($namedget)+) ($($tupleget)+) $Tuple}

	};

	(common $Vec:ident $vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

		#[allow(missing_docs)]
		/* TODO re-enable this when not using incremental compilation
		/// Displays the vector, formatted as `
		#[doc=$fmt]
		/// `.
		*/
		impl<T: Display> Display for $Vec<T> {
			fn fmt(&self, f: &mut Formatter) -> fmt::Result {
				write!(f, $fmt, $(self.$get),+)
			}
		}

		impl<T> $Vec<T> {

			/// Broadcasts a single value to all elements of a new vector.
			///
			/// This function is also named `splat()` in some libraries, or
			/// `set1()` in Intel intrinsics.
			///
			/// "Broadcast" was chosen as the name because it is explicit enough and is the
			/// same wording as the description in relevant Intel intrinsics.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(Vec4::broadcast(5), Vec4(5,5,5,5));
			/// assert_eq!(Vec4::broadcast(5), Vec4::from(5));
			/// ```
			pub fn broadcast(val: T) -> Self where T: Clone {
				let mut out: Self = unsafe { mem::uninitialized() };
				$(out.$get = val.clone();)+
				out
			}

			/// Creates a new vector with all elements set to zero.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(Vec4::zero(), Vec4(0,0,0,0));
			/// assert_eq!(Vec4::zero(), Vec4::broadcast(0));
			/// assert_eq!(Vec4::zero(), Vec4::from(0));
			/// ```
			pub fn zero() -> Self where T: Zero {
				let mut out: Self = unsafe { mem::uninitialized() };
				$(out.$get = Zero::zero();)+
				out
			}

			/// Creates a new vector with all elements set to one.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(Vec4::one(), Vec4(1,1,1,1));
			/// assert_eq!(Vec4::one(), Vec4::broadcast(1));
			/// assert_eq!(Vec4::one(), Vec4::from(1));
			/// ```
			pub fn one() -> Self where T: One {
				let mut out: Self = unsafe { mem::uninitialized() };
				$(out.$get = One::one();)+
				out
			}

			/// Are all elements of this vector equal to the given value ?
			pub fn is_broadcast(&self, val: T) -> bool where T: Clone + PartialEq {
				self == &Self::broadcast(val)
			}
			/// Are all elements of this vector equal to zero ?
			pub fn is_zero(&self) -> bool where T: Zero + PartialEq {
				self == &Self::zero()
			}
			/// Are all elements of this vector equal to one ?
			pub fn is_one(&self) -> bool where T: One + PartialEq {
				self == &Self::one()
			}

			/// Produces a vector of the first `n` integers, starting from zero,
			/// where `n` is the number of elements for this vector type.
			///
			/// The iota (ι) function, originating from APL.
			///
			/// See [this StackOverflow answer](https://stackoverflow.com/a/9244949).
			///
			/// This is mostly useful for debugging purposes and tests.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(Vec4::iota(), Vec4(0, 1, 2, 3));
			/// ```
			pub fn iota() -> Self where T: Zero + One + AddAssign + Clone {
				let mut out: Self = unsafe { mem::uninitialized() };
				let mut i = T::zero();
				$(
					out.$get = i.clone();
					i += T::one();
				)+
				out
			}

			/// Convenience method which returns the number of elements of this vector.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(0,1,2,3);
			/// assert_eq!(v.elem_count(), 4);
			/// ```
			pub fn elem_count(&self) -> usize {
				$dim
			}

			/// Converts this into a tuple with the same number of elements by consuming.
			#[cfg_attr(feature = "clippy", allow(type_complexity))]
			pub fn into_tuple(self) -> $Tuple {
				($(self.$get),+)
			}

			/// Converts this into a raw pointer of read-only data.
			pub fn as_ptr(&self) -> *const T {
				self as *const _ as *const T
			}
			/// Converts this into a raw pointer.
			pub fn as_mut_ptr(&mut self) -> *mut T {
				self as *mut _ as *mut T
			}

			/// View this vector as an immutable slice.
			pub fn as_slice(&self) -> &[T] {
				unsafe {
					slice::from_raw_parts(self.as_ptr(), $dim)
				}
			}
			/// View this vector as a mutable slice.
			pub fn as_mut_slice(&mut self) -> &mut [T] {
				unsafe {
					slice::from_raw_parts_mut(self.as_mut_ptr(), $dim)
				}
			}

			/// Collects the content of a slice into a new vector. Elements are initialized to
			/// their default values.
			pub fn from_slice(slice: &[T]) -> Self where T: Default + Clone {
				Self::from_iter(slice.into_iter().cloned())
			}

			/// Attempts to get an immutable reference to the ith element.
			pub fn get(&self, i: usize) -> Option<&T> {
				self.as_slice().get(i)
			}
			/// Attempts to get an immutable reference to the ith element, bypassing bounds
			/// checking.
			pub unsafe fn get_unchecked(&self, i: usize) -> &T {
				self.as_slice().get_unchecked(i)
			}
			/// Attempts to get a mutable reference to the ith element.
			pub fn get_mut(&mut self, i: usize) -> Option<&mut T> {
				self.as_mut_slice().get_mut(i)
			}
			/// Attempts to get a mutable reference to the ith element, bypassing bounds
			/// checking.
			pub unsafe fn get_unchecked_mut(&mut self, i: usize) -> &mut T {
				self.as_mut_slice().get_unchecked_mut(i)
			}

			/// Returns a memberwise-converted copy of this vector, using the given conversion
			/// closure.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(0_f32, 1_f32, 1.8_f32, 3.14_f32);
			/// let i = v.convert(|x| x.round() as i32);
			/// assert_eq!(i, Vec4(0, 1, 2, 3));
			/// ```
			pub fn convert<D,F>(self, f: F) -> $Vec<D> where F: Fn(T) -> D {
				$Vec::new($(f(self.$get)),+)
			}
			/// Returns a memberwise-converted copy of this vector, using `NumCast`.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(0_f32, 1_f32, 2_f32, 3_f32);
			/// let i: Vec4<i32> = v.numcast().unwrap();
			/// assert_eq!(i, Vec4(0, 1, 2, 3));
			/// ```
			pub fn numcast<D>(self) -> Option<$Vec<D>> where T: NumCast, D: NumCast {
				let mut out: $Vec<D> = unsafe { mem::uninitialized() };
				$(
					if let Some(val) = D::from(self.$get) {
						out.$get = val;
					} else {
						return None;
					}
				)+
				Some(out)
			}
			/// Converts this vector into a fixed-size array.
			pub fn into_array(self) -> [T; $dim] {
				[$(self.$get, )+]
			}

			/// Fused multiply-add. Returns `self * mul + add`, and may be implemented
			/// efficiently by the hardware.
			///
			/// The compiler is often able to detect this kind of operation, 
			/// so generally you don't need to use it. However, it can make
			/// your intent clear.
			///
			/// The name for this method is the one used by the same operation
			/// on primitive floating-point types.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let a = Vec4(0,1,2,3);
			/// let b = Vec4(4,5,6,7);
			/// let c = Vec4(8,9,0,1);
			/// assert_eq!(a*b+c, a.mul_add(b, c));
			/// ```
			pub fn mul_add(self, mul: Self, add: Self) -> Self 
				where T: MulAdd<T,T,Output=T>
			{
				let mut out: Self = unsafe { mem::uninitialized() };
				let mut iter = self.into_iter().zip(mul.into_iter().zip(add.into_iter()));
				for elem in &mut out {
					let (val, (mul, add)) = iter.next().unwrap();
					*elem = val.mul_add(mul, add);
				}
				out
			}

			/// Gets an iterator over immutable references of this vector's elements.
			pub fn iter(&self) -> slice::Iter<T> {
				self.into_iter()
			}
			/// Gets an iterator over mutable references of this vector's elements.
			pub fn iter_mut(&mut self) -> slice::IterMut<T> {
				self.into_iter()
			}

			/// Is any of the elements negative ?
			///
			/// This was intended for checking the validity of extent vectors, but can make
			/// sense for other types too.
			pub fn is_any_negative(&self) -> bool where T: Signed {
				self.iter().fold(false, |acc, x| acc || x.is_negative())
			}

			/// Are all of the elements positive ?
			pub fn are_all_positive(&self) -> bool where T: Signed {
				!self.is_any_negative()
			}

			/// Compares elements of `a` and `b`, and returns the minimum values into a new
			/// vector, using total ordering.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let a = Vec4(0,1,2,3);
			/// let b = Vec4(3,2,1,0);
			/// let m = Vec4(0,1,1,0);
			/// assert_eq!(m, Vec4::min(a, b));
			/// ```
			pub fn min(a: Self, b: Self) -> Self where T: Ord {
				Self::new($(cmp::min(a.$get, b.$get)),+)
			}
			/// Compares elements of `a` and `b`, and returns the maximum values into a new
			/// vector, using total ordering.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let a = Vec4(0,1,2,3);
			/// let b = Vec4(3,2,1,0);
			/// let m = Vec4(3,2,2,3);
			/// assert_eq!(m, Vec4::max(a, b));
			/// ```
			pub fn max(a: Self, b: Self) -> Self where T: Ord {
				Self::new($(cmp::max(a.$get, b.$get)),+)
			}
			/// Compares elements of `a` and `b`, and returns the minimum values into a new
			/// vector, using partial ordering.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let a = Vec4(0,1,2,3);
			/// let b = Vec4(3,2,1,0);
			/// let m = Vec4(0,1,1,0);
			/// assert_eq!(m, Vec4::partial_min(a, b));
			/// ```
			pub fn partial_min(a: Self, b: Self) -> Self where T: PartialOrd {
				Self::new($(partial_min(a.$get, b.$get)),+)
			}
			/// Compares elements of `a` and `b`, and returns the minimum values into a new
			/// vector, using partial ordering.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let a = Vec4(0,1,2,3);
			/// let b = Vec4(3,2,1,0);
			/// let m = Vec4(3,2,2,3);
			/// assert_eq!(m, Vec4::partial_max(a, b));
			/// ```
			pub fn partial_max(a: Self, b: Self) -> Self where T: PartialOrd  {
				Self::new($(partial_max(a.$get, b.$get)),+)
			}

			/// Returns the element which has the lowest value in this vector, using total
			/// ordering.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(-5, Vec4(0, 5, -5, 8).reduce_min());
			/// ```
			pub fn reduce_min(self) -> T where T: Ord {
				self.into_iter().min().unwrap()
			}
			/// Returns the element which has the highest value in this vector, using total
			/// ordering.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(8, Vec4(0, 5, -5, 8).reduce_max());
			/// ```
			pub fn reduce_max(self) -> T where T: Ord {
				self.into_iter().max().unwrap()
			}

			/// Returns the element which has the lowest value in this vector, using partial
			/// ordering.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(-5_f32, Vec4(0_f32, 5_f32, -5_f32, 8_f32).reduce_partial_min());
			/// ```
			pub fn reduce_partial_min(self) -> T where T: PartialOrd {
				let first = unsafe { ptr::read(self.get_unchecked(0)) };
				self.into_iter().skip(1).fold(first, partial_min)
			}
			/// Returns the element which has the highest value in this vector, using partial
			/// ordering.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(8_f32, Vec4(0_f32, 5_f32, -5_f32, 8_f32).reduce_partial_max());
			/// ```
			pub fn reduce_partial_max(self) -> T where T: PartialOrd {
				let first = unsafe { ptr::read(self.get_unchecked(0)) };
				self.into_iter().skip(1).fold(first, partial_max)
			}

			/// Returns the product of each of this vector's elements.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(1*2*3*4, Vec4(1, 2, 3, 4).product());
			/// ```
			pub fn product(self) -> T where T: Product {
				self.into_iter().product()
			}
			/// Returns the sum of each of this vector's elements.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(1+2+3+4, Vec4(1, 2, 3, 4).sum());
			/// ```
			pub fn sum(self) -> T where T: Sum {
				self.into_iter().sum()
			}
			/// Returns the average of this vector's elements.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// assert_eq!(2.5_f32, Vec4(1_f32, 2_f32, 3_f32, 4_f32).average());
			/// ```
			pub fn average(self) -> T where T: Sum + Div<T, Output=T> + From<u16> {
				self.sum() / T::from($dim)
			}

			/// Returns a new vector which elements are the respective square roots of this
			/// vector's elements.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(1f32, 2f32, 3f32, 4f32);
			/// let s = Vec4(1f32, 4f32, 9f32, 16f32);
			/// assert_eq!(v, s.sqrt());
			/// ```
			pub fn sqrt(self) -> Self where T: Float {
				Self::new($(self.$get.sqrt()),+)
			}

			/// Returns a new vector which elements are the respective reciprocal 
			/// square roots of this vector's elements.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(1f32, 0.5f32, 1f32/3f32, 0.25f32);
			/// let s = Vec4(1f32, 4f32, 9f32, 16f32);
			/// assert_eq!(v, s.rsqrt());
			/// ```
			pub fn rsqrt(self) -> Self where T: Float {
				self.sqrt().recip()
			}
			/// Returns a new vector which elements are the respective reciprocal 
			/// of this vector's elements.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(1f32, 0.5f32, 0.25f32, 0.125f32);
			/// let s = Vec4(1f32, 2f32, 4f32, 8f32);
			/// assert_eq!(v, s.recip());
			/// assert_eq!(s, v.recip());
			/// ```
			pub fn recip(self) -> Self where T: Float {
				Self::new($(self.$get.recip()),+)
			}
			/// Returns a new vector which elements are rounded to the nearest greater integer.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(0_f32, 1_f32, 1.8_f32, 3.14_f32);
			/// assert_eq!(v.ceil(), Vec4(0f32, 1f32, 2f32, 4f32));
			/// ```
			pub fn ceil(self) -> Self where T: Float {
				Self::new($(self.$get.ceil()),+)
			}
			/// Returns a new vector which elements are rounded down to the nearest lower integer.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(0_f32, 1_f32, 1.8_f32, 3.14_f32);
			/// assert_eq!(v.floor(), Vec4(0f32, 1f32, 1f32, 3f32));
			/// ```
			pub fn floor(self) -> Self where T: Float {
				Self::new($(self.$get.floor()),+)
			}
			/// Returns a new vector which elements are rounded to the nearest integer.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let v = Vec4(0_f32, 1_f32, 1.8_f32, 3.14_f32);
			/// assert_eq!(v.round(), Vec4(0f32, 1f32, 2f32, 3f32));
			/// ```
			pub fn round(self) -> Self where T: Float {
				Self::new($(self.$get.round()),+)
			}

			/// Horizontally adds adjacent pairs of elements in `self` and `rhs` into a new vector.
			///
			/// ```
			/// # use vek::vec::Vec4;
			/// let a = Vec4(0, 1, 2, 3);
			/// let b = Vec4(4, 5, 6, 7);
			/// let h = Vec4(0+1, 2+3, 4+5, 6+7);
			/// assert_eq!(h, a.hadd(b));
			/// ```
			pub fn hadd(self, rhs: Self) -> Self where T: Add<T, Output=T> {
				let mut out: Self = unsafe { mem::uninitialized() };
				let mut iter = self.into_iter().chain(rhs.into_iter());
				for elem in &mut out {
					let a = iter.next().unwrap();
					let b = iter.next().unwrap();
					*elem = a + b;
				}
				out
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the partial equality test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,6);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.partial_cmpeq(v), Vec4(1, 0, 1, 0));
				/// ```
				, $Vec, partial_cmpeq, ==, PartialEq
			}
			vec_impl_cmp!{
				/// Compares each element of two vectors with the partial not-equal test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,6);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.partial_cmpne(v), Vec4(0, 1, 0, 1));
				/// ```
				, $Vec, partial_cmpne, !=, PartialEq
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the partial greater-or-equal test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,2);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.partial_cmpge(v), Vec4(1, 1, 1, 0));
				/// ```
				, $Vec, partial_cmpge, >=, PartialOrd
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the partial greater-than test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,6);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.partial_cmpgt(v), Vec4(0, 1, 0, 1));
				/// ```
				, $Vec, partial_cmpgt, >, PartialOrd
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the partial less-or-equal test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,2);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.partial_cmple(v), Vec4(1, 0, 1, 1));
				/// ```
				, $Vec, partial_cmple, <=, PartialOrd
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the partial less-than test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,2);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.partial_cmplt(v), Vec4(0, 0, 0, 1));
				/// ```
				, $Vec, partial_cmplt, <, PartialOrd
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the partial equality test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,6);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.cmpeq(v), Vec4(1, 0, 1, 0));
				/// ```
				, $Vec, cmpeq, ==, Eq
			}
			vec_impl_cmp!{
				/// Compares each element of two vectors with the total not-equal test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,6);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.cmpne(v), Vec4(0, 1, 0, 1));
				/// ```
				, $Vec, cmpne, !=, Eq
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the total greater-or-equal test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,2);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.cmpge(v), Vec4(1, 1, 1, 0));
				/// ```
				, $Vec, cmpge, >=, Ord
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the total greater-than test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,6);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.cmpgt(v), Vec4(0, 1, 0, 1));
				/// ```
				, $Vec, cmpgt, >, Ord
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the total less-or-equal test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,2);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.cmple(v), Vec4(1, 0, 1, 1));
				/// ```
				, $Vec, cmple, <=, Ord
			}

			vec_impl_cmp!{
				/// Compares each element of two vectors with the total less-than test, returning a boolean vector.
				///
				/// ```
				/// # use vek::vec::Vec4;
				/// let u = Vec4(0,2,2,2);
				/// let v = Vec4(0,1,2,3);
				/// assert_eq!(u.cmplt(v), Vec4(0, 0, 0, 1));
				/// ```
				, $Vec, cmplt, <, Ord
			}
		}

		// TRAITS IMPLS

		impl<T: Zero + PartialEq> Zero for $Vec<T> {
			fn zero() -> Self { Self::zero() }
			fn is_zero(&self) -> bool { self.is_zero() }
		}
		impl<T: One> One for $Vec<T> {
			fn one() -> Self { Self::one() }
		}
		// TODO impl Float for Vec<Float> ??

		// OPS

		impl<T> Neg for $Vec<T> where T: Neg<Output=T> {
			type Output = Self;
			fn neg(self) -> Self::Output {
				Self::new($(-self.$get),+)
			}
		}

		vec_impl_trinop!{impl MulAdd for $Vec { mul_add } ($($get)+)}
		// vec_impl_trinop!{impl Lerp for $Vec { lerp } ($($get)+)}
		// vec_impl_trinop!{impl LerpUnclamped for $Vec { lerp_unclamped } ($($get)+)}
		// vec_impl_binop!{impl WrapFloat for $Vec { wrapped } ($($get)+)}
		// vec_impl_binop!{impl Wrap2PI for $Vec { wrapped_2pi } ($($get)+)}
		// vec_impl_binop!{impl WrapInteger for $Vec { wrapped } ($($get)+)}
		vec_impl_binop!{impl Add for $Vec { add } ($($get)+)}
		vec_impl_binop!{impl Sub for $Vec { sub } ($($get)+)}
		vec_impl_binop!{impl Mul for $Vec { mul } ($($get)+)}
		vec_impl_binop!{impl Div for $Vec { div } ($($get)+)}
		vec_impl_binop!{impl Rem for $Vec { rem } ($($get)+)}
		vec_impl_unop!{ impl AddAssign for $Vec { add_assign } ($($get)+)}
		vec_impl_unop!{ impl SubAssign for $Vec { sub_assign } ($($get)+)}
		vec_impl_unop!{ impl MulAssign for $Vec { mul_assign } ($($get)+)}
		vec_impl_unop!{ impl DivAssign for $Vec { div_assign } ($($get)+)}
		vec_impl_unop!{ impl RemAssign for $Vec { rem_assign } ($($get)+)}

		vec_impl_index!{
			$Vec 
			((usize) -> T)
			((Range<usize>) -> [T])
			((RangeFrom<usize>) -> [T])
			((RangeTo<usize>) -> [T])
			((RangeFull) -> [T])
		}

		impl<T> AsRef<[T]> for $Vec<T> {
			fn as_ref(&self) -> &[T] {
				self.as_slice()
			}
		}

		impl<T> AsMut<[T]> for $Vec<T> {
			fn as_mut(&mut self) -> &mut [T] {
				self.as_mut_slice()
			}
		}
		impl<T> Borrow<[T]> for $Vec<T> {
			fn borrow(&self) -> &[T] {
				self.as_slice()
			}
		}
		impl<T> BorrowMut<[T]> for $Vec<T> {
			fn borrow_mut(&mut self) -> &mut [T] {
				self.as_mut_slice()
			}
		}

		impl<T> AsRef<$Vec<T>> for $Vec<T> {
			fn as_ref(&self) -> &Self {
				self
			}
		}

		impl<T> AsMut<$Vec<T>> for $Vec<T> {
			fn as_mut(&mut self) -> &mut Self {
				self
			}
		}


		/// Consuming iterator over this module's vector type.
		pub struct IntoIter<T> {
			vec: $Vec<T>,
			i: usize,
		}
		
		impl<T> Iterator for IntoIter<T> {
			type Item = T;
			fn next(&mut self) -> Option<Self::Item> {
				let out = self.vec.get(self.i);
				self.i += 1;
				out.map(|x| unsafe { ptr::read_unaligned(x) }) // PERF might want to use read() instead ?
			}
			fn size_hint(&self) -> (usize, Option<usize>) {
				let rem = self.len();
				(rem, Some(rem))
			}
		}

		impl<T> ExactSizeIterator for IntoIter<T> {
			fn len(&self) -> usize {
				$dim - self.i
			}
		}


		impl<T> IntoIterator for $Vec<T> {
			type Item = T;
			type IntoIter = IntoIter<T>;
			fn into_iter(self) -> Self::IntoIter {
				Self::IntoIter { vec: self, i: 0 }
			}
		}

		impl<'a, T> IntoIterator for &'a $Vec<T> {
			type Item = &'a T;
			type IntoIter = slice::Iter<'a, T>;
			fn into_iter(self) -> Self::IntoIter {
				self.as_slice().into_iter()
			}
		}
		impl<'a, T> IntoIterator for &'a mut $Vec<T> {
			type Item = &'a mut T;
			type IntoIter = slice::IterMut<'a, T>;
			fn into_iter(self) -> Self::IntoIter {
				self.as_mut_slice().into_iter()
			}
		}
		impl<T: Default> FromIterator<T> for $Vec<T> {
			fn from_iter<I>(iter: I) -> Self where I: IntoIterator<Item = T> {
				let mut out = Self::default();
				let mut iter = iter.into_iter();
				for elem in &mut out {
					if let Some(value) = iter.next() {
						*elem = value
					} else {
						break;
					}
				}
				out
			}
		}


		// CONVERSIONS

		#[cfg_attr(feature = "clippy", allow(type_complexity))]
		impl<T> From<$Tuple> for $Vec<T> {
			fn from(tuple: $Tuple) -> Self {
				Self::new($(tuple.$tupleget),+)
			}
		}
		/// A vector can be obtained from a single scalar by broadcasting it.
		impl<T: Clone> From<T> for $Vec<T> {
			fn from(val: T) -> Self {
				Self::broadcast(val)
			}
		}

	};
}

macro_rules! vec_impl_spatial {
    ($Vec:ident) => {
        impl<T> $Vec<T> {
            /// Dot product.
            pub fn dot(self, v: Self) -> T where T: Sum + Mul<Output=T> {
                (self * v).sum()
            }
            /// The squared magnitude of a vector is its length multiplied by itself.
            pub fn magnitude_squared(self) -> T where T: Clone + Sum + Mul<Output=T> {
                let v = self.clone();
                self.dot(v)
            }
            /// The magnitude of a vector is its length.
            pub fn magnitude(self) -> T where T: Sum + Float + Mul<Output=T> {
                self.magnitude_squared().sqrt()
            }
            /// Squared distance between two point vectors.
            pub fn distance_squared(self, v: Self) -> T where T: Clone + Sum + Sub<Output=T> + Mul<Output=T> {
                (self - v).magnitude_squared()
            }
            /// Distance between two point vectors.
            pub fn distance(self, v: Self) -> T where T: Sum + Float + Mul<Output=T> + Sub<Output=T> {
                (self - v).magnitude()
            }
            /// Get a copy of this direction vector such that its length equals 1.
            pub fn normalized(self) -> Self where T: Sum + Float + Mul<Output=T> + Div<T, Output=T> {
                self / self.magnitude()
            }
            /// Divide this vector's components such that its length equals 1.
            pub fn normalize(&mut self) where T: Sum + Float + Mul<Output=T> + Div<T, Output=T> {
                *self = self.clone().normalized();
            }
            /// The reflection direction for this vector on a surface which normal is given.
            pub fn reflect(self, surface_normal: Self) -> Self 
                where T: Clone + Sum + Mul<Output=T> + Sub<Output=T> + Add<Output=T>
            {
                let dot = self.clone().dot(surface_normal.clone());
                let p = dot.clone() + dot.clone();
                let mut out: Self = unsafe { mem::uninitialized() };
                for ((out_e, v), s) in out.iter_mut().zip(self.into_iter()).zip(surface_normal.into_iter()) {
                    *out_e = v - s * p.clone();
                }
                out
            }
            /// The refraction vector for this incident vector, a surface normal and a ratio of
            /// indices of refraction (`eta`).
            pub fn refract(self, surface_normal: Self, eta: T) -> Self
                where T: Float + Sum + Mul<Output=T>
            {
                let n = surface_normal;
                let i = self;
                let n_dot_i = n.clone().dot(i.clone());
                let k = T::one() - eta * eta * (T::one() - n_dot_i * n_dot_i);
                if k < T::zero() {
                    Self::zero()
                } else {
                    i * eta - n * (eta * n_dot_i + k.sqrt())
                }
            }
            /// Orients a vector to point away from a surface as defined by its normal.
            pub fn face_forward(self, incident: Self, reference: Self) -> Self
                where T: Sum + Mul<Output=T> + Zero + PartialOrd + Neg<Output=T>
            {
                if reference.dot(incident) <= T::zero() {
                    self
                } else {
                    -self
                }
            }
        }
    };
}

macro_rules! vec_impl_cross {
    ($($Vec:ident)+) => {
        $(
            impl<T> $Vec<T> {
                /// The cross-product of this vector with another.
                pub fn cross(self, v: Self) 
                    -> Self where T: Clone + Mul<Output=T> + Sub<Output=T>
                {
                    let s = self.into_tuple();
                    let v = v.into_tuple();
                    let ss = s.clone();
                    let vv = v.clone();
                    Self::new(
                        s.1*v.2 - ss.2*vv.1,
                        s.2*v.0 - ss.0*vv.2,
                        s.0*v.1 - ss.1*vv.0
                    )
                }
            }
        )+
    }
}

macro_rules! vec_impl_point_or_direction {
    ($($Vec:ident)+) => {
        $(
            impl<T> $Vec<T> {
                /// Creates a point vector in homogeneous coordinates (sets the last coordinate to 1).
                pub fn new_point(x: T, y: T, z: T) -> Self where T: One {
                    Self::new(x, y, z, T::one())
                }
                /// Creates a direction vector in homogeneous coordinates (sets the last coordinate to 0).
                pub fn new_direction(x: T, y: T, z: T) -> Self where T: Zero {
                    Self::new(x, y, z, T::zero())
                }
                /// Turns a vector into a point vector in homogeneous coordinates (sets the last coordinate to 1).
                pub fn point<V: Into<Xyz<T>>>(v: V) -> Self where T: One {
                    let Xyz { x, y, z } = v.into();
                    Self::new_point(x, y, z)
                }
                /// Turns a vector into a direction vector in homogeneous coordinates (sets the last coordinate to 0).
                pub fn direction<V: Into<Xyz<T>>>(v: V) -> Self where T: Zero {
                    let Xyz { x, y, z } = v.into();
                    Self::new_direction(x, y, z)
                }
            }
        )+
    }
}



/// Calls `vec_impl_vec!{}` on each appropriate vector type.
macro_rules! vec_impl_all_vecs {
	($(#[$attrs:meta])+) => {
	#[cfg(feature="vec2")]
		/// A two-components generic vector type.
		pub mod vec2 {
			use super::*;

			/// A two-components generic vector type.
			///
			/// - If you intend to use it as spatial coordinates, consider using [Xy](struct.Xy.html) instead.
			/// - If you intend to use it as a spatial extent, consider using [Extent2](struct.Extent2.html) instead.
			/// - If you intend to use it as texture coordinates, consider using [Uv](struct.Uv.html) instead.
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Vec2<T>(pub T, pub T);

			vec_impl_vec!(tuple Vec2	 vec2	(2) ("({}, {})") (0 1) (x y) (0 1) (T,T));
            vec_impl_spatial!(Vec2);
		}
	#[cfg(feature="vec2")]
		pub use self::vec2::Vec2;
	
	#[cfg(feature="vec3")]
		/// A three-components generic vector type.
		pub mod vec3 {
			use super::*;
			/// A three-components generic vector type.
			///
			/// - If you intend to use it as spatial coordinates, consider using [Xyz](struct.Xyz.html) instead.
			/// - If you intend to use it as a spatial extent, consider using [Extent3](struct.Extent3.html) instead.
			/// - If you intend to use it as RGB color data, consider using [Rgb](struct.Rgb.html) instead.
			/// - If you intend to use it as texture coordinates, consider using [Uvw](struct.Uvw.html) instead.
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Vec3<T>(pub T, pub T, pub T);
			vec_impl_vec!(tuple Vec3	 vec3	(3) ("({}, {}, {})") (0 1 2) (x y z) (0 1 2) (T,T,T));
            vec_impl_spatial!(Vec3);
            vec_impl_cross!(Vec3);
		}
	#[cfg(feature="vec3")]
		pub use self::vec3::Vec3;

	// #[cfg(feature="vec4")]
		/// A four-components generic vector type.
		pub mod vec4 {
			use super::*;
			/// A four-components generic vector type.
			///
			/// - If you intend to use it as homogeneous spatial coordinates, consider using [Xyzw](struct.Xyzw.html) instead.
			/// - If you intend to use it as RGBA color data, consider using [Rgba](struct.Rgba.html) instead.
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Vec4<T>(pub T, pub T, pub T, pub T);
			vec_impl_vec!(tuple Vec4	 vec4	(4) ("({}, {}, {}, {})") (0 1 2 3) (x y z w) (0 1 2 3) (T,T,T,T));
            vec_impl_spatial!(Vec4);
            vec_impl_point_or_direction!(Vec4);
		}
	// #[cfg(feature="vec4")]
		pub use self::vec4::Vec4;

	#[cfg(feature="vec8")]
		/// An eight-components generic vector type.
		pub mod vec8 {
			use super::*;
			/// An eight-components generic vector type.
			///
			/// This type exists mostly for crunching arrays of values.  
			/// For instance, on AVX2-enabled x86 CPUs, a `Vec8<i32>` makes sense.  
			/// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
			///
			/// There's a lot of related intrinsics that are not provided as associated functions.
			/// If you find yourself needing them, use other crates such as `llvmint` or `x86intrin`.
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Vec8<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);
			vec_impl_vec!(tuple Vec8	 vec8	(8) ("({}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7) (m0 m1 m2 m3 m4 m5 m6 m7) (0 1 2 3 4 5 6 7) (T,T,T,T,T,T,T,T));
            vec_impl_spatial!(Vec8);
		}
	#[cfg(feature="vec8")]
		pub use self::vec8::Vec8;

	#[cfg(feature="vec16")]
		/// A sixteen-components generic vector type.
		pub mod vec16 {
			use super::*;
			/// A sixteen-components generic vector type.
			///
			/// This type exists mostly for crunching arrays of values.  
			/// For instance, on AVX2-enabled x86 CPUs, a `Vec16<i16>` makes sense.  
			/// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
			///
			/// There's a lot of related intrinsics that are not provided as associated functions.
			/// If you find yourself needing them, use other crates such as `llvmint` or `x86intrin`.
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Vec16<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);
			vec_impl_vec!(tuple Vec16	vec16   (16) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
            vec_impl_spatial!(Vec16);
		}
	#[cfg(feature="vec16")]
		pub use self::vec16::Vec16;

	#[cfg(feature="vec32")]
		/// A thirty-two-components generic vector type.
		pub mod vec32 {
			use super::*;
			/// A thirty-two-components generic vector type.
			///
			/// This type exists mostly for crunching arrays of values.  
			/// For instance, on AVX512-enabled x86 CPUs, a `Vec32<i16>` makes sense.  
			/// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
			///
			/// There's a lot of related intrinsics that are not provided as associated functions.
			/// If you find yourself needing them, use other crates such as `llvmint` or `x86intrin`.
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Vec32<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);
			vec_impl_vec!(tuple Vec32	vec32   (32) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
            vec_impl_spatial!(Vec32);
		}
	#[cfg(feature="vec32")]
		pub use self::vec32::Vec32;


	#[cfg(feature="vec64")]
		/// A sixty-four-components generic vector type.
		pub mod vec64 {
			use super::*;
			/// A sixty-four-components generic vector type.
			///
			/// This type exists mostly for crunching arrays of values.  
			/// For instance, on AVX512-enabled x86 CPUs, a `Vec64<i8>` makes sense.  
			/// Otherwise, LLVM is able to process it as a fixed-sized array of whichever "best" SIMD vector type available.  
			///
			/// There's a lot of related intrinsics that are not provided as associated functions.
			/// If you find yourself needing them, use other crates such as `llvmint` or `x86intrin`.
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Vec64<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);
			vec_impl_vec!(tuple Vec64	vec64   (64) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31 m32 m33 m34 m35 m36 m37 m38 m39 m40 m41 m42 m43 m44 m45 m46 m47 m48 m49 m50 m51 m52 m53 m54 m55 m56 m57 m58 m59 m60 m61 m62 m63) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
            vec_impl_spatial!(Vec64);
		}
	#[cfg(feature="vec64")]
		pub use self::vec64::Vec64;


	#[cfg(feature="xyzw")]
		/// Vector type suited for homogeneous 3D spatial coordinates.
		pub mod xyzw {
			use super::*;
			/// Vector type suited for homogeneous 3D spatial coordinates.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Xyzw<T> {
				pub x:T, pub y:T, pub z:T,
				/// In homogeneous 3D-space coordinates, `w` is often set to 
				/// `1` for points, and `0` for directions.  
				///
				/// The reason behind this: with floating-point numbers,
				/// division by zero gives infinity (a direction is then
				/// a point stretching infinitely towards another).
				pub w: T
			}
			vec_impl_vec!(struct Xyzw	xyzw	(4) ("({}, {}, {}, {})") (x y z w) (x y z w) (0 1 2 3) (T,T,T,T));
            vec_impl_spatial!(Xyzw);
            vec_impl_point_or_direction!(Xyzw);
		}
	#[cfg(feature="xyzw")]
		pub use self::xyzw::Xyzw;

	#[cfg(feature="xyz")]
		/// Vector type suited for 3D spatial coordinates.
		pub mod xyz {
			use super::*;
			/// Vector type suited for 3D spatial coordinates.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Xyz<T> { pub x:T, pub y:T, pub z:T }
			vec_impl_vec!(struct Xyz	 xyz	 (3) ("({}, {}, {})") (x y z) (x y z) (0 1 2) (T,T,T));
            vec_impl_spatial!(Xyz);
            vec_impl_cross!(Xyz);
		}
	#[cfg(feature="xyz")]
		pub use self::xyz::Xyz;

	#[cfg(feature="xy")]
		/// Vector type suited for 2D spatial coordinates.
		pub mod xy {
			use super::*;
			/// Vector type suited for 2D spatial coordinates.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Xy<T> { pub x:T, pub y:T }
			vec_impl_vec!(struct Xy	  xy	  (2) ("({}, {})") (x y) (x y) (0 1) (T,T));
            vec_impl_spatial!(Xy);
		}
	#[cfg(feature="xy")]
		pub use self::xy::Xy;

	#[cfg(feature="extent3")]
		/// Vector type suited for 3D extents (width, height and depth).
		pub mod extent3 {
			use super::*;
			/// Vector type suited for 3D extents (width, height and depth).
			///
			/// There is no `Unsigned` trait bound because it is not practical, 
			/// since we sometimes want to be
			/// able to express extents as floating-point numbers, for instance.
			///
			/// If you want to assert unsignedness at runtime, you can use the
			/// `is_all_positive()` or `is_any_negative()` methods.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Extent3<T> { pub w:T, pub h:T, pub d:T }
			vec_impl_vec!(struct Extent3 extent3 (3) ("({}, {}, {})") (w h d) (w h d) (0 1 2) (T,T,T));
            vec_impl_spatial!(Extent3);
		}
	#[cfg(feature="extent3")]
		pub use self::extent3::Extent3;

	#[cfg(feature="extent2")]
		/// Vector type suited for 2D extents (width and height).
		pub mod extent2 {
			use super::*;
			/// Vector type suited for 2D extents (width and height).
			///
			/// There is no `Unsigned` trait bound because it is not practical, 
			/// since we sometimes want to be
			/// able to express extents as floating-point numbers, for instance.
			///
			/// If you want to assert unsignedness at runtime, you can use the
			/// `is_all_positive()` or `is_any_negative()` methods.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Extent2<T> { pub w:T, pub h:T }
			vec_impl_vec!(struct Extent2 extent2 (2) ("({}, {})") (w h) (w h) (0 1) (T,T));
            vec_impl_spatial!(Extent2);
		}
	#[cfg(feature="extent2")]
		pub use self::extent2::Extent2;

	#[cfg(feature="rgba")]
		/// Vector type suited for RGBA color data.
		pub mod rgba {
			use super::*;
			/// Vector type suited for RGBA color data.
			///
			/// There is no trait bound on `ColorComponent`, but if `T` doesn't implement it, you'll
			/// miss some goodies.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Rgba<T> { pub r:T, pub g:T, pub b:T, pub a:T }
			vec_impl_vec!(struct Rgba	rgba	(4) ("rgba({}, {}, {}, {})") (r g b a) (r g b a) (0 1 2 3) (T,T,T,T));
		}
	#[cfg(feature="rgba")]
		pub use self::rgba::Rgba;

	#[cfg(feature="rgb")]
		/// Vector type suited for RGB color data.
		pub mod rgb {
			use super::*;
			/// Vector type suited for RGB color data.
			///
			/// There is no trait bound on `ColorComponent`, but if `T` doesn't implement it, you'll
			/// miss some goodies.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Rgb<T> { pub r:T, pub g:T, pub b:T }
			vec_impl_vec!(struct Rgb	 rgb	 (3) ("rgb({}, {}, {})") (r g b) (r g b) (0 1 2) (T,T,T));
		}
	#[cfg(feature="rgb")]
		pub use self::rgb::Rgb;

	#[cfg(feature="uvw")]
		/// Vector type suited for 3D texture coordinates.
		pub mod uvw {
			use super::*;
			/// Vector type suited for 3D texture coordinates.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Uvw<T> { pub u:T, pub v:T, pub w:T }
			vec_impl_vec!(struct Uvw	 uvw	 (3) ("({}, {}, {})") (u v w) (u v w) (0 1 2) (T,T,T));
		}
	#[cfg(feature="uvw")]
		pub use self::uvw::Uvw;

	#[cfg(feature="uv")]
		/// Vector type suited for 2D texture coordinates.
		pub mod uv {
			use super::*;
			/// Vector type suited for 2D texture coordinates.
			#[allow(missing_docs)]
			#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
			#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
			$(#[$attrs])+
			pub struct Uv<T> { pub u:T, pub v:T }
			vec_impl_vec!(struct Uv	  uv	  (2) ("({}, {})") (u v) (u v) (0 1) (T,T));
		}
	#[cfg(feature="uv")]
		pub use self::uv::Uv;

	}
}

pub mod repr_c {
	//! Vector types which are marked `#[repr(packed, C)]`.
	//!
	//! You can instantiate any vector type of this module with any type `T`.
	
	use super::*;
	vec_impl_all_vecs!{
		#[repr(C)]
		#[cfg_attr(all(nightly, feature="repr_align", any(target_arch="x86", target_arch="x86_64")), repr(align(16)))]
		#[cfg_attr(all(nightly, feature="repr_align", target_arch="arm"), repr(align(64)))]
		// XXX ^^^^ Not sure about the alignment on ARM ??
		// TODO assert the packing of vecs
	}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
	//! Vector types which are marked `#[repr(packed, simd)]`.
	//!
	//! You can instantiate any vector type of this module with any type as long as
	//! it is a "machine type", like `f32` and `i32`, but not `isize` or newtypes
	//! (normally, unless they're marked `#[repr(transparent)]`, but that hasn't been tested yet).
	
	use super::*;
	vec_impl_all_vecs!{#[repr(packed, simd)]}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
/// If you're on Nightly with the `repr_simd` feature enabled, this exports `self::repr_simd::*` instead.
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

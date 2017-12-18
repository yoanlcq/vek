//! Vector types.
//!
//! They do NOT derive `PartialOrd` and `Ord`, because it makes no sense for them, 
//! and functions such as `partial_min` and `partial_max` may give surprising results
//! because of this.  
//! They do have element-wise comparison functions though.

// TODO: Get rid of mem::uninitialized()

use core::borrow::{Borrow, BorrowMut};
use core::fmt::{self, Display, Formatter};
use core::iter::{FromIterator, Product, Sum};
use core::mem;
use core::ptr;
use core::cmp;
use core::ops::*;
use core::slice::{self, /*SliceIndex*/}; // NOTE: Will want to use SliceIndex once it's stabilized
use num_traits::{Zero, One, NumCast, Signed, Float};
use approx::ApproxEq;
use ops::*;

macro_rules! vec_impl_cmp {
    ($(#[$attrs:meta])*, $Vec:ident, $cmp:ident, $op:tt, $Bounds:tt) => {
        // NOTE: Rhs is taken as reference: see how std::cmp::PartialEq is implemented.
        $(#[$attrs])*
        pub fn $cmp<Rhs: AsRef<Self>>(&self, rhs: &Rhs) -> $Vec<bool> where T: $Bounds {
            let mut out: $Vec<bool> = unsafe { mem::uninitialized() };
            let mut iter = self.iter().zip(rhs.as_ref().iter());
            for elem in &mut out {
                let (a, b) = iter.next().unwrap();
                *elem = a $op b;
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
        impl<           T> $Op< $Vec<T>,     $Vec<T>> for    $Vec<T> where   T: $Op<    T,   T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>,   $Vec<T>,    $Vec<T>} }
        impl<       'c, T> $Op< $Vec<T>,     $Vec<T>> for &'c $Vec<T> where &'c T: $Op< T,   T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>,   $Vec<T>,    $Vec<T>} }
        impl<   'b,  T> $Op<    $Vec<T>, &'b $Vec<T>> for    $Vec<T> where   T: $Op<    T, &'b T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>,     $Vec<T>, &'b $Vec<T>} }
        impl<   'b, 'c, T> $Op< $Vec<T>, &'b $Vec<T>> for &'c $Vec<T> where &'c T: $Op< T, &'b T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>,     $Vec<T>, &'b $Vec<T>} }
        impl<'a,         T> $Op<&'a $Vec<T>,     $Vec<T>> for    $Vec<T> where   T: $Op<&'a T,   T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>, &'a $Vec<T>,  $Vec<T>} }
        impl<'a,     'c, T> $Op<&'a $Vec<T>,     $Vec<T>> for &'c $Vec<T> where &'c T: $Op<&'a T,    T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>, &'a $Vec<T>,  $Vec<T>} }
        impl<'a, 'b,     T> $Op<&'a $Vec<T>, &'b $Vec<T>> for    $Vec<T> where   T: $Op<&'a T, &'b T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>, &'a $Vec<T>, &'b $Vec<T>} }
        impl<'a, 'b, 'c, T> $Op<&'a $Vec<T>, &'b $Vec<T>> for &'c $Vec<T> where &'c T: $Op<&'a T, &'b T, Output=T> { vec_impl_trinop_vec_vec!{$op, $Vec<T>, &'a $Vec<T>, &'b $Vec<T>} }

        /* I give up, it's dumb.
        impl<           T> $Op< T,   T> for  $Vec<T> where   T: $Op<    T,   T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>,  T,  T, a, b} }
        impl<       'c, T> $Op< T,   T> for &'c $Vec<T> where &'c T: $Op<   T,   T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>,  T,  T, a, b} }
        impl<   'b,  T> $Op<    T, &'b T> for    $Vec<T> where   T: $Op<    T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>,    T, &'b T, a, b     } }
        impl<   'b, 'c, T> $Op< T, &'b T> for &'c $Vec<T> where &'c T: $Op< T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>,    T, &'b T, a, b     } }
        impl<'a,         T> $Op<&'a T,   T> for  $Vec<T> where   T: $Op<&'a T,   T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>, &'a T,   T, a       , b} }
        impl<'a,     'c, T> $Op<&'a T,   T> for &'c $Vec<T> where &'c T: $Op<&'a T,  T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>, &'a T,   T, a       , b} }
        impl<'a, 'b,     T> $Op<&'a T, &'b T> for    $Vec<T> where   T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>, &'a T, &'b T, a       , b     } }
        impl<'a, 'b, 'c, T> $Op<&'a T, &'b T> for &'c $Vec<T> where &'c T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_s!{$op, $Vec<T>, &'a T, &'b T, a        , b     } }

        impl<           T> $Op< $Vec<T>,     T> for  $Vec<T> where   T: $Op<    T,   T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>,    $Vec<T>,    T, b }
        impl<       'c, T> $Op< $Vec<T>,     T> for &'c $Vec<T> where &'c T: $Op<   T,   T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>,    $Vec<T>,    T, b }
        impl<   'b,  T> $Op<    $Vec<T>, &'b T> for  $Vec<T> where   T: $Op<    T, &'b T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>,  $Vec<T>, &'b T, b }
        impl<   'b, 'c, T> $Op< $Vec<T>, &'b T> for &'c $Vec<T> where &'c T: $Op<   T, &'b T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>,  $Vec<T>, &'b T, b }
        impl<'a,         T> $Op<&'a $Vec<T>,     T> for  $Vec<T> where   T: $Op<&'a T,   T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>, &'a $Vec<T>,   T, b }
        impl<'a,     'c, T> $Op<&'a $Vec<T>,     T> for &'c $Vec<T> where &'c T: $Op<&'a T,  T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>, &'a $Vec<T>,   T, b }
        impl<'a, 'b,     T> $Op<&'a $Vec<T>, &'b T> for  $Vec<T> where   T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>, &'a $Vec<T>, &'b T, b }
        impl<'a, 'b, 'c, T> $Op<&'a $Vec<T>, &'b T> for &'c $Vec<T> where &'c T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_vec_s!{$op, $Vec<T>, &'a $Vec<T>, &'b T, b }

        impl<           T> $Op< T,   $Vec<T>> for    $Vec<T> where   T: $Op<    T,   T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>,    T,  $Vec<T>, a }
        impl<       'c, T> $Op< T,   $Vec<T>> for &'c $Vec<T> where &'c T: $Op< T,   T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>,    T,  $Vec<T>, a }
        impl<   'b,  T> $Op<    T, &'b $Vec<T>> for  $Vec<T> where   T: $Op<    T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>,  T, &'b $Vec<T>, a }
        impl<   'b, 'c, T> $Op< T, &'b $Vec<T>> for &'c $Vec<T> where &'c T: $Op<   T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>,  T, &'b $Vec<T>, a }
        impl<'a,         T> $Op<&'a T,   $Vec<T>> for    $Vec<T> where   T: $Op<&'a T,   T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>, &'a T,     $Vec<T>, a }
        impl<'a,     'c, T> $Op<&'a T,   $Vec<T>> for &'c $Vec<T> where &'c T: $Op<&'a T,    T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>, &'a T,     $Vec<T>, a }
        impl<'a, 'b,     T> $Op<&'a T, &'b $Vec<T>> for  $Vec<T> where   T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>, &'a T, &'b $Vec<T>, a }
        impl<'a, 'b, 'c, T> $Op<&'a T, &'b $Vec<T>> for &'c $Vec<T> where &'c T: $Op<&'a T, &'b T, Output=T>, T: Copy { vec_impl_trinop_s_vec!{$op, $Vec<T>, &'a T, &'b $Vec<T>, a }
        */
    }
}

macro_rules! vec_impl_binop {
    (impl $Op:ident for $Vec:ident { $op:tt } ($($get:tt)+)) => {
        // NOTE: Reminder that scalars T: Copy also implement Into<$Vec<T>>.
        impl<V, T> $Op<V> for $Vec<T> where V: Into<$Vec<T>>, T: $Op<Output=T> {
            type Output = Self;
            fn $op(self, rhs: V) -> Self::Output {
                let rhs = rhs.into();
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

        impl<'a, T> $Op<&'a T> for $Vec<T> where T: $Op<&'a T, Output=T> {
            type Output = $Vec<T>;
            fn $op(self, rhs: &'a T) -> Self::Output {
                $Vec::new($(self.$get.$op(rhs)),+)
            }
        }
        impl<'a, T> $Op<T> for &'a $Vec<T> where &'a T: $Op<T, Output=T>, T: Copy {
            type Output = $Vec<T>;
            fn $op(self, rhs: T) -> Self::Output {
                $Vec::new($(self.$get.$op(rhs)),+)
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
        // NOTE: Reminder that scalars T: Copy also implement Into<$Vec<T>>.
        impl<V, T> $Op<V> for $Vec<T> where V: Into<$Vec<T>>, T: $Op<T> {
            fn $op(&mut self, rhs: V) {
                let rhs = rhs.into();
                $(self.$get.$op(rhs.$get);)+
            }
        }
        impl<'a, T> $Op<&'a $Vec<T>> for $Vec<T> where T: $Op<&'a T> {
            fn $op(&mut self, rhs: &'a $Vec<T>) {
                $(self.$get.$op(&rhs.$get);)+
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

    ($c_or_simd:ident tuple $Vec:ident $vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

        impl<T> $Vec<T> {
            /// Creates a vector from each component.
            #[cfg_attr(feature = "clippy", allow(too_many_arguments))]
            pub fn new($($namedget:T),+) -> Self {
                $Vec($($namedget),+)
            }
        }

        vec_impl_vec!{common $Vec $vec ($dim) ($fmt) ($($get)+) ($($namedget)+) ($($tupleget)+) $Tuple}
        vec_impl_vec!{$c_or_simd $Vec $vec ($dim) ($fmt) ($($get)+) ($($namedget)+) ($($tupleget)+) $Tuple}
    };

    ($c_or_simd:ident struct $Vec:ident $vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

        impl<T> $Vec<T> {
            /// Creates a vector from each component.
            #[cfg_attr(feature = "clippy", allow(too_many_arguments))]
            pub fn new($($namedget:T),+) -> Self {
                Self { $($namedget),+ }
            }
        }

        vec_impl_vec!{common $Vec $vec ($dim) ($fmt) ($($get)+) ($($namedget)+) ($($tupleget)+) $Tuple}
        vec_impl_vec!{$c_or_simd $Vec $vec ($dim) ($fmt) ($($get)+) ($($namedget)+) ($($tupleget)+) $Tuple}
    };

    (c $Vec:ident $vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

    };
    (simd $Vec:ident $vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

        use super::super::repr_c::$vec::$Vec as CVec;

        impl<T> From<CVec<T>> for $Vec<T> {
            fn from(v: CVec<T>) -> Self {
                Self::new($(v.$get),+)
            }
        }

        impl<T> From<$Vec<T>> for CVec<T> {
            fn from(v: $Vec<T>) -> Self {
                Self::new($(v.$get),+)
            }
        }

        /* WISH: Is it right?
        impl<T> AsRef<CVec<T>> for $Vec<T> {
            fn as_ref(v: &Self) -> &CVec<T> {
                unsafe {
                    mem::transmute(self)
                }
            }
        }
        impl<T> AsMut<CVec<T>> for $Vec<T> {
            fn as_ref(v: &mut Self) -> &mut CVec<T> {
                {
                    mem::transmute(self)
                }
            }
        }
        */

        impl<T> $Vec<T> {
            pub fn into_repr_c(self) -> CVec<T> {
                self.into()
            }
        }
        impl<T> CVec<T> {
            pub fn into_repr_simd(self) -> $Vec<T> {
                self.into()
            }
        }
    };
    (common $Vec:ident $vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

        #[allow(missing_docs)]
        /// Displays the vector, formatted as `
        #[doc=$fmt]
        /// `.
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
            /// assert_eq!(Vec4::broadcast(5), Vec4::new(5,5,5,5));
            /// assert_eq!(Vec4::broadcast(5), Vec4::from(5));
            /// ```
            pub fn broadcast(val: T) -> Self where T: Copy {
                Self::new($({let $get = val; $get}),+)
            }

            /// Creates a new vector with all elements set to zero.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// assert_eq!(Vec4::zero(), Vec4::new(0,0,0,0));
            /// assert_eq!(Vec4::zero(), Vec4::broadcast(0));
            /// assert_eq!(Vec4::zero(), Vec4::from(0));
            /// ```
            pub fn zero() -> Self where T: Zero {
                Self::new($({let $get = T::zero(); $get}),+)
            }

            /// Creates a new vector with all elements set to one.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// assert_eq!(Vec4::one(), Vec4::new(1,1,1,1));
            /// assert_eq!(Vec4::one(), Vec4::broadcast(1));
            /// assert_eq!(Vec4::one(), Vec4::from(1));
            /// ```
            pub fn one() -> Self where T: One {
                Self::new($({let $get = T::one(); $get}),+)
            }

            /// Are all elements of this vector equal to the given value ?
            pub fn is_broadcast(&self, val: T) -> bool where T: Copy + PartialEq {
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
            /// assert_eq!(Vec4::iota(), Vec4::new(0, 1, 2, 3));
            /// ```
            pub fn iota() -> Self where T: Zero + One + AddAssign + Copy {
                let mut out = Self::zero();
                let mut i = T::zero();
                $(
                    out.$get = i;
                    i += T::one();
                )+
                out
            }

            /// Convenience method which returns the number of elements of this vector.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let v = Vec4::new(0,1,2,3);
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

            /// Are elements of this vector tightly packed in memory ?
            // NOTE: Not public, because it is supposed to always be true.
            // If it's not, we'll have to handle an exotic target.
            pub(crate) fn is_packed(&self) -> bool {
                let ptr = self as *const _ as *const T;
                let mut i = -1isize;
                $(
                    i += 1;
                    if unsafe { ptr.offset(i) } != &self.$get as *const _ {
                        return false;
                    }
                )+
                true
            }
            /// Converts this into a raw pointer of read-only data.
            pub fn as_ptr(&self) -> *const T {
                // This ought to be true and is checked by tests.
                // Still, let's be careful about exotic architectures
                // or alignment requirement of elements.
                // This panic case is not documented because it is never supposed to happen in the
                // first place (otherwise, we missed something).
                // Also, it would have to be mentioned in all APIs that use
                // as_ptr(), such as as_slice and impl Index.
                assert!(self.is_packed());
                self as *const _ as *const T
            }
            /// Converts this into a raw pointer.
            pub fn as_mut_ptr(&mut self) -> *mut T {
                // See rationale in as_mut_ptr()
                assert!(self.is_packed());
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
            pub fn from_slice(slice: &[T]) -> Self where T: Default + Copy {
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
            /// let v = Vec4::new(0_f32, 1., 1.8, 3.14);
            /// let i = v.convert(|x| x.round() as i32);
            /// assert_eq!(i, Vec4::new(0, 1, 2, 3));
            /// ```
            pub fn convert<D,F>(self, f: F) -> $Vec<D> where F: Fn(T) -> D {
                $Vec::new($(f(self.$get)),+)
            }
            /// Returns a memberwise-converted copy of this vector, using `NumCast`.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let v = Vec4::new(0_f32, 1., 2., 3.);
            /// let i: Vec4<i32> = v.numcast().unwrap();
            /// assert_eq!(i, Vec4::new(0, 1, 2, 3));
            /// ```
            pub fn numcast<D>(self) -> Option<$Vec<D>> where T: NumCast, D: NumCast {
                Some($Vec::new($(D::from(self.$get)?),+))
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
            /// let a = Vec4::new(0,1,2,3);
            /// let b = Vec4::new(4,5,6,7);
            /// let c = Vec4::new(8,9,0,1);
            /// assert_eq!(a*b+c, a.mul_add(b, c));
            /// ```
            pub fn mul_add<V: Into<Self>>(self, mul: V, add: V) -> Self 
                where T: MulAdd<T,T,Output=T>
            {
                let mul = mul.into();
                let add = add.into();
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
            /// let a = Vec4::new(0,1,2,3);
            /// let b = Vec4::new(3,2,1,0);
            /// let m = Vec4::new(0,1,1,0);
            /// assert_eq!(m, Vec4::min(a, b));
            /// ```
            pub fn min<V>(a: V, b: V) -> Self where V: Into<Self>, T: Ord {
                let (a, b) = (a.into(), b.into());
                Self::new($(cmp::min(a.$get, b.$get)),+)
            }
            /// Compares elements of `a` and `b`, and returns the maximum values into a new
            /// vector, using total ordering.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let a = Vec4::new(0,1,2,3);
            /// let b = Vec4::new(3,2,1,0);
            /// let m = Vec4::new(3,2,2,3);
            /// assert_eq!(m, Vec4::max(a, b));
            /// ```
            pub fn max<V>(a: V, b: V) -> Self where V: Into<Self>, T: Ord {
                let (a, b) = (a.into(), b.into());
                Self::new($(cmp::max(a.$get, b.$get)),+)
            }
            /// Compares elements of `a` and `b`, and returns the minimum values into a new
            /// vector, using partial ordering.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let a = Vec4::new(0,1,2,3);
            /// let b = Vec4::new(3,2,1,0);
            /// let m = Vec4::new(0,1,1,0);
            /// assert_eq!(m, Vec4::partial_min(a, b));
            /// ```
            pub fn partial_min<V>(a: V, b: V) -> Self where V: Into<Self>, T: Ord {
                let (a, b) = (a.into(), b.into());
                Self::new($(partial_min(a.$get, b.$get)),+)
            }
            /// Compares elements of `a` and `b`, and returns the minimum values into a new
            /// vector, using partial ordering.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let a = Vec4::new(0,1,2,3);
            /// let b = Vec4::new(3,2,1,0);
            /// let m = Vec4::new(3,2,2,3);
            /// assert_eq!(m, Vec4::partial_max(a, b));
            /// ```
            pub fn partial_max<V>(a: V, b: V) -> Self where V: Into<Self>, T: Ord {
                let (a, b) = (a.into(), b.into());
                Self::new($(partial_max(a.$get, b.$get)),+)
            }

            /// Returns the element which has the lowest value in this vector, using total
            /// ordering.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// assert_eq!(-5, Vec4::new(0, 5, -5, 8).reduce_min());
            /// ```
            pub fn reduce_min(self) -> T where T: Ord {
                self.into_iter().min().unwrap()
            }
            /// Returns the element which has the highest value in this vector, using total
            /// ordering.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// assert_eq!(8, Vec4::new(0, 5, -5, 8).reduce_max());
            /// ```
            pub fn reduce_max(self) -> T where T: Ord {
                self.into_iter().max().unwrap()
            }

            /// Returns the element which has the lowest value in this vector, using partial
            /// ordering.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// assert_eq!(-5_f32, Vec4::new(0_f32, 5., -5., 8.).reduce_partial_min());
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
            /// assert_eq!(8_f32, Vec4::new(0_f32, 5., -5., 8.).reduce_partial_max());
            /// ```
            pub fn reduce_partial_max(self) -> T where T: PartialOrd {
                let first = unsafe { ptr::read(self.get_unchecked(0)) };
                self.into_iter().skip(1).fold(first, partial_max)
            }

            /// Returns the product of each of this vector's elements.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// assert_eq!(1*2*3*4, Vec4::new(1, 2, 3, 4).product());
            /// ```
            pub fn product(self) -> T where T: Product {
                self.into_iter().product()
            }
            /// Returns the sum of each of this vector's elements.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// assert_eq!(1+2+3+4, Vec4::new(1, 2, 3, 4).sum());
            /// ```
            pub fn sum(self) -> T where T: Sum {
                self.into_iter().sum()
            }
            /// Returns the average of this vector's elements.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// assert_eq!(2.5_f32, Vec4::new(1_f32, 2., 3., 4.).average());
            /// ```
            ///
            /// You should avoid using it on `u8` vectors, not only because integer
            /// overflows cause panics in debug mode, but also because of integer division, the result
            /// may not be the one you expect.
            ///
            /// ```should_panic
            /// # use vek::vec::Vec4;
            /// let red = Vec4::new(255u8, 1, 0, 0);
            /// let grey_level = red.average();
            /// assert_eq!(grey_level, 128);
            /// ```
            ///
            /// You may want to convert the elements to bigger integers
            /// (or floating-point) instead:
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let red = Vec4::new(255u8, 1, 128, 128);
            ///
            /// let red = red.convert(|c| c as u16);
            /// let grey_level = red.average() as u8;
            /// assert_eq!(grey_level, 128);
            ///
            /// let red = red.convert(|c| c as f32);
            /// let grey_level = red.average().round() as u8;
            /// assert_eq!(grey_level, 128);
            /// ```
            pub fn average(self) -> T where T: Sum + Div<T, Output=T> + From<u8> {
                self.sum() / T::from($dim as _)
            }

            /// Returns a new vector which elements are the respective square roots of this
            /// vector's elements.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let v = Vec4::new(1f32, 2f32, 3f32, 4f32);
            /// let s = Vec4::new(1f32, 4f32, 9f32, 16f32);
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
            /// let v = Vec4::new(1f32, 0.5f32, 1f32/3f32, 0.25f32);
            /// let s = Vec4::new(1f32, 4f32, 9f32, 16f32);
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
            /// let v = Vec4::new(1f32, 0.5f32, 0.25f32, 0.125f32);
            /// let s = Vec4::new(1f32, 2f32, 4f32, 8f32);
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
            /// let v = Vec4::new(0_f32, 1., 1.8, 3.14);
            /// assert_eq!(v.ceil(), Vec4::new(0f32, 1f32, 2f32, 4f32));
            /// ```
            pub fn ceil(self) -> Self where T: Float {
                Self::new($(self.$get.ceil()),+)
            }
            /// Returns a new vector which elements are rounded down to the nearest lower integer.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let v = Vec4::new(0_f32, 1., 1.8, 3.14);
            /// assert_eq!(v.floor(), Vec4::new(0f32, 1f32, 1f32, 3f32));
            /// ```
            pub fn floor(self) -> Self where T: Float {
                Self::new($(self.$get.floor()),+)
            }
            /// Returns a new vector which elements are rounded to the nearest integer.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let v = Vec4::new(0_f32, 1., 1.8, 3.14);
            /// assert_eq!(v.round(), Vec4::new(0f32, 1f32, 2f32, 3f32));
            /// ```
            pub fn round(self) -> Self where T: Float {
                Self::new($(self.$get.round()),+)
            }

            /// Horizontally adds adjacent pairs of elements in `self` and `rhs` into a new vector.
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let a = Vec4::new(0, 1, 2, 3);
            /// let b = Vec4::new(4, 5, 6, 7);
            /// let h = Vec4::new(0+1, 2+3, 4+5, 6+7);
            /// assert_eq!(h, a.hadd(b));
            /// ```
            pub fn hadd<V>(self, rhs: V) -> Self where V: Into<Self>, T: Add<T, Output=T> {
                let rhs = rhs.into();
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
                /// let u = Vec4::new(0,2,2,6);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.partial_cmpeq(&v), Vec4::new(true, false, true, false));
                /// ```
                , $Vec, partial_cmpeq, ==, PartialEq
            }
            vec_impl_cmp!{
                /// Compares each element of two vectors with the partial not-equal test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,6);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.partial_cmpne(&v), Vec4::new(false, true, false, true));
                /// ```
                , $Vec, partial_cmpne, !=, PartialEq
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the partial greater-or-equal test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,2);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.partial_cmpge(&v), Vec4::new(true, true, true, false));
                /// ```
                , $Vec, partial_cmpge, >=, PartialOrd
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the partial greater-than test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,6);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.partial_cmpgt(&v), Vec4::new(false, true, false, true));
                /// ```
                , $Vec, partial_cmpgt, >, PartialOrd
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the partial less-or-equal test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,2);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.partial_cmple(&v), Vec4::new(true, false, true, true));
                /// ```
                , $Vec, partial_cmple, <=, PartialOrd
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the partial less-than test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,2);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.partial_cmplt(&v), Vec4::new(false, false, false, true));
                /// ```
                , $Vec, partial_cmplt, <, PartialOrd
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the partial equality test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,6);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.cmpeq(&v), Vec4::new(true, false, true, false));
                /// ```
                , $Vec, cmpeq, ==, Eq
            }
            vec_impl_cmp!{
                /// Compares each element of two vectors with the total not-equal test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,6);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.cmpne(&v), Vec4::new(false, true, false, true));
                /// ```
                , $Vec, cmpne, !=, Eq
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the total greater-or-equal test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,2);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.cmpge(&v), Vec4::new(true, true, true, false));
                /// ```
                , $Vec, cmpge, >=, Ord
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the total greater-than test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,6);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.cmpgt(&v), Vec4::new(false, true, false, true));
                /// ```
                , $Vec, cmpgt, >, Ord
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the total less-or-equal test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,2);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.cmple(&v), Vec4::new(true, false, true, true));
                /// ```
                , $Vec, cmple, <=, Ord
            }

            vec_impl_cmp!{
                /// Compares each element of two vectors with the total less-than test, returning a boolean vector.
                ///
                /// ```
                /// # use vek::vec::Vec4;
                /// let u = Vec4::new(0,2,2,2);
                /// let v = Vec4::new(0,1,2,3);
                /// assert_eq!(u.cmplt(&v), Vec4::new(false, false, false, true));
                /// ```
                , $Vec, cmplt, <, Ord
            }

	        /// Returns the linear interpolation of `from` to `to` with `factor` unconstrained.  
            /// See the `Lerp` trait.
            fn lerp_unclamped_precise<V: Into<Self>, S: Into<Self>>(from: V, to: V, factor: S) -> Self
                where T: Copy + One + Mul<Output=T> + Sub<Output=T> + Add<Output=T> + MulAdd<T,T,Output=T>
            {
                let (from, to, factor) = (from.into(), to.into(), factor.into());
                from.mul_add(Self::one()-factor, to*factor)
            }
	        /// Same as `lerp_unclamped_precise`, implemented as a possibly faster but less precise operation.
            /// See the `Lerp` trait.
            fn lerp_unclamped<V: Into<Self>, S: Into<Self>>(from: V, to: V, factor: S) -> Self
                where T: Copy + One + Mul<Output=T> + Sub<Output=T> + Add<Output=T> + MulAdd<T,T,Output=T>
            {
                let (from, to, factor) = (from.into(), to.into(), factor.into());
                factor.mul_add(to - from, from)
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
        // WISH: impl Float for Vec<Float> ?
        // NOPE: Vectors can't implement these items :
        // - fn classify(self) -> FpCategory;
        // - fn integer_decode(self) -> (u64, i16, i8);


        impl<T: ApproxEq> ApproxEq for $Vec<T> where T::Epsilon: Copy {
            type Epsilon = T::Epsilon;

            fn default_epsilon() -> T::Epsilon {
                T::default_epsilon()
            }

            fn default_max_relative() -> T::Epsilon {
                T::default_max_relative()
            }

            fn default_max_ulps() -> u32 {
                T::default_max_ulps()
            }

            fn relative_eq(&self, other: &Self, epsilon: T::Epsilon, max_relative: T::Epsilon) -> bool {
                for (l, r) in self.iter().zip(other.iter()) {
                    if !T::relative_eq(l, r, epsilon, max_relative) {
                        return false;
                    }
                }
                true
            }

            fn ulps_eq(&self, other: &Self, epsilon: T::Epsilon, max_ulps: u32) -> bool {
                for (l, r) in self.iter().zip(other.iter()) {
                    if !T::ulps_eq(l, r, epsilon, max_ulps) {
                        return false;
                    }
                }
                true
            }
        }


        // OPS

        impl<T> Neg for $Vec<T> where T: Neg<Output=T> {
            type Output = Self;
            fn neg(self) -> Self::Output {
                Self::new($(-self.$get),+)
            }
        }

        impl<T> Lerp<T> for $Vec<T>
            where T: Copy + One + Mul<Output=T> + Sub<Output=T> + Add<Output=T> + MulAdd<T,T,Output=T>
        {
            fn lerp_unclamped_precise(from: Self, to: Self, factor: T) -> Self {
                Self::lerp_unclamped_precise(from, to, factor)
            }
            fn lerp_unclamped(from: Self, to: Self, factor: T) -> Self {
                Self::lerp_unclamped(from, to, factor)
            }
        }

        impl<T> Lerp<$Vec<T>> for $Vec<T>
            where T: Copy + One + Mul<Output=T> + Sub<Output=T> + Add<Output=T> + MulAdd<T,T,Output=T>
        {
            fn lerp_unclamped_precise(from: Self, to: Self, factor: Self) -> Self {
                Self::lerp_unclamped_precise(from, to, factor)
            }
            fn lerp_unclamped(from: Self, to: Self, factor: Self) -> Self {
                Self::lerp_unclamped(from, to, factor)
            }
        }
        impl<T: Float + Copy> Wrap<T> for $Vec<T> {
            fn wrapped(self, upper: T) -> Self {
                self.wrapped(Self::broadcast(upper))
            }
        }
        impl<T: Float + Copy> Wrap<$Vec<T>> for $Vec<T> {
            fn wrapped(self, upper: $Vec<T>) -> Self {
                self - (self/upper).floor() * upper
            }
        }


        vec_impl_trinop!{impl MulAdd for $Vec { mul_add } ($($get)+)}
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
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
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
        #[cfg_attr(feature = "clippy", allow(type_complexity))]
        impl<T> From<[T; $dim]> for $Vec<T> {
            fn from(array: [T; $dim]) -> Self {
                Self::new($(unsafe { ptr::read(&array[$tupleget])}),+)
            }
        }
        /// A vector can be obtained from a single scalar by broadcasting it.
        ///
        /// This conversion is important because it allows scalars to be
        /// smoothly accepted as operands in most vector operations.
        impl<T: Copy> From<T> for $Vec<T> {
            fn from(val: T) -> Self {
                Self::broadcast(val)
            }
        }

        // We can't do this :(
        /*
        impl<U, T: From<U>> From<$Vec<U>> for $Vec<T> {
            fn from(v: $Vec<U>) -> Self {
                Self::new($(v.$get.into()),+)
            }
        }
        */
    };
}

macro_rules! vec_impl_spatial {
    ($Vec:ident) => {
        impl<T> $Vec<T> {
            /// Dot product between this vector and another.
            pub fn dot(self, v: Self) -> T where T: Sum + Mul<Output=T> {
                (self * v).sum()
            }
            /// The squared magnitude of a vector is its spatial length, squared.
            /// It is slightly cheaper to compute than `magnitude` because it avoids a square root.
            pub fn magnitude_squared(self) -> T where T: Copy + Sum + Mul<Output=T> {
                self.dot(self)
            }
            /// The magnitude of a vector is its spatial length.
            pub fn magnitude(self) -> T where T: Sum + Float {
                self.magnitude_squared().sqrt()
            }
            /// Squared distance between two point vectors.
            /// It is slightly cheaper to compute than `distance` because it avoids a square root.
            pub fn distance_squared(self, v: Self) -> T where T: Copy + Sum + Sub<Output=T> + Mul<Output=T> {
                (self - v).magnitude_squared()
            }
            /// Distance between two point vectors.
            pub fn distance(self, v: Self) -> T where T: Sum + Float {
                (self - v).magnitude()
            }
            /// Get a copy of this direction vector such that its length equals 1.
            pub fn normalized(self) -> Self where T: Sum + Float {
                self / self.magnitude()
            }
            /// Divide this vector's components such that its length equals 1.
            pub fn normalize(&mut self) where T: Sum + Float {
                *self = self.normalized();
            }
            /// Is this vector normalized ? (Uses `ApproxEq`)
            pub fn is_normalized(self) -> bool where T: ApproxEq + Sum + Float {
                self.magnitude_squared().relative_eq(&T::one(), T::default_epsilon(), T::default_max_relative())
            }
            /// Get the smallest angle, in radians, between two direction vectors.
            pub fn angle_radians(self, v: Self) -> T where T: Sum + Float {
                self.normalized().dot(v.normalized()).acos()
            }
            /// Get the smallest angle, in degrees, between two direction vectors.
            pub fn angle_degrees(self, v: Self) -> T
                where T: From<u16> + Sum + Float
            {
                <T as From<u16>>::from(360_u16) * self.angle_radians(v)
            }
            /// The reflection direction for this vector on a surface which normal is given.
            pub fn reflect(self, surface_normal: Self) -> Self 
                where T: Copy + Sum + Mul<Output=T> + Sub<Output=T> + Add<Output=T>
            {
                let dot = self.dot(surface_normal);
                let p = dot + dot;
                let mut out: Self = unsafe { mem::uninitialized() };
                for ((out_e, v), s) in out.iter_mut().zip(self.into_iter()).zip(surface_normal.into_iter()) {
                    *out_e = v - s * p;
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
                let n_dot_i = n.dot(i);
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


#[allow(unused_macros)]
macro_rules! vec_impl_spatial_2d {
    ($Vec:ident) => {
        impl<T> $Vec<T> {
            /// A signed value which tells in which half-space of the line segment `ab` this point lies.
            ///
            /// Returns:
            ///
            /// - ` < 0`: This point lies in the half-space right of segment `ab`.
            /// - `== 0`: This point lies in the infinite line along segment `ab`.
            /// - ` > 0`: This point lies in the half-space left of segment `ab`.
            pub fn determine_side(self, a: Self, b: Self) -> T 
                where T: Copy + Sub<Output=T> + Mul<Output=T>
            {
                let (cx, cy) = self.into_tuple();
                let (ax, ay) = a.into_tuple();
                let (bx, by) = b.into_tuple();
                let d1 = (bx - ax) * (cy - ay);
                let d2 = (by - ay) * (cx - ax);
                d1 - d2
            }
            /// The signed area of the triangle defined by points `(a, b, c)`.
            pub fn signed_triangle_area(a: Self, b: Self, c: Self) -> T where T: Float {
                let two = T::one() + T::one();
                c.determine_side(a, b)/two
            }
            /// The area of the triangle defined by points `(a, b, c)`.
            pub fn triangle_area(a: Self, b: Self, c: Self) -> T where T: Float {
                Self::signed_triangle_area(a, b, c).abs()
            }
            /// Gets a 2D-rotated copy of this vector.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::Vec2;
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
            /// assert_relative_eq!(Vec2::unit_x().rotated_z(0_f32), Vec2::unit_x());
            /// assert_relative_eq!(Vec2::unit_x().rotated_z(PI/2.), Vec2::unit_y());
            /// assert_relative_eq!(Vec2::unit_x().rotated_z(PI), -Vec2::unit_x());
            /// assert_relative_eq!(Vec2::unit_x().rotated_z(PI*1.5), -Vec2::unit_y());
            /// assert_relative_eq!(Vec2::unit_x().rotated_z(PI*2.), Vec2::unit_x(), epsilon = 0.000001);
            /// # }
            /// ```
            pub fn rotated_z(self, angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                let Self { x, y } = self;
                Self::new(c*x - s*y, s*x + c*y)
            }
            /// Get the unit vector which has `x` set to 1.
            pub fn unit_x    () -> Self where T: Zero + One { Self::new(T::one(), T::zero()) }
            /// Get the unit vector which has `y` set to 1.
            pub fn unit_y    () -> Self where T: Zero + One { Self::new(T::zero(), T::one()) }
            /// Get the unit vector which has `x` set to -1.
            pub fn left      () -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_x() }
            /// Get the unit vector which has `x` set to 1.
            pub fn right     () -> Self where T: Zero + One {  Self::unit_x() }
            /// Get the unit vector which has `y` set to 1.
            pub fn up        () -> Self where T: Zero + One {  Self::unit_y() }
            /// Get the unit vector which has `y` set to -1.
            pub fn down      () -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_y() }
        }
    };
}


#[allow(unused_macros)]
macro_rules! vec_impl_spatial_3d {
    ($($Vec:ident)+) => {
        $(
            impl<T> $Vec<T> {
                /// The cross-product of this vector with another.
                ///
                /// On two noncolinear vectors, the result is perpendicular to the plane they
                /// define. 
                ///
                /// The result's facing direction depends on the handedness of your
                /// coordinate system:
                /// If we let `f` a forward vector and `u` an up vector, then we have :
                ///
                /// - Right-handed: `f.cross(u)` points to the right.
                /// - Left-handed: `f.cross(u)` points to the left.
                ///
                /// There's a trick to remember this which involves your hand:
                /// spread your fingers such that your middle finger points upwards
                /// and your index finger points forwards, then your thumb points
                /// in the direction of `f.cross(u)`.
                ///
                /// ```
                /// # extern crate vek;
                /// # #[macro_use] extern crate approx;
                /// # use vek::Vec3;
                /// # fn main() {
                /// let i = Vec3::<f32>::unit_x();
                /// let j = Vec3::<f32>::unit_y();
                /// let k = Vec3::<f32>::unit_z();
                /// assert_relative_eq!(i.cross(j), k);
                /// # }
                /// ```
                pub fn cross(self, b: Self) 
                    -> Self where T: Copy + Mul<Output=T> + Sub<Output=T>
                {
                    let a = self;
                    Self::new(
                        a.y*b.z - a.z*b.y,
                        a.z*b.x - a.x*b.z,
                        a.x*b.y - a.y*b.x
                    )
                }
                /// Performs spherical linear interpolation between this vector and another,
                /// without implicitly constraining `factor` to be between 0 and 1.
                ///
                /// The vectors are not required to be normalized; their length
                /// is also linearly interpolated in the process.
                ///
                /// ```
                /// # extern crate vek;
                /// # #[macro_use] extern crate approx;
                /// # use vek::Vec3;
                /// # fn main() {
                /// let u = Vec3::<f32>::unit_x();
                /// let v = Vec3::<f32>::unit_y() * 2.;
                /// let slerp = Vec3::slerp(u, v, 0.5);
                /// assert_relative_eq!(slerp.magnitude(), 1.5);
                /// assert_relative_eq!(slerp.x, slerp.y);
                /// # }
                /// ```
                pub fn slerp_unclamped(from: Self, to: Self, factor: T) -> Self
                    where T: Sum + Float + Clamp + Lerp<T>
                {
                    // From GLM, gtx/rotate_vector.inl
                    let (mag_from, mag_to) = (from.magnitude(), to.magnitude());
                    let (from, to) = (from/mag_from, to/mag_to);
                    let cos_alpha = from.dot(to);
                    let alpha = cos_alpha.acos();
                    let sin_alpha = alpha.sin();
                    let t1 = ((T::one() - factor) * alpha).sin() / sin_alpha;
                    let t2 = (factor * alpha).sin() / sin_alpha;
                    (from * t1 + to * t2) * Lerp::lerp_unclamped(mag_from, mag_to, factor)
                }
                /// Performs spherical linear interpolation between this vector and another,
                /// implicitly constraining `factor` to be between 0 and 1.
                ///
                /// The vectors are not required to be normalized; their length
                /// is also interpolated in the process.
                pub fn slerp(from: Self, to: Self, factor: T) -> Self
                    where T: Sum + Float + Clamp + Lerp<T>
                {
                    Self::slerp_unclamped(from, to, factor.clamped01())
                }

                /// Get the unit vector which has `x` set to 1.
                pub fn unit_x    () -> Self where T: Zero + One { Self::new(T::one(), T::zero(), T::zero()) }
                /// Get the unit vector which has `y` set to 1.
                pub fn unit_y    () -> Self where T: Zero + One { Self::new(T::zero(), T::one(), T::zero()) }
                /// Get the unit vector which has `z` set to 1.
                pub fn unit_z    () -> Self where T: Zero + One { Self::new(T::zero(), T::zero(), T::one()) }
                /// Get the unit vector which has `x` set to -1.
                pub fn left      () -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_x() }
                /// Get the unit vector which has `x` set to 1.
                pub fn right     () -> Self where T: Zero + One {  Self::unit_x() }
                /// Get the unit vector which has `y` set to 1.
                pub fn up        () -> Self where T: Zero + One {  Self::unit_y() }
                /// Get the unit vector which has `y` set to -1.
                pub fn down      () -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_y() }
                /// Get the unit vector which has `z` set to 1 ("forward" in a left-handed coordinate system).
                pub fn forward_lh() -> Self where T: Zero + One {  Self::unit_z() }
                /// Get the unit vector which has `z` set to -1 ("forward" in a right-handed coordinate system).
                pub fn forward_rh() -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_z() }
                /// Get the unit vector which has `z` set to -1 ("back" in a left-handed coordinate system).
                pub fn back_lh   () -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_z() }
                /// Get the unit vector which has `z` set to 1 ("back" in a right-handed coordinate system).
                pub fn back_rh   () -> Self where T: Zero + One {  Self::unit_z() }
            }
        )+
    }
}

macro_rules! vec_impl_spatial_4d {
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
                #[cfg(feature="vec3")]
                pub fn from_point<V: Into<Vec3<T>>>(v: V) -> Self where T: One {
                    let Vec3 { x, y, z } = v.into();
                    Self::new_point(x, y, z)
                }
                /// Turns a vector into a direction vector in homogeneous coordinates (sets the last coordinate to 0).
                #[cfg(feature="vec3")]
                pub fn from_direction<V: Into<Vec3<T>>>(v: V) -> Self where T: Zero {
                    let Vec3 { x, y, z } = v.into();
                    Self::new_direction(x, y, z)
                }
                /// Get the unit direction vector which has `x` set to 1.
                pub fn unit_x    () -> Self where T: Zero + One { Self::new(T::one(), T::zero(), T::zero(), T::zero()) }
                /// Get the unit direction vector which has `y` set to 1.
                pub fn unit_y    () -> Self where T: Zero + One { Self::new(T::zero(), T::one(), T::zero(), T::zero()) }
                /// Get the unit direction vector which has `z` set to 1.
                pub fn unit_z    () -> Self where T: Zero + One { Self::new(T::zero(), T::zero(), T::one(), T::zero()) }
                /// Get the vector which has `w` set to 1 and all other elements to zero.
                pub fn unit_w    () -> Self where T: Zero + One { Self::new(T::zero(), T::zero(), T::zero(), T::one()) }
                /// Get the unit direction vector which has `x` set to -1.
                pub fn left      () -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_x() }
                /// Get the unit direction vector which has `x` set to 1.
                pub fn right     () -> Self where T: Zero + One {  Self::unit_x() }
                /// Get the unit direction vector which has `y` set to 1.
                pub fn up        () -> Self where T: Zero + One {  Self::unit_y() }
                /// Get the unit direction vector which has `y` set to -1.
                pub fn down      () -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_y() }
                /// Get the unit direction vector which has `z` set to 1 ("forward" in a left-handed coordinate system).
                pub fn forward_lh() -> Self where T: Zero + One {  Self::unit_z() }
                /// Get the unit direction vector which has `z` set to -1 ("forward" in a right-handed coordinate system).
                pub fn forward_rh() -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_z() }
                /// Get the unit direction vector which has `z` set to -1 ("back" in a left-handed coordinate system).
                pub fn back_lh   () -> Self where T: Zero + One + Neg<Output=T> { -Self::unit_z() }
                /// Get the unit direction vector which has `z` set to 1 ("back" in a right-handed coordinate system).
                pub fn back_rh   () -> Self where T: Zero + One {  Self::unit_z() }
            }
        )+
    }
}

#[cfg(feature="image")]
macro_rules! vec_impl_pixel_rgb {
    ($Vec:ident) => {
        extern crate image;

        use self::image::{Primitive, Pixel, ColorType, Luma, LumaA};

        impl<T> Pixel for $Vec<T> 
            where T: ColorComponent + Copy + Clone + Primitive
        {
            type Subpixel = T;
            fn channel_count() -> u8 {
                3
            }
            fn channels(&self) -> &[Self::Subpixel] {
                self.as_slice()
            }
            fn channels_mut(&mut self) -> &mut [Self::Subpixel] {
                self.as_mut_slice()
            }
            fn color_model() -> &'static str {
                "RGB"
            }
            fn color_type() -> ColorType {
                ColorType::RGB(Self::channel_count() * mem::size_of::<T>() as u8 * 8_u8)
            }
            fn channels4(&self) -> (Self::Subpixel, Self::Subpixel, Self::Subpixel, Self::Subpixel) {
                (self.r, self.g, self.b, T::full())
            }
            fn from_channels(a: Self::Subpixel, b: Self::Subpixel, c: Self::Subpixel, _d: Self::Subpixel) -> Self {
                Self::new(a, b, c)
            }
            fn from_slice(slice: &[Self::Subpixel]) -> &Self {
                assert!(slice.len() >= Self::channel_count() as _);
                unsafe { &*(slice.as_ptr() as *const _ as *const Self) }
            }
            fn from_slice_mut(slice: &mut [Self::Subpixel]) -> &mut Self {
                assert!(slice.len() >= Self::channel_count() as _);
                unsafe { &mut *(slice.as_mut_ptr() as *mut _ as *mut Self) }
            }
            fn to_rgb(&self) -> image::Rgb<Self::Subpixel> {
                image::Rgb { data: [self.r, self.g, self.b] }
            }
            fn to_rgba(&self) -> image::Rgba<Self::Subpixel> {
                image::Rgba { data: [self.r, self.g, self.b, T::full()] }
            }
            fn to_luma(&self) -> Luma<Self::Subpixel> {
                let three = T::one() + T::one() + T::one();
                let c = (self.r + self.g + self.b) / three;
                Luma { data: [c] }
            }
            fn to_luma_alpha(&self) -> LumaA<Self::Subpixel> {
                LumaA { data: [self.to_luma().data[0], T::full()] }
            }
            fn map<F>(&self, mut f: F) -> Self where F: FnMut(Self::Subpixel) -> Self::Subpixel {
                Self { r: f(self.r), g: f(self.g), b: f(self.b) }
            }
            fn apply<F>(&mut self, f: F) where F: FnMut(Self::Subpixel) -> Self::Subpixel {
                *self = self.map(f);
            }
            fn map_with_alpha<F, G>(&self, mut f: F, mut _g: G) -> Self
                where F: FnMut(Self::Subpixel) -> Self::Subpixel, G: FnMut(Self::Subpixel) -> Self::Subpixel
            {
                Self { r: f(self.r), g: f(self.g), b: f(self.b) }
            }
            fn apply_with_alpha<F, G>(&mut self, f: F, g: G)
                where F: FnMut(Self::Subpixel) -> Self::Subpixel, G: FnMut(Self::Subpixel) -> Self::Subpixel
            {
                *self = self.map_with_alpha(f, g);
            }

            fn map2<F>(&self, other: &Self, mut f: F) -> Self
                where F: FnMut(Self::Subpixel, Self::Subpixel) -> Self::Subpixel
            {
                Self {
                    r: f(self.r, other.r),
                    g: f(self.g, other.g),
                    b: f(self.b, other.b)
                }
            }
            fn apply2<F>(&mut self, other: &Self, f: F)
                where F: FnMut(Self::Subpixel, Self::Subpixel) -> Self::Subpixel
            {
                *self = self.map2(other, f);
            }

            fn invert(&mut self) {
                *self = self.inverted();
            }
            fn blend(&mut self, other: &Self) {
                self.apply2(other, |a, b| {
                    let a = <f64 as NumCast>::from(a).unwrap();
                    let b = <f64 as NumCast>::from(b).unwrap();
                    let m = (a+b)/2f64;
                    <T as NumCast>::from(m.round()).unwrap()
                });
            }
        }
    }
}


#[cfg(feature="image")]
macro_rules! vec_impl_pixel_rgba {
    ($Vec:ident) => {
        extern crate image;

        use self::image::{Primitive, Pixel, ColorType, Luma, LumaA};

        impl<T> Pixel for $Vec<T> 
            where T: ColorComponent + Copy + Clone + Primitive
        {
            type Subpixel = T;
            fn channel_count() -> u8 {
                4
            }
            fn channels(&self) -> &[Self::Subpixel] {
                self.as_slice()
            }
            fn channels_mut(&mut self) -> &mut [Self::Subpixel] {
                self.as_mut_slice()
            }
            fn color_model() -> &'static str {
                "RGBA"
            }
            fn color_type() -> ColorType {
                ColorType::RGBA(Self::channel_count() * mem::size_of::<T>() as u8 * 8_u8)
            }
            fn channels4(&self) -> (Self::Subpixel, Self::Subpixel, Self::Subpixel, Self::Subpixel) {
                (self.r, self.g, self.b, self.a)
            }
            fn from_channels(a: Self::Subpixel, b: Self::Subpixel, c: Self::Subpixel, d: Self::Subpixel) -> Self {
                Self::new(a, b, c, d)
            }
            fn from_slice(slice: &[Self::Subpixel]) -> &Self {
                assert!(slice.len() >= Self::channel_count() as _);
                unsafe { &*(slice.as_ptr() as *const _ as *const Self) }
            }
            fn from_slice_mut(slice: &mut [Self::Subpixel]) -> &mut Self {
                assert!(slice.len() >= Self::channel_count() as _);
                unsafe { &mut *(slice.as_mut_ptr() as *mut _ as *mut Self) }
            }
            fn to_rgb(&self) -> image::Rgb<Self::Subpixel> {
                image::Rgb { data: [self.r, self.g, self.b] }
            }
            fn to_rgba(&self) -> image::Rgba<Self::Subpixel> {
                image::Rgba { data: [self.r, self.g, self.b, self.a] }
            }
            fn to_luma(&self) -> Luma<Self::Subpixel> {
                let three = T::one() + T::one() + T::one();
                let c = (self.r + self.g + self.b) / three;
                Luma { data: [c] }
            }
            fn to_luma_alpha(&self) -> LumaA<Self::Subpixel> {
                LumaA { data: [self.to_luma().data[0], self.a] }
            }
            fn map<F>(&self, mut f: F) -> Self where F: FnMut(Self::Subpixel) -> Self::Subpixel {
                Self { r: f(self.r), g: f(self.g), b: f(self.b), a: f(self.a) }
            }
            fn apply<F>(&mut self, f: F) where F: FnMut(Self::Subpixel) -> Self::Subpixel {
                *self = self.map(f);
            }
            fn map_with_alpha<F, G>(&self, mut f: F, mut g: G) -> Self
                where F: FnMut(Self::Subpixel) -> Self::Subpixel, G: FnMut(Self::Subpixel) -> Self::Subpixel
            {
                Self { r: f(self.r), g: f(self.g), b: f(self.b), a: g(self.a) }
            }
            fn apply_with_alpha<F, G>(&mut self, f: F, g: G)
                where F: FnMut(Self::Subpixel) -> Self::Subpixel, G: FnMut(Self::Subpixel) -> Self::Subpixel
            {
                *self = self.map_with_alpha(f, g);
            }

            fn map2<F>(&self, other: &Self, mut f: F) -> Self
                where F: FnMut(Self::Subpixel, Self::Subpixel) -> Self::Subpixel
            {
                Self {
                    r: f(self.r, other.r),
                    g: f(self.g, other.g),
                    b: f(self.b, other.b),
                    a: f(self.a, other.a)
                }
            }
            fn apply2<F>(&mut self, other: &Self, f: F)
                where F: FnMut(Self::Subpixel, Self::Subpixel) -> Self::Subpixel
            {
                *self = self.map2(other, f);
            }

            fn invert(&mut self) {
                *self = self.inverted();
            }
            fn blend(&mut self, other: &Self) {
                self.apply2(other, |a, b| {
                    let a = <f64 as NumCast>::from(a).unwrap();
                    let b = <f64 as NumCast>::from(b).unwrap();
                    let m = (a+b)/2f64;
                    <T as NumCast>::from(m.round()).unwrap()
                });
            }
        }
    }
}

#[cfg(feature="rgba")]
macro_rules! vec_impl_color_rgba {
    ($Vec:ident) => {

        #[cfg(feature="image")]
        vec_impl_pixel_rgba!{$Vec}

        impl<T: ColorComponent> Rgba<T> {
            pub fn new_opaque(r: T, g: T, b: T) -> Self {
                Self::new(r, g, b, T::full())
            }
            pub fn new_transparent(r: T, g: T, b: T) -> Self {
                Self::new(r, g, b, T::zero())
            }
            #[cfg(feature="rgb")]
            pub fn from_opaque<V: Into<Rgb<T>>>(color: V) -> Self {
                let Rgb { r, g, b } = color.into();
                Self::new_opaque(r, g, b)
            }
            #[cfg(feature="rgb")]
            pub fn from_transparent<V: Into<Rgb<T>>>(color: V) -> Self {
                let Rgb { r, g, b } = color.into();
                Self::new_transparent(r, g, b)
            }
        }
        impl<T> Rgba<T> {
            #[cfg(feature="rgb")]
            pub fn from_translucent<V: Into<Rgb<T>>>(color: V, opacity: T) -> Self {
                let Rgb { r, g, b } = color.into();
                Self::new(r, g, b, opacity)
            }
        }
        impl<T: ColorComponent> $Vec<T> {
            pub fn black   () -> Self { Self::new_opaque(T::zero(), T::zero(), T::zero()) }
            pub fn white   () -> Self { Self::new_opaque(T::full(), T::full(), T::full()) }
            pub fn red     () -> Self { Self::new_opaque(T::full(), T::zero(), T::zero()) }
            pub fn green   () -> Self { Self::new_opaque(T::zero(), T::full(), T::zero()) }
            pub fn blue    () -> Self { Self::new_opaque(T::zero(), T::zero(), T::full()) }
            pub fn cyan    () -> Self { Self::new_opaque(T::zero(), T::full(), T::full()) }
            pub fn magenta () -> Self { Self::new_opaque(T::full(), T::zero(), T::full()) }
            pub fn yellow  () -> Self { Self::new_opaque(T::full(), T::full(), T::zero()) }
            pub fn gray(value: T) -> Self where T: Copy { Self::from(Rgba::new_opaque(value, value, value)) }
            pub fn grey(value: T) -> Self where T: Copy { Self::from(Rgba::new_opaque(value, value, value)) }
            pub fn gray_level_via_numcast_f64(self) -> Option<T> where T: NumCast { self.grey_level_via_numcast_f64() }
            pub fn grey_level_via_numcast_f64(self) -> Option<T> where T: NumCast {
                let $Vec::<f64> { r, g, b, .. } = self.numcast()?;
                let avg = (r + g + b) / 3.;
                T::from(avg)
            }

            /// Returns this color with RGB elements inverted. Alpha is preserved.
            ///
            /// ```
            /// # use vek::Rgba;
            /// let opaque_orange = Rgba::new(255_u8, 128, 0, 255_u8);
            /// assert_eq!(opaque_orange.inverted(), Rgba::new(0, 127, 255, 255));
            /// assert_eq!(Rgba::black().inverted(), Rgba::white());
            /// assert_eq!(Rgba::white().inverted(), Rgba::black());
            /// assert_eq!(Rgba::red().inverted(), Rgba::cyan());
            /// ```
            pub fn inverted(mut self) -> Self where T: Sub<Output=T> {
                self.r = T::full() - self.r;
                self.g = T::full() - self.g;
                self.b = T::full() - self.b;
                self
            }
        }
    };
}

#[cfg(feature="rgb")]
macro_rules! vec_impl_color_rgb {
    ($Vec:ident) => {

        #[cfg(feature="image")]
        vec_impl_pixel_rgb!{$Vec}

        impl<T: ColorComponent> $Vec<T> {
            pub fn black   () -> Self { Self::new(T::zero(), T::zero(), T::zero()) }
            pub fn white   () -> Self { Self::new(T::full(), T::full(), T::full()) }
            pub fn red     () -> Self { Self::new(T::full(), T::zero(), T::zero()) }
            pub fn green   () -> Self { Self::new(T::zero(), T::full(), T::zero()) }
            pub fn blue    () -> Self { Self::new(T::zero(), T::zero(), T::full()) }
            pub fn cyan    () -> Self { Self::new(T::zero(), T::full(), T::full()) }
            pub fn magenta () -> Self { Self::new(T::full(), T::zero(), T::full()) }
            pub fn yellow  () -> Self { Self::new(T::full(), T::full(), T::zero()) }
            pub fn gray(value: T) -> Self where T: Copy { Self::new(value, value, value) }
            pub fn grey(value: T) -> Self where T: Copy { Self::new(value, value, value) }
            pub fn gray_level_via_numcast_f64(self) -> Option<T> where T: NumCast { self.grey_level_via_numcast_f64() }
            pub fn grey_level_via_numcast_f64(self) -> Option<T> where T: NumCast {
                let $Vec::<f64> { r, g, b, .. } = self.numcast()?;
                let avg = (r + g + b) / 3.;
                T::from(avg)
            }
            /// Returns this color with RGB elements inverted.
            ///
            /// ```
            /// # use vek::Rgb;
            /// let orange = Rgb::new(255_u8, 128, 0);
            /// assert_eq!(orange.inverted(), Rgb::new(0, 127, 255));
            /// assert_eq!(Rgb::black().inverted(), Rgb::white());
            /// assert_eq!(Rgb::white().inverted(), Rgb::black());
            /// assert_eq!(Rgb::red().inverted(), Rgb::cyan());
            /// ```
            pub fn inverted(mut self) -> Self where T: Sub<Output=T> {
                self.r = T::full() - self.r;
                self.g = T::full() - self.g;
                self.b = T::full() - self.b;
                self
            }
        }
    }
}

/// Calls `vec_impl_vec!{}` on each appropriate vector type.
macro_rules! vec_impl_all_vecs {
    ($c_or_simd:ident $(#[$repr_attrs:meta])+) => {

        #[cfg(feature="vec2")]
        /// Vector type suited for 2D spatial coordinates.
        pub mod vec2 {
            use super::*;
            /// Vector type suited for 2D spatial coordinates.
            #[allow(missing_docs)]
            #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            $(#[$repr_attrs])+
            pub struct Vec2<T> { pub x:T, pub y:T }
            vec_impl_vec!($c_or_simd struct Vec2   vec2      (2) ("({}, {})") (x y) (x y) (0 1) (T,T));
            vec_impl_spatial!(Vec2);
            vec_impl_spatial_2d!(Vec2);

            #[cfg(feature="vec3")]
            impl<T> From<Vec3<T>> for Vec2<T> {
                fn from(v: Vec3<T>) -> Self {
                    Self::new(v.x, v.y)
                }
            }
    	    // #[cfg(feature="vec4")] // Commented out, see rationale in Cargo.toml
            impl<T> From<Vec4<T>> for Vec2<T> {
                fn from(v: Vec4<T>) -> Self {
                    Self::new(v.x, v.y)
                }
            }
            #[cfg(feature="extent2")]
            impl<T> From<Extent2<T>> for Vec2<T> {
                fn from(v: Extent2<T>) -> Self {
                    Self::new(v.w, v.h)
                }
            }
        }
        #[cfg(feature="vec2")]
        pub use self::vec2::Vec2;

        #[cfg(feature="vec3")]
        /// Vector type suited for 3D spatial coordinates.
        pub mod vec3 {
            use super::*;
            /// Vector type suited for 3D spatial coordinates.
            #[allow(missing_docs)]
            #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            $(#[$repr_attrs])+
            pub struct Vec3<T> { pub x:T, pub y:T, pub z:T }
            vec_impl_vec!($c_or_simd struct Vec3     vec3     (3) ("({}, {}, {})") (x y z) (x y z) (0 1 2) (T,T,T));
            vec_impl_spatial!(Vec3);
            vec_impl_spatial_3d!(Vec3);

            #[cfg(feature="vec2")]
            impl<T: Zero> From<Vec2<T>> for Vec3<T> {
                fn from(v: Vec2<T>) -> Self {
                    Self::new(v.x, v.y, T::zero())
                }
            }
    	    // #[cfg(feature="vec4")] // Commented out, see rationale in Cargo.toml
            impl<T> From<Vec4<T>> for Vec3<T> {
                fn from(v: Vec4<T>) -> Self {
                    Self::new(v.x, v.y, v.z)
                }
            }
            #[cfg(feature="extent3")]
            impl<T> From<Extent3<T>> for Vec3<T> {
                fn from(v: Extent3<T>) -> Self {
                    Self::new(v.w, v.h, v.d)
                }
            }
            #[cfg(feature="rgb")]
            impl<T> From<Rgb<T>> for Vec3<T> {
                fn from(v: Rgb<T>) -> Self {
                    Self::new(v.r, v.g, v.b)
                }
            }
            #[cfg(feature="uvw")]
            impl<T> From<Uvw<T>> for Vec3<T> {
                fn from(v: Uvw<T>) -> Self {
                    Self::new(v.u, v.v, v.w)
                }
            }
        }
        #[cfg(feature="vec3")]
        pub use self::vec3::Vec3;

    	// #[cfg(feature="vec4")] // Commented out, see rationale in Cargo.toml
        /// Vector type suited for homogeneous 3D spatial coordinates.
        pub mod vec4 {
            use super::*;
            /// Vector type suited for homogeneous 3D spatial coordinates.
            #[allow(missing_docs)]
            #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq/*, Ord, PartialOrd*/)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            $(#[$repr_attrs])+
            pub struct Vec4<T> {
                pub x:T, pub y:T, pub z:T,
                /// In homogeneous 3D-space coordinates, `w` is often set to 
                /// `1` for points, and `0` for directions.  
                ///
                /// One reason behind this: with floating-point numbers,
                /// division by zero gives infinity (a direction is then
                /// a point stretching infinitely towards another).
                pub w: T
            }
            vec_impl_vec!($c_or_simd struct Vec4   vec4    (4) ("({}, {}, {}, {})") (x y z w) (x y z w) (0 1 2 3) (T,T,T,T));
            vec_impl_spatial!(Vec4);
            vec_impl_spatial_4d!(Vec4);

            #[cfg(feature="vec3")]
            impl<T: Zero> From<Vec3<T>> for Vec4<T> {
                fn from(v: Vec3<T>) -> Self {
                    Self::new(v.x, v.y, v.z, T::zero())
                }
            }
            #[cfg(feature="vec2")]
            impl<T: Zero> From<Vec2<T>> for Vec4<T> {
                fn from(v: Vec2<T>) -> Self {
                    Self::new(v.x, v.y, T::zero(), T::zero())
                }
            }
            #[cfg(feature="rgba")]
            impl<T> From<Rgba<T>> for Vec4<T> {
                fn from(v: Rgba<T>) -> Self {
                    Self::new(v.r, v.g, v.b, v.a)
                }
            }

        }
    	// #[cfg(feature="vec4")] // Commented out, see rationale in Cargo.toml
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
            $(#[$repr_attrs])+
            pub struct Vec8<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);
            vec_impl_vec!($c_or_simd tuple Vec8     vec8   (8) ("({}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7) (m0 m1 m2 m3 m4 m5 m6 m7) (0 1 2 3 4 5 6 7) (T,T,T,T,T,T,T,T));
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
            $(#[$repr_attrs])+
            pub struct Vec16<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);
            vec_impl_vec!($c_or_simd tuple Vec16   vec16   (16) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
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
            $(#[$repr_attrs])+
            pub struct Vec32<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);
            vec_impl_vec!($c_or_simd tuple Vec32   vec32   (32) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
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
            $(#[$repr_attrs])+
            pub struct Vec64<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);
            vec_impl_vec!($c_or_simd tuple Vec64   vec64   (64) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31 m32 m33 m34 m35 m36 m37 m38 m39 m40 m41 m42 m43 m44 m45 m46 m47 m48 m49 m50 m51 m52 m53 m54 m55 m56 m57 m58 m59 m60 m61 m62 m63) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
            vec_impl_spatial!(Vec64);
        }
        #[cfg(feature="vec64")]
        pub use self::vec64::Vec64;

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
            $(#[$repr_attrs])+
            pub struct Extent3<T> { pub w:T, pub h:T, pub d:T }
            vec_impl_vec!($c_or_simd struct Extent3 extent3 (3) ("({}, {}, {})") (w h d) (w h d) (0 1 2) (T,T,T));
            vec_impl_spatial!(Extent3);

            #[cfg(feature="vec3")]
            impl<T> From<Vec3<T>> for Extent3<T> {
                fn from(v: Vec3<T>) -> Self {
                    Self::new(v.x, v.y, v.z)
                }
            }
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
            $(#[$repr_attrs])+
            pub struct Extent2<T> { pub w:T, pub h:T }
            vec_impl_vec!($c_or_simd struct Extent2 extent2 (2) ("({}, {})") (w h) (w h) (0 1) (T,T));
            vec_impl_spatial!(Extent2);

            #[cfg(feature="vec2")]
            impl<T> From<Vec2<T>> for Extent2<T> {
                fn from(v: Vec2<T>) -> Self {
                    Self::new(v.x, v.y)
                }
            }
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
            $(#[$repr_attrs])+
            pub struct Rgba<T> { pub r:T, pub g:T, pub b:T, pub a:T }
            vec_impl_vec!($c_or_simd struct Rgba   rgba    (4) ("rgba({}, {}, {}, {})") (r g b a) (r g b a) (0 1 2 3) (T,T,T,T));
            vec_impl_color_rgba!{Rgba}

            #[cfg(feature="vec4")]
            impl<T> From<Vec4<T>> for Rgba<T> {
                fn from(v: Vec4<T>) -> Self {
                    Self::new(v.x, v.y, v.z, v.w)
                }
            }
            #[cfg(feature="rgb")]
            impl<T: ColorComponent> From<Rgb<T>> for Rgba<T> {
                fn from(v: Rgb<T>) -> Self {
                    Self::from_opaque(v)
                }
            }
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
            $(#[$repr_attrs])+
            pub struct Rgb<T> { pub r:T, pub g:T, pub b:T }
            vec_impl_vec!($c_or_simd struct Rgb     rgb     (3) ("rgb({}, {}, {})") (r g b) (r g b) (0 1 2) (T,T,T));
            vec_impl_color_rgb!{Rgb}

            #[cfg(feature="vec3")]
            impl<T> From<Vec3<T>> for Rgb<T> {
                fn from(v: Vec3<T>) -> Self {
                    Self::new(v.x, v.y, v.z)
                }
            }
            #[cfg(feature="rgba")]
            impl<T> From<Rgba<T>> for Rgb<T> {
                fn from(v: Rgba<T>) -> Self {
                    Self::new(v.r, v.g, v.b)
                }
            }
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
            $(#[$repr_attrs])+
            pub struct Uvw<T> { pub u:T, pub v:T, pub w:T }
            vec_impl_vec!($c_or_simd struct Uvw     uvw     (3) ("({}, {}, {})") (u v w) (u v w) (0 1 2) (T,T,T));

            #[cfg(feature="vec3")]
            impl<T> From<Vec3<T>> for Uvw<T> {
                fn from(v: Vec3<T>) -> Self {
                    Self::new(v.x, v.y, v.z)
                }
            }
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
            $(#[$repr_attrs])+
            pub struct Uv<T> { pub u:T, pub v:T }
            vec_impl_vec!($c_or_simd struct Uv   uv      (2) ("({}, {})") (u v) (u v) (0 1) (T,T));

            #[cfg(feature="vec2")]
            impl<T> From<Vec2<T>> for Uv<T> {
                fn from(v: Vec2<T>) -> Self {
                    Self::new(v.x, v.y)
                }
            }
        }
        #[cfg(feature="uv")]
        pub use self::uv::Uv;

    }
}

pub mod repr_c {
    //! Vector types which are marked `#[repr(C)]`.
    //!
    //! See also the `repr_simd` neighbour module, which is available on Nightly
    //! with the `repr_simd` feature enabled.

   
    use super::*;
    vec_impl_all_vecs!{
        c
        #[repr(C)]
        #[cfg_attr(all(nightly, feature="repr_align", any(target_arch="x86", target_arch="x86_64")), repr(align(16)))]
        #[cfg_attr(all(nightly, feature="repr_align", target_arch="arm"), repr(align(64)))]
        // XXX ^^^^ Not sure about the alignment on ARM ??
        // TODO assert the packing of vecs
    }
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Vector types which are marked `#[repr(simd)]`.
    
    use super::*;
    vec_impl_all_vecs!{
        simd
        #[repr(simd)]
    }
}

pub use self::repr_c::*;

#[cfg(test)]
mod tests {
    macro_rules! for_each_type {
        ($vec:ident $Vec:ident $($T:ident)+) => {
            mod $vec {
                mod repr_c {
                    $(mod $T {
                        use $crate::vec::repr_c::$Vec;
                        #[test]
                        fn is_packed() {
                            assert!($Vec::<$T>::default().is_packed());
                        }
                    })+
                }
                #[cfg(feature="repr_simd")]
                mod repr_simd {
                    $(mod $T {
                        use $crate::vec::repr_simd::$Vec;
                        #[test]
                        fn is_packed() {
                            assert!($Vec::<$T>::default().is_packed());
                        }
                    })+
                }
            }
        };
    }
    // Vertical editing helps here :)
    #[cfg(feature="vec2")]    for_each_type!{vec2    Vec2    i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="vec3")]    for_each_type!{vec3    Vec3    i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    /*#[cfg(feature="vec4")]*/for_each_type!{vec4    Vec4    i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="vec8")]    for_each_type!{vec8    Vec8    i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="vec16")]   for_each_type!{vec16   Vec16   i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="vec32")]   for_each_type!{vec32   Vec32   i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="vec64")]   for_each_type!{vec64   Vec64   i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="rgba")]    for_each_type!{rgba    Rgba    i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="rgb")]     for_each_type!{rgb     Rgb     i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="extent3")] for_each_type!{extent3 Extent3 i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="extent2")] for_each_type!{extent2 Extent2 i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="uv")]      for_each_type!{uv      Uv      i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="uvw")]     for_each_type!{uvw     Uvw     i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
}

//! Vector types.

use core::borrow::{Borrow, BorrowMut};
use core::fmt::{self, Display, Formatter};
use core::iter::FromIterator;
use core::mem;
use core::ops::*;
use core::slice::{self, /*SliceIndex*/}; // NOTE: Will want to use SliceIndex once it's stabilized
use num_traits::{Zero, One, NumCast, Signed};

macro_rules! vec_declare_types {
    ($(#[$attrs:meta])+) => {
        /// A two-components generic vector type.
        ///
        /// - If you intend to use it as spatial coordinates, consider using [Xy](struct.Xy.html) instead.
        /// - If you intend to use it as a spatial extent, consider using [Extent2](struct.Extent2.html) instead.
        /// - If you intend to use it as texture coordinates, consider using [Uv](struct.Uv.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Vec2<T>(pub T, pub T);
        /// A three-components generic vector type.
        ///
        /// - If you intend to use it as spatial coordinates, consider using [Xyz](struct.Xyz.html) instead.
        /// - If you intend to use it as a spatial extent, consider using [Extent3](struct.Extent3.html) instead.
        /// - If you intend to use it as RGB color data, consider using [Rgb](struct.Rgb.html) instead.
        /// - If you intend to use it as texture coordinates, consider using [Uvw](struct.Uvw.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Vec3<T>(pub T, pub T, pub T);
        /// A four-components generic vector type.
        ///
        /// - If you intend to use it as homogeneous spatial coordinates, consider using [Xyzw](struct.Xyzw.html) instead.
        /// - If you intend to use it as RGBA color data, consider using [Rgba](struct.Rgba.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Vec4<T>(pub T, pub T, pub T, pub T);

        /// An eight-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX2-enabled x86 CPUs, a `Vec8<i32>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        ///
        /// There's a lot of related intrinsics that are not provided as associated functions.
        /// If you find yourself needing them, use other crates such as `llvmint` or `x86intrin`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Vec8<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A sixteen-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX2-enabled x86 CPUs, a `Vec16<i16>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        ///
        /// There's a lot of related intrinsics that are not provided as associated functions.
        /// If you find yourself needing them, use other crates such as `llvmint` or `x86intrin`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Vec16<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A thirty-two-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX512-enabled x86 CPUs, a `Vec32<i16>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        ///
        /// There's a lot of related intrinsics that are not provided as associated functions.
        /// If you find yourself needing them, use other crates such as `llvmint` or `x86intrin`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Vec32<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A sixty-four-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX512-enabled x86 CPUs, a `Vec64<i8>` makes sense.  
        /// Otherwise, LLVM is able to process it as a fixed-sized array of whichever "best" SIMD vector type available.  
        ///
        /// There's a lot of related intrinsics that are not provided as associated functions.
        /// If you find yourself needing them, use other crates such as `llvmint` or `x86intrin`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Vec64<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);


        /// Vector type suited for homogeneous 3D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Xyzw<T> { pub x:T, pub y:T, pub z:T, pub w:T }
        /// Vector type suited for 3D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Xyz<T> { pub x:T, pub y:T, pub z:T }
        /// Vector type suited for 2D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Xy<T> { pub x:T, pub y:T }

        /// Vector type suited for 3D extents (width, height and depth).
        ///
        /// There is no `Unsigned` trait bound because it is not practical, 
        /// since we sometimes want to be
        /// able to express extents as floating-point numbers, for instance.
        ///
        /// If you want to assert unsignedness at runtime, you can use the
        /// `is_all_positive()` or `is_any_negative()` methods.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Extent3<T> { pub w:T, pub h:T, pub d:T }
        /// Vector type suited for 2D extents (width and height).
        ///
        /// There is no `Unsigned` trait bound because it is not practical, 
        /// since we sometimes want to be
        /// able to express extents as floating-point numbers, for instance.
        ///
        /// If you want to assert unsignedness at runtime, you can use the
        /// `is_all_positive()` or `is_any_negative()` methods.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Extent2<T> { pub w:T, pub h:T }


        /// Vector type suited for RGBA color data.
        ///
        /// There is no trait bound on `ColorComponent`, but if `T` doesn't implement it, you'll
        /// miss some goodies.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Rgba<T> { pub r:T, pub g:T, pub b:T, pub a:T }
        /// Vector type suited for RGB color data.
        ///
        /// There is no trait bound on `ColorComponent`, but if `T` doesn't implement it, you'll
        /// miss some goodies.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Rgb<T> { pub r:T, pub g:T, pub b:T }

        /// Vector type suited for 3D texture coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Uvw<T> { pub u:T, pub v:T, pub w:T }
        /// Vector type suited for 2D texture coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        pub struct Uv<T> { pub u:T, pub v:T }
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

    (tuple $Vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

        impl<T> $Vec<T> {
            /// Creates a vector from each component.
            #[cfg_attr(feature = "clippy", allow(too_many_arguments))]
            pub fn new($($namedget:T),+) -> Self {
                $Vec($($namedget),+)
            }
        }

        vec_impl_vec!{common $Vec ($dim) ($fmt) ($($get)+) ($($namedget)+) ($($tupleget)+) $Tuple}

    };

    (struct $Vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

        impl<T> $Vec<T> {
            /// Creates a vector from each component.
            #[cfg_attr(feature = "clippy", allow(too_many_arguments))]
            pub fn new($($namedget:T),+) -> Self {
                Self { $($namedget),+ }
            }
        }

        vec_impl_vec!{common $Vec ($dim) ($fmt) ($($get)+) ($($namedget)+) ($($tupleget)+) $Tuple}

    };

    (common $Vec:ident ($dim:expr) ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) ($($tupleget:tt)+) $Tuple:ty) => {

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
            /// "Broadcast" was chosen as the name because it is explicit enough and the
            /// same wording as the description in relevant Intel intrinsics.
            pub fn broadcast(val: T) -> Self where T: Clone {
                let mut out: Self = unsafe { mem::uninitialized() };
                $(out.$get = val.clone();)+
                out
            }

            /// Creates a new vector with all elements set to zero.
            pub fn zero() -> Self where T: Zero {
                let mut out: Self = unsafe { mem::uninitialized() };
                $(out.$get = Zero::zero();)+
                out
            }

            /// Creates a new vector with all elements set to one.
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

            /// The iota (ι) function, originating from APL.
            ///
            /// See [this StackOverflow answer](https://stackoverflow.com/a/9244949).
            ///
            /// Produces a vector of the first `n` integers, starting from zero,
            /// where `n` is the number of elements for this vector type.
            ///
            /// This is mostly useful for debugging purposes and tests.
            ///
            /// ```
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
            /// let v = Vec4(0_f32, 1_f32, 1.8_f32, 3.14_f32);
            /// let i = v.convert(|x| x.round() as i32);
            /// assert_eq!(i, Vec4(0, 1, 2, 3));
            /// ```
            pub fn convert<D,F>(self, f: F) -> $Vec<D> where F: Fn(T) -> D {
                let mut out: $Vec<D> = unsafe { mem::uninitialized() };
                $(out.$get = f(self.$get);)+
                out
            }
            /// Returns a memberwise-converted copy of this vector, using `NumCast`.
            ///
            /// ```
            /// let v = Vec4(0_f32, 1_f32, 2_f32, 3_f32);
            /// let i: Vec4<i32> = v.cast().unwrap();
            /// assert_eq!(i, Vec4(0, 1, 2, 3));
            /// ```
            pub fn cast<D>(self) -> Option<$Vec<D>> where T: NumCast, D: NumCast {
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

            /// Fused-multiply-add. Returns `self * mul + add`, and may be implemented
            /// efficiently by the hardware.
            ///
            /// The compiler is often able to detect this kind of operation, 
            /// so generally you don't need to use it. However, it can make
            /// your intent clear.
            ///
            /// ```
            /// let a = Vec4(0,1,2,3);
            /// let b = Vec4(4,5,6,7);
            /// let c = Vec4(8,9,0,1);
            /// assert_eq!(a*b+c, a.fmadd(b, c));
            /// ```
            pub fn fmadd(self, mul: Self, add: Self) -> Self 
                where T: Mul<T, Output=T> + Add<T, Output=T>
            {
                self * mul + add
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

            /// Returns the element which has the lowest value in this vector.
            ///
            /// ```
            /// assert_eq!(-5, Vec4(0, 5, -5, 8).reduce_min());
            /// ```
            pub fn reduce_min(&self) -> T { unimplemented!() }
            /// Returns the element which has the highest value in this vector.
            ///
            /// ```
            /// assert_eq!(8, Vec4(0, 5, -5, 8).reduce_max());
            /// ```
            pub fn reduce_max(&self) -> T { unimplemented!() }
            /// Returns the product of each of this vector's elements.
            ///
            /// ```
            /// assert_eq!(1*2*3*4, Vec4(1, 2, 3, 4).reduce_product());
            /// ```
            pub fn reduce_product(&self) -> T { unimplemented!() }
            /// Returns the sum of each of this vector's elements.
            ///
            /// ```
            /// assert_eq!(1+2+3+4, Vec4(1, 2, 3, 4).reduce_sum());
            /// ```
            pub fn reduce_sum(&self) -> T { unimplemented!() }
            /// Returns the average of this vector's elements.
            ///
            /// ```
            /// assert_eq!(2.5_f32, Vec4(1_f32, 2_f32, 3_f32, 4_f32).reduce_average());
            /// ```
            pub fn reduce_average(&self) -> T { unimplemented!() }

            /// Returns a new vector which elements are the respective square roots of this
            /// vector's elements.
            ///
            /// ```
            /// let v = Vec4(1f32, 2f32, 3f32, 4f32),
            /// let s = Vec4(1f32, 4f32, 9f32, 16f32),
            /// assert_eq!(v, s.sqrt());
            /// ```
            pub fn sqrt(self) -> Self { unimplemented!() }
            /// Returns a new vector which elements are the respective reciprocal 
            /// square roots of this vector's elements.
            ///
            /// ```
            /// let v = Vec4(1f32, 0.5f32, 1f32/3f32, 0.25f32),
            /// let s = Vec4(1f32, 4f32, 9f32, 16f32),
            /// assert_eq!(v, s.rsqrt());
            /// ```
            pub fn rsqrt(self) -> Self { unimplemented!() }
            /// Returns a new vector which elements are the respective reciprocal 
            /// of this vector's elements.
            ///
            /// ```
            /// let v = Vec4(1f32, 0.5f32, 0.25f32, 0.125f32),
            /// let s = Vec4(1f32, 2f32, 4f32, 8f32),
            /// assert_eq!(v, s.reciprocal());
            /// assert_eq!(s, v.reciprocal());
            /// ```
            pub fn reciprocal(self) -> Self { unimplemented!() }
            /// Horizontally adds adjacent pairs of elements in `self` and `rhs` into a new vector.
            ///
            /// ```
            /// let a = Vec4(0, 1, 2, 3);
            /// let b = Vec4(4, 5, 6, 7);
            /// let h = Vec4(0+1, 2+3, 4+5, 5+7);
            /// assert_eq!(h, a.hadd(b));
            /// ```
            pub fn hadd(self, _rhs: Self) -> Self { unimplemented!() }
            /// Returns a new vector which elements are rounded to the nearest greater integer.
            ///
            /// ```
            /// let v = Vec4(0_f32, 1_f32, 1.8_f32, 3.14_f32);
            /// assert_eq!(v.ceil(), Vec4(0f32, 1f32, 2f32, 4f32));
            /// ```
            pub fn ceil(self) -> Self { unimplemented!() }
            /// Returns a new vector which elements are rounded down to the nearest lower integer.
            ///
            /// ```
            /// let v = Vec4(0_f32, 1_f32, 1.8_f32, 3.14_f32);
            /// assert_eq!(v.floor(), Vec4(0f32, 1f32, 1f32, 3f32));
            /// ```
            pub fn floor(self) -> Self { unimplemented!() }
            /// Returns a new vector which elements are rounded to the nearest integer.
            ///
            /// ```
            /// let v = Vec4(0_f32, 1_f32, 1.8_f32, 3.14_f32);
            /// assert_eq!(v.round(), Vec4(0f32, 1f32, 2f32, 3f32));
            /// ```
            pub fn round(self) -> Self { unimplemented!() }

            /// Compares each element of two vectors with the equality test, returning a boolean vector.
            ///
            /// ```
            /// let u = Vec4(0,2,2,6);
            /// let v = Vec4(0,1,2,3);
            /// assert_eq!(u.cmpeq(v), Vec4(true, false, true, false));
            /// ```
            pub fn cmpeq(&self, _rhs: &Self) -> $Vec<bool> { unimplemented!() }
            /// Compares each element of two vectors with the not-equal test, returning a boolean vector.
            ///
            /// ```
            /// let u = Vec4(0,2,2,6);
            /// let v = Vec4(0,1,2,3);
            /// assert_eq!(u.cmpeq(v), Vec4(true, false, true, false));
            /// ```
            pub fn cmpne(&self, _rhs: &Self) -> $Vec<bool> { unimplemented!() }
            /// Compares each element of two vectors with the greater-or-equal test, returning a boolean vector.
            ///
            /// ```
            /// let u = Vec4(0,2,2,6);
            /// let v = Vec4(0,1,2,3);
            /// assert_eq!(u.cmpeq(v), Vec4(true, false, true, false));
            /// ```
            pub fn cmpge(&self, _rhs: &Self) -> $Vec<bool> { unimplemented!() }
            /// Compares each element of two vectors with the greater-than test, returning a boolean vector.
            ///
            /// ```
            /// let u = Vec4(0,2,2,6);
            /// let v = Vec4(0,1,2,3);
            /// assert_eq!(u.cmpeq(v), Vec4(true, false, true, false));
            /// ```
            pub fn cmpgt(&self, _rhs: &Self) -> $Vec<bool> { unimplemented!() }
            /// Compares each element of two vectors with the less-or-equal test, returning a boolean vector.
            ///
            /// ```
            /// let u = Vec4(0,2,2,6);
            /// let v = Vec4(0,1,2,3);
            /// assert_eq!(u.cmpeq(v), Vec4(true, false, true, false));
            /// ```
            pub fn cmple(&self, _rhs: &Self) -> $Vec<bool> { unimplemented!() }
            /// Compares each element of two vectors with the less-than test, returning a boolean vector.
            ///
            /// ```
            /// let u = Vec4(0,2,2,6);
            /// let v = Vec4(0,1,2,3);
            /// assert_eq!(u.cmpeq(v), Vec4(true, false, true, false));
            /// ```
            pub fn cmplt(&self, _rhs: &Self) -> $Vec<bool> { unimplemented!() }
        }

        // IMPLS OF TRAITS

        impl<T: Zero + PartialEq> Zero for $Vec<T> {
            fn zero() -> Self { Self::zero() }
            fn is_zero(&self) -> bool { self.is_zero() }
        }
        impl<T: One> One for $Vec<T> {
            fn one() -> Self { Self::one() }
        }

        // OPS

        impl<T> Neg for $Vec<T> where T: Neg<Output=T> {
            type Output = Self;
            fn neg(self) -> Self::Output {
                Self::new($(-self.$get),+)
            }
        }

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


        /*
        impl<T> IntoIterator for $Vec<T> {
            type Item = T;
            type IntoIter = IntoIter<T, Self>;
            fn into_iter(self) -> Self::IntoIter {
                IntoIter::new(self)
            }
        }
        */

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
            fn from(t: $Tuple) -> Self {
                Self::new($(t.$tupleget),+)
            }
        }
    };
}


/// Calls vec_impl_vec!() on each appropriate vector type.
macro_rules! vec_impl_all_vecs {
    () => {
        vec_impl_vec!(tuple Vec2  (2) ("({}, {})") (0 1) (x y) (0 1) (T,T));
        vec_impl_vec!(tuple Vec3  (3) ("({}, {}, {})") (0 1 2) (x y z) (0 1 2) (T,T,T));
        vec_impl_vec!(tuple Vec4  (4) ("({}, {}, {}, {})") (0 1 2 3) (x y z w) (0 1 2 3) (T,T,T,T));
        vec_impl_vec!(tuple Vec8  (8) ("({}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7) (m0 m1 m2 m3 m4 m5 m6 m7) (0 1 2 3 4 5 6 7) (T,T,T,T,T,T,T,T));
        vec_impl_vec!(tuple Vec16 (16) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
        vec_impl_vec!(tuple Vec32 (32) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
        vec_impl_vec!(tuple Vec64 (64) ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31 m32 m33 m34 m35 m36 m37 m38 m39 m40 m41 m42 m43 m44 m45 m46 m47 m48 m49 m50 m51 m52 m53 m54 m55 m56 m57 m58 m59 m60 m61 m62 m63) (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
        vec_impl_vec!(struct Xy (2) ("({}, {})") (x y) (x y) (0 1) (T,T));
        vec_impl_vec!(struct Xyz (3) ("({}, {}, {})") (x y z) (x y z) (0 1 2) (T,T,T));
        vec_impl_vec!(struct Xyzw (4) ("({}, {}, {}, {})") (x y z w) (x y z w) (0 1 2 3) (T,T,T,T));
        vec_impl_vec!(struct Rgb (3) ("rgb({}, {}, {})") (r g b) (r g b) (0 1 2) (T,T,T));
        vec_impl_vec!(struct Rgba (4) ("rgba({}, {}, {}, {})") (r g b a) (r g b a) (0 1 2 3) (T,T,T,T));
        vec_impl_vec!(struct Extent2 (2) ("({}, {})") (w h) (w h) (0 1) (T,T));
        vec_impl_vec!(struct Extent3 (3) ("({}, {}, {})") (w h d) (w h d) (0 1 2) (T,T,T));
        vec_impl_vec!(struct Uv  (2) ("({}, {})") (u v) (u v) (0 1) (T,T));
        vec_impl_vec!(struct Uvw (3) ("({}, {}, {})") (u v w) (u v w) (0 1 2) (T,T,T));
    }
}

pub mod repr_c {
    //! Vector types which are marked `#[repr(packed, C)]`.
    //!
    //! You can instantiate any vector type of this module with any type `T`.
    
    use super::*;
    vec_declare_types!{
        #[repr(C)]
        #[cfg_attr(all(nightly, feature="repr_align", any(target_arch="x86", target_arch="x86_64")), repr(align(16)))]
        #[cfg_attr(all(nightly, feature="repr_align", target_arch="arm"), repr(align(64)))]
        // XXX ^^^^ Not sure about the alignment on ARM ??
        // TODO assert the packing of vecs
    }
    vec_impl_all_vecs!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Vector types which are marked `#[repr(packed, simd)]`.
    //!
    //! You can instantiate any vector type of this module with any type as long as
    //! it is a "machine type", like `f32` and `i32`, but not `isize` or newtypes
    //! (normally, unless they're marked `#[repr(transparent)]`, but that hasn't been tested yet).
    
    use super::*;
    vec_declare_types!{#[repr(packed, simd)]}
    vec_impl_all_vecs!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
/// If you're on Nightly with the `repr_simd` feature enabled, this exports `self::repr_simd::*` instead.
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

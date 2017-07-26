//! TODO document that shuffling is well-done by destructuring and thus not implemented here.
//! TODO document the guidelines for supported types. To make it short, we want to be able to go as
//! far as supporting vectors of bignums.

extern crate num_traits;

use self::num_traits::{Zero, One, Float, Signed};
use core::iter::FromIterator;
use core::slice;
use core::marker::PhantomData;
use core::mem;
use core::ptr;
use core::borrow::{Borrow, BorrowMut};
use core::ops::*;
use core::fmt::{self, Display, Formatter};
use mat::Mat2;
use clamp::PartialMinMax;
use color_component::ColorComponent;

macro_rules! vec_declare_types {
    (#[$attrs:meta]) => {
        /// A two-components generic vector type.
        ///
        /// - If you intend to use it as spatial coordinates, consider using [Xy](struct.Xy.html) instead.
        /// - If you intend to use it as texture coordinates, consider using [Uv](struct.Uv.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Vec2<T>(pub T, pub T);
        /// A three-components generic vector type.
        ///
        /// - If you intend to use it as spatial coordinates, consider using [Xyz](struct.Xyz.html) instead.
        /// - If you intend to use it as RGB color data, consider using [Rgb](struct.Rgb.html) instead.
        /// - If you intend to use it as texture coordinates, consider using [Uvw](struct.Uvw.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Vec3<T>(pub T, pub T, pub T);
        /// A four-components generic vector type.
        ///
        /// - If you intend to use it as homogeneous spatial coordinates, consider using [Xyzw](struct.Xyzw.html) instead.
        /// - If you intend to use it as RGBA color data, consider using [Rgba](struct.Rgba.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Vec4<T>(pub T, pub T, pub T, pub T);

        /// An eight-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX2-enabled x86 CPUs, a `Vec8<i32>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Vec8<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A sixteen-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX2-enabled x86 CPUs, a `Vec16<i16>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Vec16<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A thirty-two-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX512-enabled x86 CPUs, a `Vec32<i16>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Vec32<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A sixty-four-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX512-enabled x86 CPUs, a `Vec64<i8>` makes sense.  
        /// Otherwise, LLVM is able to process it as a fixed-sized array of whichever "best" SIMD vector type available.  
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Vec64<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);


        /// Vector type suited for homogeneous 3D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Xyzw<T> { pub x:T, pub y:T, pub z:T, pub w:T }
        /// Vector type suited for 3D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Xyz<T> { pub x:T, pub y:T, pub z:T }
        /// Vector type suited for 2D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
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
        #[$attrs]
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
        #[$attrs]
        pub struct Extent2<T> { pub w:T, pub h:T }


        /// Vector type suited for RGBA color data.
        ///
        /// There is no trait bound on `ColorComponent`, but if `T` doesn't implement it, you'll
        /// miss some goodies.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Rgba<T> { pub r:T, pub g:T, pub b:T, pub a:T }
        /// Vector type suited for RGB color data.
        ///
        /// There is no trait bound on `ColorComponent`, but if `T` doesn't implement it, you'll
        /// miss some goodies.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Rgb<T> { pub r:T, pub g:T, pub b:T }

        /// Vector type suited for 3D texture coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Uvw<T> { pub u:T, pub v:T, pub w:T }
        /// Vector type suited for 2D texture coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Uv<T> { pub u:T, pub v:T }

        #[allow(missing_docs)] pub type Vec4f = Vec4<f32>;
        #[allow(missing_docs)] pub type Vec4i = Vec4<i32>;
        #[allow(missing_docs)] pub type Vec4u = Vec4<u32>;
        #[allow(missing_docs)] pub type Vec4b = Vec4<bool>;
        #[allow(missing_docs)] pub type Vec3f = Vec3<f32>;
        #[allow(missing_docs)] pub type Vec3i = Vec3<i32>;
        #[allow(missing_docs)] pub type Vec3u = Vec3<u32>;
        #[allow(missing_docs)] pub type Vec3b = Vec3<bool>;
        #[allow(missing_docs)] pub type Vec2f = Vec2<f32>;
        #[allow(missing_docs)] pub type Vec2i = Vec2<i32>;
        #[allow(missing_docs)] pub type Vec2u = Vec2<u32>;
        #[allow(missing_docs)] pub type Vec2b = Vec2<bool>;
        #[allow(missing_docs)] pub type Extent3f = Extent3<f32>;
        #[allow(missing_docs)] pub type Extent3u = Extent3<u32>;
        #[allow(missing_docs)] pub type Extent3s = Extent2<u16>;
        #[allow(missing_docs)] pub type Extent3b = Extent3<bool>;
        #[allow(missing_docs)] pub type Extent2f = Extent2<f32>;
        #[allow(missing_docs)] pub type Extent2u = Extent2<u32>;
        #[allow(missing_docs)] pub type Extent2s = Extent2<u16>;
        #[allow(missing_docs)] pub type Extent2b = Extent2<bool>;
        #[allow(missing_docs)] pub type Rgbaf  = Rgba<f32>;
        #[allow(missing_docs)] pub type Rgba32 = Rgba<u8>;
        #[allow(missing_docs)] pub type Rgbf   = Rgb<f32>;
        #[allow(missing_docs)] pub type Rgb24  = Rgb<u8>;
        #[allow(missing_docs)] pub type TexUv  = Uv<f32>;
        #[allow(missing_docs)] pub type TexUvw = Uvw<f32>;

        /// Displays an `Rgba` value as `rgba(r,g,b,a)`.
        impl<T: Display> Display for Rgba<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "rgba({}, {}, {}, {})", self.r, self.g, self.b, self.a)
            }
        }
        /// Displays an `Rgb` value as `rgb(r,g,b)`.
        impl<T: Display> Display for Rgb<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "rgb({}, {}, {})", self.r, self.g, self.b)
            }
        }
        /// Displays an `Xyzw` value as `(x,y,z,w)`.
        impl<T: Display> Display for Xyzw<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "({}, {}, {}, {})", self.x, self.y, self.z, self.w)
            }
        }
        /// Displays an `Xyz` value as `(x,y,z)`.
        impl<T: Display> Display for Xyz<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "({}, {}, {})", self.x, self.y, self.z)
            }
        }
        /// Displays an `Xy` value as `(x,y)`.
        impl<T: Display> Display for Xy<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "({}, {})", self.x, self.y)
            }
        }
        /// Displays an `Uvw` value as `(u,v,w)`.
        impl<T: Display> Display for Uvw<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "({}, {}, {})", self.u, self.v, self.w)
            }
        }
        /// Displays an `Uv` value as `(u,v)`.
        impl<T: Display> Display for Uv<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "({}, {})", self.u, self.v)
            }
        }
        /// Displays an `Extent3` value as `(width,height,depth)`.
        impl<T: Display> Display for Extent3<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "({}, {}, {})", self.w, self.h, self.d)
            }
        }
        /// Displays an `Extent2` value as `(width,height)`.
        impl<T: Display> Display for Extent2<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "({}, {})", self.w, self.h)
            }
        }
    }
}

macro_rules! vec_impl_vec {
    ($Vec:ident ($fmt:expr) ($($get:tt)+) ($($namedget:tt)+) $Tuple:ty) => {
        #[cfg_attr(feature = "cargo-clippy", allow(type_complexity))]
        #[allow(missing_docs)]
        impl<T> $Vec<T> {
            #[cfg_attr(feature = "cargo-clippy", allow(too_many_arguments))]
            pub fn new($($namedget:T),+) -> Self {
                $Vec($($namedget),+)
            }
            pub fn into_tuple(self) -> $Tuple {
                ($(self.$get),+)
            }
            pub fn to_tuple(&self) -> $Tuple where T: Clone {
                ($(self.$get.clone()),+)
            }
        }
        #[cfg_attr(feature = "cargo-clippy", allow(type_complexity))]
        impl<T> From<$Tuple> for $Vec<T> {
            fn from(t: $Tuple) -> Self {
                $Vec($(t.$get),+)
            }
        }
        impl<T: Display> Display for $Vec<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, $fmt, $(self.$get),+)
            }
        }
    }
}

macro_rules! vec_impl_all_vecs {
    () => {
        vec_impl_vec!(Vec2  ("({}, {})") (0 1) (x y) (T,T));
        vec_impl_vec!(Vec3  ("({}, {}, {})") (0 1 2) (x y z) (T,T,T));
        vec_impl_vec!(Vec4  ("({}, {}, {}, {})") (0 1 2 3) (x y z w) (T,T,T,T));
        vec_impl_vec!(Vec8  ("({}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7) (m0 m1 m2 m3 m4 m5 m6 m7) (T,T,T,T,T,T,T,T));
        vec_impl_vec!(Vec16 ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
        vec_impl_vec!(Vec32 ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
        vec_impl_vec!(Vec64 ("({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})") (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63) (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31 m32 m33 m34 m35 m36 m37 m38 m39 m40 m41 m42 m43 m44 m45 m46 m47 m48 m49 m50 m51 m52 m53 m54 m55 m56 m57 m58 m59 m60 m61 m62 m63) (T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T));
    }
}

macro_rules! vec_impl_upgrade_tuple2 {
    ($($Self:ident)+) => { 
        $(
            impl<T: Default> From<(T,T)> for $Self<T> {
                fn from(t: (T,T)) -> Self {
                    Self::from(Vec2::new(t.0, t.1))
                }
            }
        )+
    }
}
macro_rules! vec_impl_upgrade_tuple3 {
    ($($Self:ident)+) => { 
        $(
            impl<T: Default> From<(T,T,T)> for $Self<T> {
                fn from(t: (T,T,T)) -> Self {
                    Self::from(Vec3::new(t.0, t.1, t.2))
                }
            }
        )+
    }
}

macro_rules! vec_impl_into_tuple2 {
    ($($Self:ident)+) => { 
        $(
            impl<T> From<(T,T)> for $Self<T> {
                fn from(t: (T,T)) -> Self {
                    Self::from(Vec2::new(t.0, t.1))
                }
            }

            impl<T> $Self<T> {
                pub fn into_tuple( self) -> (T,T) { self.into_vec2().into_tuple() }
                pub fn to_tuple  (&self) -> (T,T) where T: Clone { self.  to_vec2().into_tuple() }
            }
        )+
    }
}
macro_rules! vec_impl_into_tuple3 {
    ($($Self:ident)+) => { 
        $(
            impl<T> From<(T,T,T)> for $Self<T> {
                fn from(t: (T,T,T)) -> Self {
                    Self::from(Vec3::new(t.0, t.1, t.2))
                }
            }

            impl<T> $Self<T> {
                pub fn into_tuple( self) -> (T,T,T) { self.into_vec3().into_tuple() }
                pub fn to_tuple  (&self) -> (T,T,T) where T: Clone { self.  to_vec3().into_tuple() }
            }
        )+
    }
}
macro_rules! vec_impl_into_tuple4 {
    ($($Self:ident)+) => { 
        $(
            impl<T> From<(T,T,T,T)> for $Self<T> {
                fn from(t: (T,T,T,T)) -> Self {
                    Self::from(Vec4::new(t.0, t.1, t.2, t.3))
                }
            }

            impl<T> $Self<T> {
                pub fn into_tuple( self) -> (T,T,T,T) { self.into_vec4().into_tuple() }
                pub fn to_tuple  (&self) -> (T,T,T,T) where T: Clone { self.  to_vec4().into_tuple() }
            }
        )+
    }
}



#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
/// Moving iterator for vectors, only safe if based on this module's assumptions.
pub struct IntoIter<T, V: AsRef<[T]>> { v: V, i: usize, _booh: PhantomData<T> }

impl<T, V: AsRef<[T]>> IntoIter<T,V> {
    fn new(v: V) -> Self {
        Self { v, i: 0, _booh: PhantomData }
    }
}

impl<T, V: AsRef<[T]>> Iterator for IntoIter<T,V> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let out = self.v.as_ref().get(self.i);
        self.i += 1;
        out.map(|x| unsafe { ptr::read(x) }) // XXX might want to use read_unaligned() ?
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let rem = self.v.as_ref().len() - self.i;
        (rem, Some(rem))
    }
}

impl<T, V: AsRef<[T]>> ExactSizeIterator for IntoIter<T, V> {
    fn len(&self) -> usize {
        self.v.as_ref().len() - self.i
    }
}




macro_rules! vec_impl_new {
    ($Type:ident $($get:tt)+) => {
        impl<T> $Type<T> {
            pub fn new($($get:T),+) -> Self {
                Self {
                    $($get),+
                }
            }
        }
    }
}
macro_rules! vec_impl_basic_ops {
    ($len:expr, $Type:ident $($get:tt)+) => {
        impl<T> $Type<T> {
            pub fn as_slice(&self) -> &[T] {
                unsafe {
                    let p = self as *const _ as *const T;
                    slice::from_raw_parts(p, $len)
                }
            }
            pub fn as_mut_slice(&mut self) -> &mut [T] {
                unsafe {
                    let p = self as *mut _ as *mut T;
                    slice::from_raw_parts_mut(p, $len)
                }
            }
            pub fn from_slice(slice: &[T]) -> Self where T: Default + Clone {
                Self::from_iter(slice.into_iter().cloned())
            }
            pub fn get(&self, i: usize) -> Option<&T> {
                self.as_slice().get(i)
            }
            pub unsafe fn get_unchecked(&self, i: usize) -> &T {
                self.as_slice().get_unchecked(i)
            }
            pub fn get_mut(&mut self, i: usize) -> Option<&mut T> {
                self.as_mut_slice().get_mut(i)
            }
            pub unsafe fn get_unchecked_mut(&mut self, i: usize) -> &mut T {
                self.as_mut_slice().get_unchecked_mut(i)
            }

            // Utilities for checking the validity of Extents, but can make
            // sense for other types too.
            pub fn is_any_negative(&self) -> bool where T: Signed {
                self.iter().fold(false, |acc, x| acc || x.is_negative())
            }
            pub fn is_all_positive(&self) -> bool where T: Signed {
                !self.is_any_negative()
            }

            pub fn convert<D,F>(self, f: F) -> $Type<D> where F: Fn(T) -> D {
                let mut out: $Type<D> = unsafe { mem::uninitialized() };
                $(out.$get = f(self.$get);)+
                out
            }
            pub fn into_array(self) -> [T; $len] {
                [$(self.$get, )+]
            }
            pub fn iter(&self) -> slice::Iter<T> {
                self.into_iter()
            }
            pub fn iter_mut(&mut self) -> slice::IterMut<T> {
                self.into_iter()
            }
            pub fn broadcast(val: T) -> Self where T: Clone {
                let mut out: Self = unsafe { mem::uninitialized() };
                $(out.$get = val.clone();)+
                out
            }
            pub fn zero() -> Self where T: Zero {
                let mut out: Self = unsafe { mem::uninitialized() };
                $(out.$get = T::zero();)+
                out
            }
            pub fn one() -> Self where T: One {
                let mut out: Self = unsafe { mem::uninitialized() };
                $(out.$get = T::one();)+
                out
            }
            pub fn dimension(&self) -> usize {
                $len
            }
        }
        
        impl<T> IntoIterator for $Type<T> {
            type Item = T;
            type IntoIter = IntoIter<T, Self>;
            fn into_iter(self) -> Self::IntoIter {
                IntoIter::new(self)
            }
        }

        impl<'a, T> IntoIterator for &'a $Type<T> {
            type Item = &'a T;
            type IntoIter = slice::Iter<'a, T>;
            fn into_iter(self) -> Self::IntoIter {
                self.as_slice().into_iter()
            }
        }
        impl<'a, T> IntoIterator for &'a mut $Type<T> {
            type Item = &'a mut T;
            type IntoIter = slice::IterMut<'a, T>;
            fn into_iter(self) -> Self::IntoIter {
                self.as_mut_slice().into_iter()
            }
        }
        impl<T: Default> FromIterator<T> for $Type<T> {
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

        impl<T> AsRef<[T]> for $Type<T> {
            fn as_ref(&self) -> &[T] {
                self.as_slice()
            }
        }
        impl<T> AsMut<[T]> for $Type<T> {
            fn as_mut(&mut self) -> &mut [T] {
                self.as_mut_slice()
            }
        }
        impl<T> Borrow<[T]> for $Type<T> {
            fn borrow(&self) -> &[T] {
                self.as_slice()
            }
        }
        impl<T> BorrowMut<[T]> for $Type<T> {
            fn borrow_mut(&mut self) -> &mut [T] {
                self.as_mut_slice()
            }
        }

        impl<T> Add<T> for $Type<T> where T: Add<Output=T> + Clone {
            type Output = Self;
            fn add(self, rhs: T) -> Self { $Type::new( $(self.$get + rhs.clone(), )+ ) }
        }
        impl<T> Add for $Type<T> where T: Add<Output=T> {
            type Output = Self;
            fn add(self, rhs: Self) -> Self { $Type::new( $(self.$get + rhs.$get, )+ ) }
        }
        impl<T> AddAssign<T> for $Type<T> where T: Add<Output=T> + Clone {
            fn add_assign(&mut self, rhs: T) { *self = self.clone() + rhs }
        }
        impl<T> AddAssign for $Type<T> where T: Add<Output=T> + Clone {
            fn add_assign(&mut self, rhs: Self) { *self = self.clone() + rhs; }
        }
        impl<T> Sub<T> for $Type<T> where T: Sub<Output=T> + Clone {
            type Output = Self;
            fn sub(self, rhs: T) -> Self { $Type::new( $(self.$get - rhs.clone(), )+ ) }
        }
        impl<T> Sub for $Type<T> where T: Sub<Output=T> {
            type Output = Self;
            fn sub(self, rhs: Self) -> Self { $Type::new( $(self.$get - rhs.$get, )+ ) }
        }
        impl<T> SubAssign<T> for $Type<T> where T: Sub<Output=T> + Clone {
            fn sub_assign(&mut self, rhs: T) { *self = self.clone() - rhs; }
        }
        impl<T> SubAssign for $Type<T> where T: Sub<Output=T> + Clone {
            fn sub_assign(&mut self, rhs: Self) { *self = self.clone() - rhs; }
        }
        impl<T> Mul<T> for $Type<T> where T: Mul<Output=T> + Clone {
            type Output = Self;
            fn mul(self, rhs: T) -> Self { $Type::new( $(self.$get * rhs.clone(), )+ ) }
        }
        impl<T> Mul for $Type<T> where T: Mul<Output=T> {
            type Output = Self;
            fn mul(self, rhs: Self) -> Self { $Type::new( $(self.$get * rhs.$get, )+ ) }
        }
        impl<T> MulAssign<T> for $Type<T> where T: Mul<Output=T> + Clone {
            fn mul_assign(&mut self, rhs: T) { *self = self.clone() * rhs; }
        }
        impl<T> MulAssign for $Type<T> where T: Mul<Output=T> + Clone {
            fn mul_assign(&mut self, rhs: Self) { *self = self.clone() * rhs; }
        }
        impl<T> Div<T> for $Type<T> where T: Div<Output=T> + Clone {
            type Output = Self;
            fn div(self, rhs: T) -> Self { $Type::new( $(self.$get / rhs.clone(), )+ ) }
        }
        impl<T> Div for $Type<T> where T: Div<Output=T> {
            type Output = Self;
            fn div(self, rhs: Self) -> Self { $Type::new( $(self.$get / rhs.$get, )+ ) }
        }
        impl<T> DivAssign<T> for $Type<T> where T: Div<Output=T> + Clone {
            fn div_assign(&mut self, rhs: T) { *self = self.clone() / rhs; }
        }
        impl<T> DivAssign for $Type<T> where T: Div<Output=T> + Clone {
            fn div_assign(&mut self, rhs: Self) { *self = self.clone() / rhs; }
        }
        impl<T> Rem<T> for $Type<T> where T: Rem<Output=T> + Clone {
            type Output = Self;
            fn rem(self, rhs: T) -> Self { $Type::new( $(self.$get % rhs.clone(), )+ ) }
        }
        impl<T> Rem for $Type<T> where T: Rem<Output=T> {
            type Output = Self;
            fn rem(self, rhs: Self) -> Self { $Type::new( $(self.$get % rhs.$get, )+ ) }
        }
        impl<T> RemAssign<T> for $Type<T> where T: Rem<Output=T> + Clone {
            fn rem_assign(&mut self, rhs: T) { *self = self.clone() % rhs; }
        }
        impl<T> RemAssign for $Type<T> where T: Rem<Output=T> + Clone {
            fn rem_assign(&mut self, rhs: Self) { *self = self.clone() % rhs; }
        }
        impl<T> Neg for $Type<T> where T: Neg<Output=T> {
            type Output = Self;
            fn neg(self) -> Self { $Type::new( $(-self.$get, )+ ) }
        }
        impl<T> IndexMut<usize> for $Type<T> {
            fn index_mut(&mut self, i: usize) -> &mut T {
                &mut self.as_mut_slice()[i]
            }
        }
        impl<T> Index<usize> for $Type<T> {
            type Output = T;
            fn index(&self, i: usize) -> &T {
                &self.as_slice()[i]
            }
        }
        // WISH: impl mul with matrices
        // WISH: impl ops with references
    }
}

macro_rules! vec_impl_from_same_dim {
    (($Self:ident $into:ident $to:ident $as:ident $as_mut:ident) from $($Other:ident)+) => {
        $(
            impl<T> From<$Other<T>> for $Self<T> {
                fn from(v: $Other<T>) -> Self {
                    unsafe { 
                        let mut out: Self = mem::uninitialized();
                        debug_assert_eq!(v.dimension(), out.dimension());
                        ptr::copy_nonoverlapping(v.as_slice().as_ptr(), out.as_mut_slice().as_mut_ptr(), out.dimension());
                        out
                    }
                }
            }
            #[cfg_attr(feature = "cargo-clippy", allow(useless_transmute))]
            impl<T> AsRef<$Self<T>> for $Other<T> { fn as_ref(&    self) -> &    $Self<T> { unsafe { mem::transmute(self) } } }
            #[cfg_attr(feature = "cargo-clippy", allow(useless_transmute))]
            impl<T> AsMut<$Self<T>> for $Other<T> { fn as_mut(&mut self) -> &mut $Self<T> { unsafe { mem::transmute(self) } } }
            impl<T> $Other<T> {
                pub fn $into  (     self) ->      $Self<T> { self.into() }
                pub fn $to    (&    self) ->      $Self<T> where T: Clone { self.clone().into() }
                pub fn $as    (&    self) -> &    $Self<T> { self.as_ref() }
                pub fn $as_mut(&mut self) -> &mut $Self<T> { self.as_mut() }
            }
        )+
    }
}
macro_rules! vec_impl_upgrade {
    (($Down:ident $into_down:ident $to_down:ident $as_down:ident $as_mut_down:ident) for $(($Up:ident $into_up:ident $to_up:ident))+) => {
        $(
            impl<T> From<$Up<T>> for $Down<T> {
                fn from(v: $Up<T>) -> Self {
                    unsafe { 
                        let mut out: Self = mem::uninitialized();
                        ptr::copy_nonoverlapping(v.as_slice().as_ptr(), out.as_mut_slice().as_mut_ptr(), out.dimension());
                        out
                    }
                }
            }
            impl<T: Default> From<$Down<T>> for $Up<T> {
                fn from(v: $Down<T>) -> Self {
                    unsafe {
                        let mut out = Self::default();
                        ptr::copy_nonoverlapping(v.as_slice().as_ptr(), out.as_mut_slice().as_mut_ptr(), out.dimension());
                        out
                    }
                }
            }
            #[cfg_attr(feature = "cargo-clippy", allow(useless_transmute))]
            impl<T> AsRef<$Down<T>> for $Up<T> { fn as_ref(&    self) -> &    $Down<T> { unsafe { mem::transmute(self) } } }
            #[cfg_attr(feature = "cargo-clippy", allow(useless_transmute))]
            impl<T> AsMut<$Down<T>> for $Up<T> { fn as_mut(&mut self) -> &mut $Down<T> { unsafe { mem::transmute(self) } } }
            impl<T: Default> $Down<T> {
                pub fn $into_up( self) -> $Up<T> { self.into() }
                pub fn $to_up  (&self) -> $Up<T> where T: Clone { self.clone().into() }
            }
            impl<T> $Up<T> {
                pub fn $into_down  (     self) ->      $Down<T> { self.into() }
                pub fn $to_down    (&    self) ->      $Down<T> where T: Clone { self.clone().into() }
                pub fn $as_down    (&    self) -> &    $Down<T> { self.as_ref() }
                pub fn $as_mut_down(&mut self) -> &mut $Down<T> { self.as_mut() }
            }
        )+
    }
}
macro_rules! vec_impl_spatial_ops {
    ($Exactly:ident, $($Type:ident)+) => {
        $(
            impl<T> $Type<T> {
                pub fn dot<V>(self, v: V) -> T where T: Zero + Mul<Output=T>, V: $Exactly<T> {
                    self.into_iter().zip(v.into().into_iter()).fold(T::zero(), |acc, (a, b)| acc + a*b)
                }
                pub fn squared_magnitude(self) -> T where T: Zero + Mul<Output=T> + Clone {
                    let v = self.clone();
                    self.dot(v)
                }
                pub fn magnitude(self) -> T where T: Float {
                    self.squared_magnitude().sqrt()
                }
                pub fn normalized(self) -> Self where T: Float {
                    let len = self.clone().magnitude();
                    self / len
                }
            }
        )+
    }
}
macro_rules! vec_impl_distance {
    ($($Type:ident)+) => {
        $(
            impl<T> $Type<T> {
                pub fn distance(self, other: Self) -> T where T: Float {
                    (self - other).magnitude()
                }
                pub fn reflect(self, surface_normal: Self) -> Self
                    where T: Zero + Mul<Output=T> + Sub<Output=T> + Clone
                {
                    let dot = self.clone().dot(surface_normal.clone());
                    let p = dot.clone() + dot.clone();
                    let mut out: Self = unsafe { mem::uninitialized() };
                    for ((out_e, v), s) in out.iter_mut().zip(self.into_iter()).zip(surface_normal.into_iter()) {
                        *out_e = v - s * p.clone();
                    }
                    out
                }
                pub fn refract(self, surface_normal: Self, eta: T) -> Self 
                    where T: Float
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
                pub fn face_forward(n: Self, i: Self, nref: Self) -> Self
                    where T: Zero + Mul<Output=T> + Neg<Output=T> + PartialOrd
                {
                    if nref.dot(i) < T::zero() {
                        n
                    } else {
                        -n
                    }
                }
            }
        )+
    }
}

macro_rules! vec_impl_cross {
    ($($Type:ident)+) => {
        $(
            impl<T> $Type<T> {
                pub fn cross(self, v: Self) -> Self where T: Clone + Mul<Output=T> + Sub<Output=T> {
                    unsafe {
                        let s = Vec3 (
                            self.get_unchecked(0).clone(),
                            self.get_unchecked(1).clone(),
                            self.get_unchecked(2).clone()
                        );
                        let v = Vec3 (
                            v.get_unchecked(0).clone(),
                            v.get_unchecked(1).clone(),
                            v.get_unchecked(2).clone()
                        );
                        let ss = s.clone();
                        let vv = v.clone();
                        Self::new(
                            s.1*v.2 - ss.2*vv.1,
                            s.2*v.0 - ss.0*vv.2,
                            s.0*v.1 - ss.1*vv.0
                        )
                    }
                }
            }
        )+
    }
}

macro_rules! vec_impl_point_or_direction {
    ($($Type:ident)+) => {
        $(
            impl<T> $Type<T> {
                pub fn new_point(x: T, y: T, z: T) -> Self where T: One {
                    Self::new(x, y, z, T::one())
                }
                pub fn new_direction(x: T, y: T, z: T) -> Self where T: Zero {
                    Self::new(x, y, z, T::zero())
                }
                pub fn point<V: Into<Xyz<T>>>(v: V) -> Self where T: One {
                    let Xyz { x, y, z } = v.into();
                    Self::new_point(x, y, z)
                }
                pub fn direction<V: Into<Xyz<T>>>(v: V) -> Self where T: Zero {
                    let Xyz { x, y, z } = v.into();
                    Self::new_direction(x, y, z)
                }
            }
        )+
    }
}

macro_rules! vec_impl_rotate2d {
    ($($Type:ident)+) => {
        $(
            impl<T: Float> $Type<T> {
                pub fn rotated_z(self, angle_radians: T) -> Self {
                    Mat2::rotation_z(angle_radians) * self
                }
            }
        )+
    }
}

macro_rules! vec_impl_directions_2d {
    ($($Self:ident)+) => {
        $(
            impl<T: Zero + One> $Self<T> {
                pub fn unit_x() -> Self { Self::from(Xyzw::new_direction(T:: one(), T::zero(), T::zero())) }
                pub fn unit_y() -> Self { Self::from(Xyzw::new_direction(T::zero(), T:: one(), T::zero())) }
                pub fn right () -> Self { Self::unit_x() }
                pub fn up    () -> Self { Self::unit_y() }
            }
            impl<T: Zero + One + Neg<Output=T>> $Self<T> {
                pub fn left() -> Self { -Self::right() }
                pub fn down() -> Self { -Self::up()    }
            }
        )+
    }
}
macro_rules! vec_impl_directions_3d {
    ($($Self:ident)+) => {
        $(
            impl<T: Zero + One> $Self<T> {
                pub fn unit_z    () -> Self { Self::from(Xyzw::new_direction(T::zero(), T::zero(), T::one())) }
                /// Forward direction vector in left-handed coordinate space.
                pub fn forward_lh() -> Self { Self::unit_z() }
                /// Backwards direction vector in right-handed coordinate space.
                pub fn back_rh   () -> Self { Self::unit_z() }
            }

            impl<T: Zero + One + Neg<Output=T>> $Self<T> {
                /// Forward direction vector in right-handed coordinate space.
                pub fn forward_rh() -> Self { -Self::back_rh() }
                /// Backwards direction vector in left-handed coordinate space.
                pub fn back_lh   () -> Self { -Self::forward_lh() }
            }
        )+
    }
}

macro_rules! vec_impl_rgb_constants {
    ($($Self:ident)+) => {
        $(
            impl<T: ColorComponent> $Self<T> {
                pub fn black   () -> Self { Self::from(Rgba::new_opaque(T::zero(), T::zero(), T::zero())) }
                pub fn white   () -> Self { Self::from(Rgba::new_opaque(T::full(), T::full(), T::full())) }
                pub fn red     () -> Self { Self::from(Rgba::new_opaque(T::full(), T::zero(), T::zero())) }
                pub fn green   () -> Self { Self::from(Rgba::new_opaque(T::zero(), T::full(), T::zero())) }
                pub fn blue    () -> Self { Self::from(Rgba::new_opaque(T::zero(), T::zero(), T::full())) }
                pub fn cyan    () -> Self { Self::from(Rgba::new_opaque(T::zero(), T::full(), T::full())) }
                pub fn magenta () -> Self { Self::from(Rgba::new_opaque(T::full(), T::zero(), T::full())) }
                pub fn yellow  () -> Self { Self::from(Rgba::new_opaque(T::full(), T::full(), T::zero())) }
                pub fn gray(value: T) -> Self where T: Copy { Self::from(Rgba::new_opaque(value, value, value)) }
                // NOTE: Let's not get started with the 'gray' vs 'grey' debate. I picked 'gray' because that's
                // what the Unity Engine happens to favor. From that, there's no point in implementing aliases
                // just because people might prefer to spell 'grey' on a whim. A choice has to be made.
            }
            impl<T: PartialMinMax> $Self<T> {
                // The highest color component, i.e max(r,g,b).
                pub fn max_color_component(self) -> T {
                    T::partial_max(T::partial_max(self.r, self.g), self.b)
                }
                // The lowest color component, i.e in(r,g,b).
                pub fn min_color_component(self) -> T {
                    T::partial_min(T::partial_min(self.r, self.g), self.b)
                }
                // WISH: impl grayscale(self) -> T fast, precisely, and preventing integer overflow.
                // Once it's done, implement into_gray(self) -> Self, which preserves opacity if any.
                
                // WISH: impl from_html_hex(s: &str) or something like that
            }
        )+
    }
}

macro_rules! vec_complete_mod {
    () => {
        // WISH: blend() and invert()

        impl<T: ColorComponent> Rgba<T> {
            pub fn new_opaque(r: T, g: T, b: T) -> Self {
                Self::new(r, g, b, T::full())
            }
            pub fn new_transparent(r: T, g: T, b: T) -> Self {
                Self::new(r, g, b, T::zero())
            }
            pub fn opaque<V: Into<Rgb<T>>>(color: V) -> Self {
                let Rgb { r, g, b } = color.into();
                Self::new_opaque(r, g, b)
            }
            pub fn transparent<V: Into<Rgb<T>>>(color: V) -> Self {
                let Rgb { r, g, b } = color.into();
                Self::new_transparent(r, g, b)
            }
        }
        impl<T> Rgba<T> {
            pub fn translucent<V: Into<Rgb<T>>>(color: V, opacity: T) -> Self {
                let Rgb { r, g, b } = color.into();
                Self::new(r, g, b, opacity)
            }
        }


        // NOTE: Traits for type that convert _exactly_ into the given type.
        // Implement only if neither "shortening" nor "extension" takes place during conversion.
        pub trait Exactly2<T>: Into<Vec2<T>> + From<Vec2<T>> {}
        pub trait Exactly3<T>: Into<Vec3<T>> + From<Vec3<T>> {}
        pub trait Exactly4<T>: Into<Vec4<T>> + From<Vec4<T>> {}

        impl<T> Exactly2<T> for Vec2<T> {}
        impl<T> Exactly2<T> for Xy<T> {}
        impl<T> Exactly2<T> for Uv<T> {}
        impl<T> Exactly2<T> for Extent2<T> {}
        impl<T> Exactly3<T> for Vec3<T> {}
        impl<T> Exactly3<T> for Xyz<T> {}
        impl<T> Exactly3<T> for Uvw<T> {}
        impl<T> Exactly3<T> for Rgb<T> {}
        impl<T> Exactly3<T> for Extent3<T> {}
        impl<T> Exactly4<T> for Vec4<T> {}
        impl<T> Exactly4<T> for Xyzw<T> {}
        impl<T> Exactly4<T> for Rgba<T> {}


        vec_impl_new!(Xyzw x y z w);
        vec_impl_new!(Xyz  x y z  );
        vec_impl_new!(Xy   x y    );
        vec_impl_new!(Uvw  u v w  );
        vec_impl_new!(Uv   u v    );
        vec_impl_new!(Rgba r g b a);
        vec_impl_new!(Rgb  r g b  );
        vec_impl_new!(Extent3 w h d);
        vec_impl_new!(Extent2 w h  );

        vec_impl_basic_ops!(2, Vec2  0 1    );
        vec_impl_basic_ops!(3, Vec3  0 1 2  );
        vec_impl_basic_ops!(4, Vec4  0 1 2 3);
        vec_impl_basic_ops!(8, Vec8  0 1 2 3 4 5 6 7 );
        vec_impl_basic_ops!(16, Vec16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15);
        vec_impl_basic_ops!(32, Vec32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31);
        vec_impl_basic_ops!(64, Vec64 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63);
        vec_impl_basic_ops!(4, Xyzw x y z w);
        vec_impl_basic_ops!(3, Xyz  x y z  );
        vec_impl_basic_ops!(2, Xy   x y    );
        vec_impl_basic_ops!(3, Uvw  u v w  );
        vec_impl_basic_ops!(2, Uv   u v    );
        vec_impl_basic_ops!(4, Rgba r g b a);
        vec_impl_basic_ops!(3, Rgb  r g b  );
        vec_impl_basic_ops!(3, Extent3 w h d);
        vec_impl_basic_ops!(2, Extent2 w h  );


        vec_impl_upgrade_tuple2!(Vec4 Vec3 Xyzw Xyz Rgba Rgb Uvw Extent3);
        vec_impl_upgrade_tuple3!(Vec4 Xyzw Rgba);
        vec_impl_into_tuple4!(Xyzw Rgba);
        vec_impl_into_tuple3!(Xyz Rgb Uvw Extent3);
        vec_impl_into_tuple2!(Xy Uv Extent2);


        vec_impl_from_same_dim!((Vec2    into_vec2    to_vec2    as_vec2    as_mut_vec2   ) from Xy Uv Extent2);
        vec_impl_from_same_dim!((Vec3    into_vec3    to_vec3    as_vec3    as_mut_vec3   ) from Xyz Uvw Rgb Extent3);
        vec_impl_from_same_dim!((Vec4    into_vec4    to_vec4    as_vec4    as_mut_vec4   ) from Xyzw Rgba);
        vec_impl_from_same_dim!((Xyzw    into_xyzw    to_xyzw    as_xyzw    as_mut_xyzw   ) from Vec4 Rgba);
        vec_impl_from_same_dim!((Xyz     into_xyz     to_xyz     as_xyz     as_mut_xyz    ) from Vec3 Rgb Uvw Extent3);
        vec_impl_from_same_dim!((Xy      into_xy      to_xy      as_xy      as_mut_xy     ) from Vec2 Uv Extent2);
        vec_impl_from_same_dim!((Uvw     into_uvw     to_uvw     as_uvw     as_mut_uvw    ) from Vec3 Xyz Rgb Extent3);
        vec_impl_from_same_dim!((Uv      into_uv      to_uv      as_uv      as_mut_uv     ) from Vec2 Xy Extent2);
        vec_impl_from_same_dim!((Rgba    into_rgba    to_rgba    as_rgba    as_mut_rgba   ) from Vec4 Xyzw);
        vec_impl_from_same_dim!((Rgb     into_rgb     to_rgb     as_rgb     as_mut_rgb    ) from Vec3 Xyz Uvw Extent3);
        vec_impl_from_same_dim!((Extent3 into_extent3 to_extent3 as_extent3 as_mut_extent3) from Vec3 Xyz Uvw Rgb);
        vec_impl_from_same_dim!((Extent2 into_extent2 to_extent2 as_extent2 as_mut_extent2) from Vec2 Xy Uv);

        // Lots of fun
        //($down_dim:expr, ($Down:ident $into_down:ident $to_down:ident $as_down:ident $as_mut_down:ident) for $(($Up:ident $into_up:ident $to_up:ident))+) => {
        vec_impl_upgrade!((Vec3    into_vec3    to_vec3    as_vec3    as_mut_vec3   ) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) );
        vec_impl_upgrade!((Xyz     into_xyz     to_xyz     as_xyz     as_mut_xyz    ) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) );
        vec_impl_upgrade!((Rgb     into_rgb     to_rgb     as_rgb     as_mut_rgb    ) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) );
        vec_impl_upgrade!((Uvw     into_uvw     to_uvw     as_uvw     as_mut_uvw    ) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) );
        vec_impl_upgrade!((Extent3 into_extent3 to_extent3 as_extent3 as_mut_extent3) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) );
        vec_impl_upgrade!((Vec2    into_vec2    to_vec2    as_vec2    as_mut_vec2   ) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) (Vec3 into_vec3 to_vec3) (Xyz into_xyz to_xyz) (Rgb into_rgb to_rgb) (Uvw into_uvw to_uvw) (Extent3 into_extent3 to_extent3));
        vec_impl_upgrade!((Xy      into_xy      to_xy      as_xy      as_mut_xy     ) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) (Vec3 into_vec3 to_vec3) (Xyz into_xyz to_xyz) (Rgb into_rgb to_rgb) (Uvw into_uvw to_uvw) (Extent3 into_extent3 to_extent3));
        vec_impl_upgrade!((Uv      into_uv      to_uv      as_uv      as_mut_uv     ) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) (Vec3 into_vec3 to_vec3) (Xyz into_xyz to_xyz) (Rgb into_rgb to_rgb) (Uvw into_uvw to_uvw) (Extent3 into_extent3 to_extent3));
        vec_impl_upgrade!((Extent2 into_extent2 to_extent2 as_extent2 as_mut_extent2) for (Vec4 into_vec4 to_vec4) (Xyzw into_xyzw to_xyzw) (Rgba into_rgba to_rgba) (Vec3 into_vec3 to_vec3) (Xyz into_xyz to_xyz) (Rgb into_rgb to_rgb) (Uvw into_uvw to_uvw) (Extent3 into_extent3 to_extent3));

        vec_impl_spatial_ops!(Exactly4, Vec4 Xyzw);
        vec_impl_spatial_ops!(Exactly3, Vec3 Xyz Extent3);
        vec_impl_spatial_ops!(Exactly2, Vec2 Xy Extent2);
        vec_impl_distance!(Vec4 Vec3 Vec2 Xyzw Xyz Xy);
        vec_impl_cross!(Vec3 Xyz Extent3);
        vec_impl_rotate2d!(Vec2 Xy Extent2);
        vec_impl_point_or_direction!(Vec4 Xyzw);
        vec_impl_directions_2d!(Vec4 Vec3 Vec2 Xyzw Xyz Xy);
        vec_impl_directions_3d!(Vec4 Vec3      Xyzw Xyz   );

        vec_impl_rgb_constants!(Rgba Rgb);
    }
}


pub mod repr_simd {
    use super::*;
    vec_declare_types!(#[repr(packed,simd)]);
    vec_impl_all_vecs!();
    vec_complete_mod!();
}
pub mod repr_c {
    use super::*;
    vec_declare_types!(#[repr(packed,C)]);
    vec_impl_all_vecs!();
    vec_complete_mod!();
}

pub use self::repr_simd::*;

#[allow(dead_code)]
pub(crate) mod repr_c_aliases {
    use super::repr_c;
    pub type CVec2<T>    = repr_c::Vec2<T>;
    pub type CVec3<T>    = repr_c::Vec3<T>;
    pub type CVec4<T>    = repr_c::Vec4<T>;
    pub type CVec8<T>    = repr_c::Vec8<T>;
    pub type CVec16<T>   = repr_c::Vec16<T>;
    pub type CVec32<T>   = repr_c::Vec32<T>;
    pub type CVec64<T>   = repr_c::Vec64<T>;
    pub type CXyzw<T>    = repr_c::Xyzw<T>;
    pub type CXyz<T>     = repr_c::Xyz<T>;
    pub type CXy<T>      = repr_c::Xy<T>;
    pub type CRgba<T>    = repr_c::Rgba<T>;
    pub type CRgb<T>     = repr_c::Rgb<T>;
    pub type CUvw<T>     = repr_c::Uvw<T>;
    pub type CUv<T>      = repr_c::Uv<T>;
    pub type CExtent2<T> = repr_c::Extent2<T>;
    pub type CExtent3<T> = repr_c::Extent3<T>;
}


#[cfg(test)]
mod test {
    use super::*;

    fn accept_into_vec2_u32<V: Into<Vec2<u32>>>(_: V) {}
    fn accept_asref_vec2_u32<V: AsRef<Vec2<u32>>>(_: V) {}

    #[test]
    fn foo() {
        accept_into_vec2_u32(Vec4(0,0,0,0));
        accept_asref_vec2_u32(Vec4(0,0,0,0));
        accept_into_vec2_u32((0,0));
        // These purposefully won't compile right now.
        // accept_asref_vec2_u32((0,0,0,0));
        // accept_into_vec2_u32((0,0,0,0));
    }
}

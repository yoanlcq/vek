//! Vector types.

use core::fmt::{self, Display, Formatter};

macro_rules! vec_declare_types {
    (#[$attrs:meta]) => {
        /// A two-components generic vector type.
        ///
        /// - If you intend to use it as spatial coordinates, consider using [Xy](struct.Xy.html) instead.
        /// - If you intend to use it as texture coordinates, consider using [Uv](struct.Uv.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Vec2<T>(pub T, pub T);
        /// A three-components generic vector type.
        ///
        /// - If you intend to use it as spatial coordinates, consider using [Xyz](struct.Xyz.html) instead.
        /// - If you intend to use it as RGB color data, consider using [Rgb](struct.Rgb.html) instead.
        /// - If you intend to use it as texture coordinates, consider using [Uvw](struct.Uvw.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Vec3<T>(pub T, pub T, pub T);
        /// A four-components generic vector type.
        ///
        /// - If you intend to use it as homogeneous spatial coordinates, consider using [Xyzw](struct.Xyzw.html) instead.
        /// - If you intend to use it as RGBA color data, consider using [Rgba](struct.Rgba.html) instead.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Vec4<T>(pub T, pub T, pub T, pub T);

        /// An eight-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX2-enabled x86 CPUs, a `Vec8<i32>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Vec8<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A sixteen-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX2-enabled x86 CPUs, a `Vec16<i16>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Vec16<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A thirty-two-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX512-enabled x86 CPUs, a `Vec32<i16>` makes sense.  
        /// Otherwise, LLVM lowers it to a fixed-sized array of whichever "best" SIMD vector type is available.  
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Vec32<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);

        /// A sixty-four-components generic vector type.
        ///
        /// This type exists mostly for crunching arrays of values.  
        /// For instance, on AVX512-enabled x86 CPUs, a `Vec64<i8>` makes sense.  
        /// Otherwise, LLVM is able to process it as a fixed-sized array of whichever "best" SIMD vector type available.  
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Vec64<T>(pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T, pub T);


        /// Vector type suited for homogeneous 3D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Xyzw<T> { pub x:T, pub y:T, pub z:T, pub w:T }
        /// Vector type suited for 3D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Xyz<T> { pub x:T, pub y:T, pub z:T }
        /// Vector type suited for 2D spatial coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
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
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
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
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Extent2<T> { pub w:T, pub h:T }


        /// Vector type suited for RGBA color data.
        ///
        /// There is no trait bound on `ColorComponent`, but if `T` doesn't implement it, you'll
        /// miss some goodies.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Rgba<T> { pub r:T, pub g:T, pub b:T, pub a:T }
        /// Vector type suited for RGB color data.
        ///
        /// There is no trait bound on `ColorComponent`, but if `T` doesn't implement it, you'll
        /// miss some goodies.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Rgb<T> { pub r:T, pub g:T, pub b:T }

        /// Vector type suited for 3D texture coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Uvw<T> { pub u:T, pub v:T, pub w:T }
        /// Vector type suited for 2D texture coordinates.
        #[allow(missing_docs)]
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[$attrs]
        pub struct Uv<T> { pub u:T, pub v:T }

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



pub mod repr_c {
    //! Vector types which are marked `#[repr(packed, C)]`.

    use super::*;
    vec_declare_types!{#[repr(packed, C)]}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Vector types which are marked `#[repr(packed, simd)]`.

    use super::*;
    vec_declare_types!{#[repr(packed, simd)]}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

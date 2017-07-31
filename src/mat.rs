//! Matrix types.

//use core::mem;
use vec;

macro_rules! mat_declare_types {
    ($lines:ident) => {
        /// 4x4 matrix.
        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Mat4<T> {
            #[allow(missing_docs)]
            pub $lines: CVec4<Vec4<T>>,
        }
        /// 3x3 matrix.
        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Mat3<T> {
            #[allow(missing_docs)]
            pub $lines: CVec3<Vec3<T>>,
        }
        /// 2x2 matrix.
        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Mat2<T> {
            #[allow(missing_docs)]
            pub $lines: CVec2<Vec2<T>>,
        }

        // TODO impl Display
    }
}

/*
macro_rules! mat_impl_identity {
    (impl $Mat:ident<$T:ty> { lines: $lines:ident, }) => {
        /*
        impl Default for $Mat<$T> {
            fn default() -> Self {
                Self::identity()
            }
        }
        impl $Mat<$T> {
            pub fn identity() -> Self {
                let out = Self::zero();
                $($(out.$lines.$get)+ .$get = $one;)+)+
                out
            }
        }
        */
        impl $Mat<$T> {
            pub fn zero() -> Self {
                Self { $lines: $CVec::zero() }
            }
        }
    }
}
*/

macro_rules! mat_declare_modules {
    () => {
        pub mod column_major {
            //! Column-major matrices.

            use super::*;
            mat_declare_types!{cols}
        }
        pub mod row_major {
            //! Row-major matrices.

            use super::*;
            mat_declare_types!{rows}
        }
        pub use column_major::*;
    }
}

pub mod repr_c {
    //! Matrix types which use `#[repr(packed, C)]` vectors exclusively.

    use super::vec::repr_c::{Vec2, Vec3, Vec4};
    use super::vec::repr_c::{Vec2 as CVec2, Vec3 as CVec3, Vec4 as CVec4};

    mat_declare_modules!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Matrix types which use a `#[repr(packed, C)]` vector of `#[repr(packed, simd)]` vectors.

    use super::vec::repr_simd::{Vec2, Vec3, Vec4};
    use super::vec::repr_c::{Vec2 as CVec2, Vec3 as CVec3, Vec4 as CVec4};

    mat_declare_modules!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

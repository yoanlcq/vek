//! Matrix types.

use vec;

pub mod repr_c {
    //! Matrix types which use `#[repr(packed, C)]` vectors exclusively.

    use super::vec;

    pub mod column_major {
        //! Column-major matrices.

        use super::vec::repr_c::Vec4;
        use super::vec::repr_c::Vec4 as CVec4;

        /// This type is a placeholder.
        pub struct Mat4<T> {
            /// The matrix's columns.
            pub cols: CVec4<Vec4<T>>,
        }
    }
    pub mod row_major {
        //! Row-major matrices.

        use super::vec::repr_c::Vec4;
        use super::vec::repr_c::Vec4 as CVec4;

        /// This type is a placeholder.
        pub struct Mat4<T> {
            /// The matrix's rows.
            pub rows: CVec4<Vec4<T>>,
        }
    }
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Matrix types which use a `#[repr(packed, C)]` vector of `#[repr(packed, simd)]` vectors.

    use super::vec;

    pub mod column_major {
        //! Column-major matrices.

        use super::vec::repr_simd::Vec4;
        use super::vec::repr_c::Vec4 as CVec4;

        /// This type is a placeholder.
        pub struct Mat4<T> {
            /// The matrix's columns.
            pub cols: CVec4<Vec4<T>>,
        }
    }
    pub mod row_major {
        //! Row-major matrices.

        use super::vec::repr_simd::Vec4;
        use super::vec::repr_c::Vec4 as CVec4;

        /// This type is a placeholder.
        pub struct Mat4<T> {
            /// The matrix's rows.
            pub rows: CVec4<Vec4<T>>,
        }
    }
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

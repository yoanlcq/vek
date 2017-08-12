//! Matrix types.

use core::mem;
use core::fmt::{self, Display, Formatter};
use num_traits::{Zero, One};
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
    }
}

macro_rules! mat_impl_mat {
    (row_major $Mat:ident $CVec:ident $Vec:ident ($($get:tt)+)) => {
        /// Displays this matrix using the following format:  
        ///
        /// (`i` being the number of rows and `j` the number of columns)
        ///
        /// ```text
        /// ( m00 ... m0j
        ///   ... ... ...
        ///   mi0 ... mij )
        /// ```
        ///
        /// This format doesn't depend on the matrix's storage layout.
        impl<T: Display> Display for $Mat<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "(")?;
                for row in &self.rows {
                    for elem in row {
                        write!(f, " {}", elem)?;
                    }
                    writeln!(f, "")?;
                    write!(f, " ")?;
                }
                write!(f, " )")
            }
        }
        mat_impl_mat!{common rows $Mat $CVec $Vec ($($get)+)}
    };
    (column_major $Mat:ident $CVec:ident $Vec:ident ($($get:tt)+)) => {
        /// Displays this matrix using the following format:  
        ///
        /// (`i` being the number of rows and `j` the number of columns)
        ///
        /// ```text
        /// ( m00 ... m0j
        ///   ... ... ...
        ///   mi0 ... mij )
        /// ```
        ///
        /// This format doesn't depend on the matrix's storage layout.
        impl<T: Display> Display for $Mat<T> {
            fn fmt(&self, _f: &mut Formatter) -> fmt::Result {
                //write!(f, "{}", self.transposed())
                unimplemented!()
            }
        }
        mat_impl_mat!{common cols $Mat $CVec $Vec ($($get)+)}
    };
    (common $lines:ident $Mat:ident $CVec:ident $Vec:ident ($($get:tt)+)) => {
        /// The default value for a square matrix is the identity.
        ///
        /// ```
        /// assert_eq!(Mat4::<f32>::default(), Mat4::<f32>::identity());
        /// ```
        impl<T: Zero + One> Default for $Mat<T> {
            fn default() -> Self {
                Self::identity()
            }
        }
        impl<T> $Mat<T> {
            /// The identity matrix, which is also the default value for square matrices.
            ///
            /// ```
            /// assert_eq!(Mat4::<f32>::default(), Mat4::<f32>::identity());
            /// ```
            pub fn identity() -> Self where T: Zero + One {
                let mut out = Self::zero();
                $(out.$lines.$get.$get = T::one();)+
                out
            }
            /// The matrix with all elements set to zero.
            pub fn zero() -> Self where T: Zero {
                let mut out: Self = unsafe { mem::uninitialized() };
                $(out.$lines.$get = $Vec::zero();)+
                out
            }
            /// The matrix's transpose.
            pub fn transposed(self) -> Self {
                unimplemented!()
            }
            /// Transpose this matrix.
            pub fn transpose(&mut self) {
                unimplemented!()
            }
        }
    };
}

macro_rules! mat_impl_all_mats {
    ($layout:ident) => {
        mat_impl_mat!{$layout Mat2 CVec2 Vec2 (0 1)}
        // TODO uncomment stuff below when I'm done with implementations
        // mat_impl_mat!{$layout Mat3 CVec3 Vec3 (0 1 2)}
        // mat_impl_mat!{$layout Mat4 CVec4 Vec4 (0 1 2 3)}
    }
}

macro_rules! mat_declare_modules {
    () => {
        pub mod column_major {
            //! Matrices stored in column-major layout.
            //!
            //! Multiplying a column-major matrix by one or more column vectors is fast
            //! due to the way it is implemented in SIMD 
            //! (a `matrix * vector` mutliply is four broadcasts, one SIMD product and three fused-multiply-adds).  
            //!
            //! Because `matrix * matrix` and `matrix * vector` products are fastest with this layout,
            //! it's the preferred one for most computer graphics libraries and application.

            use super::*;
            mat_declare_types!{cols}
            mat_impl_all_mats!{column_major}
        }
        pub mod row_major {
            //! Matrices stored in row-major layout.
            //!
            //! Unlike the column-major layout, row-major matrices are good at being the
            //! right-hand-side of a product with one or more row vectors.
            //!
            //! Also, their indexing order matches the existing mathematical conventions.

            use super::*;
            mat_declare_types!{rows}
            mat_impl_all_mats!{row_major}
        }
        pub use column_major::*;
    }
}

pub mod repr_c {
    //! Matrix types which use `#[repr(packed, C)]` vectors exclusively.
    //! 
    //! See also the `repr_simd` neighbour module, which is available on Nightly
    //! with the `repr_simd` feature enabled.
    //!
    //! You can instantiate any matrix type from this module with any type T.

    use super::*;
    use super::vec::repr_c::{Vec2, Vec3, Vec4};
    use super::vec::repr_c::{Vec2 as CVec2, Vec3 as CVec3, Vec4 as CVec4};

    mat_declare_modules!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Matrix types which use a `#[repr(packed, C)]` vector of `#[repr(packed, simd)]` vectors.
    //!
    //! You can instantiate any matrix type from this module with any type T if
    //! and only if T is one of the "machine types".  
    //! These include `f32` and `i32`, but not `isize` or
    //! newtypes (normally, unless they're `#[repr(transparent)]`, but this hasn't been tested).
    
    use super::*;
    use super::vec::repr_simd::{Vec2, Vec3, Vec4};
    use super::vec::repr_c::{Vec2 as CVec2, Vec3 as CVec3, Vec4 as CVec4};

    mat_declare_modules!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
/// If you're on Nightly with the `repr_simd` feature enabled, this exports `self::repr_simd::*` instead.
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

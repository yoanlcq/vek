//! Matrix types.

//use core::mem;
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
    (row_major $Mat:ident ($($get:tt)+)) => {
        /// Displays this matrix as: (`i` being the number of rows and `j` the number of columns)
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
        mat_impl_mat!{common rows $Mat ($($get)+)}
    };
    (column_major $Mat:ident ($($get:tt)+)) => {
        /// Displays this matrix as: (`i` being the number of rows and `j` the number of columns)
        /// ```text
        /// ( m00 ... m0j
        ///   ... ... ...
        ///   mi0 ... mij )
        /// ```
        ///
        /// This format doesn't depend on the matrix's storage layout.
        impl<T: Display> Display for $Mat<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "{}", self.transposed())
            }
        }
        mat_impl_mat!{common cols $Mat ($($get)+)}
    };
    (common $lines:ident $Mat:ident ($($get:tt)+)) => {
        /// The default value for a square matrix is the identity.
        ///
        /// ```
        /// assert_eq!(Mat4::default(), Mat4::identity());
        /// ```
        impl<T: Zero + One + PartialEq> Default for $Mat<T> {
            fn default() -> Self {
                Self::identity()
            }
        }
        impl<T> $Mat<T> {
            /// The identity matrix.
            pub fn identity() -> Self where T: Zero + One + PartialEq {
                let mut out = Self::zero();
                $(out.$lines.$get.$get = T::one();)+
                out
            }
            /// The matrix with all elements set to zero.
            pub fn zero() -> Self where T: Zero + PartialEq {
                Self { $lines: Zero::zero() }
            }
            /// The matrix's transpose.
            pub fn transposed(&self) -> Self {
                unimplemented!()
            }
        }
    };
}

macro_rules! mat_impl_all_mats {
    ($layout:ident) => {
        mat_impl_mat!{$layout Mat2 (0 1)}
        mat_impl_mat!{$layout Mat3 (0 1 2)}
        mat_impl_mat!{$layout Mat4 (0 1 2 3)}
    }
}

macro_rules! mat_declare_modules {
    () => {
        pub mod column_major {
            //! Matrices stored in column-major order.

            use super::*;
            mat_declare_types!{cols}
            mat_impl_all_mats!{column_major}
        }
        pub mod row_major {
            //! Matrices stored in row-major order.

            use super::*;
            mat_declare_types!{rows}
            mat_impl_all_mats!{row_major}
        }
        pub use column_major::*;
    }
}

pub mod repr_c {
    //! Matrix types which use `#[repr(packed, C)]` vectors exclusively.

    use super::*;
    use super::vec::repr_c::{Vec2, Vec3, Vec4};
    use super::vec::repr_c::{Vec2 as CVec2, Vec3 as CVec3, Vec4 as CVec4};

    mat_declare_modules!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Matrix types which use a `#[repr(packed, C)]` vector of `#[repr(packed, simd)]` vectors.

    use super::*;
    use super::vec::repr_simd::{Vec2, Vec3, Vec4};
    use super::vec::repr_c::{Vec2 as CVec2, Vec3 as CVec3, Vec4 as CVec4};

    mat_declare_modules!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

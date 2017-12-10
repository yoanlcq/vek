//! Matrix types.

use core::mem;
use core::ptr;
use core::iter::Sum;
use core::fmt::{self, Display, Formatter};
use core::ops::*;
use num_traits::{Zero, One, Float};
use ops::MulAdd;
use vec;
#[cfg(feature="geom")]
use geom::{FrustumPlanes, Rect};
#[cfg(feature="quaternion")]
use quaternion;

macro_rules! mat_impl_mat {
    (rows $Mat:ident $CVec:ident $Vec:ident ($nrows:tt x $ncols:tt) ($($get:tt)+)) => {

        mat_impl_mat!{common rows $Mat $CVec $Vec ($nrows x $ncols) ($($get)+)}

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

        impl $Mat<f32> {
            pub fn as_gl_uniform_params(&self) -> (u32, *const f32) {
                (0, self.rows.as_ptr() as _)
            }
        }

        use super::column_major::$Mat as Transpose;

        /// Multiplies a row vector with a row-major matrix, giving a row vector.
        ///
        /// With SIMD vectors, this is the most efficient way.
        ///
        /// ```
        /// use vek::mat::row_major::Mat4;
        /// use vek::vec::Vec4;
        ///
        /// let m = Mat4::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let v = Vec4::new(0, 1, 2, 3);
        /// let r = Vec4::new(26, 32, 18, 24);
        /// assert_eq!(v * m, r);
        /// ```
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Clone> Mul<$Mat<T>> for $Vec<T> {
            type Output = Self;
            fn mul(self, rhs: $Mat<T>) -> Self::Output {
                let mut out = rhs.rows[0].clone() * $Vec::broadcast(self[0].clone());
                for i in 1..$nrows {
                    out = rhs.rows[i].clone().mul_add($Vec::broadcast(self[i].clone()), out);
                }
                out
            }
        }
        /// Multiplies a row-major matrix with a column vector, giving a column vector.
        ///
        /// ```
        /// use vek::mat::row_major::Mat4;
        /// use vek::vec::Vec4;
        ///
        /// let m = Mat4::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let v = Vec4::new(0, 1, 2, 3);
        /// let r = Vec4::new(14, 38, 12, 26);
        /// assert_eq!(m * v, r);
        /// ```
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Clone> Mul<$Vec<T>> for $Mat<T> {
            type Output = $Vec<T>;
            fn mul(self, v: $Vec<T>) -> Self::Output {
                // PERF: This transposes the matrix, but we could do better.
                Transpose::from(self) * v
            }
        }
        /// Multiplies a row-major matrix with another.
        ///
        /// ```
        /// use vek::mat::row_major::Mat4;
        ///
        /// let m = Mat4::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let r = Mat4::new(
        ///     26, 32, 18, 24,
        ///     82, 104, 66, 88,
        ///     38, 56, 74, 92,
        ///     54, 68, 42, 56
        /// );
        /// assert_eq!(m * m, r);
        /// assert_eq!(m, m * Mat4::identity());
        /// assert_eq!(m, Mat4::identity() * m);
        /// ```
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Clone> Mul for $Mat<T> {
            type Output = Self;
            fn mul(self, rhs: Self) -> Self::Output {
                Self {
                    rows: $CVec {
                        $($get: self.rows.$get.clone() * rhs.clone(),)+
                    }
                }
            }
        }
        /// Multiplies a row-major matrix with a column-major matrix, 
        /// producing a column-major matrix.
        ///
        /// ```
        /// use vek::mat::row_major::Mat4 as Rows4;
        /// use vek::mat::column_major::Mat4 as Cols4;
        ///
        /// let m = Rows4::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let b = Cols4::from(m);
        /// let r = Cols4::new(
        ///     26, 32, 18, 24,
        ///     82, 104, 66, 88,
        ///     38, 56, 74, 92,
        ///     54, 68, 42, 56
        /// );
        /// assert_eq!(m * b, r);
        /// assert_eq!(m * Cols4::identity(), m.into());
        /// assert_eq!(Rows4::identity() * b, m.into());
        /// ```
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Clone> Mul<Transpose<T>> for $Mat<T> {
            type Output = Transpose<T>;
            fn mul(self, rhs: Transpose<T>) -> Self::Output {
                // PERF: This transposes the matrix, but we could do better.
                Transpose::from(self) * rhs
            }
        }
        impl<T> From<Transpose<T>> for $Mat<T> {
            fn from(m: Transpose<T>) -> Self {
                Self {
                    rows: m.transposed().cols
                }
            }
        }
    };
    (cols $Mat:ident $CVec:ident $Vec:ident ($nrows:tt x $ncols:tt) ($($get:tt)+)) => {

        mat_impl_mat!{common cols $Mat $CVec $Vec ($nrows x $ncols) ($($get)+)}

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
                for y in 0..$nrows {
                    for x in 0..$ncols {
                        let elem = unsafe {
                            self.cols.get_unchecked(x).get_unchecked(y)
                        };
                        write!(f, " {}", elem)?;
                    }
                    writeln!(f, "")?;
                    write!(f, " ")?;
                }
                write!(f, " )")
            }
        }

        impl $Mat<f32> {
            pub fn as_gl_uniform_params(&self) -> (u32, *const f32) {
                (1, self.cols.as_ptr() as _)
            }
        }

        use super::row_major::$Mat as Transpose;

        /// Multiplies a row vector with a column-major matrix, giving a row vector.
        ///
        /// ```
        /// use vek::mat::column_major::Mat4;
        /// use vek::vec::Vec4;
        ///
        /// let m = Mat4::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let v = Vec4::new(0, 1, 2, 3);
        /// let r = Vec4::new(26, 32, 18, 24);
        /// assert_eq!(v * m, r);
        /// ```
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Clone> Mul<$Mat<T>> for $Vec<T> {
            type Output = Self;
            fn mul(self, rhs: $Mat<T>) -> Self::Output {
                // PERF: This transposes the matrix, but we could do better.
                self * Transpose::from(rhs)
            }
        }
        /// Multiplies a column-major matrix with a column vector, giving a column vector.
        ///
        /// With SIMD vectors, this is the most efficient way.
        ///
        /// ```
        /// use vek::mat::column_major::Mat4;
        /// use vek::vec::Vec4;
        ///
        /// let m = Mat4::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let v = Vec4::new(0, 1, 2, 3);
        /// let r = Vec4::new(14, 38, 12, 26);
        /// assert_eq!(m * v, r);
        /// ```
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Clone> Mul<$Vec<T>> for $Mat<T> {
            type Output = $Vec<T>;
            fn mul(self, v: $Vec<T>) -> Self::Output {
                let mut out = self.cols[0].clone() * $Vec::broadcast(v[0].clone());
                for i in 1..$ncols {
                    out = self.cols[i].clone().mul_add($Vec::broadcast(v[i].clone()), out);
                }
                out
            }
        }
        /// Multiplies a column-major matrix with another.
        ///
        /// ```
        /// use vek::mat::column_major::Mat4;
        ///
        /// let m = Mat4::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let r = Mat4::new(
        ///     26, 32, 18, 24,
        ///     82, 104, 66, 88,
        ///     38, 56, 74, 92,
        ///     54, 68, 42, 56
        /// );
        /// assert_eq!(m * m, r);
        /// assert_eq!(m, m * Mat4::identity());
        /// assert_eq!(m, Mat4::identity() * m);
        /// ```
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Clone> Mul for $Mat<T> {
            type Output = Self;
            fn mul(self, rhs: Self) -> Self::Output {
                Self {
                    cols: $CVec {
                        $($get: self.clone() * rhs.cols.$get.clone(),)+
                    }
                }
            }
        }
        /// Multiplies a column-major matrix with a row-major matrix.
        ///
        /// ```
        /// use vek::mat::row_major::Mat4 as Rows4;
        /// use vek::mat::column_major::Mat4 as Cols4;
        ///
        /// let m = Cols4::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let b = Rows4::from(m);
        /// let r = Rows4::new(
        ///     26, 32, 18, 24,
        ///     82, 104, 66, 88,
        ///     38, 56, 74, 92,
        ///     54, 68, 42, 56
        /// );
        /// assert_eq!(m * b, r);
        /// assert_eq!(m * Rows4::identity(), m.into());
        /// assert_eq!(Cols4::identity() * b, m.into());
        /// ```
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Clone> Mul<Transpose<T>> for $Mat<T> {
            type Output = Transpose<T>;
            fn mul(self, rhs: Transpose<T>) -> Self::Output {
                // PERF: This transposes the matrix, but we could do better.
                Transpose::from(self) * rhs
            }
        }
        impl<T> From<Transpose<T>> for $Mat<T> {
            fn from(m: Transpose<T>) -> Self {
                Self {
                    cols: m.transposed().rows
                }
            }
        }

    };
    (common $lines:ident $Mat:ident $CVec:ident $Vec:ident ($nrows:tt x $ncols:tt) ($($get:tt)+)) => {
        /// The default value for a square matrix is the identity.
        ///
        /// ```
        /// # use vek::mat::Mat4;
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
            /// # use vek::mat::Mat4;
            /// assert_eq!(Mat4::<f32>::default(), Mat4::<f32>::identity());
            /// ```
            pub fn identity() -> Self where T: Zero + One {
                let mut out = Self::zero();
                $(out.$lines.$get.$get = T::one();)+
                out
            }
            /// The matrix with all elements set to zero.
            pub fn zero() -> Self where T: Zero {
                Self {
                    $lines: $CVec {
                        $($get: $Vec::zero(),)+
                    }
                }
            }
            /// The matrix's transpose.
            ///
            /// ```
            /// # use vek::mat::Mat4;
            ///
            /// let m = Mat4::new(
            ///     0, 1, 2, 3,
            ///     4, 5, 6, 7,
            ///     8, 9, 0, 1,
            ///     2, 3, 4, 5
            /// );
            /// let t = Mat4::new(
            ///     0, 4, 8, 2,
            ///     1, 5, 9, 3,
            ///     2, 6, 0, 4,
            ///     3, 7, 1, 5
            /// );
            /// assert_eq!(m.transposed(), t);
            /// assert_eq!(m, m.transposed().transposed());
            /// ```
            pub fn transposed(self) -> Self {
                // PERF: This implementation sucks!!
                let mut out: Self = unsafe { mem::uninitialized() };
                for y in 0..$nrows {
                    for x in 0..$ncols {
                        unsafe {
                            let e = self.$lines.get_unchecked(y).get_unchecked(x);
                            *out.$lines.get_unchecked_mut(x).get_unchecked_mut(y) = ptr::read(e);
                            // ^ ptr::read is safe because we consume the matrix, therefore its
                            // elements.
                        }
                    }
                }
                out
            }
            /// Transpose this matrix.
            ///
            /// ```
            /// # use vek::mat::Mat4;
            ///
            /// let mut m = Mat4::new(
            ///     0, 1, 2, 3,
            ///     4, 5, 6, 7,
            ///     8, 9, 0, 1,
            ///     2, 3, 4, 5
            /// );
            /// let t = Mat4::new(
            ///     0, 4, 8, 2,
            ///     1, 5, 9, 3,
            ///     2, 6, 0, 4,
            ///     3, 7, 1, 5
            /// );
            /// m.transpose();
            /// assert_eq!(m, t);
            /// ```
            pub fn transpose(&mut self) {
                unsafe {
                    *self = ptr::read(self).transposed();
                }
            }
            /// Multiply elements of this matrix with another's.
            ///
            /// ```
            /// # use vek::mat::Mat4;
            ///
            /// let m = Mat4::new(
            ///     0, 1, 2, 3,
            ///     1, 2, 3, 4,
            ///     2, 3, 4, 5,
            ///     3, 4, 5, 6,
            /// );
            /// let r = Mat4::new(
            ///     0, 1, 4, 9,
            ///     1, 4, 9, 16,
            ///     4, 9, 16, 25,
            ///     9, 16, 25, 36,
            /// );
            /// assert_eq!(m.mul_memberwise(m), r);
            /// ```
            pub fn mul_memberwise(self, m: Self) -> Self where T: Mul<Output=T> {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get * m.$lines.$get),+
                    )
                }
            }
            pub fn row_count(&self) -> usize {
                $nrows
            }
            pub fn col_count(&self) -> usize {
                $ncols
            }
        }

        impl<T> Mul<T> for $Mat<T> 
            where T: Clone + Zero + Add<Output=T> + Mul<Output=T>
        {
            type Output = Self;
            fn mul(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get * rhs.clone()),+
                    )
                }
            }
        }

        impl<T> MulAssign for $Mat<T>
            where T: Clone + Zero + Add<Output=T> + Mul<Output=T> + MulAdd<T,T,Output=T>
        { 
            fn mul_assign(&mut self, rhs: Self) { *self = self.clone() * rhs; }
        }

        impl<T> MulAssign<T> for $Mat<T>
            where T: Clone + Zero + Add<Output=T> + Mul<Output=T>
        { 
            fn mul_assign(&mut self, rhs: T) { *self = self.clone() * rhs; }
        }


        impl<T> Add for $Mat<T> where T: Add<Output=T> {
            type Output = Self;
            fn add(self, rhs: Self) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get + rhs.$lines.$get),+
                    )
                }
            }
        }
        impl<T> Sub for $Mat<T> where T: Sub<Output=T> {
            type Output = Self;
            fn sub(self, rhs: Self) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get - rhs.$lines.$get),+
                    )
                }
            }
        }
        impl<T> Div for $Mat<T> where T: Div<Output=T> {
            type Output = Self;
            fn div(self, rhs: Self) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get / rhs.$lines.$get),+
                    )
                }
            }
        }
        impl<T> Rem for $Mat<T> where T: Rem<Output=T> {
            type Output = Self;
            fn rem(self, rhs: Self) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get % rhs.$lines.$get),+
                    )
                }
            }
        }
        impl<T> Neg for $Mat<T> where T: Neg<Output=T> {
            type Output = Self;
            fn neg(self) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(-self.$lines.$get),+
                    )
                }
            }
        }

        impl<T> Add<T> for $Mat<T> where T: Clone + Add<Output=T> {
            type Output = Self;
            fn add(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get + rhs.clone()),+
                    )
                }
            }
        }
        impl<T> Sub<T> for $Mat<T> where T: Clone + Sub<Output=T> {
            type Output = Self;
            fn sub(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get - rhs.clone()),+
                    )
                }
            }
        }
        impl<T> Div<T> for $Mat<T> where T: Clone + Div<Output=T> {
            type Output = Self;
            fn div(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get / rhs.clone()),+
                    )
                }
            }
        }
        impl<T> Rem<T> for $Mat<T> where T: Clone + Rem<Output=T> {
            type Output = Self;
            fn rem(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get % rhs.clone()),+
                    )
                }
            }
        }

        impl<T: Add<Output=T> + Clone> AddAssign    for $Mat<T> { fn add_assign(&mut self, rhs: Self) { *self = self.clone() + rhs; } }
        impl<T: Add<Output=T> + Clone> AddAssign<T> for $Mat<T> { fn add_assign(&mut self, rhs: T   ) { *self = self.clone() + rhs; } }
        impl<T: Sub<Output=T> + Clone> SubAssign    for $Mat<T> { fn sub_assign(&mut self, rhs: Self) { *self = self.clone() - rhs; } }
        impl<T: Sub<Output=T> + Clone> SubAssign<T> for $Mat<T> { fn sub_assign(&mut self, rhs: T   ) { *self = self.clone() - rhs; } }
        impl<T: Div<Output=T> + Clone> DivAssign    for $Mat<T> { fn div_assign(&mut self, rhs: Self) { *self = self.clone() / rhs; } }
        impl<T: Div<Output=T> + Clone> DivAssign<T> for $Mat<T> { fn div_assign(&mut self, rhs: T   ) { *self = self.clone() / rhs; } }
        impl<T: Rem<Output=T> + Clone> RemAssign    for $Mat<T> { fn rem_assign(&mut self, rhs: Self) { *self = self.clone() % rhs; } }
        impl<T: Rem<Output=T> + Clone> RemAssign<T> for $Mat<T> { fn rem_assign(&mut self, rhs: T   ) { *self = self.clone() % rhs; } }

    };
}


macro_rules! mat_impl_mat4 {
    (rows) => {

        mat_impl_mat4!{common rows}

        impl<T> Mat4<T> {
            /// Creates a new 4x4 matrix from elements in a layout-agnostic way.
            ///
            /// The parameters are named `mij` where `i` is the row index and
            /// `j` the column index. Their order is always the same regardless
            /// of the matrix's layout.
            pub fn new(
                m00: T, m01: T, m02: T, m03: T,
                m10: T, m11: T, m12: T, m13: T,
                m20: T, m21: T, m22: T, m23: T,
                m30: T, m31: T, m32: T, m33: T
            ) -> Self {
                Self {
                    rows: CVec4::new(
                        Vec4::new(m00, m01, m02, m03),
                        Vec4::new(m10, m11, m12, m13),
                        Vec4::new(m20, m21, m22, m23),
                        Vec4::new(m30, m31, m32, m33)
                    )
                }
            }
        }
    };
    (cols) => {

        mat_impl_mat4!{common cols}

        impl<T> Mat4<T> {
            /// Creates a new 4x4 matrix from elements in a layout-agnostic way.
            ///
            /// The parameters are named `mij` where `i` is the row index and
            /// `j` the column index. Their order is always the same regardless
            /// of the matrix's layout.
            pub fn new(
                m00: T, m01: T, m02: T, m03: T,
                m10: T, m11: T, m12: T, m13: T,
                m20: T, m21: T, m22: T, m23: T,
                m30: T, m31: T, m32: T, m33: T
            ) -> Self {
                Self {
                    cols: CVec4::new(
                        Vec4::new(m00, m10, m20, m30),
                        Vec4::new(m01, m11, m21, m31),
                        Vec4::new(m02, m12, m22, m32),
                        Vec4::new(m03, m13, m23, m33)
                    )
                }
            }
        }
    };
    (common $lines:ident) => {

        use super::row_major::Mat4 as Rows4;

        impl<T> Mat4<T> {

            //
            // BASIC
            //

            // Taken verbatim from datenwolf's linmath.h
            // As mentioned in the original, it assumes that the matrix is invertible.
            pub fn inverted(self) -> Self where T: Float
            {
                let mut m = Rows4::from(self).rows;
                let s = [
                    m[0][0]*m[1][1] - m[1][0]*m[0][1],
                    m[0][0]*m[1][2] - m[1][0]*m[0][2],
                    m[0][0]*m[1][3] - m[1][0]*m[0][3],
                    m[0][1]*m[1][2] - m[1][1]*m[0][2],
                    m[0][1]*m[1][3] - m[1][1]*m[0][3],
                    m[0][2]*m[1][3] - m[1][2]*m[0][3],
                ];
                let c = [
                    m[2][0]*m[3][1] - m[3][0]*m[2][1],
                    m[2][0]*m[3][2] - m[3][0]*m[2][2],
                    m[2][0]*m[3][3] - m[3][0]*m[2][3],
                    m[2][1]*m[3][2] - m[3][1]*m[2][2],
                    m[2][1]*m[3][3] - m[3][1]*m[2][3],
                    m[2][2]*m[3][3] - m[3][2]*m[2][3],
                ];
                
                let idet = T::one() / ( s[0]*c[5]-s[1]*c[4]+s[2]*c[3]+s[3]*c[2]-s[4]*c[1]+s[5]*c[0] );
                
                m[0][0] = ( m[1][1] * c[5] - m[1][2] * c[4] + m[1][3] * c[3]) * idet;
                m[0][1] = (-m[0][1] * c[5] + m[0][2] * c[4] - m[0][3] * c[3]) * idet;
                m[0][2] = ( m[3][1] * s[5] - m[3][2] * s[4] + m[3][3] * s[3]) * idet;
                m[0][3] = (-m[2][1] * s[5] + m[2][2] * s[4] - m[2][3] * s[3]) * idet;
                m[1][0] = (-m[1][0] * c[5] + m[1][2] * c[2] - m[1][3] * c[1]) * idet;
                m[1][1] = ( m[0][0] * c[5] - m[0][2] * c[2] + m[0][3] * c[1]) * idet;
                m[1][2] = (-m[3][0] * s[5] + m[3][2] * s[2] - m[3][3] * s[1]) * idet;
                m[1][3] = ( m[2][0] * s[5] - m[2][2] * s[2] + m[2][3] * s[1]) * idet;
                m[2][0] = ( m[1][0] * c[4] - m[1][1] * c[2] + m[1][3] * c[0]) * idet;
                m[2][1] = (-m[0][0] * c[4] + m[0][1] * c[2] - m[0][3] * c[0]) * idet;
                m[2][2] = ( m[3][0] * s[4] - m[3][1] * s[2] + m[3][3] * s[0]) * idet;
                m[2][3] = (-m[2][0] * s[4] + m[2][1] * s[2] - m[2][3] * s[0]) * idet;
                m[3][0] = (-m[1][0] * c[3] + m[1][1] * c[1] - m[1][2] * c[0]) * idet;
                m[3][1] = ( m[0][0] * c[3] - m[0][1] * c[1] + m[0][2] * c[0]) * idet;
                m[3][2] = (-m[3][0] * s[3] + m[3][1] * s[1] - m[3][2] * s[0]) * idet;
                m[3][3] = ( m[2][0] * s[3] - m[2][1] * s[1] + m[2][2] * s[0]) * idet;

                Rows4 { rows: m }.into()
            }

            pub fn orthonormalized(self) -> Self where T: Float + Sum + SubAssign {
                let mut r = Rows4::from(self).rows;

                r[2] = r[2].normalized();

                let s = r[1].dot(r[2]);
                let h = r[2] * s;
                r[1] -= h;

                r[1] -= h;
                r[1] = r[1].normalized();

                let s = r[0].dot(r[1]);
                let h = r[1] * s;
                r[0] -= h;
                r[0] = r[0].normalized();

                Rows4 { rows: r }.into()
            }

            //
            // TRANSFORMS
            //

            #[cfg(feature="vec2")]
            pub fn translation_2d<V: Into<Vec2<T>>>(v: V) -> Self where T: Zero + One {
                let Vec2 { x, y } = v.into();
                Self::new(
                    T::one() , T::zero(), T::zero(), x,
                    T::zero(), T::one() , T::zero(), y,
                    T::zero(), T::zero(), T::one() , T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one(),
                )
            }
            #[cfg(feature="vec3")]
            pub fn translation_3d<V: Into<Vec3<T>>>(v: V) -> Self where T: Zero + One {
                let Vec3 { x, y, z } = v.into();
                Self::new(
                    T::one() , T::zero(), T::zero(), x,
                    T::zero(), T::one() , T::zero(), y,
                    T::zero(), T::zero(), T::one() , z,
                    T::zero(), T::zero(), T::zero(), T::one(),
                )
            }
            #[cfg(feature="vec3")]
            pub fn translate_in_place_3d<V: Into<Vec3<T>>>(&mut self, v: V) where T: Clone + Zero + One + AddAssign + Sum {
                let Vec3 { x, y, z } = v.into();
                let t = Vec4 { x, y, z, w: T::zero() };
                let mut rows = Rows4::from(self.clone()).rows;
                rows[3][0] += rows[0].clone().dot(t.clone());
                rows[3][1] += rows[1].clone().dot(t.clone());
                rows[3][2] += rows[2].clone().dot(t.clone());
                rows[3][3] += rows[3].clone().dot(t.clone());
                *self = Rows4 { rows }.into();
            }

            #[cfg(feature="vec3")]
            pub fn scaling_3d<V: Into<Vec3<T>>>(v: V) -> Self where T: Zero + One {
                let Vec3 { x, y, z } = v.into();
                Self::new(
                    x, T::zero(), T::zero(), T::zero(),
                    T::zero(), y, T::zero(), T::zero(),
                    T::zero(), T::zero(), z, T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
            pub fn rotation_x(angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    T::one(), T::zero(), T::zero(), T::zero(),
                    T::zero(), c, -s, T::zero(),
                    T::zero(), s, c, T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
            pub fn rotation_y(angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, T::zero(), s, T::zero(),
                    T::zero(), T::one(), T::zero(), T::zero(),
                    -s, T::zero(), c, T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
            pub fn rotation_z(angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, -s, T::zero(), T::zero(),
                    s,  c, T::zero(), T::zero(),
                    T::zero(), T::zero(), T::one(), T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }

            #[cfg(feature="vec3")]
            pub fn rotation_3d<V: Into<Vec3<T>>>(angle_radians: T, axis: V) -> Self where T: Float + Sum {
                let Vec3 { x, y, z } = axis.into().normalized();
                let s = angle_radians.sin();
                let c = angle_radians.cos();
                let oc = T::one() - c;
                Self::new(
                    oc*x*x + c  , oc*x*y - z*s, oc*z*x + y*s, T::zero(),
                    oc*x*y + z*s, oc*y*y + c  , oc*y*z - x*s, T::zero(),
                    oc*z*x - y*s, oc*y*z + x*s, oc*z*z + c  , T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }

            //
            // VIEW
            //

            #[cfg(feature="vec3")]
            pub fn look_at<V: Into<Vec3<T>>>(eye: V, center: V, up: V) -> Self
                where T: Float + Sum + AddAssign
            {
                let (eye, center, up) = (eye.into(), center.into(), up.into());
                let f = (center - eye).normalized();
                let s = f.cross(up).normalized();
                let t = s.cross(f);
                let mut out = Self::new(
                    s.x, t.x, -f.x, T::zero(),
                    s.y, t.y, -f.y, T::zero(),
                    s.z, t.z, -f.z, T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                );
                out.translate_in_place_3d(-eye);
                out
            }

            //
            // PROJECTIONS
            //

            #[cfg(feature="geom")]
            pub fn orthographic (o: FrustumPlanes<T>) -> Self
                where T: Copy + Zero + One 
                       + Add<Output=T> + Sub<Output=T> + Neg<Output=T>
                       + Mul<Output=T> + Div<Output=T>
            {
                let FrustumPlanes { left: l, right: r, bottom: b, top: t, near: n, far: f } = o;
                let two = T::one() + T::one();
                Self::new(
                    two/(r-l), T::zero(), T::zero(), T::zero(),
                    T::zero(), two/(t-b), T::zero(), T::zero(),
                    T::zero(), T::zero(), two/(f-n), T::zero(),
                    -(r+l)/(r-l), -(t+b)/(t-b), -(f+n)/(f-n), T::one()
                )
            }
            pub fn perspective (fov_y_radians: T, aspect_ratio: T, near: T, far: T) -> Self 
                where T: Float
            {
                assert!(fov_y_radians > T::zero());
                let two = T::one() + T::one();
                let a = T::one() / ((fov_y_radians / two).tan());
                Self::new(
                    a / aspect_ratio, T::zero(), T::zero(), T::zero(),
                    T::zero(), a, T::zero(), T::zero(),
                    T::zero(), T::zero(), -((far + near) / (far - near)), T::one(),
                    T::zero(), T::zero(), -((two * far * near) / (far - near)), T::zero()
                )
            }
            // NOTE: Right-handed
            pub fn perspective_fov (fov_y_radians: T, width: T, height: T, near: T, far: T) -> Self 
                where T: Float
            {
                assert!(width > T::zero());
                assert!(height > T::zero());
                assert!(fov_y_radians > T::zero());

                let two = T::one() + T::one();
                let rad = fov_y_radians;
                let h = (rad/two).cos() / (rad/two).sin();
                let w = h * height / width;

                Self::new(
                    w, T::zero(), T::zero(), T::zero(),
                    T::zero(), h, T::zero(), T::zero(),
                    T::zero(), T::zero(), -(far + near) / (far - near), -T::one(),
                    T::zero(), T::zero(), -(two * far * near) / (far - near), T::zero()
                )
            }
            pub fn infinite_perspective (fov_y_radians: T, aspect_ratio: T, near: T) -> Self 
                where T: Float
            {
                Self::tweaked_infinite_perspective(fov_y_radians, aspect_ratio, near, T::zero())
            }

            pub fn tweaked_infinite_perspective (fov_y_radians: T, aspect_ratio: T, near: T, epsilon: T) -> Self 
                where T: Float
            {
                let two = T::one() + T::one();
                let range = (fov_y_radians / two).tan() * near;
                let left = -range * aspect_ratio;
                let right = range * aspect_ratio;
                let bottom = -range;
                let top = range;

                Self::new(
                    (two * near) / (right - left), T::zero(), T::zero(), T::zero(),
                    T::zero(), (two * near) / (top - bottom), T::zero(), T::zero(),
                    T::zero(), T::zero(), epsilon - T::one(), -T::one(),
                    T::zero(), T::zero(), (epsilon - two) * near, T::zero()
                )
            }
            #[cfg(feature="geom")]
            pub fn frustum (o: FrustumPlanes<T>) -> Self 
                where T: Copy + Zero + One 
                       + Add<Output=T> + Sub<Output=T> + Neg<Output=T>
                       + Mul<Output=T> + Div<Output=T>
            {
                let FrustumPlanes { left: l, right: r, bottom: b, top: t, near: n, far: f } = o;
                let two = T::one() + T::one();

                Self::new(
                    two*n/(r-l), T::zero(), T::zero(), T::zero(),
                    T::zero(), two*n/(t-b), T::zero(), T::zero(),
                    (r+l)/(r-l), (t+b)/(t-b), -(f+n)/(f-n), -T::one(),
                    T::zero(), T::zero(), -two*(f*n)/(f-n), T::zero()
                )
            }

            //
            // PICKING
            //

            // GLM's pickMatrix. Creates a projection matrix that can be
            // used to restrict drawing to a small region of the viewport.
            //
            // u16 is chosen as viewport units because f32 has a direct conversion from it.
            #[cfg(all(feature="vec3", feature="vec2", feature="geom"))]
            pub fn picking_region<V2: Into<Vec2<T>>>(center: V2, delta: V2, viewport: Rect<u16, u16>) -> Self
                where T: Zero + One + Copy + From<u16> + PartialOrd + Sub<Output=T> + Div<Output=T> + MulAdd<T,T,Output=T> + Mul<Output=T>
            {
                let (center, delta, viewport) = (center.into(), delta.into(), viewport.convert(|p| T::from(p), |e| T::from(e)));
                assert!(delta.x > T::zero());
                assert!(delta.y > T::zero());
                let two = T::one() + T::one();

                let tr = Vec3::new(
                    (viewport.w - two * (center.x - viewport.x)) / delta.x,
                    (viewport.h - two * (center.y - viewport.y)) / delta.y,
                    T::zero()
                );
                let sc = Vec3::new(
                    viewport.w / delta.x,
                    viewport.h / delta.y,
                    T::one()
                );
                Self::scaling_3d(sc) * Self::translation_3d(tr)
            }
            #[cfg(all(feature="vec3", feature="geom"))]
            pub fn world_to_viewport<V3>(obj: V3, modelview: Self, proj: Self, viewport: Rect<u16, u16>) -> Vec3<T>
                where T: Zero + One + Copy + From<u16> + Add<Output=T> + Mul<Output=T> + Div<Output=T> + MulAdd<T,T,Output=T> + DivAssign,
                      V3: Into<Vec3<T>>
            {
                let viewport = viewport.convert(|p| T::from(p), |e| T::from(e));
                let mut tmp = Vec4::point(obj.into());
                tmp = modelview * tmp;
                tmp = proj * tmp;

                let two = T::one() + T::one();
                let half_one = T::one() / two;

                tmp /= tmp.w;
                tmp = tmp / two + half_one;
                tmp.x = tmp.x * viewport.w + viewport.x;
                tmp.y = tmp.y * viewport.h + viewport.y;

                tmp.into()
            }
            #[cfg(all(feature="vec3", feature="geom"))]
            pub fn viewport_to_world<V3>(ray: V3, modelview: Self, proj: Self, viewport: Rect<u16, u16>) -> Vec3<T>
                where T: Float + From<u16> + MulAdd<T,T,Output=T>,
                      V3: Into<Vec3<T>>
            {
                let viewport: Rect<T,T> = viewport.convert(|p| p.into(), |e| e.into());
                let inverse = (proj * modelview).inverted();
                let two = T::one() + T::one();

                let mut tmp = Vec4::point(ray.into());
                tmp.x = (tmp.x - viewport.x) / viewport.w;
                tmp.y = (tmp.y - viewport.y) / viewport.h;
                tmp = tmp * two - T::one();

                let mut obj = inverse * tmp;
                let obj = obj / obj.w;

                obj.into()
            }
        }
        #[cfg(feature="mat3")]
        use super::mat3::Mat3;
        #[cfg(feature="mat3")]
        impl<T> From<Mat3<T>> for Mat4<T> where T: Zero + One {
            fn from(m: Mat3<T>) -> Self {
                let m = m.$lines;
                Self {
                    $lines: CVec4::new(
                        m.x.into(),
                        m.y.into(),
                        m.z.into(),
                        Vec4::new(T::zero(), T::zero(), T::zero(), T::one())
                    )
                }
            }
        }
        #[cfg(feature="mat2")]
        use super::mat2::Mat2;
        #[cfg(feature="mat2")]
        impl<T> From<Mat2<T>> for Mat4<T> where T: Zero + One {
            fn from(m: Mat2<T>) -> Self {
                let m = m.$lines;
                Self {
                    $lines: CVec4::new(
                        m.x.into(),
                        m.y.into(),
                        Vec4::new(T::zero(), T::zero(), T::one(), T::zero()),
                        Vec4::new(T::zero(), T::zero(), T::zero(), T::one())
                    )
                }
            }
        }

        #[cfg(feature="quaternion")]
        impl<T> From<Quaternion<T>> for Mat4<T>
            where T: Copy + Zero + One + Mul<Output=T> + Add<Output=T> + Sub<Output=T>
        {
            fn from(q: Quaternion<T>) -> Self {
                let Quaternion { w: a, x: b, y: c, z: d } = q;
                let (a2, b2, c2, d2) = (a*a, b*b, c*c, d*d);
                let two = T::one() + T::one();
                Self::new(
                    a2 + b2 - c2 - d2, two*(b*c + a*d), two*(b*d - a*c), T::zero(),
                    two*(b*c - a*d), a2 - b2 + c2 - d2, two*(c*d + a*b), T::zero(),
                    two*(b*d + a*c), two*(c*d - a*b), a2 - b2 - c2 + d2, T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
        }

        #[cfg(feature="quaternion")]
        impl<T> From<Mat4<T>> for Quaternion<T>
            where T: Float + Zero + One + Mul<Output=T> + Add<Output=T> + Sub<Output=T>
        {
            fn from(m: Mat4<T>) -> Self {
                use super::row_major::Mat4 as Rows4;
                let m = Rows4::from(m).rows;
                let r = T::zero();
                let perm = [ 0, 1, 2, 0, 1 ];
                let mut p = &perm[..];

                for i in 0..3 {
                    let min = m[i][i];
                    if min < r {
                        continue;
                    }
                    p = &perm[i..];
                }

                let r = (T::one() + m[p[0]][p[0]] - m[p[1]][p[1]] - m[p[2]][p[2]]).sqrt();

                if r < T::epsilon() {
                    Self::from_xyzw(T::zero(), T::zero(), T::zero(), T::one())
                } else {
                    let two = T::one() + T::one();
                    Self::from_xyzw(
                        (m[p[0]][p[1]] - m[p[1]][p[0]])/(two*r),
                        (m[p[2]][p[0]] - m[p[0]][p[2]])/(two*r),
                        (m[p[2]][p[1]] - m[p[1]][p[2]])/(two*r),
                        r/two
                    )
                }
            }
        }
    };
}

macro_rules! mat_impl_mat3 {
    (rows) => {

        mat_impl_mat3!{common rows}

        impl<T> Mat3<T> {
            /// Creates a new 3x3 matrix from elements in a layout-agnostic way.
            ///
            /// The parameters are named `mij` where `i` is the row index and
            /// `j` the column index. Their order is always the same regardless
            /// of the matrix's layout.
            pub fn new(
                m00: T, m01: T, m02: T,
                m10: T, m11: T, m12: T,
                m20: T, m21: T, m22: T,
            ) -> Self {
                Self {
                    rows: CVec3::new(
                        Vec3::new(m00, m01, m02),
                        Vec3::new(m10, m11, m12),
                        Vec3::new(m20, m21, m22)
                    )
                }
            }
        }
    };
    (cols) => {

        mat_impl_mat3!{common cols}

        impl<T> Mat3<T> {
            /// Creates a new 3x3 matrix from elements in a layout-agnostic way.
            ///
            /// The parameters are named `mij` where `i` is the row index and
            /// `j` the column index. Their order is always the same regardless
            /// of the matrix's layout.
            pub fn new(
                m00: T, m01: T, m02: T,
                m10: T, m11: T, m12: T,
                m20: T, m21: T, m22: T
            ) -> Self {
                Self {
                    cols: CVec3::new(
                        Vec3::new(m00, m10, m20),
                        Vec3::new(m01, m11, m21),
                        Vec3::new(m02, m12, m22)
                    )
                }
            }
        }
    };
    (common $lines:ident) => {
        impl<T> Mat3<T> {
            #[cfg(feature="vec2")]
            pub fn translation_2d<V: Into<Vec2<T>>>(v: V) -> Self where T: Zero + One {
                let v = v.into();
                Self::new(
                    T::one() , T::zero(), v.x,
                    T::zero(), T::one() , v.y,
                    T::zero(), T::zero(), T::one()
                )
            }
            #[cfg(feature="vec3")]
            pub fn scaling_3d<V: Into<Vec3<T>>>(v: V) -> Self where T: Zero {
                let Vec3 { x, y, z } = v.into();
                Self::new(
                    x, T::zero(), T::zero(),
                    T::zero(), y, T::zero(),
                    T::zero(), T::zero(), z
                )
            }
            pub fn rotation_x(angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    T::one(), T::zero(), T::zero(),
                    T::zero(), c, -s,
                    T::zero(), s, c
                )
            }
            pub fn rotation_y(angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, T::zero(), s,
                    T::zero(), T::one(), T::zero(),
                    -s, T::zero(), c
                )
            }
            pub fn rotation_z(angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, -s, T::zero(),
                    s,  c, T::zero(),
                    T::zero(), T::zero(), T::one()
                )
            }

            #[cfg(feature="vec3")]
            pub fn rotation_3d<V: Into<Vec3<T>>>(angle_radians: T, axis: V) -> Self where T: Float + Sum {
                let Vec3 { x, y, z } = axis.into().normalized();
                let s = angle_radians.sin();
                let c = angle_radians.cos();
                let oc = T::one() - c;
                Self::new(
                    oc*x*x + c  , oc*x*y - z*s, oc*z*x + y*s,
                    oc*x*y + z*s, oc*y*y + c  , oc*y*z - x*s,
                    oc*z*x - y*s, oc*y*z + x*s, oc*z*z + c  
                )
            }
        }

        // #[cfg(feature="mat4")] // Commented out, see rationale in Cargo.toml
        use super::mat4::Mat4;
        // #[cfg(feature="mat4")] // Commented out, see rationale in Cargo.toml
        impl<T> From<Mat4<T>> for Mat3<T> {
            fn from(m: Mat4<T>) -> Self {
                let m = m.$lines;
                Self {
                    $lines: CVec3::new(
                        m.x.into(),
                        m.y.into(),
                        m.z.into()
                    )
                }
            }
        }
        #[cfg(feature="mat2")]
        use super::mat2::Mat2;
        #[cfg(feature="mat2")]
        impl<T> From<Mat2<T>> for Mat3<T> where T: Zero + One {
            fn from(m: Mat2<T>) -> Self {
                let m = m.$lines;
                Self {
                    $lines: CVec3::new(
                        m.x.into(),
                        m.y.into(),
                        Vec3::new(T::zero(), T::zero(), T::one())
                    )
                }
            }
        }
        #[cfg(feature="quaternion")]
        impl<T> From<Quaternion<T>> for Mat3<T> 
            where T: Copy + Zero + One + Mul<Output=T> + Add<Output=T> + Sub<Output=T>
        {
            fn from(q: Quaternion<T>) -> Self {
                Mat4::from(q).into()
            }
        }
        #[cfg(feature="quaternion")]
        impl<T> From<Mat3<T>> for Quaternion<T> 
            where T: Zero + One + Mul<Output=T> + Add<Output=T> + Sub<Output=T> + Float
        {
            fn from(m: Mat3<T>) -> Self {
                Mat4::from(m).into()
            }
        }

    };
}

macro_rules! mat_impl_mat2 {
    (rows) => {

        mat_impl_mat2!{common rows}

        impl<T> Mat2<T> {
            /// Creates a new 2x2 matrix from elements in a layout-agnostic way.
            ///
            /// The parameters are named `mij` where `i` is the row index and
            /// `j` the column index. Their order is always the same regardless
            /// of the matrix's layout.
            pub fn new(
                m00: T, m01: T,
                m10: T, m11: T
            ) -> Self {
                Self {
                    rows: CVec2::new(
                        Vec2::new(m00, m01),
                        Vec2::new(m10, m11)
                    )
                }
            }
        }
    };
    (cols) => {

        mat_impl_mat2!{common cols}

        impl<T> Mat2<T> {
            /// Creates a new 2x2 matrix from elements in a layout-agnostic way.
            ///
            /// The parameters are named `mij` where `i` is the row index and
            /// `j` the column index. Their order is always the same regardless
            /// of the matrix's layout.
            pub fn new(
                m00: T, m01: T,
                m10: T, m11: T
            ) -> Self {
                Self {
                    cols: CVec2::new(
                        Vec2::new(m00, m10),
                        Vec2::new(m01, m11),
                    )
                }
            }
        }
    };
    (common $lines:ident) => {
        impl<T> Mat2<T> {
            pub fn rotation_z(angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, -s,
                    s,  c
                )
            }
            pub fn scaling_2d<V: Into<Vec2<T>>>(v: V) -> Self where T: Zero {
                let Vec2 { x, y } = v.into();
                Self::new(
                    x, T::zero(),
                    T::zero(), y
                )
            }
            pub fn shear_x(k: T) -> Self where T: Zero + One {
                Self::new(
                    T::one(), k,
                    T::zero(), T::one()
                )
            }
            pub fn shear_y(k: T) -> Self where T: Zero + One {
                Self::new(
                    T::one(), T::zero(),
                    k, T::one()
                )
            }
        }
        #[cfg(feature="mat3")]
        use super::mat3::Mat3;
        #[cfg(feature="mat3")]
        impl<T> From<Mat3<T>> for Mat2<T> {
            fn from(m: Mat3<T>) -> Self {
                let m = m.$lines;
                Self {
                    $lines: CVec2::new(
                        m.x.into(),
                        m.y.into(),
                    )
                }
            }
        }
        // #[cfg(feature="mat4")] // Commented out, see rationale in Cargo.toml
        use super::mat4::Mat4;
        // #[cfg(feature="mat4")] // Commented out, see rationale in Cargo.toml
        impl<T> From<Mat4<T>> for Mat2<T> {
            fn from(m: Mat4<T>) -> Self {
                let m = m.$lines;
                Self {
                    $lines: CVec2::new(
                        m.x.into(),
                        m.y.into(),
                    )
                }
            }
        }

    };
}




macro_rules! mat_impl_all_mats {
    ($lines:ident) => {
        // #[cfg(feature="mat4")] // Commented out, see rationale in Cargo.toml
        /// 4x4 matrix.
        pub mod mat4 {
            use super::*;
            /// 4x4 matrix.
            #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq/* , Ord, PartialOrd */)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            pub struct Mat4<T> {
                #[allow(missing_docs)]
                pub $lines: CVec4<Vec4<T>>,
            }
            mat_impl_mat!{$lines Mat4 CVec4 Vec4 (4 x 4) (x y z w)}
            mat_impl_mat4!{$lines}
        }
        // #[cfg(feature="mat4")] // Commented out, see rationale in Cargo.toml
        pub use self::mat4::Mat4;

        /// 3x3 matrix.
        #[cfg(feature="mat3")]
        pub mod mat3 {
            use super::*;
            /// 3x3 matrix.
            #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq/* , Ord, PartialOrd */)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            pub struct Mat3<T> {
                #[allow(missing_docs)]
                pub $lines: CVec3<Vec3<T>>,
            }
            mat_impl_mat!{$lines Mat3 CVec3 Vec3 (3 x 3) (x y z)}
            mat_impl_mat3!{$lines}
        }
        #[cfg(feature="mat3")]
        pub use self::mat3::Mat3;

        /// 2x2 matrix.
        #[cfg(feature="mat2")]
        pub mod mat2 {
            use super::*;
            /// 2x2 matrix.
            #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq/* , Ord, PartialOrd */)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            pub struct Mat2<T> {
                #[allow(missing_docs)]
                pub $lines: CVec2<Vec2<T>>,
            }
            mat_impl_mat!{$lines Mat2 CVec2 Vec2 (2 x 2) (x y)}
            mat_impl_mat2!{$lines}
        }
        #[cfg(feature="mat2")]
        pub use self::mat2::Mat2;

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
            mat_impl_all_mats!{cols}
        }
        pub mod row_major {
            //! Matrices stored in row-major layout.
            //!
            //! Unlike the column-major layout, row-major matrices are good at being the
            //! right-hand-side of a product with one or more row vectors.
            //!
            //! Also, their indexing order matches the existing mathematical conventions.

            use super::*;
            mat_impl_all_mats!{rows}
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
    #[cfg(feature="vec2")]
    use super::vec::repr_c::{Vec2, Vec2 as CVec2};
    #[cfg(feature="vec3")]
    use super::vec::repr_c::{Vec3, Vec3 as CVec3};
    // #[cfg(feature="vec4")] // Commented out, see rationale in Cargo.toml
    use super::vec::repr_c::{Vec4, Vec4 as CVec4};

    #[cfg(feature="quaternion")]
    use super::quaternion::repr_c::Quaternion;

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
    //!
    //! *Caution:* The size of a `#[repr_simd]` vector is never guaranteed to be
    //! exactly equal to the sum of its elements (for instance, an SIMD `Vec3<f32>` actually contains
    //! 4 `f32` elements on x86). This has also an impact on `repr_simd` matrices.
    //!
    //! Therefore, be careful when sending these as raw data (as you may want to do with OpenGL).
    
    use super::*;
    #[cfg(feature="vec2")]
    use super::vec::repr_simd::{Vec2};
    #[cfg(feature="vec2")]
    use super::vec::repr_c::{Vec2 as CVec2};
    #[cfg(feature="vec3")]
    use super::vec::repr_simd::{Vec3};
    #[cfg(feature="vec3")]
    use super::vec::repr_c::{Vec3 as CVec3};
    // #[cfg(feature="vec4")] // Commented out, see rationale in Cargo.toml
    use super::vec::repr_simd::{Vec4};
    // #[cfg(feature="vec4")] // Commented out, see rationale in Cargo.toml
    use super::vec::repr_c::{Vec4 as CVec4};

    #[cfg(feature="quaternion")]
    use super::quaternion::repr_simd::Quaternion;

    mat_declare_modules!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
/// If you're on Nightly with the `repr_simd` feature enabled, this exports `self::repr_simd::*` instead.
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

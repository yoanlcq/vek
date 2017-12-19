//! Matrix types.

use core::mem;
use core::ptr;
use core::slice;
use core::iter::Sum;
use core::fmt::{self, Display, Formatter};
use core::ops::*;
use num_traits::{Zero, One, Float, NumCast};
use approx::ApproxEq;
use ops::MulAdd;
use vec;
#[allow(unused_imports)]
#[cfg(feature="geom")]
use geom::{FrustumPlanes, Rect};
#[cfg(feature="quaternion")]
use quaternion;

macro_rules! mat_impl_mat {
    (rows $Mat:ident $CVec:ident $Vec:ident ($nrows:tt x $ncols:tt) ($($get:tt)+)) => {

        mat_impl_mat!{common rows $Mat $CVec $Vec ($nrows x $ncols) ($($get)+)}


        impl<T> $Mat<T> {
            /// Converts this matrix into a fixed-size array of elements.
            pub fn into_row_array(self) -> [T; $nrows*$ncols] {
                unimplemented!{}
            }
            /// Gets a const pointer to this matrix's elements.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            pub fn as_row_ptr(&self) -> *const T {
                assert!(self.is_packed());
                self.rows.as_ptr() as *const _ as *const T
            }
            /// Gets a mut pointer to this matrix's elements.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            pub fn as_mut_row_ptr(&mut self) -> *mut T {
                assert!(self.is_packed());
                self.rows.as_mut_ptr() as *mut _ as *mut T
            }
            /// View this matrix as an immutable slice.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            pub fn as_row_slice(&self) -> &[T] {
                unsafe {
                    slice::from_raw_parts(self.as_row_ptr(), $nrows*$ncols)
                }
            }
            /// View this matrix as a mutable slice.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            pub fn as_mut_row_slice(&mut self) -> &mut [T] {
                unsafe {
                    slice::from_raw_parts_mut(self.as_mut_row_ptr(), $nrows*$ncols)
                }
            }
        }


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

        /// Index this matrix in a layout-agnostic way with an `(i, j)` (row index, column index) tuple.
        ///
        /// Matrices cannot be indexed by `Vec2`s because that would be likely to cause confusion:
        /// should `x` be the row index (because it's the first element) or the column index
        /// (because it's a horizontal position) ?
        impl<T> Index<(usize, usize)> for $Mat<T> {
            type Output = T;
            fn index(&self, t: (usize, usize)) -> &Self::Output {
                let (i, j) = t;
                &self.rows[i][j]
            }
        }
        impl<T> IndexMut<(usize, usize)> for $Mat<T> {
            fn index_mut(&mut self, t: (usize, usize)) -> &mut Self::Output {
                let (i, j) = t;
                &mut self.rows[i][j]
            }
        }

        impl<T> $Mat<T> {
            /// Gets the `transpose` parameter to pass to 
            /// OpenGL `glUniformMatrix*()` functions.
            ///
            /// The return value is a plain `bool` which you may directly cast
            /// to a `GLboolean`.
            pub fn gl_should_transpose(&self) -> bool {
                true
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
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Copy> Mul<$Mat<T>> for $Vec<T> {
            type Output = Self;
            fn mul(self, rhs: $Mat<T>) -> Self::Output {
                let mut out = rhs.rows[0] * $Vec::broadcast(self[0]);
                for i in 1..$nrows {
                    out = rhs.rows[i].mul_add($Vec::broadcast(self[i]), out);
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
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Copy> Mul<$Vec<T>> for $Mat<T> {
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
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Copy> Mul for $Mat<T> {
            type Output = Self;
            fn mul(self, rhs: Self) -> Self::Output {
                Self {
                    rows: $CVec {
                        $($get: self.rows.$get * rhs,)+
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
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Copy> Mul<Transpose<T>> for $Mat<T> {
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

        impl<T> $Mat<T> {
            /// Converts this matrix into a fixed-size array of elements.
            pub fn into_col_array(self) -> [T; $nrows*$ncols] {
                unimplemented!{}
            }
            /// Gets a const pointer to this matrix's elements.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            pub fn as_col_ptr(&self) -> *const T {
                assert!(self.is_packed());
                self.cols.as_ptr() as *const _ as *const T
            }
            /// Gets a mut pointer to this matrix's elements.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            pub fn as_mut_col_ptr(&mut self) -> *mut T {
                assert!(self.is_packed());
                self.cols.as_mut_ptr() as *mut _ as *mut T
            }
            /// View this matrix as an immutable slice.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            pub fn as_col_slice(&self) -> &[T] {
                unsafe {
                    slice::from_raw_parts(self.as_col_ptr(), $nrows*$ncols)
                }
            }
            /// View this matrix as a mutable slice.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            pub fn as_mut_col_slice(&mut self) -> &mut [T] {
                unsafe {
                    slice::from_raw_parts_mut(self.as_mut_col_ptr(), $nrows*$ncols)
                }
            }
        }


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

        /// Index this matrix in a layout-agnostic way with an `(i, j)` (row index, column index) tuple.
        ///
        /// Matrices cannot be indexed by `Vec2`s because that would be likely to cause confusion:
        /// should `x` be the row index (because it's the first element) or the column index
        /// (because it's a horizontal position) ?
        impl<T> Index<(usize, usize)> for $Mat<T> {
            type Output = T;
            fn index(&self, t: (usize, usize)) -> &Self::Output {
                let (i, j) = t;
                &self.cols[j][i]
            }
        }
        impl<T> IndexMut<(usize, usize)> for $Mat<T> {
            fn index_mut(&mut self, t: (usize, usize)) -> &mut Self::Output {
                let (i, j) = t;
                &mut self.cols[j][i]
            }
        }


        impl<T> $Mat<T> {
            /// Gets the `transpose` parameter to pass to 
            /// OpenGL `glUniformMatrix*()` functions.
            ///
            /// The return value is a plain `bool` which you may directly cast
            /// to a `GLboolean`.
            pub fn gl_should_transpose(&self) -> bool {
                false
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
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Copy> Mul<$Mat<T>> for $Vec<T> {
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
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Copy> Mul<$Vec<T>> for $Mat<T> {
            type Output = $Vec<T>;
            fn mul(self, v: $Vec<T>) -> Self::Output {
                let mut out = self.cols[0] * $Vec::broadcast(v[0]);
                for i in 1..$ncols {
                    out = self.cols[i].mul_add($Vec::broadcast(v[i]), out);
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
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Copy> Mul for $Mat<T> {
            type Output = Self;
            fn mul(self, rhs: Self) -> Self::Output {
                Self {
                    cols: $CVec {
                        $($get: self * rhs.cols.$get,)+
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
        impl<T: MulAdd<T,T,Output=T> + Mul<Output=T> + Copy> Mul<Transpose<T>> for $Mat<T> {
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
        impl<T: Zero + PartialEq> Zero for $Mat<T> {
            fn zero() -> Self { Self::zero() }
            fn is_zero(&self) -> bool { self.is_zero() }
        }
        impl<T: Zero + One + Copy + MulAdd<T,T,Output=T>> One for $Mat<T> {
            fn one() -> Self { Self::identity() }
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
            /// Are all elements of this matrix equal to zero ?
            pub fn is_zero(&self) -> bool where T: Zero + PartialEq {
                self == &Self::zero()
            }
            /// Is this matrix the identity ? (uses ApproxEq)
            pub fn is_identity(&self) -> bool where T: Zero + One, Self: ApproxEq {
                Self::relative_eq(self, &Self::identity(), Self::default_epsilon(), Self::default_max_relative())
            }
            /// Returns a memberwise-converted copy of this matrix, using the given conversion
            /// closure.
            // TODO this example
            // ```
            // # use vek::vec::Vec4;
            // let v = Vec4::new(0_f32, 1., 1.8, 3.14);
            // let i = v.convert(|x| x.round() as i32);
            // assert_eq!(i, Vec4::new(0, 1, 2, 3));
            // ```
            pub fn convert<D,F>(self, f: F) -> $Mat<D> where F: Fn(T) -> D + Copy { // FIXME: There ought to be a way to get rid of this Copy bound
                $Mat {
                    $lines: $CVec {
                        $($get: self.$lines.$get.convert(f),)+
                    }
                }
            }
            /// Returns a memberwise-converted copy of this matrix, using `NumCast`.
            // TODO this example
            // ```ignored
            // # use vek::vec::Vec4;
            // let v = Vec4::new(0_f32, 1., 2., 3.);
            // let i: Vec4<i32> = v.numcast().unwrap();
            // assert_eq!(i, Vec4::new(0, 1, 2, 3));
            // ```
            pub fn numcast<D>(self) -> Option<$Mat<D>> where T: NumCast, D: NumCast {
                Some($Mat {
                    $lines: $CVec {
                        $($get: self.$lines.$get.numcast()?,)+
                    }
                })
            }
            /// Initializes a new matrix with elements of the diagonal set to `val`
            /// and the other to zero.
            ///
            /// In a way, this is the same as single-argument matrix constructors
            /// in GLSL and GLM.
            ///
            /// ```
            /// # use vek::Mat4;
            /// assert_eq!(Mat4::broadcast_diagonal(0), Mat4::zero());
            /// assert_eq!(Mat4::broadcast_diagonal(1), Mat4::identity());
            /// assert_eq!(Mat4::broadcast_diagonal(2), Mat4::new(
            ///     2,0,0,0,
            ///     0,2,0,0,
            ///     0,0,2,0,
            ///     0,0,0,2,
            /// ));
            /// ```
            pub fn broadcast_diagonal(val: T) -> Self where T: Zero + Copy {
                let mut out = Self::zero();
                $(out.$lines.$get.$get = val;)+
                out
            }
            /// Initializes a matrix by its diagonal, setting other elements to zero.
            pub fn from_diagonal(d: $Vec<T>) -> Self where T: Zero + Copy {
                let mut out = Self::zero();
                $(out.$lines.$get.$get = d.$get;)+
                out
            }
            /// Gets the matrix's diagonal into a vector.
            ///
            /// ```
            /// # use vek::{Mat4, Vec4};
            /// assert_eq!(Mat4::<u32>::zero().diagonal(), Vec4::zero());
            /// assert_eq!(Mat4::<u32>::identity().diagonal(), Vec4::one());
            ///
            /// let mut m = Mat4::zero();
            /// m[(0, 0)] = 1;
            /// m[(1, 1)] = 2;
            /// m[(2, 2)] = 3;
            /// m[(3, 3)] = 4;
            /// assert_eq!(m.diagonal(), Vec4::new(1, 2, 3, 4));
            /// assert_eq!(m.diagonal(), Vec4::iota() + 1);
            pub fn diagonal(self) -> $Vec<T> {
                $Vec::new($(self.$lines.$get.$get),+)
            }
            /// The sum of the diagonal's elements.
            ///
            /// ```
            /// # use vek::Mat4;
            /// assert_eq!(Mat4::<u32>::zero().trace(), 0);
            /// assert_eq!(Mat4::<u32>::identity().trace(), 4);
            /// ```
            pub fn trace(self) -> T where T: Sum {
                self.diagonal().sum()
            }
            /// The matrix's transpose.
            ///
            /// For orthogonal matrices, the transpose is the same as the inverse.
            /// All pure rotation matrices are orthogonal, and therefore can be inverted
            /// faster by simply computing their transpose.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::Mat4;
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
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
            ///
            /// let m = Mat4::rotation_x(PI/7.);
            /// assert_relative_eq!(m * m.transposed(), Mat4::identity());
            /// assert_relative_eq!(m.transposed() * m, Mat4::identity());
            /// // This is supposed to hold true, but our inversion implementation
            /// // loses to much precision in some elements.
            /// // assert_relative_eq!(m.transposed(), m.inverted());
            /// # }
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
            /// Convenience for getting the number of rows of this matrix.
            pub fn row_count(&self) -> usize {
                $nrows
            }
            /// Convenience for getting the number of columns of this matrix.
            pub fn col_count(&self) -> usize {
                $ncols
            }
            /// Are all elements of this matrix tightly packed together in memory ?
            /// 
            /// This might not be the case for matrices in the `repr_simd` module
            /// (it depends on the target architecture).
            pub fn is_packed(&self) -> bool {
                let ptr = self as *const _ as *const T;
                let mut i = -1isize;
                // Maybe some of the tests are overkill
                if !self.$lines.is_packed() {
                    return false;
                }
                $(
                    i += 1;
                    if !self.$lines.$get.is_packed() {
                        return false;
                    }
                    const_assert_eq!($nrows, $ncols);
                    if unsafe { ptr.offset(i*($nrows+1)) } != &self.$lines.$get.$get as *const _ {
                        return false;
                    }
                )+
                true
            }
        }

        impl<T> Mul<T> for $Mat<T> 
            where T: Copy + Zero + Add<Output=T> + Mul<Output=T>
        {
            type Output = Self;
            fn mul(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get * rhs),+
                    )
                }
            }
        }

        impl<T> MulAssign for $Mat<T>
            where T: Copy + Zero + Add<Output=T> + Mul<Output=T> + MulAdd<T,T,Output=T>
        { 
            fn mul_assign(&mut self, rhs: Self) { *self = *self * rhs; }
        }

        impl<T> MulAssign<T> for $Mat<T>
            where T: Copy + Zero + Add<Output=T> + Mul<Output=T>
        { 
            fn mul_assign(&mut self, rhs: T) { *self = *self * rhs; }
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

        impl<T> Add<T> for $Mat<T> where T: Copy + Add<Output=T> {
            type Output = Self;
            fn add(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get + rhs),+
                    )
                }
            }
        }
        impl<T> Sub<T> for $Mat<T> where T: Copy + Sub<Output=T> {
            type Output = Self;
            fn sub(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get - rhs),+
                    )
                }
            }
        }
        impl<T> Div<T> for $Mat<T> where T: Copy + Div<Output=T> {
            type Output = Self;
            fn div(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get / rhs),+
                    )
                }
            }
        }
        impl<T> Rem<T> for $Mat<T> where T: Copy + Rem<Output=T> {
            type Output = Self;
            fn rem(self, rhs: T) -> Self::Output {
                Self {
                    $lines: $CVec::new(
                        $(self.$lines.$get % rhs),+
                    )
                }
            }
        }

        impl<T: Add<Output=T> + Copy> AddAssign    for $Mat<T> { fn add_assign(&mut self, rhs: Self) { *self = *self + rhs; } }
        impl<T: Add<Output=T> + Copy> AddAssign<T> for $Mat<T> { fn add_assign(&mut self, rhs: T   ) { *self = *self + rhs; } }
        impl<T: Sub<Output=T> + Copy> SubAssign    for $Mat<T> { fn sub_assign(&mut self, rhs: Self) { *self = *self - rhs; } }
        impl<T: Sub<Output=T> + Copy> SubAssign<T> for $Mat<T> { fn sub_assign(&mut self, rhs: T   ) { *self = *self - rhs; } }
        impl<T: Div<Output=T> + Copy> DivAssign    for $Mat<T> { fn div_assign(&mut self, rhs: Self) { *self = *self / rhs; } }
        impl<T: Div<Output=T> + Copy> DivAssign<T> for $Mat<T> { fn div_assign(&mut self, rhs: T   ) { *self = *self / rhs; } }
        impl<T: Rem<Output=T> + Copy> RemAssign    for $Mat<T> { fn rem_assign(&mut self, rhs: Self) { *self = *self % rhs; } }
        impl<T: Rem<Output=T> + Copy> RemAssign<T> for $Mat<T> { fn rem_assign(&mut self, rhs: T   ) { *self = *self % rhs; } }

        impl<T: ApproxEq> ApproxEq for $Mat<T> where T::Epsilon: Copy {
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
                for (l, r) in self.$lines.iter().zip(other.$lines.iter()) {
                    if !ApproxEq::relative_eq(l, r, epsilon, max_relative) {
                        return false;
                    }
                }
                true
            }

            fn ulps_eq(&self, other: &Self, epsilon: T::Epsilon, max_ulps: u32) -> bool {
                for (l, r) in self.$lines.iter().zip(other.$lines.iter()) {
                    if !ApproxEq::ulps_eq(l, r, epsilon, max_ulps) {
                        return false;
                    }
                }
                true
            }
        }

    };
}


macro_rules! mat_impl_mat4 {
    (rows) => {

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
        mat_impl_mat4!{common rows}
    };
    (cols) => {
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
        mat_impl_mat4!{common cols}
    };
    (common $lines:ident) => {

        use super::row_major::Mat4 as Rows4;

        impl<T> Mat4<T> {

            //
            // BASIC
            //

            /// Inverts this matrix, blindly assuming that it is invertible.
            /// See `inverted()` for more info.
            pub fn invert(&mut self) where T: Float {
                *self = self.inverted()
            }
            /// Returns this matrix's inverse, blindly assuming that it is invertible.
            ///
            /// All affine matrices have inverses; Your matrices may be affine
            /// as long as they consist of any combination of pure rotations,
            /// translations, scales and shears.
            ///
            /// ```ignore
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::Mat4;
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
            /// let a = Mat4::rotation_x(PI*4./5.);
            /// let b = a.inverted();
            /// assert_relative_eq!(a*b, Mat4::identity());
            /// assert_relative_eq!(b*a, Mat4::identity());
            /// # }
            /// ```
            // FIXME: Make the above doc-test actually pass
            // TODO: Steal
            // https://github.com/niswegmann/small-matrix-inverse/blob/master/invert4x4_sse.h
            // https://lxjk.github.io/2017/09/03/Fast-4x4-Matrix-Inverse-with-SSE-SIMD-Explained.html
            //
            // Taken verbatim from datenwolf's linmath.h
            // As mentioned in the original, it assumes that the matrix is invertible.
            // It appears to lose quite a bunch of precision though. There should be a better way.
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

            #[allow(dead_code)]
            fn orthonormalize(&mut self) where T: Float + Sum + SubAssign {
                *self = self.orthonormalized();
            }
            #[allow(dead_code)]
            /// XXX I don't know exactly what this does. Make it public when I do.
            // Taken verbatim from linmath.h - I don't know exactly what it does.
            fn orthonormalized(self) -> Self where T: Float + Sum + SubAssign {
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
            // MULTIPLY BY
            //

            /// Shortcut for `self * Vec4::from_point(rhs)`.
            #[cfg(feature="vec3")]
            pub fn mul_point<V: Into<Vec3<T>> + From<Vec4<T>>>(self, rhs: V) -> V
                where T: Float + MulAdd<T,T,Output=T>
            {
                V::from(self * Vec4::from_point(rhs))
            }
            /// Shortcut for `self * Vec4::from_direction(rhs)`.
            #[cfg(feature="vec3")]
            pub fn mul_direction<V: Into<Vec3<T>> + From<Vec4<T>>>(self, rhs: V) -> V
                where T: Float + MulAdd<T,T,Output=T>
            {
                V::from(self * Vec4::from_direction(rhs))
            }

            //
            // TRANSFORMS
            //

            #[cfg(feature="vec2")]
            pub fn translate_2d<V: Into<Vec2<T>>>(&mut self, v: V)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.translated_2d(v);
            }
            #[cfg(feature="vec2")]
            pub fn translated_2d<V: Into<Vec2<T>>>(self, v: V) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::translation_2d(v) * self
            }
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
            pub fn translate_3d<V: Into<Vec3<T>>>(&mut self, v: V)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.translated_3d(v);
            }
            #[cfg(feature="vec3")]
            pub fn translated_3d<V: Into<Vec3<T>>>(self, v: V) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::translation_3d(v) * self
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
            #[allow(dead_code)]
            /// XXX: This was from linmath.h. I'm not confident what this does. Make it pub when I do.
            #[cfg(feature="vec3")]
            fn translate_in_place_3d<V: Into<Vec3<T>>>(&mut self, v: V) where T: Copy + Zero + One + AddAssign + Sum {
                let Vec3 { x, y, z } = v.into();
                let t = Vec4 { x, y, z, w: T::zero() };
                let mut rows = Rows4::from(*self).rows;
                rows[3][0] += rows[0].dot(t);
                rows[3][1] += rows[1].dot(t);
                rows[3][2] += rows[2].dot(t);
                rows[3][3] += rows[3].dot(t);
                *self = Rows4 { rows }.into();
            }

            #[cfg(feature="vec3")]
            pub fn scale_3d<V: Into<Vec3<T>>>(&mut self, v: V)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.scaled_3d(v);
            }
            #[cfg(feature="vec3")]
            pub fn scaled_3d<V: Into<Vec3<T>>>(self, v: V) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::scaling_3d(v) * self
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
            pub fn rotate_x(&mut self, angle_radians: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_x(angle_radians);
            }
            pub fn rotated_x(self, angle_radians: T) -> Self 
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::rotation_x(angle_radians) * self
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
            pub fn rotate_y(&mut self, angle_radians: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_y(angle_radians);
            }
            pub fn rotated_y(self, angle_radians: T) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::rotation_y(angle_radians) * self
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
            pub fn rotate_z(&mut self, angle_radians: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_z(angle_radians);
            }
            pub fn rotated_z(self, angle_radians: T) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::rotation_z(angle_radians) * self
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
            pub fn rotate_3d<V: Into<Vec3<T>>>(&mut self, angle_radians: T, axis: V)
                where T: Float + MulAdd<T,T,Output=T> + Sum
            {
                *self = self.rotated_3d(angle_radians, axis);
            }
            #[cfg(feature="vec3")]
            pub fn rotated_3d<V: Into<Vec3<T>>>(self, angle_radians: T, axis: V) -> Self
                where T: Float + MulAdd<T,T,Output=T> + Sum
            {
                Self::rotation_3d(angle_radians, axis) * self
            }
            /// 3D rotation matrix. `axis` is not required to be normalized.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec4};
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
            /// let v = Vec4::unit_x();
            ///
            /// let q = Mat4::rotation_z(PI);
            /// assert_relative_eq!(q * v, -v);
            ///
            /// let q = Mat4::rotation_z(PI * 0.5);
            /// assert_relative_eq!(q * v, Vec4::unit_y());
            ///
            /// let q = Mat4::rotation_z(PI * 1.5);
            /// assert_relative_eq!(q * v, -Vec4::unit_y());
            ///
            /// let angles = 32;
            /// for i in 0..angles {
            ///     let theta = PI * 2. * (i as f32) / (angles as f32);
            ///
            ///     // See what rotating unit vectors do for most angles between 0 and 2*PI.
            ///     // It's helpful to picture this as a right-handed coordinate system.
            ///
            ///     let v = Vec4::unit_y();
            ///     let m = Mat4::rotation_x(theta);
            ///     assert_relative_eq!(m * v, Vec4::new(0., theta.cos(), theta.sin(), 0.));
            ///
            ///     let v = Vec4::unit_z();
            ///     let m = Mat4::rotation_y(theta);
            ///     assert_relative_eq!(m * v, Vec4::new(theta.sin(), 0., theta.cos(), 0.));
            ///
            ///     let v = Vec4::unit_x();
            ///     let m = Mat4::rotation_z(theta);
            ///     assert_relative_eq!(m * v, Vec4::new(theta.cos(), theta.sin(), 0., 0.));
            ///
            ///     assert_relative_eq!(Mat4::rotation_x(theta), Mat4::rotation_3d(theta, Vec4::unit_x()));
            ///     assert_relative_eq!(Mat4::rotation_y(theta), Mat4::rotation_3d(theta, Vec4::unit_y()));
            ///     assert_relative_eq!(Mat4::rotation_z(theta), Mat4::rotation_3d(theta, Vec4::unit_z()));
            /// }
            /// # }
            /// ```
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
            // CHANGE OF BASIS
            //

            /// Builds a change of basis matrix that transforms points and directions from
            /// any space to the canonical one.
            ///
            /// `origin` is the origin of the child space.  
            /// `i`, `j` and `k` are all required to be normalized;
            /// They are the unit basis vector along the target space x-axis, y-axis and z-axis
            /// respectively, expressed in canonical-space coordinates.  
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec3};
            /// # fn main() {
            ///     let origin = Vec3::new(1_f32, 2., 3.);
            ///     let i = Vec3::unit_z();
            ///     let j = Vec3::unit_y();
            ///     let k = Vec3::unit_x();
            ///     let m = Mat4::basis_to_local(origin, i, j, k);
            ///     assert_relative_eq!(m.mul_point(origin), Vec3::zero());
            ///     assert_relative_eq!(m.mul_point(origin+i), Vec3::unit_x());
            ///     assert_relative_eq!(m.mul_point(origin+j), Vec3::unit_y());
            ///     assert_relative_eq!(m.mul_point(origin+k), Vec3::unit_z());
            ///
            ///     // `local_to_basis` and `basis_to_local` undo each other
            ///     let a = Mat4::<f32>::basis_to_local(origin, i, j, k);
            ///     let b = Mat4::<f32>::local_to_basis(origin, i, j, k);
            ///     assert_relative_eq!(a*b, Mat4::identity());
            ///     assert_relative_eq!(b*a, Mat4::identity());
            /// # }
            /// ```
            ///
            /// Slightly more contrived example:
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec3};
            /// # fn main() {
            ///     let origin = Vec3::new(1_f32, 2., 3.);
            ///     let r = Mat4::rotation_3d(3., Vec3::new(2_f32, 1., 3.));
            ///     let i = r.mul_direction(Vec3::unit_x());
            ///     let j = r.mul_direction(Vec3::unit_y());
            ///     let k = r.mul_direction(Vec3::unit_z());
            ///     let m = Mat4::basis_to_local(origin, i, j, k);
            ///     assert_relative_eq!(m.mul_point(origin), Vec3::zero(), epsilon = 0.000001);
            ///     assert_relative_eq!(m.mul_point(origin+i), Vec3::unit_x(), epsilon = 0.000001);
            ///     assert_relative_eq!(m.mul_point(origin+j), Vec3::unit_y(), epsilon = 0.000001);
            ///     assert_relative_eq!(m.mul_point(origin+k), Vec3::unit_z(), epsilon = 0.000001);
            /// # }
            /// ```
            #[cfg(feature="vec3")]
            pub fn basis_to_local<V: Into<Vec3<T>>>(origin: V, i: V, j: V, k: V) -> Self
                where T: Zero + One + Neg<Output=T> + Float + Sum
            {
                let (origin, i, j, k) = (origin.into(), i.into(), j.into(), k.into());
                Self::new(
                    i.x, i.y, i.z, -i.dot(origin),
                    j.x, j.y, j.z, -j.dot(origin),
                    k.x, k.y, k.z, -k.dot(origin),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
            /// Builds a change of basis matrix that transforms points and directions from
            /// canonical space to another space.
            ///
            /// `origin` is the origin of the child space.  
            /// `i`, `j` and `k` are all required to be normalized;
            /// They are the unit basis vector along the target space x-axis, y-axis and z-axis
            /// respectively, expressed in canonical-space coordinates.  
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec3};
            /// # fn main() {
            ///     let origin = Vec3::new(1_f32, 2., 3.);
            ///     let i = Vec3::unit_z();
            ///     let j = Vec3::unit_y();
            ///     let k = Vec3::unit_x();
            ///     let m = Mat4::local_to_basis(origin, i, j, k);
            ///     assert_relative_eq!(origin,   m.mul_point(Vec3::zero()));
            ///     assert_relative_eq!(origin+i, m.mul_point(Vec3::unit_x()));
            ///     assert_relative_eq!(origin+j, m.mul_point(Vec3::unit_y()));
            ///     assert_relative_eq!(origin+k, m.mul_point(Vec3::unit_z()));
            ///
            ///     // `local_to_basis` and `basis_to_local` undo each other
            ///     let a = Mat4::<f32>::local_to_basis(origin, i, j, k);
            ///     let b = Mat4::<f32>::basis_to_local(origin, i, j, k);
            ///     assert_relative_eq!(a*b, Mat4::identity());
            ///     assert_relative_eq!(b*a, Mat4::identity());
            /// # }
            /// ```
            ///
            /// Slightly more contrived example:
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec3};
            /// # fn main() {
            ///     // Sanity test
            ///     let origin = Vec3::new(1_f32, 2., 3.);
            ///     let r = Mat4::rotation_3d(3., Vec3::new(2_f32, 1., 3.));
            ///     let i = r.mul_direction(Vec3::unit_x());
            ///     let j = r.mul_direction(Vec3::unit_y());
            ///     let k = r.mul_direction(Vec3::unit_z());
            ///     let m = Mat4::local_to_basis(origin, i, j, k);
            ///     assert_relative_eq!(origin,   m.mul_point(Vec3::zero()));
            ///     assert_relative_eq!(origin+i, m.mul_point(Vec3::unit_x()));
            ///     assert_relative_eq!(origin+j, m.mul_point(Vec3::unit_y()));
            ///     assert_relative_eq!(origin+k, m.mul_point(Vec3::unit_z()));
            /// # }
            /// ```
            #[cfg(feature="vec3")]
            pub fn local_to_basis<V: Into<Vec3<T>>>(origin: V, i: V, j: V, k: V) -> Self where T: Zero + One {
                let (origin, i, j, k) = (origin.into(), i.into(), j.into(), k.into());
                Self::new(
                    i.x, j.x, k.x, origin.x,
                    i.y, j.y, k.y, origin.y,
                    i.z, j.z, k.z, origin.z,
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }


            //
            // VIEW
            //

            /// Builds a "look at" view transform for left-handed spaces
            /// from an eye position, a target position, and up vector.
            /// Commonly used for cameras.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec4};
            /// # fn main() {
            /// let eye = Vec4::new(1_f32, 0., 1., 1.);
            /// let target = Vec4::new(2_f32, 0., 2., 1.);
            /// let view = Mat4::<f32>::look_at_view_lh(eye, target, Vec4::up());
            /// assert_relative_eq!(view * eye, Vec4::unit_w());
            /// assert_relative_eq!(view * target, Vec4::new(0_f32, 0., 2_f32.sqrt(), 1.));
            /// # }
            /// ```
            #[cfg(feature="vec3")]
            pub fn look_at_view_lh<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Float + Sum
            {
                // From GLM
                let (eye, target, up) = (eye.into(), target.into(), up.into());
                let f = (target - eye).normalized();
                let s = up.cross(f).normalized();
                let u = f.cross(s);
                Self::new(
                    s.x, s.y, s.z, -s.dot(eye),
                    u.x, u.y, u.z, -u.dot(eye),
                    f.x, f.y, f.z, -f.dot(eye),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }

            /// Builds a "look at" model transform for left-handed spaces
            /// from an eye position, a target position, and up vector.
            /// Preferred for transforming objects.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec4};
            /// # fn main() {
            /// let eye = Vec4::new(1_f32, 0., 1., 1.);
            /// let target = Vec4::new(2_f32, 0., 2., 1.);
            /// let model = Mat4::<f32>::look_at_model_lh(eye, target, Vec4::up());
            /// assert_relative_eq!(model * Vec4::unit_w(), eye);
            /// let d = 2_f32.sqrt();
            /// assert_relative_eq!(model * Vec4::new(0_f32, 0., d, 1.), target);
            ///
            /// // A "model" look-at essentially undoes a "view" look-at
            /// let view = Mat4::look_at_view_lh(eye, target, Vec4::up());
            /// assert_relative_eq!(view * model, Mat4::identity());
            /// assert_relative_eq!(model * view, Mat4::identity());
            /// # }
            /// ```
            #[cfg(feature="vec3")]
            pub fn look_at_model_lh<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Float + Sum
            {
                // Advanced 3D Game Programming with DirectX 10.0, p. 173
                let (eye, target, up) = (eye.into(), target.into(), up.into());
                let f = (target - eye).normalized();
                let s = up.cross(f).normalized();
                let u = f.cross(s);
                Self::new(
                    s.x, u.x, f.x, eye.x,
                    s.y, u.y, f.y, eye.y,
                    s.z, u.z, f.z, eye.z,
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }

            /// Builds a "look at" model transform for right-handed spaces
            /// from an eye position, a target position, and up vector.
            /// Preferred for transforming objects.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::mat::row_major::Mat4;
            /// # use vek::Vec4;
            /// # fn main() {
            /// let eye = Vec4::new(1_f32, 0., 1., 1.);
            /// let target = Vec4::new(2_f32, 0., 2., 1.);
            /// let model = Mat4::<f32>::look_at_model_rh(eye, target, Vec4::up());
            /// assert_relative_eq!(model * Vec4::unit_w(), Vec4::new(1_f32, 0., 1., 1.));
            /// assert_relative_eq!(model * Vec4::new(0_f32, 0., -2_f32.sqrt(), 1.), Vec4::new(2_f32, 0., 2., 1.));
            ///
            /// // A "model" look-at essentially undoes a "view" look-at
            /// let view = Mat4::<f32>::look_at_view_rh(eye, target, Vec4::up());
            /// assert_relative_eq!(view * model, Mat4::identity());
            /// assert_relative_eq!(model * view, Mat4::identity());
            /// # }
            /// ```
            #[cfg(feature="vec3")]
            pub fn look_at_model_rh<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Float + Sum + MulAdd<T,T,Output=T>
            {
                let (eye, target, up) = (eye.into(), target.into(), up.into());
                let f = (target - eye).normalized();
                let s = f.cross(up).normalized();
                let u = s.cross(f);
                Self::new(
                    s.x, u.x, -f.x, eye.x,
                    s.y, u.y, -f.y, eye.y,
                    s.z, u.z, -f.z, eye.z,
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }

            /// Builds a "look at" view transform for right-handed spaces
            /// from an eye position, a target position, and up vector.
            /// Commonly used for cameras.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec4};
            /// # fn main() {
            /// let eye = Vec4::new(1_f32, 0., 1., 1.);
            /// let target = Vec4::new(2_f32, 0., 2., 1.);
            /// let view = Mat4::<f32>::look_at_view_rh(eye, target, Vec4::up());
            /// assert_relative_eq!(view * eye, Vec4::unit_w());
            /// assert_relative_eq!(view * target, Vec4::new(0_f32, 0., -2_f32.sqrt(), 1.));
            /// # }
            /// ```
            #[cfg(feature="vec3")]
            pub fn look_at_view_rh<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Float + Sum
            {
                // From GLM
                let (eye, target, up) = (eye.into(), target.into(), up.into());
                let f = (target - eye).normalized();
                let s = f.cross(up).normalized();
                let u = s.cross(f);
                Self::new(
                     s.x,  s.y,  s.z, -s.dot(eye),
                     u.x,  u.y,  u.z, -u.dot(eye),
                    -f.x, -f.y, -f.z,  f.dot(eye),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }



            //
            // PROJECTIONS
            //

            #[cfg(feature="geom")]
            pub fn orthographic_without_depth_planes (o: FrustumPlanes<T>) -> Self where T: Float {
                let two = T::one() + T::one();
                let FrustumPlanes { left, right, top, bottom, .. } = o;
                let mut m = Self::identity();
                m[(0, 0)] = two / (right - left);
                m[(1, 1)] = two / (top - bottom);
                m[(0, 3)] = - (right + left) / (right - left);
                m[(1, 3)] = - (top + bottom) / (top - bottom);
                m
            }
            #[cfg(feature="geom")]
            pub fn orthographic_lh_zo (o: FrustumPlanes<T>) -> Self where T: Float {
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::orthographic_without_depth_planes(o);
                m[(2, 2)] = T::one() / (far - near);
                m[(2, 3)] = - near / (far - near);
                m
            }
            #[cfg(feature="geom")]
            pub fn orthographic_lh_no (o: FrustumPlanes<T>) -> Self where T: Float {
                let two = T::one() + T::one();
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::orthographic_without_depth_planes(o);
                m[(2, 2)] = two / (far - near);
                m[(2, 3)] = - (far + near) / (far - near);
                m
            }
            #[cfg(feature="geom")]
            pub fn orthographic_rh_zo (o: FrustumPlanes<T>) -> Self where T: Float {
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::orthographic_without_depth_planes(o);
                m[(2, 2)] = - T::one() / (far - near);
                m[(2, 3)] = - near / (far - near);
                m
            }
            #[cfg(feature="geom")]
            pub fn orthographic_rh_no (o: FrustumPlanes<T>) -> Self where T: Float {
                let two = T::one() + T::one();
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::orthographic_without_depth_planes(o);
                m[(2, 2)] = - two / (far - near);
                m[(2, 3)] = - (far + near) / (far - near);
                m
            }

            #[cfg(feature="geom")]
            pub fn frustum_lh_zo (o: FrustumPlanes<T>) -> Self where T: Float {
                let two = T::one() + T::one();
                let FrustumPlanes { left, right, top, bottom, near, far } = o;
                let mut m = Self::zero();
                m[(0, 0)] = (two * near) / (right - left);
                m[(1, 1)] = (two * near) / (top - bottom);
                m[(0, 2)] = (right + left) / (right - left);
                m[(1, 2)] = (top + bottom) / (top - bottom);
                m[(2, 2)] = far / (far - near);
                m[(3, 2)] = T::one();
                m[(2, 3)] = -(far * near) / (far - near);
                m
            }
            #[cfg(feature="geom")]
            pub fn frustum_lh_no (o: FrustumPlanes<T>) -> Self where T: Float {
                let two = T::one() + T::one();
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::frustum_lh_zo(o);
                m[(2, 2)] = (far + near) / (far - near);
                m[(2, 3)] = -(two * far * near) / (far - near);
                m
            }
            #[cfg(feature="geom")]
            pub fn frustum_rh_zo (o: FrustumPlanes<T>) -> Self where T: Float {
                let mut m = Self::frustum_lh_zo(o);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }
            #[cfg(feature="geom")]
            pub fn frustum_rh_no (o: FrustumPlanes<T>) -> Self where T: Float {
                let mut m = Self::frustum_lh_no(o);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }

            /// Creates a perspective projection matrix for right-handed spaces, with zero-to-one depth clip planes.
            pub fn perspective_rh_zo (fov_y_radians: T, aspect_ratio: T, near: T, far: T) -> Self 
                where T: Float
            {
                assert!((aspect_ratio - T::epsilon()).abs() > T::zero());
                let two = T::one() + T::one();
                let tan_half_fovy = (fov_y_radians / two).tan();
                let m00 = T::one() / (aspect_ratio * tan_half_fovy);
                let m11 = T::one() / tan_half_fovy;
                let m22 = far / (near - far);
                let m23 = -(far*near) / (far-near);
                let m32 = -T::one();
                Self::new(
                    m00, T::zero(), T::zero(), T::zero(),
                    T::zero(), m11, T::zero(), T::zero(),
                    T::zero(), T::zero(), m22, m23,
                    T::zero(), T::zero(), m32, T::zero()
                )
            }
            /// Creates a perspective projection matrix for left-handed spaces, with zero-to-one depth clip planes.
            pub fn perspective_lh_zo (fov_y_radians: T, aspect_ratio: T, near: T, far: T) -> Self 
                where T: Float
            {
                let mut m = Self::perspective_rh_zo(fov_y_radians, aspect_ratio, near, far);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }

            /// Creates a perspective projection matrix for right-handed spaces, with negative-one-to-one depth clip planes.
            pub fn perspective_rh_no (fov_y_radians: T, aspect_ratio: T, near: T, far: T) -> Self 
                where T: Float
            {
                assert!((aspect_ratio - T::epsilon()).abs() > T::zero());
                let two = T::one() + T::one();
                let tan_half_fovy = (fov_y_radians / two).tan();
                let m00 = T::one() / (aspect_ratio * tan_half_fovy);
                let m11 = T::one() / tan_half_fovy;
                let m22 = -(far + near) / (far - near);
                let m23 = -(two*far*near) / (far-near);
                let m32 = -T::one();
                Self::new(
                    m00, T::zero(), T::zero(), T::zero(),
                    T::zero(), m11, T::zero(), T::zero(),
                    T::zero(), T::zero(), m22, m23,
                    T::zero(), T::zero(), m32, T::zero()
                )
            }
            /// Creates a perspective projection matrix for left-handed spaces, with negative-one-to-one depth clip planes.
            pub fn perspective_lh_no (fov_y_radians: T, aspect_ratio: T, near: T, far: T) -> Self 
                where T: Float
            {
                let mut m = Self::perspective_rh_no(fov_y_radians, aspect_ratio, near, far);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }

            /// Creates a perspective projection matrix for right-handed spaces, with zero-to-one depth clip planes.
            ///
            /// # Panics
            /// `width`, `height` and `fov_y_radians` must all be strictly greater than zero.
            pub fn perspective_fov_rh_zo (fov_y_radians: T, width: T, height: T, near: T, far: T) -> Self 
                where T: Float
            {
                assert!(width > T::zero());
                assert!(height > T::zero());
                assert!(fov_y_radians > T::zero());

                let two = T::one() + T::one();
                let rad = fov_y_radians;
                let h = (rad/two).cos() / (rad/two).sin();
                let w = h * height / width;

                let m00 = w;
                let m11 = h;
                let m22 = far / (near - far);
                let m23 = -(far * near) / (far - near);
                let m32 = -T::one();
                Self::new(
                    m00, T::zero(), T::zero(), T::zero(),
                    T::zero(), m11, T::zero(), T::zero(),
                    T::zero(), T::zero(), m22, m23,
                    T::zero(), T::zero(), m32, T::zero()
                )
            }

            /// Creates a perspective projection matrix for left-handed spaces, with zero-to-one depth clip planes.
            ///
            /// # Panics
            /// `width`, `height` and `fov_y_radians` must all be strictly greater than zero.
            pub fn perspective_fov_lh_zo (fov_y_radians: T, width: T, height: T, near: T, far: T) -> Self 
                where T: Float
            {
                let mut m = Self::perspective_fov_rh_zo(fov_y_radians, width, height, near, far);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }

            /// Creates a perspective projection matrix for right-handed spaces, with negative-one-to-one depth clip planes.
            ///
            /// # Panics
            /// `width`, `height` and `fov_y_radians` must all be strictly greater than zero.
            pub fn perspective_fov_rh_no (fov_y_radians: T, width: T, height: T, near: T, far: T) -> Self 
                where T: Float
            {
                assert!(width > T::zero());
                assert!(height > T::zero());
                assert!(fov_y_radians > T::zero());

                let two = T::one() + T::one();
                let rad = fov_y_radians;
                let h = (rad/two).cos() / (rad/two).sin();
                let w = h * height / width;

                let m00 = w;
                let m11 = h;
                let m22 = -(far + near) / (far - near);
                let m23 = -(two * far * near) / (far - near);
                let m32 = -T::one();
                Self::new(
                    m00, T::zero(), T::zero(), T::zero(),
                    T::zero(), m11, T::zero(), T::zero(),
                    T::zero(), T::zero(), m22, m23,
                    T::zero(), T::zero(), m32, T::zero()
                )
            }
            /// Creates a perspective projection matrix for left-handed spaces, with negative-one-to-one depth clip planes.
            ///
            /// # Panics
            /// `width`, `height` and `fov_y_radians` must all be strictly greater than zero.
            pub fn perspective_fov_lh_no (fov_y_radians: T, width: T, height: T, near: T, far: T) -> Self 
                where T: Float 
            {
                let mut m = Self::perspective_fov_rh_no(fov_y_radians, width, height, near, far);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }


            /// Creates an infinite perspective projection matrix for right-handed spaces.
            ///
	        /// [Link to PDF](http://www.terathon.com/gdc07_lengyel.pdf)
            // From GLM
            pub fn tweaked_infinite_perspective_rh (fov_y_radians: T, aspect_ratio: T, near: T, epsilon: T) -> Self 
                where T: Float
            {
                let two = T::one() + T::one();
                let range = (fov_y_radians / two).tan() * near;
                let left = -range * aspect_ratio;
                let right = range * aspect_ratio;
                let bottom = -range;
                let top = range;

                let m00 = (two * near) / (right - left);
                let m11 = (two * near) / (top - bottom);
                let m22 = epsilon - T::one();
                let m23 = (epsilon - two) * near;
                let m32 = -T::one();
                Self::new(
                    m00, T::zero(), T::zero(), T::zero(),
                    T::zero(), m11, T::zero(), T::zero(),
                    T::zero(), T::zero(), m22, m23,
                    T::zero(), T::zero(), m32, T::zero()
                )
            }

            /// Creates an infinite perspective projection matrix for left-handed spaces.
            pub fn tweaked_infinite_perspective_lh (fov_y_radians: T, aspect_ratio: T, near: T, epsilon: T) -> Self 
                where T: Float
            {
                let mut m = Self::tweaked_infinite_perspective_rh(fov_y_radians, aspect_ratio, near, epsilon);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }

            /// Creates an infinite perspective projection matrix for right-handed spaces.
            pub fn infinite_perspective_rh (fov_y_radians: T, aspect_ratio: T, near: T) -> Self 
                where T: Float
            {
                Self::tweaked_infinite_perspective_rh(fov_y_radians, aspect_ratio, near, T::zero())
            }

            /// Creates an infinite perspective projection matrix for left-handed spaces.
            pub fn infinite_perspective_lh (fov_y_radians: T, aspect_ratio: T, near: T) -> Self 
                where T: Float
            {
                Self::tweaked_infinite_perspective_lh(fov_y_radians, aspect_ratio, near, T::zero())
            }

            //
            // PICKING
            //

            /// GLM's pickMatrix. Creates a projection matrix that can be
            /// used to restrict drawing to a small region of the viewport.
            ///
            /// # Panics
            /// `delta`'s `x` and `y` are required to be strictly greater than zero.
            #[cfg(all(feature="vec3", feature="vec2", feature="geom"))]
            pub fn picking_region<V2: Into<Vec2<T>>>(center: V2, delta: V2, viewport: Rect<T, T>) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                let (center, delta) = (center.into(), delta.into());
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

            /// Returns a matrix that projects from world-space to screen-space,
            /// for a depth clip space ranging from -1 to 1 (`GL_DEPTH_NEGATIVE_ONE_TO_ONE`,
            /// hence the `_no` suffix).
            #[cfg(all(feature="vec3", feature="geom"))]
            pub fn world_to_viewport_no<V3>(obj: V3, modelview: Self, proj: Self, viewport: Rect<T, T>) -> Vec3<T>
                where T: Float + MulAdd<T,T,Output=T>,
                      V3: Into<Vec3<T>>
            {
                // GLM's projectNO()
                let mut tmp = Vec4::from_point(obj.into());
                tmp = modelview * tmp;
                tmp = proj * tmp;

                let two = T::one() + T::one();
                let half_one = T::one() / two;

                tmp = tmp / tmp.w; // Float doesn't imply DivAssign
                tmp = tmp / two + half_one;
                tmp.x = tmp.x * viewport.w + viewport.x;
                tmp.y = tmp.y * viewport.h + viewport.y;

                tmp.into()
            }

            /// Returns a matrix that projects from world-space to screen-space,
            /// for a depth clip space ranging from 0 to 1 (`GL_DEPTH_ZERO_TO_ONE`,
            /// hence the `_zo` suffix).
            #[cfg(all(feature="vec3", feature="geom"))]
            pub fn world_to_viewport_zo<V3>(obj: V3, modelview: Self, proj: Self, viewport: Rect<T, T>) -> Vec3<T>
                where T: Float + MulAdd<T,T,Output=T>,
                      V3: Into<Vec3<T>>
            {
                // GLM's projectZO()
                let mut tmp = Vec4::from_point(obj.into());
                tmp = modelview * tmp;
                tmp = proj * tmp;

                let two = T::one() + T::one();
                let half_one = T::one() / two;

                tmp = tmp / tmp.w; // Float doesn't imply DivAssign
                tmp.x = tmp.x / two + half_one;
                tmp.y = tmp.y / two + half_one;
                tmp.x = tmp.x * viewport.w + viewport.x;
                tmp.y = tmp.y * viewport.h + viewport.y;

                tmp.into()
            }

            /// Returns a matrix that unprojects from screen-space to world-space,
            /// for a depth clip space ranging from 0 to 1 (`GL_DEPTH_ZERO_TO_ONE`,
            /// hence the `_zo` suffix).
            #[cfg(all(feature="vec3", feature="geom"))]
            pub fn viewport_to_world_zo<V3>(ray: V3, modelview: Self, proj: Self, viewport: Rect<T, T>) -> Vec3<T>
                where T: Float + MulAdd<T,T,Output=T>,
                      V3: Into<Vec3<T>>
            {
                let inverse = (proj * modelview).inverted();
                let two = T::one() + T::one();

                let mut tmp = Vec4::from_point(ray.into());
                tmp.x = (tmp.x - viewport.x) / viewport.w;
                tmp.y = (tmp.y - viewport.y) / viewport.h;
                tmp.x = tmp.x * two - T::one();
                tmp.y = tmp.y * two - T::one();

                let mut obj = inverse * tmp;
                obj = obj / obj.w; // Float doesn't imply DivAssign

                obj.into()
            }
            /// Returns a matrix that unprojects from screen-space to world-space,
            /// for a depth clip space ranging from -1 to 1 (`GL_DEPTH_NEGATIVE_ONE_TO_ONE`,
            /// hence the `_no` suffix).
            #[cfg(all(feature="vec3", feature="geom"))]
            pub fn viewport_to_world_no<V3>(ray: V3, modelview: Self, proj: Self, viewport: Rect<T, T>) -> Vec3<T>
                where T: Float + MulAdd<T,T,Output=T>,
                      V3: Into<Vec3<T>>
            {
                let inverse = (proj * modelview).inverted();
                let two = T::one() + T::one();

                let mut tmp = Vec4::from_point(ray.into());
                tmp.x = (tmp.x - viewport.x) / viewport.w;
                tmp.y = (tmp.y - viewport.y) / viewport.h;
                tmp = tmp * two - T::one();

                let mut obj = inverse * tmp;
                obj = obj / obj.w; // Float doesn't imply DivAssign

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

        /// Rotation matrices can be obtained from quaternions.
        /// **This implementation only works properly if the quaternion is normalized**.
        ///
        /// ```
        /// # extern crate vek;
        /// # #[macro_use] extern crate approx;
        /// # use vek::{Quaternion, Mat4, Vec4};
        /// use std::f32::consts::PI;
        ///
        /// # fn main() {
        /// let angles = 32;
        /// for i in 0..angles {
        ///     let theta = PI * 2. * (i as f32) / (angles as f32);
        ///
        ///     assert_relative_eq!(Mat4::rotation_x(theta), Mat4::from(Quaternion::rotation_x(theta)), epsilon = 0.000001);
        ///     assert_relative_eq!(Mat4::rotation_y(theta), Mat4::from(Quaternion::rotation_y(theta)), epsilon = 0.000001);
        ///     assert_relative_eq!(Mat4::rotation_z(theta), Mat4::from(Quaternion::rotation_z(theta)), epsilon = 0.000001);
        ///
        ///     assert_relative_eq!(Mat4::rotation_x(theta), Mat4::rotation_3d(theta, Vec4::unit_x()));
        ///     assert_relative_eq!(Mat4::rotation_y(theta), Mat4::rotation_3d(theta, Vec4::unit_y()));
        ///     assert_relative_eq!(Mat4::rotation_z(theta), Mat4::rotation_3d(theta, Vec4::unit_z()));
        ///
        ///     // See what rotating unit vectors do for most angles between 0 and 2*PI.
        ///     // It's helpful to picture this as a right-handed coordinate system.
        ///
        ///     let v = Vec4::unit_y();
        ///     let m = Mat4::rotation_x(theta);
        ///     assert_relative_eq!(m * v, Vec4::new(0., theta.cos(), theta.sin(), 0.));
        ///
        ///     let v = Vec4::unit_z();
        ///     let m = Mat4::rotation_y(theta);
        ///     assert_relative_eq!(m * v, Vec4::new(theta.sin(), 0., theta.cos(), 0.));
        ///
        ///     let v = Vec4::unit_x();
        ///     let m = Mat4::rotation_z(theta);
        ///     assert_relative_eq!(m * v, Vec4::new(theta.cos(), theta.sin(), 0., 0.));
        /// }
        /// # }
        /// ```
        // NOTE: Logically, this conversion should be implemented for Mat3,
        // and Mat4 would do it by converting itself from a Mat3.
        // Here, we have the other way round, and I'm fine with this, because
        // at best it's better, SIMD-wise, and at worst we don't care that much, seriously.
        #[cfg(feature="quaternion")]
        impl<T> From<Quaternion<T>> for Mat4<T>
            where T: Copy + Zero + One + Mul<Output=T> + Add<Output=T> + Sub<Output=T>
        {
            fn from(q: Quaternion<T>) -> Self {
                // From both GEA and Matrix FAQ
                let Quaternion { x, y, z, w } = q;
                let one = T::one();
                let two = one+one;
                let (x2, y2, z2) = (x*x, y*y, z*z);
                let m00 = one - two*y2 - two*z2;
                let m11 = one - two*x2 - two*z2;
                let m22 = one - two*x2 - two*y2;
                let m01 = two*x*y + two*z*w;
                let m10 = two*x*y - two*z*w;
                let m02 = two*x*z - two*y*w;
                let m20 = two*x*z + two*y*w;
                let m12 = two*y*z + two*x*w;
                let m21 = two*y*z - two*x*w;
                // NOTE: transpose, because otherwise the rotation goes the opposite way.
                // See tests.
                Self::new(
                    m00, m10, m20, T::zero(),
                    m01, m11, m21, T::zero(),
                    m02, m12, m22, T::zero(),
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

#[allow(unused_macros)]
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
            pub fn translate_2d<V: Into<Vec2<T>>>(&mut self, v: V)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.translated_2d(v);
            }
            #[cfg(feature="vec2")]
            pub fn translated_2d<V: Into<Vec2<T>>>(self, v: V) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::translation_2d(v) * self
            }
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
            pub fn scale_3d<V: Into<Vec3<T>>>(&mut self, v: V)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.scaled_3d(v);
            }
            #[cfg(feature="vec3")]
            pub fn scaled_3d<V: Into<Vec3<T>>>(self, v: V) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::scaling_3d(v) * self
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
            pub fn rotate_x(&mut self, angle_radians: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_x(angle_radians);
            }
            pub fn rotated_x(self, angle_radians: T) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::rotation_x(angle_radians) * self
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
            pub fn rotate_y(&mut self, angle_radians: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_y(angle_radians);
            }
            pub fn rotated_y(self, angle_radians: T) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::rotation_y(angle_radians) * self
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
            pub fn rotate_z(&mut self, angle_radians: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_z(angle_radians);
            }
            pub fn rotated_z(self, angle_radians: T) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::rotation_z(angle_radians) * self
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
            pub fn rotate_3d<V: Into<Vec3<T>>>(&mut self, angle_radians: T, axis: V)
                where T: Float + MulAdd<T,T,Output=T> + Sum
            {
                *self = self.rotated_3d(angle_radians, axis);
            }
            #[cfg(feature="vec3")]
            pub fn rotated_3d<V: Into<Vec3<T>>>(self, angle_radians: T, axis: V) -> Self
                where T: Float + MulAdd<T,T,Output=T> + Sum
            {
                Self::rotation_3d(angle_radians, axis) * self
            }
            /// 3D rotation matrix. `axis` is not required to be normalized.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Mat3, Vec3, Quaternion};
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
            /// let angles = 32;
            /// for i in 0..angles {
            ///     let theta = PI * 2. * (i as f32) / (angles as f32);
            ///
            ///     assert_relative_eq!(Mat3::rotation_x(theta), Mat3::from(Quaternion::rotation_x(theta)), epsilon = 0.000001);
            ///     assert_relative_eq!(Mat3::rotation_y(theta), Mat3::from(Quaternion::rotation_y(theta)), epsilon = 0.000001);
            ///     assert_relative_eq!(Mat3::rotation_z(theta), Mat3::from(Quaternion::rotation_z(theta)), epsilon = 0.000001);
            ///
            ///     assert_relative_eq!(Mat3::rotation_x(theta), Mat3::rotation_3d(theta, Vec3::unit_x()));
            ///     assert_relative_eq!(Mat3::rotation_y(theta), Mat3::rotation_3d(theta, Vec3::unit_y()));
            ///     assert_relative_eq!(Mat3::rotation_z(theta), Mat3::rotation_3d(theta, Vec3::unit_z()));
            ///
            ///     assert_relative_eq!(Mat3::rotation_x(theta), Mat3::from(Mat4::rotation_3d(theta, Vec3::unit_x())));
            ///     assert_relative_eq!(Mat3::rotation_y(theta), Mat3::from(Mat4::rotation_3d(theta, Vec3::unit_y())));
            ///     assert_relative_eq!(Mat3::rotation_z(theta), Mat3::from(Mat4::rotation_3d(theta, Vec3::unit_z())));
            ///
            ///     assert_relative_eq!(Mat4::rotation_x(theta), Mat4::from(Mat3::rotation_3d(theta, Vec3::unit_x())));
            ///     assert_relative_eq!(Mat4::rotation_y(theta), Mat4::from(Mat3::rotation_3d(theta, Vec3::unit_y())));
            ///     assert_relative_eq!(Mat4::rotation_z(theta), Mat4::from(Mat3::rotation_3d(theta, Vec3::unit_z())));
            ///
            ///     // See what rotating unit vectors do for most angles between 0 and 2*PI.
            ///     // It's helpful to picture this as a right-handed coordinate system.
            ///
            ///     let v = Vec3::unit_y();
            ///     let m = Mat3::rotation_x(theta);
            ///     assert_relative_eq!(m * v, Vec3::new(0., theta.cos(), theta.sin()));
            ///
            ///     let v = Vec3::unit_z();
            ///     let m = Mat3::rotation_y(theta);
            ///     assert_relative_eq!(m * v, Vec3::new(theta.sin(), 0., theta.cos()));
            ///
            ///     let v = Vec3::unit_x();
            ///     let m = Mat3::rotation_z(theta);
            ///     assert_relative_eq!(m * v, Vec3::new(theta.cos(), theta.sin(), 0.));
            /// }
            /// # }
            /// ```
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

#[allow(unused_macros)]
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
            pub fn rotate_z(&mut self, angle_radians: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_z(angle_radians);
            }
            pub fn rotated_z(self, angle_radians: T) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::rotation_z(angle_radians) * self
            }
            pub fn rotation_z(angle_radians: T) -> Self where T: Float {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, -s,
                    s,  c
                )
            }
            pub fn scale_2d<V: Into<Vec2<T>>>(&mut self, v: V)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.scaled_2d(v);
            }
            pub fn scaled_2d<V: Into<Vec2<T>>>(self, v: V) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::scaling_2d(v) * self
            }
            pub fn scaling_2d<V: Into<Vec2<T>>>(v: V) -> Self where T: Zero {
                let Vec2 { x, y } = v.into();
                Self::new(
                    x, T::zero(),
                    T::zero(), y
                )
            }
            pub fn shear_x(&mut self, k: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.sheared_x(k);
            }
            pub fn sheared_x(self, k: T) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::shearing_x(k) * self
            }
            pub fn shearing_x(k: T) -> Self where T: Zero + One {
                Self::new(
                    T::one(), k,
                    T::zero(), T::one()
                )
            }
            pub fn shear_y(&mut self, k: T)
                where T: Float + MulAdd<T,T,Output=T>
            {
                *self = self.sheared_y(k);
            }
            pub fn sheared_y(self, k: T) -> Self
                where T: Float + MulAdd<T,T,Output=T>
            {
                Self::shearing_y(k) * self
            }
            pub fn shearing_y(k: T) -> Self where T: Zero + One {
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
            //! Also, their natural indexing order matches the existing mathematical conventions.

            use super::*;
            mat_impl_all_mats!{rows}
        }
        /// Column-major layout is the default.
        ///
        /// Rationale:
        /// - (Matrix * Vector) multiplications are more efficient;
        /// - This is the layout expected by OpenGL;
        pub use column_major::*;
    }
}

pub mod repr_c {
    //! Matrix types which use `#[repr(C)]` vectors exclusively.
    //! 
    //! See also the `repr_simd` neighbour module, which is available on Nightly
    //! with the `repr_simd` feature enabled.

    use super::*;
    #[allow(unused_imports)]
    #[cfg(feature="vec2")]
    use super::vec::repr_c::{Vec2, Vec2 as CVec2};
    #[allow(unused_imports)]
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
    //! Matrix types which use a `#[repr(C)]` vector of `#[repr(simd)]` vectors.
   
    use super::*;
    #[allow(unused_imports)]
    #[cfg(feature="vec2")]
    use super::vec::repr_simd::{Vec2};
    #[allow(unused_imports)]
    #[cfg(feature="vec2")]
    use super::vec::repr_c::{Vec2 as CVec2};
    #[allow(unused_imports)]
    #[cfg(feature="vec3")]
    use super::vec::repr_simd::{Vec3};
    #[allow(unused_imports)]
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

pub use self::repr_c::*;


#[cfg(test)]
mod tests {
    macro_rules! for_each_type {
        ($mat:ident $Mat:ident $($T:ident)+) => {
            mod $mat {
                // repr_c matrices should be packed.
                // repr_simd matrices are not necessarily expected to.
                mod repr_c {
                    $(mod $T {
                        use $crate::mat::repr_c::$Mat;
                        #[test]
                        fn is_packed() {
                            assert!($Mat::<$T>::default().is_packed());
                        }
                    })+
                }
            }
        };
    }
    // Vertical editing helps here :)
    #[cfg(feature="mat2")]    for_each_type!{mat2 Mat2 i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    #[cfg(feature="mat3")]    for_each_type!{mat3 Mat3 i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    /*#[cfg(feature="mat4")]*/for_each_type!{mat4 Mat4 i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
}

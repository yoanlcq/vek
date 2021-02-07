//! Matrix types.

use std::mem;
use std::ptr;
use std::slice;
use std::ops::Add;
use std::fmt::{self, Display, Formatter, Debug};
use std::ops::*;
use num_traits::{Zero, One, real::Real, FloatConst, NumCast, AsPrimitive};
use approx::{AbsDiffEq, RelativeEq, UlpsEq};
use crate::ops::MulAdd;
use crate::vec;
use crate::geom::{Rect, FrustumPlanes}; // NOTE: Rect is therefore always repr_c here
use crate::quaternion;
use crate::transform;

// Needed because mem::transmute() isn't clever enough to figure out that e.g [T; 16] and [[T; 4]; 4]
// always have the exact same size and layout, regardless of T. The opposite is impossible.
// See https://github.com/rust-lang/rust/issues/47966
#[inline(always)]
unsafe fn transmute_unchecked<S, D>(s: S) -> D {
    debug_assert_eq!(mem::size_of::<S>(), mem::size_of::<D>());
    let d = ptr::read(&s as *const _ as *const D);
    mem::forget(s);
    d
}

macro_rules! mat_impl_mat {
    (rows $Mat:ident $MintRowMat:ident $MintColMat:ident $CVec:ident $Vec:ident ($nrows:tt x $ncols:tt) ($($get:tt)+)) => {

        mat_impl_mat!{common rows $Mat $CVec $Vec ($nrows x $ncols) ($($get)+)}

        use super::column_major::$Mat as Transpose;

        #[cfg(feature = "mint")]
        impl<T> From<mint::$MintRowMat<T>> for $Mat<T> {
            fn from(m: mint::$MintRowMat<T>) -> Self {
                Self {
                    rows: $CVec {
                        $($get : m.$get.into()),+
                    }
                }
            }
        }

        #[cfg(feature = "mint")]
        impl<T> Into<mint::$MintRowMat<T>> for $Mat<T> {
            fn into(self) -> mint::$MintRowMat<T> {
                mint::$MintRowMat {
                    $($get: self.rows.$get.into()),+
                }
            }
        }

        #[cfg(feature = "mint")]
        impl<T> From<mint::$MintColMat<T>> for $Mat<T> {
            fn from(m: mint::$MintColMat<T>) -> Self {
                Transpose::from(m).into()
            }
        }

        #[cfg(feature = "mint")]
        impl<T> Into<mint::$MintColMat<T>> for $Mat<T> {
            fn into(self) -> mint::$MintColMat<T> {
                Transpose::from(self).into()
            }
        }

        impl<T> $Mat<T> {
            /// Returns a row-wise-converted copy of this matrix, using the given conversion
            /// closure.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<f32>::new(
            ///     0.25, 1.25, 5.56, 8.66,
            ///     8.53, 2.92, 3.86, 9.36,
            ///     1.02, 0.28, 5.52, 6.06,
            ///     6.20, 7.01, 4.90, 5.26
            /// );
            /// let m = m.map_rows(|row| row.map(|x| x.round() as i32));
            /// assert_eq!(m, Mat4::new(
            ///     0, 1, 6, 9,
            ///     9, 3, 4, 9,
            ///     1, 0, 6, 6,
            ///     6, 7, 5, 5
            /// ));
            /// ```
            pub fn map_rows<D,F>(self, mut f: F) -> $Mat<D> where F: FnMut($Vec<T>) -> $Vec<D> {
                $Mat { rows: $CVec::new($(f(self.rows.$get)),+) }
            }

            /// Converts this matrix into a fixed-size array of elements.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// ];
            /// assert_eq!(m.into_row_array(), array);
            /// ```
            pub fn into_row_array(self) -> [T; $nrows*$ncols] {
                let m = mem::ManuallyDrop::new(self);
                let mut cur = 0;
                unsafe {
                    let mut array: mem::MaybeUninit<[T; $nrows*$ncols]> = mem::MaybeUninit::uninit();
                    for i in 0..$nrows {
                        let row = m.rows.get_unchecked(i);
                        $(
                            mem::forget(mem::replace((&mut *array.as_mut_ptr()).get_unchecked_mut(cur), ptr::read(&row.$get)));
                            cur += 1;
                        )+
                    }
                    array.assume_init()
                }
            }
            /// Converts this matrix into a fixed-size array of fixed-size arrays of elements.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     [  0,  1,  2,  3, ],
            ///     [  4,  5,  6,  7, ],
            ///     [  8,  9, 10, 11, ],
            ///     [ 12, 13, 14, 15, ],
            /// ];
            /// assert_eq!(m.into_row_arrays(), array);
            /// ```
            pub fn into_row_arrays(self) -> [[T; $ncols]; $nrows] {
                unsafe {
                    transmute_unchecked(self.into_row_array())
                }
            }
            /// Converts a fixed-size array of elements into a matrix.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// ];
            /// assert_eq!(m, Mat4::from_row_array(array));
            /// ```
            pub fn from_row_array(array: [T; $nrows*$ncols]) -> Self {
                let array = mem::ManuallyDrop::new(array);
                let mut cur = 0;
                unsafe {
                    let mut m: mem::MaybeUninit<Self> = mem::MaybeUninit::uninit();
                    for i in 0..$nrows {
                        let row = (&mut *m.as_mut_ptr()).rows.get_unchecked_mut(i);
                        $(
                            mem::forget(mem::replace(&mut row.$get, ptr::read(array.get_unchecked(cur))));
                            cur += 1;
                        )+
                    }
                    m.assume_init()
                }
            }
            /// Converts a fixed-size array of fixed-size array of elements into a matrix.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     [  0,  1,  2,  3, ],
            ///     [  4,  5,  6,  7, ],
            ///     [  8,  9, 10, 11, ],
            ///     [ 12, 13, 14, 15, ],
            /// ];
            /// assert_eq!(m, Mat4::from_row_arrays(array));
            /// ```
            pub fn from_row_arrays(array: [[T; $ncols]; $nrows]) -> Self {
                Self::from_row_array(unsafe { transmute_unchecked(array) })
            }
            /// Converts this matrix into a fixed-size array of elements.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     0, 4, 8, 12,
            ///     1, 5, 9, 13,
            ///     2, 6, 10, 14,
            ///     3, 7, 11, 15
            /// ];
            /// assert_eq!(m.into_col_array(), array);
            /// ```
            pub fn into_col_array(self) -> [T; $nrows*$ncols] {
                let m = mem::ManuallyDrop::new(self);
                let mut cur = 0;
                unsafe {
                    let mut array: mem::MaybeUninit<[T; $nrows*$ncols]> = mem::MaybeUninit::uninit();
                    $(
                        for i in 0..$nrows {
                            mem::forget(mem::replace((&mut *array.as_mut_ptr()).get_unchecked_mut(cur), ptr::read(&m.rows.get_unchecked(i).$get)));
                            cur += 1;
                        }
                    )+
                    array.assume_init()
                }
            }
            /// Converts this matrix into a fixed-size array of fixed-size arrays of elements.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     [ 0, 4,  8, 12, ],
            ///     [ 1, 5,  9, 13, ],
            ///     [ 2, 6, 10, 14, ],
            ///     [ 3, 7, 11, 15, ],
            /// ];
            /// assert_eq!(m.into_col_arrays(), array);
            /// ```
            pub fn into_col_arrays(self) -> [[T; $nrows]; $ncols] {
                unsafe {
                    transmute_unchecked(self.into_col_array())
                }
            }
            /// Converts a fixed-size array of elements into a matrix.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     0, 4, 8, 12,
            ///     1, 5, 9, 13,
            ///     2, 6, 10, 14,
            ///     3, 7, 11, 15
            /// ];
            /// assert_eq!(m, Mat4::from_col_array(array));
            /// ```
            pub fn from_col_array(array: [T; $nrows*$ncols]) -> Self {
                let array = mem::ManuallyDrop::new(array);
                let mut cur = 0;
                unsafe {
                    let mut m: mem::MaybeUninit<Self> = mem::MaybeUninit::uninit();
                    $(
                        for i in 0..$nrows {
                            mem::forget(mem::replace(&mut (&mut *m.as_mut_ptr()).rows.get_unchecked_mut(i).$get, ptr::read(array.get_unchecked(cur))));
                            cur += 1;
                        }
                    )+
                    m.assume_init()
                }
            }
            /// Converts a fixed-size array of fixed-size arrays of elements into a matrix.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     [ 0, 4,  8, 12, ],
            ///     [ 1, 5,  9, 13, ],
            ///     [ 2, 6, 10, 14, ],
            ///     [ 3, 7, 11, 15, ],
            /// ];
            /// assert_eq!(m, Mat4::from_col_arrays(array));
            /// ```
            pub fn from_col_arrays(array: [[T; $nrows]; $ncols]) -> Self {
                Self::from_col_array(unsafe { transmute_unchecked(array) })
            }
            /// Gets a const pointer to this matrix's elements.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            /// You may check this with the `is_packed()` method.
            pub fn as_row_ptr(&self) -> *const T {
                debug_assert!(self.is_packed());
                self.rows.as_ptr() as *const _ as *const T
            }
            /// Gets a mut pointer to this matrix's elements.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            /// You may check this with the `is_packed()` method.
            pub fn as_mut_row_ptr(&mut self) -> *mut T {
                debug_assert!(self.is_packed());
                self.rows.as_mut_ptr() as *mut _ as *mut T
            }
            /// View this matrix as an immutable slice.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            /// You may check this with the `is_packed()` method.
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
            /// You may check this with the `is_packed()` method.
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
        /// Note that elements are not comma-separated.
        /// This format doesn't depend on the matrix's storage layout.
        impl<T: Display> Display for $Mat<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "(")?;
                let mut rows = self.rows.iter();
                // first row goes after the opening paren
                if let Some(row) = rows.next(){
                    for elem in row {
                        write!(f, " ")?;
                        elem.fmt(f)?;
                    }
                }
                // subsequent rows start on a new line
                for row in rows{
                    write!(f, "\n ")?;
                    for elem in row {
                        write!(f, " ")?;
                        elem.fmt(f)?;
                    }
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
            /// Gets the `transpose` parameter to pass to OpenGL `glUniformMatrix*()` functions.
            ///
            /// The return value is a plain `bool` which you may directly cast
            /// to a `GLboolean`.
            ///
            /// This takes `&self` to prevent surprises when changing the type
            /// of matrix you plan to send.
            pub fn gl_should_transpose(&self) -> bool {
                true
            }
            /// The `transpose` parameter to pass to OpenGL `glUniformMatrix*()` functions.
            pub const GL_SHOULD_TRANSPOSE: bool = true;
        }

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
        /// let m = Mat4::<u32>::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let r = Mat4::<u32>::new(
        ///     26, 32, 18, 24,
        ///     82, 104, 66, 88,
        ///     38, 56, 74, 92,
        ///     54, 68, 42, 56
        /// );
        /// assert_eq!(m * m, r);
        /// assert_eq!(m, m * Mat4::<u32>::identity());
        /// assert_eq!(m, Mat4::<u32>::identity() * m);
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
        /// let m = Rows4::<u32>::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let b = Cols4::from(m);
        /// let r = Cols4::<u32>::new(
        ///     26, 32, 18, 24,
        ///     82, 104, 66, 88,
        ///     38, 56, 74, 92,
        ///     54, 68, 42, 56
        /// );
        /// assert_eq!(m * b, r);
        /// assert_eq!(m * Cols4::<u32>::identity(), m.into());
        /// assert_eq!(Rows4::<u32>::identity() * b, m.into());
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
    (cols $Mat:ident $MintRowMat:ident $MintColMat:ident $CVec:ident $Vec:ident ($nrows:tt x $ncols:tt) ($($get:tt)+)) => {

        mat_impl_mat!{common cols $Mat $CVec $Vec ($nrows x $ncols) ($($get)+)}

        use super::row_major::$Mat as Transpose;

        #[cfg(feature = "mint")]
        impl<T> From<mint::$MintColMat<T>> for $Mat<T> {
            fn from(m: mint::$MintColMat<T>) -> Self {
                Self {
                    cols: $CVec {
                        $($get : m.$get.into()),+
                    }
                }
            }
        }

        #[cfg(feature = "mint")]
        impl<T> Into<mint::$MintColMat<T>> for $Mat<T> {
            fn into(self) -> mint::$MintColMat<T> {
                mint::$MintColMat {
                    $($get: self.cols.$get.into()),+
                }
            }
        }

        #[cfg(feature = "mint")]
        impl<T> From<mint::$MintRowMat<T>> for $Mat<T> {
            fn from(m: mint::$MintRowMat<T>) -> Self {
                Transpose::from(m).into()
            }
        }

        #[cfg(feature = "mint")]
        impl<T> Into<mint::$MintRowMat<T>> for $Mat<T> {
            fn into(self) -> mint::$MintRowMat<T> {
                Transpose::from(self).into()
            }
        }


        impl<T> $Mat<T> {
            /// Returns a column-wise-converted copy of this matrix, using the given conversion
            /// closure.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<f32>::new(
            ///     0.25, 1.25, 5.56, 8.66,
            ///     8.53, 2.92, 3.86, 9.36,
            ///     1.02, 0.28, 5.52, 6.06,
            ///     6.20, 7.01, 4.90, 5.26
            /// );
            /// let m = m.map_cols(|col| col.map(|x| x.round() as i32));
            /// assert_eq!(m, Mat4::new(
            ///     0, 1, 6, 9,
            ///     9, 3, 4, 9,
            ///     1, 0, 6, 6,
            ///     6, 7, 5, 5
            /// ));
            /// ```
            pub fn map_cols<D,F>(self, mut f: F) -> $Mat<D> where F: FnMut($Vec<T>) -> $Vec<D> {
                $Mat { cols: $CVec::new($(f(self.cols.$get)),+) }
            }


            /// Converts this matrix into a fixed-size array of elements.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     0, 4, 8, 12,
            ///     1, 5, 9, 13,
            ///     2, 6, 10, 14,
            ///     3, 7, 11, 15
            /// ];
            /// assert_eq!(m.into_col_array(), array);
            /// ```
            pub fn into_col_array(self) -> [T; $nrows*$ncols] {
                let m = mem::ManuallyDrop::new(self);
                let mut cur = 0;
                unsafe {
                    let mut array: mem::MaybeUninit<[T; $nrows*$ncols]> = mem::MaybeUninit::uninit();
                    for i in 0..$ncols {
                        let col = m.cols.get_unchecked(i);
                        $(
                            mem::forget(mem::replace((&mut *array.as_mut_ptr()).get_unchecked_mut(cur), ptr::read(&col.$get)));
                            cur += 1;
                        )+
                    }
                    array.assume_init()
                }
            }
            /// Converts this matrix into a fixed-size array of fixed-size arrays of elements.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     [ 0, 4,  8, 12, ],
            ///     [ 1, 5,  9, 13, ],
            ///     [ 2, 6, 10, 14, ],
            ///     [ 3, 7, 11, 15, ],
            /// ];
            /// assert_eq!(m.into_col_arrays(), array);
            /// ```
            pub fn into_col_arrays(self) -> [[T; $nrows]; $ncols] {
                unsafe {
                    transmute_unchecked(self.into_col_array())
                }
            }
            /// Converts a fixed-size array of elements into a matrix.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     0, 4, 8, 12,
            ///     1, 5, 9, 13,
            ///     2, 6, 10, 14,
            ///     3, 7, 11, 15
            /// ];
            /// assert_eq!(m, Mat4::from_col_array(array));
            /// ```
            pub fn from_col_array(array: [T; $nrows*$ncols]) -> Self {
                let array = mem::ManuallyDrop::new(array);
                let mut cur = 0;
                unsafe {
                    let mut m: mem::MaybeUninit<Self> = mem::MaybeUninit::uninit();
                    for i in 0..$ncols {
                        let col = (&mut *m.as_mut_ptr()).cols.get_unchecked_mut(i);
                        $(
                            mem::forget(mem::replace(&mut col.$get, ptr::read(array.get_unchecked(cur))));
                            cur += 1;
                        )+
                    }
                    m.assume_init()
                }
            }
            /// Converts a fixed-size array of fixed-size arrays of elements into a matrix.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     [ 0, 4,  8, 12, ],
            ///     [ 1, 5,  9, 13, ],
            ///     [ 2, 6, 10, 14, ],
            ///     [ 3, 7, 11, 15, ],
            /// ];
            /// assert_eq!(m, Mat4::from_col_arrays(array));
            /// ```
            pub fn from_col_arrays(array: [[T; $nrows]; $ncols]) -> Self {
                Self::from_col_array(unsafe { transmute_unchecked(array) })
            }
            /// Converts this matrix into a fixed-size array of elements.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// ];
            /// assert_eq!(m.into_row_array(), array);
            /// ```
            pub fn into_row_array(self) -> [T; $nrows*$ncols] {
                let m = mem::ManuallyDrop::new(self);
                let mut cur = 0;
                unsafe {
                    let mut array: mem::MaybeUninit<[T; $nrows*$ncols]> = mem::MaybeUninit::uninit();
                    $(
                        for i in 0..$ncols {
                            mem::forget(mem::replace((&mut *array.as_mut_ptr()).get_unchecked_mut(cur), ptr::read(&m.cols.get_unchecked(i).$get)));
                            cur += 1;
                        }
                    )+
                    array.assume_init()
                }
            }
            /// Converts this matrix into a fixed-size array of fixed-size arrays of elements.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     [  0,  1,  2,  3, ],
            ///     [  4,  5,  6,  7, ],
            ///     [  8,  9, 10, 11, ],
            ///     [ 12, 13, 14, 15, ],
            /// ];
            /// assert_eq!(m.into_row_arrays(), array);
            /// ```
            pub fn into_row_arrays(self) -> [[T; $ncols]; $nrows] {
                unsafe {
                    transmute_unchecked(self.into_row_array())
                }
            }
            /// Converts a fixed-size array of elements into a matrix.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// ];
            /// assert_eq!(m, Mat4::from_row_array(array));
            /// ```
            pub fn from_row_array(array: [T; $nrows*$ncols]) -> Self {
                let array = mem::ManuallyDrop::new(array);
                let mut cur = 0;
                unsafe {
                    let mut m: mem::MaybeUninit<Self> = mem::MaybeUninit::uninit();
                    $(
                        for i in 0..$ncols {
                            mem::forget(mem::replace(&mut (&mut *m.as_mut_ptr()).cols.get_unchecked_mut(i).$get, ptr::read(array.get_unchecked(cur))));
                            cur += 1;
                        }
                    )+
                    m.assume_init()
                }
            }
            /// Converts a fixed-size array of fixed-size array of elements into a matrix.
            ///
            /// ```
            /// use vek::mat::repr_c::column_major::Mat4;
            ///
            /// let m = Mat4::<u32>::new(
            ///      0,  1,  2,  3,
            ///      4,  5,  6,  7,
            ///      8,  9, 10, 11,
            ///     12, 13, 14, 15
            /// );
            /// let array = [
            ///     [  0,  1,  2,  3, ],
            ///     [  4,  5,  6,  7, ],
            ///     [  8,  9, 10, 11, ],
            ///     [ 12, 13, 14, 15, ],
            /// ];
            /// assert_eq!(m, Mat4::from_row_arrays(array));
            /// ```
            pub fn from_row_arrays(array: [[T; $ncols]; $nrows]) -> Self {
                Self::from_row_array(unsafe { transmute_unchecked(array) })
            }

            /// Gets a const pointer to this matrix's elements.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            /// You may check this with the `is_packed()` method.
            pub fn as_col_ptr(&self) -> *const T {
                debug_assert!(self.is_packed());
                self.cols.as_ptr() as *const _ as *const T
            }
            /// Gets a mut pointer to this matrix's elements.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            /// You may check this with the `is_packed()` method.
            pub fn as_mut_col_ptr(&mut self) -> *mut T {
                debug_assert!(self.is_packed());
                self.cols.as_mut_ptr() as *mut _ as *mut T
            }
            /// View this matrix as an immutable slice.
            ///
            /// # Panics
            /// Panics if the matrix's elements are not tightly packed in memory,
            /// which may be the case for matrices in the `repr_simd` module.
            /// You may check this with the `is_packed()` method.
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
            /// You may check this with the `is_packed()` method.
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
        /// Note that elements are not comma-separated.
        /// This format doesn't depend on the matrix's storage layout.
        impl<T: Display> Display for $Mat<T> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "(")?;
                // first row goes after the opening paren
                for x in 0..$ncols {
                    write!(f, " ")?;
                    let elem = unsafe {
                        self.cols.get_unchecked(x).get_unchecked(0)
                    };
                    elem.fmt(f)?;
                }
                // subsequent rows start on a new line
                for y in 1..$nrows {
                    write!(f, "\n ")?;
                    for x in 0..$ncols {
                        write!(f, " ")?;
                        let elem = unsafe {
                            self.cols.get_unchecked(x).get_unchecked(y)
                        };
                        elem.fmt(f)?;
                    }
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
            /// Gets the `transpose` parameter to pass to OpenGL `glUniformMatrix*()` functions.
            ///
            /// The return value is a plain `bool` which you may directly cast
            /// to a `GLboolean`.
            ///
            /// This takes `&self` to prevent surprises when changing the type
            /// of matrix you plan to send.
            pub fn gl_should_transpose(&self) -> bool {
                false
            }
            /// The `transpose` parameter to pass to OpenGL `glUniformMatrix*()` functions.
            pub const GL_SHOULD_TRANSPOSE: bool = false;
        }

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
        /// let m = Mat4::<u32>::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let r = Mat4::<u32>::new(
        ///     26, 32, 18, 24,
        ///     82, 104, 66, 88,
        ///     38, 56, 74, 92,
        ///     54, 68, 42, 56
        /// );
        /// assert_eq!(m * m, r);
        /// assert_eq!(m, m * Mat4::<u32>::identity());
        /// assert_eq!(m, Mat4::<u32>::identity() * m);
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
        /// let m = Cols4::<u32>::new(
        ///     0, 1, 2, 3,
        ///     4, 5, 6, 7,
        ///     8, 9, 0, 1,
        ///     2, 3, 4, 5
        /// );
        /// let b = Rows4::from(m);
        /// let r = Rows4::<u32>::new(
        ///     26, 32, 18, 24,
        ///     82, 104, 66, 88,
        ///     38, 56, 74, 92,
        ///     54, 68, 42, 56
        /// );
        /// assert_eq!(m * b, r);
        /// assert_eq!(m * Rows4::<u32>::identity(), m.into());
        /// assert_eq!(Cols4::<u32>::identity() * b, m.into());
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
        // Welp, not using RelativeEq here - integers don't implement it :(
        impl<T: Zero + PartialEq> Zero for $Mat<T> {
            fn zero() -> Self { Self::zero() }
            fn is_zero(&self) -> bool { self == &Self::zero() }
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
            /// Applies the function f to each element of this matrix, in-place.
            ///
            /// For an example, see the `map()` method.
            pub fn apply<F>(&mut self, f: F) where T: Copy, F: FnMut(T) -> T {
                *self = self.map(f);
            }
            /// Applies the function f to each element of this matrix, in-place.
            ///
            /// For an example, see the `map2()` method.
            pub fn apply2<F, S>(&mut self, other: $Mat<S>, f: F) where T: Copy, F: FnMut(T, S) -> T {
                *self = self.map2(other, f);
            }
            /// Returns a memberwise-converted copy of this matrix, using `NumCast`.
            ///
            /// ```
            /// # use vek::Mat4;
            /// let m = Mat4::<f32>::identity();
            /// let m: Mat4<i32> = m.numcast().unwrap();
            /// assert_eq!(m, Mat4::identity());
            /// ```
            pub fn numcast<D>(self) -> Option<$Mat<D>> where T: NumCast, D: NumCast {
                // NOTE: Should use `?` for conciseness, but docs.rs uses rustc 1.22 and doesn't seem to like that.
                Some($Mat {
                    $lines: $CVec {
                        $($get: match self.$lines.$get.numcast() {
                            Some(x) => x,
                            None => return None,
                        },)+
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
            pub fn with_diagonal(d: $Vec<T>) -> Self where T: Zero + Copy {
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
            pub fn trace(self) -> T where T: Add<T, Output=T> {
                self.diagonal().sum()
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
            /// Convenience constant representing the number of rows for matrices of this type.
            pub const ROW_COUNT: usize = $nrows;
            /// Convenience constant representing the number of columns for matrices of this type.
            pub const COL_COUNT: usize = $ncols;

            /// Are all elements of this matrix tightly packed together in memory ?
            ///
            /// This might not be the case for matrices in the `repr_simd` module
            /// (it depends on the target architecture).
            pub fn is_packed(&self) -> bool {
                mem::size_of::<Self>() == $nrows*$ncols*mem::size_of::<T>()
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

        impl<T: AbsDiffEq> AbsDiffEq for $Mat<T> where T::Epsilon: Copy {
            type Epsilon = T::Epsilon;

            fn default_epsilon() -> T::Epsilon {
                T::default_epsilon()
            }

            fn abs_diff_eq(&self, other: &Self, epsilon: Self::Epsilon) -> bool {
                for (l, r) in self.$lines.iter().zip(other.$lines.iter()) {
                    if !AbsDiffEq::abs_diff_eq(l, r, epsilon) {
                        return false;
                    }
                }
                true
            }
        }

        impl<T: UlpsEq> UlpsEq for $Mat<T> where T::Epsilon: Copy {
            fn default_max_ulps() -> u32 {
                T::default_max_ulps()
            }

            fn ulps_eq(&self, other: &Self, epsilon: T::Epsilon, max_ulps: u32) -> bool {
                for (l, r) in self.$lines.iter().zip(other.$lines.iter()) {
                    if !UlpsEq::ulps_eq(l, r, epsilon, max_ulps) {
                        return false;
                    }
                }
                true
            }
        }

        impl<T: RelativeEq> RelativeEq for $Mat<T> where T::Epsilon: Copy {
            fn default_max_relative() -> T::Epsilon {
                T::default_max_relative()
            }

            fn relative_eq(&self, other: &Self, epsilon: T::Epsilon, max_relative: T::Epsilon) -> bool {
                for (l, r) in self.$lines.iter().zip(other.$lines.iter()) {
                    if !RelativeEq::relative_eq(l, r, epsilon, max_relative) {
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
        use super::column_major::Mat4 as Cols4;

        impl<T> Mat4<T> {
            /// Returns an element-wise-converted copy of this matrix, using the given conversion
            /// closure.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<f32>::new(
            ///     0.25, 1.25, 5.56, 8.66,
            ///     8.53, 2.92, 3.86, 9.36,
            ///     1.02, 0.28, 5.52, 6.06,
            ///     6.20, 7.01, 4.90, 5.26
            /// );
            /// let m = m.map(|x| x.round() as i32);
            /// assert_eq!(m, Mat4::new(
            ///     0, 1, 6, 9,
            ///     9, 3, 4, 9,
            ///     1, 0, 6, 6,
            ///     6, 7, 5, 5
            /// ));
            /// ```
            pub fn map<D,F>(self, mut f: F) -> Mat4<D> where F: FnMut(T) -> D {
                let m = self.$lines;
                Mat4 {
                    $lines: CVec4::new(
                        Vec4::new(f(m.x.x), f(m.x.y), f(m.x.z), f(m.x.w)),
                        Vec4::new(f(m.y.x), f(m.y.y), f(m.y.z), f(m.y.w)),
                        Vec4::new(f(m.z.x), f(m.z.y), f(m.z.z), f(m.z.w)),
                        Vec4::new(f(m.w.x), f(m.w.y), f(m.w.z), f(m.w.w))
                    )
                }
            }

            /// Returns a memberwise-converted copy of this matrix, using `AsPrimitive`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let v = Vec4::new(0_f32, 1., 2., 3.);
            /// let i: Vec4<i32> = v.as_();
            /// assert_eq!(i, Vec4::new(0, 1, 2, 3));
            /// ```
            ///
            /// # Safety
            ///
            /// **In Rust versions before 1.45.0**, some uses of the `as` operator were not entirely safe.
            /// In particular, it was undefined behavior if
            /// a truncated floating point value could not fit in the target integer
            /// type ([#10184](https://github.com/rust-lang/rust/issues/10184));
            ///
            /// ```ignore
            /// # use num_traits::AsPrimitive;
            /// let x: u8 = (1.04E+17).as_(); // UB
            /// ```
            pub fn as_<D>(self) -> Mat4<D> where T: AsPrimitive<D>, D: 'static + Copy {
                let m = self.$lines;
                Mat4 {
                    $lines: CVec4::new(
                        Vec4::new(m.x.x.as_(), m.x.y.as_(), m.x.z.as_(), m.x.w.as_()),
                        Vec4::new(m.y.x.as_(), m.y.y.as_(), m.y.z.as_(), m.y.w.as_()),
                        Vec4::new(m.z.x.as_(), m.z.y.as_(), m.z.z.as_(), m.z.w.as_()),
                        Vec4::new(m.w.x.as_(), m.w.y.as_(), m.w.z.as_(), m.w.w.as_())
                    )
                }
            }

            /// Applies the function f to each element of two matrices, pairwise, and returns the result.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let a = Mat4::<f32>::new(
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99
            /// );
            /// let b = Mat4::<i32>::new(
            ///     0, 1, 0, 0,
            ///     1, 0, 0, 0,
            ///     0, 0, 1, 0,
            ///     0, 0, 0, 1
            /// );
            /// let m = a.map2(b, |a, b| a.round() as i32 + b);
            /// assert_eq!(m, Mat4::new(
            ///     0, 2, 3, 3,
            ///     1, 1, 3, 3,
            ///     0, 1, 4, 3,
            ///     0, 1, 3, 4
            /// ));
            /// ```
            pub fn map2<D,F,S>(self, other: Mat4<S>, mut f: F) -> Mat4<D> where F: FnMut(T, S) -> D {
                let m = self.$lines;
                let o = other.$lines;
                Mat4 {
                    $lines: CVec4::new(
                        Vec4::new(f(m.x.x, o.x.x), f(m.x.y, o.x.y), f(m.x.z, o.x.z), f(m.x.w, o.x.w)),
                        Vec4::new(f(m.y.x, o.y.x), f(m.y.y, o.y.y), f(m.y.z, o.y.z), f(m.y.w, o.y.w)),
                        Vec4::new(f(m.z.x, o.z.x), f(m.z.y, o.z.y), f(m.z.z, o.z.z), f(m.z.w, o.z.w)),
                        Vec4::new(f(m.w.x, o.w.x), f(m.w.y, o.w.y), f(m.w.z, o.w.z), f(m.w.w, o.w.w))
                    )
                }
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
            /// // By the way, demonstrate ways to invert a rotation matrix,
            /// // from fastest (specific) to slowest (general-purpose).
            /// let m = Mat4::rotation_x(PI/7.);
            /// let id = Mat4::identity();
            /// assert_relative_eq!(id, m * m.transposed());
            /// assert_relative_eq!(id, m.transposed() * m);
            /// assert_relative_eq!(id, m * m.inverted_affine_transform_no_scale());
            /// assert_relative_eq!(id, m.inverted_affine_transform_no_scale() * m);
            /// assert_relative_eq!(id, m * m.inverted_affine_transform());
            /// assert_relative_eq!(id, m.inverted_affine_transform() * m);
            /// assert_relative_eq!(id, m * m.inverted());
            /// assert_relative_eq!(id, m.inverted() * m);
            /// # }
            /// ```
            // NOTE: Implemented on a per-matrix basis to avoid a Clone bound on T
            pub fn transposed(self) -> Self {
                let s = self.$lines;
                Self {
                    $lines: CVec4::new(
                        Vec4::new(s.x.x, s.y.x, s.z.x, s.w.x),
                        Vec4::new(s.x.y, s.y.y, s.z.y, s.w.y),
                        Vec4::new(s.x.z, s.y.z, s.z.z, s.w.z),
                        Vec4::new(s.x.w, s.y.w, s.z.w, s.w.w)
                    )
                }
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
            // NOTE: Implemented on a per-matrix basis to avoid a Clone bound on T
            pub fn transpose(&mut self) {
                mem::swap(&mut self.$lines.x.y, &mut self.$lines.y.x);
                mem::swap(&mut self.$lines.x.z, &mut self.$lines.z.x);
                mem::swap(&mut self.$lines.x.w, &mut self.$lines.w.x);
                mem::swap(&mut self.$lines.y.z, &mut self.$lines.z.y);
                mem::swap(&mut self.$lines.y.w, &mut self.$lines.w.y);
                mem::swap(&mut self.$lines.z.w, &mut self.$lines.w.z);
            }

            /// Get this matrix's determinant.
            ///
            /// A matrix is invertible if its determinant is non-zero.
            pub fn determinant(self) -> T where T: Copy + Mul<T,Output=T> + Sub<T,Output=T> + Add<T, Output=T> {
                // http://www.euclideanspace.com/maths/algebra/matrix/functions/determinant/fourD/index.htm
                let CVec4 {
                    x: Vec4 { x: m00, y: m01, z: m02, w: m03 },
                    y: Vec4 { x: m10, y: m11, z: m12, w: m13 },
                    z: Vec4 { x: m20, y: m21, z: m22, w: m23 },
                    w: Vec4 { x: m30, y: m31, z: m32, w: m33 },
                } = Rows4::from(self).rows;
                m03 * m12 * m21 * m30 - m02 * m13 * m21 * m30 -
                m03 * m11 * m22 * m30 + m01 * m13 * m22 * m30 +
                m02 * m11 * m23 * m30 - m01 * m12 * m23 * m30 -
                m03 * m12 * m20 * m31 + m02 * m13 * m20 * m31 +
                m03 * m10 * m22 * m31 - m00 * m13 * m22 * m31 -
                m02 * m10 * m23 * m31 + m00 * m12 * m23 * m31 +
                m03 * m11 * m20 * m32 - m01 * m13 * m20 * m32 -
                m03 * m10 * m21 * m32 + m00 * m13 * m21 * m32 +
                m01 * m10 * m23 * m32 - m00 * m11 * m23 * m32 -
                m02 * m11 * m20 * m33 + m01 * m12 * m20 * m33 +
                m02 * m10 * m21 * m33 - m00 * m12 * m21 * m33 -
                m01 * m10 * m22 * m33 + m00 * m11 * m22 * m33
            }

            //
            // BASIC
            //

            /// Inverts this matrix, blindly assuming that it is invertible.
            /// See `inverted()` for more info.
            pub fn invert(&mut self) where T: Real {
                *self = self.inverted()
            }
            /// Returns this matrix's inverse, blindly assuming that it is invertible.
            ///
            /// All affine matrices have inverses; Your matrices may be affine
            /// as long as they consist of any combination of pure rotations,
            /// translations, scales and shears.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// use vek::vec::repr_c::Vec3;
            /// use vek::mat::repr_c::row_major::Mat4 as Rows4;
            /// use vek::mat::repr_c::column_major::Mat4 as Cols4;
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
            /// let a = Rows4::scaling_3d(1.77_f32)
            ///     .rotated_3d(PI*4./5., Vec3::new(5., 8., 10.))
            ///     .translated_3d(Vec3::new(1., 2., 3.));
            /// let b = a.inverted();
            /// assert_relative_eq!(a*b, Rows4::identity(), epsilon = 0.000001);
            /// assert_relative_eq!(b*a, Rows4::identity(), epsilon = 0.000001);
            ///
            /// let a = Cols4::scaling_3d(1.77_f32)
            ///     .rotated_3d(PI*4./5., Vec3::new(5., 8., 10.))
            ///     .translated_3d(Vec3::new(1., 2., 3.));
            /// let b = a.inverted();
            /// assert_relative_eq!(a*b, Cols4::identity(), epsilon = 0.000001);
            /// assert_relative_eq!(b*a, Cols4::identity(), epsilon = 0.000001);
            ///
            /// // Beware, projection matrices are not invertible!
            /// // Notice that we assert _inequality_ below.
            /// let a = Cols4::perspective_rh_zo(60_f32.to_radians(), 16./9., 0.001, 1000.) * a;
            /// let b = a.inverted();
            /// assert_relative_ne!(a*b, Cols4::identity(), epsilon = 0.000001);
            /// assert_relative_ne!(b*a, Cols4::identity(), epsilon = 0.000001);
            /// # }
            /// ```
            // NOTE: Stolen from
            // https://lxjk.github.io/2017/09/03/Fast-4x4-Matrix-Inverse-with-SSE-SIMD-Explained.html
            pub fn inverted(self) -> Self where T: Real
            {
                // NOTE: The VecShuffle_2323() macro in the article swaps its arguments
                let m = self.$lines;
                let a = Vec4::shuffle_lo_hi_0101(m.x, m.y);
                let b = Vec4::shuffle_hi_lo_2323(m.y, m.x);
                let c = Vec4::shuffle_lo_hi_0101(m.z, m.w);
                let d = Vec4::shuffle_hi_lo_2323(m.w, m.z);

                let det_a = Vec4::broadcast(m.x.x * m.y.y - m.x.y * m.y.x);
                let det_b = Vec4::broadcast(m.x.z * m.y.w - m.x.w * m.y.z);
                let det_c = Vec4::broadcast(m.z.x * m.w.y - m.z.y * m.w.x);
                let det_d = Vec4::broadcast(m.z.z * m.w.w - m.z.w * m.w.z);

                let d_c = d.mat2_rows_adj_mul(c);
                let a_b = a.mat2_rows_adj_mul(b);
                let x_ = det_d * a - b.mat2_rows_mul(d_c);
                let w_ = det_a * d - c.mat2_rows_mul(a_b);
                let y_ = det_b * c - d.mat2_rows_mul_adj(a_b);
                let z_ = det_c * b - a.mat2_rows_mul_adj(d_c);

                let tr = a_b * d_c.shuffled((0,2,1,3));
                let tr = tr.hadd(tr);
                let tr = tr.hadd(tr);

                let det_m = det_a * det_d + det_b * det_c - tr;

                let adj_sign_mask = Vec4::new(T::one(), -T::one(), -T::one(), T::one());
                let r_det_m = adj_sign_mask / det_m;

                let x_ = x_ * r_det_m;
                let y_ = y_ * r_det_m;
                let z_ = z_ * r_det_m;
                let w_ = w_ * r_det_m;

                Self {
                    $lines: CVec4::new(
                        Vec4::shuffle_lo_hi(x_, y_, (3,1,3,1)),
                        Vec4::shuffle_lo_hi(x_, y_, (2,0,2,0)),
                        Vec4::shuffle_lo_hi(z_, w_, (3,1,3,1)),
                        Vec4::shuffle_lo_hi(z_, w_, (2,0,2,0))
                    )
                }
            }

            /// Returns this matrix's inverse, blindly assuming that it is an invertible transform
            /// matrix which scale is 1.
            ///
            /// See `inverted_affine_transform_no_scale()` for more info.
            pub fn invert_affine_transform_no_scale(&mut self) where T: Real {
                *self = self.inverted_affine_transform_no_scale()
            }
            /// Returns this matrix's inverse, blindly assuming that it is an invertible transform
            /// matrix which scale is 1.
            ///
            /// A transform matrix is invertible this way as long as it consists
            /// of translations, rotations, and shears.
            /// **It's not guaranteed to work if the scale is not 1.**
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// use vek::vec::repr_c::Vec3;
            /// use vek::mat::repr_c::row_major::Mat4 as Rows4;
            /// use vek::mat::repr_c::column_major::Mat4 as Cols4;
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
            /// let a = Rows4::rotation_3d(PI*4./5., Vec3::new(5., 8., 10.))
            ///     .translated_3d(Vec3::new(1., 2., 3.));
            /// let b = a.inverted_affine_transform_no_scale();
            /// assert_relative_eq!(a*b, Rows4::identity(), epsilon = 0.000001);
            /// assert_relative_eq!(b*a, Rows4::identity(), epsilon = 0.000001);
            ///
            /// let a = Cols4::rotation_3d(PI*4./5., Vec3::new(5., 8., 10.))
            ///     .translated_3d(Vec3::new(1., 2., 3.));
            /// let b = a.inverted_affine_transform_no_scale();
            /// assert_relative_eq!(a*b, Cols4::identity(), epsilon = 0.000001);
            /// assert_relative_eq!(b*a, Cols4::identity(), epsilon = 0.000001);
            ///
            /// // Look! It stops working as soon as we add a scale.
            /// // Notice that we assert _inequality_ below.
            /// let a = Rows4::scaling_3d(5_f32)
            ///     .rotated_3d(PI*4./5., Vec3::new(5., 8., 10.))
            ///     .translated_3d(Vec3::new(1., 2., 3.));
            /// let b = a.inverted_affine_transform_no_scale();
            /// assert_relative_ne!(a*b, Rows4::identity(), epsilon = 0.000001);
            /// assert_relative_ne!(b*a, Rows4::identity(), epsilon = 0.000001);
            /// # }
            /// ```
            // NOTE: Stolen from
            // https://lxjk.github.io/2017/09/03/Fast-4x4-Matrix-Inverse-with-SSE-SIMD-Explained.html
            pub fn inverted_affine_transform_no_scale(self) -> Self where T: Real
            {
                // NOTE: The VecShuffle_2323() macro in the article swaps its arguments
                let m = Cols4::from(self).cols;
                let t0 = Vec4::shuffle_lo_hi_0101(m.x, m.y);
                let t1 = Vec4::shuffle_hi_lo_2323(m.y, m.x);
                let r0 = Vec4::shuffle_lo_hi(t0, m.z, (0,2,0,3));
                let r1 = Vec4::shuffle_lo_hi(t0, m.z, (1,3,1,3));
                let r2 = Vec4::shuffle_lo_hi(t1, m.z, (0,2,2,3));
                let r3 = Vec4::unit_w()
                       - r0 * m.w.shuffled(0)
                       - r1 * m.w.shuffled(1)
                       - r2 * m.w.shuffled(2);
                Cols4 { cols: CVec4::new(r0, r1, r2, r3) }.into()
            }
            /// Inverts this matrix, blindly assuming that it is an invertible transform matrix.
            /// See `inverted_affine_transform()` for more info.
            pub fn invert_affine_transform(&mut self) where T: Real {
                *self = self.inverted_affine_transform()
            }
            /// Returns this matrix's inverse, blindly assuming that it is an invertible transform
            /// matrix.
            ///
            /// A transform matrix is invertible this way as long as it consists
            /// of translations, rotations, scales and shears.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// use vek::vec::repr_c::Vec3;
            /// use vek::mat::repr_c::row_major::Mat4 as Rows4;
            /// use vek::mat::repr_c::column_major::Mat4 as Cols4;
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
            /// let a = Rows4::scaling_3d(1.77_f32)
            ///     .rotated_3d(PI*4./5., Vec3::new(5., 8., 10.))
            ///     .translated_3d(Vec3::new(1., 2., 3.));
            /// let b = a.inverted_affine_transform();
            /// assert_relative_eq!(a*b, Rows4::identity(), epsilon = 0.000001);
            /// assert_relative_eq!(b*a, Rows4::identity(), epsilon = 0.000001);
            ///
            /// let a = Cols4::scaling_3d(1.77_f32)
            ///     .rotated_3d(PI*4./5., Vec3::new(5., 8., 10.))
            ///     .translated_3d(Vec3::new(1., 2., 3.));
            /// let b = a.inverted_affine_transform();
            /// assert_relative_eq!(a*b, Cols4::identity(), epsilon = 0.000001);
            /// assert_relative_eq!(b*a, Cols4::identity(), epsilon = 0.000001);
            /// # }
            /// ```
            // NOTE: Stolen from
            // https://lxjk.github.io/2017/09/03/Fast-4x4-Matrix-Inverse-with-SSE-SIMD-Explained.html
            pub fn inverted_affine_transform(self) -> Self where T: Real
            {
                // NOTE: The VecShuffle_2323() macro in the article swaps its arguments
                let m = Cols4::from(self).cols;
                let t0 = Vec4::shuffle_lo_hi_0101(m.x, m.y);
                let t1 = Vec4::shuffle_hi_lo_2323(m.y, m.x);
                let r0 = Vec4::shuffle_lo_hi(t0, m.z, (0,2,0,3));
                let r1 = Vec4::shuffle_lo_hi(t0, m.z, (1,3,1,3));
                let r2 = Vec4::shuffle_lo_hi(t1, m.z, (0,2,2,3));
                // PERF: Could use mul_add()
                let size_sqr = r0 * r0 + r1 * r1 + r2 * r2;
                let epsilon = T::epsilon(); // XXX: Might prefer the one from RelativeEq ???
                // PERF: Could use _mm_blendv_ps(), like in this part of the article's source code.
                let size_sqr = size_sqr.map(|x| if x.abs() > epsilon { x } else { T::one() });
                let r0 = r0 / size_sqr;
                let r1 = r1 / size_sqr;
                let r2 = r2 / size_sqr;
                let r3 = Vec4::unit_w()
                       - r0 * m.w.shuffled(0)
                       - r1 * m.w.shuffled(1)
                       - r2 * m.w.shuffled(2);
                Cols4 { cols: CVec4::new(r0, r1, r2, r3) }.into()
            }




            #[allow(dead_code)]
            /// XXX I don't know exactly what this does. Make it public when I do.
            fn orthonormalize(&mut self) where T: Real + Add<T, Output=T> + SubAssign {
                *self = self.orthonormalized();
            }
            #[allow(dead_code)]
            /// XXX I don't know exactly what this does. Make it public when I do.
            // Taken verbatim from linmath.h - I don't know exactly what it does.
            fn orthonormalized(self) -> Self where T: Real + Add<T, Output=T> + SubAssign {
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
            pub fn mul_point<V: Into<Vec3<T>> + From<Vec4<T>>>(self, rhs: V) -> V
                where T: Real + MulAdd<T,T,Output=T>
            {
                V::from(self * Vec4::from_point(rhs))
            }
            /// Shortcut for `self * Vec4::from_direction(rhs)`.
            pub fn mul_direction<V: Into<Vec3<T>> + From<Vec4<T>>>(self, rhs: V) -> V
                where T: Real + MulAdd<T,T,Output=T>
            {
                V::from(self * Vec4::from_direction(rhs))
            }

            //
            // TRANSFORMS
            //

            /// Translates this matrix in 2D.
            pub fn translate_2d<V: Into<Vec2<T>>>(&mut self, v: V)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.translated_2d(v);
            }
            /// Returns this matrix translated in 2D.
            pub fn translated_2d<V: Into<Vec2<T>>>(self, v: V) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::translation_2d(v) * self
            }
            /// Creates a 2D translation matrix.
            pub fn translation_2d<V: Into<Vec2<T>>>(v: V) -> Self where T: Zero + One {
                let Vec2 { x, y } = v.into();
                Self::new(
                    T::one() , T::zero(), T::zero(), x,
                    T::zero(), T::one() , T::zero(), y,
                    T::zero(), T::zero(), T::one() , T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one(),
                )
            }
            /// Translates this matrix in 3D.
            pub fn translate_3d<V: Into<Vec3<T>>>(&mut self, v: V)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.translated_3d(v);
            }
            /// Returns this matrix translated in 3D.
            pub fn translated_3d<V: Into<Vec3<T>>>(self, v: V) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::translation_3d(v) * self
            }
            /// Creates a 3D translation matrix.
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
            fn translate_in_place_3d<V: Into<Vec3<T>>>(&mut self, v: V) where T: Copy + Zero + One + AddAssign + Add<T, Output=T> {
                let Vec3 { x, y, z } = v.into();
                let t = Vec4 { x, y, z, w: T::zero() };
                let mut rows = Rows4::from(*self).rows;
                rows[3] = [
                    rows[0].dot(t),
                    rows[1].dot(t),
                    rows[2].dot(t),
                    rows[3].dot(t),
                ].into();
                *self = Rows4 { rows }.into();
            }

            /// Scales this matrix in 3D.
            pub fn scale_3d<V: Into<Vec3<T>>>(&mut self, v: V)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.scaled_3d(v);
            }
            /// Returns this matrix scaled in 3D.
            pub fn scaled_3d<V: Into<Vec3<T>>>(self, v: V) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::scaling_3d(v) * self
            }
            /// Creates a 3D scaling matrix.
            pub fn scaling_3d<V: Into<Vec3<T>>>(v: V) -> Self where T: Zero + One {
                let Vec3 { x, y, z } = v.into();
                Self::new(
                    x, T::zero(), T::zero(), T::zero(),
                    T::zero(), y, T::zero(), T::zero(),
                    T::zero(), T::zero(), z, T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
            /// Rotates this matrix around the X axis.
            pub fn rotate_x(&mut self, angle_radians: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_x(angle_radians);
            }
            /// Returns this matrix rotated around the X axis.
            pub fn rotated_x(self, angle_radians: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::rotation_x(angle_radians) * self
            }
            /// Creates a matrix that rotates around the X axis.
            pub fn rotation_x(angle_radians: T) -> Self where T: Real {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    T::one(), T::zero(), T::zero(), T::zero(),
                    T::zero(), c, -s, T::zero(),
                    T::zero(), s, c, T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
            /// Rotates this matrix around the Y axis.
            pub fn rotate_y(&mut self, angle_radians: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_y(angle_radians);
            }
            /// Returns this matrix rotated around the Y axis.
            pub fn rotated_y(self, angle_radians: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::rotation_y(angle_radians) * self
            }
            /// Creates a matrix that rotates around the Y axis.
            pub fn rotation_y(angle_radians: T) -> Self where T: Real {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, T::zero(), s, T::zero(),
                    T::zero(), T::one(), T::zero(), T::zero(),
                    -s, T::zero(), c, T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
            /// Rotates this matrix around the Z axis.
            pub fn rotate_z(&mut self, angle_radians: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_z(angle_radians);
            }
            /// Returns this matrix rotated around the Z axis.
            pub fn rotated_z(self, angle_radians: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::rotation_z(angle_radians) * self
            }
            /// Creates a matrix that rotates around the Z axis.
            pub fn rotation_z(angle_radians: T) -> Self where T: Real {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, -s, T::zero(), T::zero(),
                    s,  c, T::zero(), T::zero(),
                    T::zero(), T::zero(), T::one(), T::zero(),
                    T::zero(), T::zero(), T::zero(), T::one()
                )
            }
            /// Rotates this matrix around a 3D axis.
            /// The axis is not required to be normalized.
            pub fn rotate_3d<V: Into<Vec3<T>>>(&mut self, angle_radians: T, axis: V)
                where T: Real + MulAdd<T,T,Output=T> + Add<T, Output=T>
            {
                *self = self.rotated_3d(angle_radians, axis);
            }
            /// Returns this matrix rotated around a 3D axis.
            /// The axis is not required to be normalized.
            pub fn rotated_3d<V: Into<Vec3<T>>>(self, angle_radians: T, axis: V) -> Self
                where T: Real + MulAdd<T,T,Output=T> + Add<T, Output=T>
            {
                Self::rotation_3d(angle_radians, axis) * self
            }
            /// Creates a matrix that rotates around a 3D axis.
            /// The axis is not required to be normalized.
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
            /// let m = Mat4::rotation_z(PI);
            /// assert_relative_eq!(m * v, -v);
            ///
            /// let m = Mat4::rotation_z(PI * 0.5);
            /// assert_relative_eq!(m * v, Vec4::unit_y());
            ///
            /// let m = Mat4::rotation_z(PI * 1.5);
            /// assert_relative_eq!(m * v, -Vec4::unit_y());
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
            pub fn rotation_3d<V: Into<Vec3<T>>>(angle_radians: T, axis: V) -> Self where T: Real + Add<T, Output=T> {
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


            /// Creates a matrix that would rotate a `from` direction to `to`.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Vec4, Mat4};
            ///
            /// # fn main() {
            /// let (from, to) = (Vec4::<f32>::unit_x(), Vec4::<f32>::unit_z());
            /// let m = Mat4::<f32>::rotation_from_to_3d(from, to);
            /// assert_relative_eq!(m * from, to);
            ///
            /// let (from, to) = (Vec4::<f32>::unit_x(), -Vec4::<f32>::unit_x());
            /// let m = Mat4::<f32>::rotation_from_to_3d(from, to);
            /// assert_relative_eq!(m * from, to);
            /// # }
            /// ```
            pub fn rotation_from_to_3d<V: Into<Vec3<T>>>(from: V, to: V) -> Self
                where T: Real + Add<T, Output=T>
            {
                Self::from(Quaternion::rotation_from_to_3d(from, to))
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
            pub fn basis_to_local<V: Into<Vec3<T>>>(origin: V, i: V, j: V, k: V) -> Self
                where T: Zero + One + Neg<Output=T> + Real + Add<T, Output=T>
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

            /// Builds a "look at" view transform
            /// from an eye position, a target position, and up vector.
            /// Commonly used for cameras - in short, it maps points from
            /// world-space to eye-space.
            #[deprecated(since = "0.9.7", note = "Use look_at_lh() or look_at_rh() instead depending on your space's handedness")]
            pub fn look_at<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Real + Add<T, Output=T>
            {
                Self::look_at_lh(eye, target, up)
            }

            /// Builds a "look at" view transform for left-handed spaces
            /// from an eye position, a target position, and up vector.
            /// Commonly used for cameras - in short, it maps points from
            /// world-space to eye-space.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec4};
            /// # fn main() {
            /// let eye = Vec4::new(1_f32, 0., 1., 1.);
            /// let target = Vec4::new(2_f32, 0., 2., 1.);
            /// let view = Mat4::<f32>::look_at_lh(eye, target, Vec4::unit_y());
            /// assert_relative_eq!(view * eye, Vec4::unit_w());
            /// assert_relative_eq!(view * target, Vec4::new(0_f32, 0., 2_f32.sqrt(), 1.));
            /// # }
            /// ```
            pub fn look_at_lh<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Real + Add<T, Output=T>
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

            /// Builds a "look at" view transform for right-handed spaces
            /// from an eye position, a target position, and up vector.
            /// Commonly used for cameras - in short, it maps points from
            /// world-space to eye-space.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, Vec4};
            /// # fn main() {
            /// let eye = Vec4::new(1_f32, 0., 1., 1.);
            /// let target = Vec4::new(2_f32, 0., 2., 1.);
            /// let view = Mat4::<f32>::look_at_rh(eye, target, Vec4::unit_y());
            /// assert_relative_eq!(view * eye, Vec4::unit_w());
            /// assert_relative_eq!(view * target, Vec4::new(0_f32, 0., -2_f32.sqrt(), 1.));
            /// # }
            /// ```
            pub fn look_at_rh<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Real + Add<T, Output=T>
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

            /// Builds a "look at" model transform
            /// from an eye position, a target position, and up vector.
            /// Preferred for transforming objects.
            #[deprecated(since = "0.9.7", note = "Use model_look_at_lh() or model_look_at_rh() instead depending on your space's handedness")]
            pub fn model_look_at<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Real + Add<T, Output=T>
            {
                Self::model_look_at_lh(eye, target, up)
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
            /// let model = Mat4::<f32>::model_look_at_lh(eye, target, Vec4::unit_y());
            /// assert_relative_eq!(model * Vec4::unit_w(), eye);
            /// let d = 2_f32.sqrt();
            /// assert_relative_eq!(model * Vec4::new(0_f32, 0., d, 1.), target);
            ///
            /// // A "model" look-at essentially undoes a "view" look-at
            /// let view = Mat4::look_at_lh(eye, target, Vec4::unit_y());
            /// assert_relative_eq!(view * model, Mat4::identity());
            /// assert_relative_eq!(model * view, Mat4::identity());
            /// # }
            /// ```
            pub fn model_look_at_lh<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Real + Add<T, Output=T>
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
            /// # use vek::{Mat4, Vec4};
            /// # fn main() {
            /// let eye = Vec4::new(1_f32, 0., -1., 1.);
            /// let forward = Vec4::new(0_f32, 0., -1., 0.);
            /// let model = Mat4::<f32>::model_look_at_rh(eye, eye + forward, Vec4::unit_y());
            /// assert_relative_eq!(model * Vec4::unit_w(), eye);
            /// assert_relative_eq!(model * forward, forward);
            ///
            /// // A "model" look-at essentially undoes a "view" look-at
            /// let view = Mat4::look_at_rh(eye, eye + forward, Vec4::unit_y());
            /// assert_relative_eq!(view * model, Mat4::identity());
            /// assert_relative_eq!(model * view, Mat4::identity());
            /// # }
            /// ```
            pub fn model_look_at_rh<V: Into<Vec3<T>>>(eye: V, target: V, up: V) -> Self
                where T: Real + Add<T, Output=T> + MulAdd<T,T,Output=T>
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


            //
            // PROJECTIONS
            //

            /// Returns an orthographic projection matrix that doesn't use near and far planes.
            pub fn orthographic_without_depth_planes (o: FrustumPlanes<T>) -> Self where T: Real {
                let two = T::one() + T::one();
                let FrustumPlanes { left, right, top, bottom, .. } = o;
                let mut m = Self::identity();
                m[(0, 0)] = two / (right - left);
                m[(1, 1)] = two / (top - bottom);
                m[(0, 3)] = - (right + left) / (right - left);
                m[(1, 3)] = - (top + bottom) / (top - bottom);
                m
            }
            /// Returns an orthographic projection matrix for left-handed spaces,
            /// for a depth clip space ranging from 0 to 1 (`GL_DEPTH_ZERO_TO_ONE`,
            /// hence the `_zo` suffix).
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, FrustumPlanes, Vec4};
            /// # fn main() {
            /// let m = Mat4::orthographic_lh_zo(FrustumPlanes {
            ///     left: -1_f32, right: 1., bottom: -1., top: 1.,
            ///     near: 0., far: 1.
            /// });
            /// let v = Vec4::new(0_f32, 0., 1., 1.); // "forward"
            /// assert_relative_eq!(m * v, Vec4::new(0., 0., 1., 1.)); // "far"
            /// # }
            /// ```
            pub fn orthographic_lh_zo (o: FrustumPlanes<T>) -> Self where T: Real {
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::orthographic_without_depth_planes(o);
                m[(2, 2)] = T::one() / (far - near);
                m[(2, 3)] = - near / (far - near);
                m
            }
            /// Returns an orthographic projection matrix for left-handed spaces,
            /// for a depth clip space ranging from -1 to 1 (`GL_DEPTH_NEGATIVE_ONE_TO_ONE`,
            /// hence the `_no` suffix).
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, FrustumPlanes, Vec4};
            /// # fn main() {
            /// let m = Mat4::orthographic_lh_no(FrustumPlanes {
            ///     left: -1_f32, right: 1., bottom: -1., top: 1.,
            ///     near: 0., far: 1.
            /// });
            /// let v = Vec4::new(0_f32, 0., 1., 1.); // "forward"
            /// assert_relative_eq!(m * v, Vec4::new(0., 0., 1., 1.)); // "far"
            /// # }
            /// ```
            pub fn orthographic_lh_no (o: FrustumPlanes<T>) -> Self where T: Real {
                let two = T::one() + T::one();
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::orthographic_without_depth_planes(o);
                m[(2, 2)] = two / (far - near);
                m[(2, 3)] = - (far + near) / (far - near);
                m
            }
            /// Returns an orthographic projection matrix for right-handed spaces,
            /// for a depth clip space ranging from 0 to 1 (`GL_DEPTH_ZERO_TO_ONE`,
            /// hence the `_zo` suffix).
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, FrustumPlanes, Vec4};
            /// # fn main() {
            /// let m = Mat4::orthographic_rh_zo(FrustumPlanes {
            ///     left: -1_f32, right: 1., bottom: -1., top: 1.,
            ///     near: 0., far: 1.
            /// });
            /// let v = Vec4::new(0_f32, 0., -1., 1.); // "forward"
            /// assert_relative_eq!(m * v, Vec4::new(0., 0., 1., 1.)); // "far"
            /// # }
            /// ```
            pub fn orthographic_rh_zo (o: FrustumPlanes<T>) -> Self where T: Real {
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::orthographic_without_depth_planes(o);
                m[(2, 2)] = - T::one() / (far - near);
                m[(2, 3)] = - near / (far - near);
                m
            }
            /// Returns an orthographic projection matrix for right-handed spaces,
            /// for a depth clip space ranging from -1 to 1 (`GL_DEPTH_NEGATIVE_ONE_TO_ONE`,
            /// hence the `_no` suffix).
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Mat4, FrustumPlanes, Vec4};
            /// # fn main() {
            /// let m = Mat4::orthographic_rh_no(FrustumPlanes {
            ///     left: -1_f32, right: 1., bottom: -1., top: 1.,
            ///     near: 0., far: 1.
            /// });
            /// let v = Vec4::new(0_f32, 0., -1., 1.); // "forward"
            /// assert_relative_eq!(m * v, Vec4::new(0., 0., 1., 1.)); // "far"
            /// # }
            /// ```
            pub fn orthographic_rh_no (o: FrustumPlanes<T>) -> Self where T: Real {
                let two = T::one() + T::one();
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::orthographic_without_depth_planes(o);
                m[(2, 2)] = - two / (far - near);
                m[(2, 3)] = - (far + near) / (far - near);
                m
            }

            /// Creates a perspective projection matrix from a frustum
            /// (left-handed, zero-to-one depth clip planes).
            pub fn frustum_lh_zo (o: FrustumPlanes<T>) -> Self where T: Real {
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
            /// Creates a perspective projection matrix from a frustum
            /// (left-handed, negative-one-to-one depth clip planes).
            pub fn frustum_lh_no (o: FrustumPlanes<T>) -> Self where T: Real {
                let two = T::one() + T::one();
                let FrustumPlanes { near, far, .. } = o;
                let mut m = Self::frustum_lh_zo(o);
                m[(2, 2)] = (far + near) / (far - near);
                m[(2, 3)] = -(two * far * near) / (far - near);
                m
            }
            /// Creates a perspective projection matrix from a frustum
            /// (right-handed, zero-to-one depth clip planes).
            pub fn frustum_rh_zo (o: FrustumPlanes<T>) -> Self where T: Real {
                let mut m = Self::frustum_lh_zo(o);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }
            /// Creates a perspective projection matrix from a frustum
            /// (right-handed, negative-one-to-one depth clip planes).
            pub fn frustum_rh_no (o: FrustumPlanes<T>) -> Self where T: Real {
                let mut m = Self::frustum_lh_no(o);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }

            /// Creates a perspective projection matrix for right-handed spaces, with zero-to-one depth clip planes.
            pub fn perspective_rh_zo (fov_y_radians: T, aspect_ratio: T, near: T, far: T) -> Self
                where T: Real + FloatConst + Debug
            {
                // Assertions from cgmath
                debug_assert!(fov_y_radians > T::zero(), "The vertical field of view cannot be below zero, found: {:?}", fov_y_radians);
                debug_assert!(fov_y_radians < (T::PI()+T::PI()), "The vertical field of view cannot be greater than a half turn, found: {:?} radians", fov_y_radians);
                debug_assert!(aspect_ratio > T::zero(), "The aspect ratio cannot be below zero, found: {:?}", aspect_ratio);
                debug_assert!(near > T::zero(), "The near plane distance cannot be below zero, found: {:?}", near);
                debug_assert!(far  > T::zero(), "The far plane distance cannot be below zero, found: {:?}", far);
                debug_assert!(far  > near, "The far plane cannot be closer than the near plane, found: far: {:?}, near: {:?}", far, near);
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
                where T: Real + FloatConst + Debug
            {
                let mut m = Self::perspective_rh_zo(fov_y_radians, aspect_ratio, near, far);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }

            /// Creates a perspective projection matrix for right-handed spaces, with negative-one-to-one depth clip planes.
            pub fn perspective_rh_no (fov_y_radians: T, aspect_ratio: T, near: T, far: T) -> Self
                where T: Real + FloatConst + Debug
            {
                // Assertions from cgmath
                debug_assert!(fov_y_radians > T::zero(), "The vertical field of view cannot be below zero, found: {:?}", fov_y_radians);
                debug_assert!(fov_y_radians < (T::PI()+T::PI()), "The vertical field of view cannot be greater than a half turn, found: {:?} radians", fov_y_radians);
                debug_assert!(aspect_ratio > T::zero(), "The aspect ratio cannot be below zero, found: {:?}", aspect_ratio);
                debug_assert!(near > T::zero(), "The near plane distance cannot be below zero, found: {:?}", near);
                debug_assert!(far  > T::zero(), "The far plane distance cannot be below zero, found: {:?}", far);
                debug_assert!(far  > near, "The far plane cannot be closer than the near plane, found: far: {:?}, near: {:?}", far, near);
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
                where T: Real + FloatConst + Debug
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
                where T: Real + FloatConst + Debug
            {
                debug_assert!(width > T::zero(), "viewport width cannot be below zero, found: {:?}", width);
                debug_assert!(height > T::zero(), "viewport height cannot be below zero, found: {:?}", height);
                // Assertions from cgmath
                debug_assert!(fov_y_radians > T::zero(), "The vertical field of view cannot be below zero, found: {:?}", fov_y_radians);
                debug_assert!(fov_y_radians < (T::PI()+T::PI()), "The vertical field of view cannot be greater than a half turn, found: {:?} radians", fov_y_radians);
                debug_assert!(near > T::zero(), "The near plane distance cannot be below zero, found: {:?}", near);
                debug_assert!(far  > T::zero(), "The far plane distance cannot be below zero, found: {:?}", far);
                debug_assert!(far  > near, "The far plane cannot be closer than the near plane, found: far: {:?}, near: {:?}", far, near);

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
                where T: Real + FloatConst + Debug
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
                where T: Real + FloatConst + Debug
            {
                debug_assert!(width > T::zero(), "viewport width cannot be below zero, found: {:?}", width);
                debug_assert!(height > T::zero(), "viewport height cannot be below zero, found: {:?}", height);
                // Assertions from cgmath
                debug_assert!(fov_y_radians > T::zero(), "The vertical field of view cannot be below zero, found: {:?}", fov_y_radians);
                debug_assert!(fov_y_radians < (T::PI()+T::PI()), "The vertical field of view cannot be greater than a half turn, found: {:?} radians", fov_y_radians);
                debug_assert!(near > T::zero(), "The near plane distance cannot be below zero, found: {:?}", near);
                debug_assert!(far  > T::zero(), "The far plane distance cannot be below zero, found: {:?}", far);
                debug_assert!(far  > near, "The far plane cannot be closer than the near plane, found: far: {:?}, near: {:?}", far, near);

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
                where T: Real + FloatConst + Debug
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
                where T: Real + FloatConst + Debug
            {
                // Assertions from cgmath
                debug_assert!(fov_y_radians > T::zero(), "The vertical field of view cannot be below zero, found: {:?}", fov_y_radians);
                debug_assert!(fov_y_radians < (T::PI()+T::PI()), "The vertical field of view cannot be greater than a half turn, found: {:?} radians", fov_y_radians);
                debug_assert!(aspect_ratio > T::zero(), "The aspect ratio cannot be below zero, found: {:?}", aspect_ratio);
                debug_assert!(near > T::zero(), "The near plane distance cannot be below zero, found: {:?}", near);

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
                where T: Real + FloatConst + Debug
            {
                let mut m = Self::tweaked_infinite_perspective_rh(fov_y_radians, aspect_ratio, near, epsilon);
                m[(2, 2)] = -m[(2, 2)];
                m[(3, 2)] = -m[(3, 2)];
                m
            }

            /// Creates an infinite perspective projection matrix for right-handed spaces.
            pub fn infinite_perspective_rh (fov_y_radians: T, aspect_ratio: T, near: T) -> Self
                where T: Real + FloatConst + Debug
            {
                Self::tweaked_infinite_perspective_rh(fov_y_radians, aspect_ratio, near, T::zero())
            }

            /// Creates an infinite perspective projection matrix for left-handed spaces.
            pub fn infinite_perspective_lh (fov_y_radians: T, aspect_ratio: T, near: T) -> Self
                where T: Real + FloatConst + Debug
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
            pub fn picking_region<V2: Into<Vec2<T>>>(center: V2, delta: V2, viewport: Rect<T, T>) -> Self
                where T: Real + MulAdd<T,T,Output=T>
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

            /// Projects a world-space coordinate into screen space,
            /// for a depth clip space ranging from -1 to 1 (`GL_DEPTH_NEGATIVE_ONE_TO_ONE`,
            /// hence the `_no` suffix).
            pub fn world_to_viewport_no<V3>(obj: V3, modelview: Self, proj: Self, viewport: Rect<T, T>) -> Vec3<T>
                where T: Real + MulAdd<T,T,Output=T>,
                      V3: Into<Vec3<T>>
            {
                // GLM's projectNO()
                let mut tmp = Vec4::from_point(obj.into());
                tmp = modelview * tmp;
                tmp = proj * tmp;

                let two = T::one() + T::one();
                let half_one = T::one() / two;

                tmp = tmp / tmp.w; // Real doesn't imply DivAssign
                tmp = tmp / two + half_one;
                tmp.x = tmp.x * viewport.w + viewport.x;
                tmp.y = tmp.y * viewport.h + viewport.y;

                tmp.into()
            }

            /// Projects a world-space coordinate into screen space,
            /// for a depth clip space ranging from 0 to 1 (`GL_DEPTH_ZERO_TO_ONE`,
            /// hence the `_zo` suffix).
            pub fn world_to_viewport_zo<V3>(obj: V3, modelview: Self, proj: Self, viewport: Rect<T, T>) -> Vec3<T>
                where T: Real + MulAdd<T,T,Output=T>,
                      V3: Into<Vec3<T>>
            {
                // GLM's projectZO()
                let mut tmp = Vec4::from_point(obj.into());
                tmp = modelview * tmp;
                tmp = proj * tmp;

                let two = T::one() + T::one();
                let half_one = T::one() / two;

                tmp = tmp / tmp.w; // Real doesn't imply DivAssign
                tmp.x = tmp.x / two + half_one;
                tmp.y = tmp.y / two + half_one;
                tmp.x = tmp.x * viewport.w + viewport.x;
                tmp.y = tmp.y * viewport.h + viewport.y;

                tmp.into()
            }

            /// Projects a screen-space coordinate into world space,
            /// for a depth clip space ranging from 0 to 1 (`GL_DEPTH_ZERO_TO_ONE`,
            /// hence the `_zo` suffix).
            pub fn viewport_to_world_zo<V3>(ray: V3, modelview: Self, proj: Self, viewport: Rect<T, T>) -> Vec3<T>
                where T: Real + MulAdd<T,T,Output=T>,
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
                obj = obj / obj.w; // Real doesn't imply DivAssign

                obj.into()
            }
            /// Projects a screen-space coordinate into world space,
            /// for a depth clip space ranging from -1 to 1 (`GL_DEPTH_NEGATIVE_ONE_TO_ONE`,
            /// hence the `_no` suffix).
            pub fn viewport_to_world_no<V3>(ray: V3, modelview: Self, proj: Self, viewport: Rect<T, T>) -> Vec3<T>
                where T: Real + MulAdd<T,T,Output=T>,
                      V3: Into<Vec3<T>>
            {
                let inverse = (proj * modelview).inverted();
                let two = T::one() + T::one();

                let mut tmp = Vec4::from_point(ray.into());
                tmp.x = (tmp.x - viewport.x) / viewport.w;
                tmp.y = (tmp.y - viewport.y) / viewport.h;
                tmp = tmp * two - T::one();

                let mut obj = inverse * tmp;
                obj = obj / obj.w; // Real doesn't imply DivAssign

                obj.into()
            }
        }
        use super::mat3::Mat3;
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
        use super::mat2::Mat2;
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

        /// A `Mat4` can be obtained from a `Transform`, by rotating, then scaling, then
        /// translating.
        impl<T> From<Transform<T,T,T>> for Mat4<T>
            where T: Real + MulAdd<T,T,Output=T>
        {
            fn from(xform: Transform<T,T,T>) -> Self {
                let Transform { position, orientation, scale } = xform;
                Mat4::from(orientation).scaled_3d(scale).translated_3d(position)
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

        /* NOTE: Commented until I find an implementation that actually works
        /// A quaternion may be obtained from a rotation matrix.
        ///
        /// ```
        /// # extern crate vek;
        /// # #[macro_use] extern crate approx;
        /// # use vek::{Mat4, Quaternion, Vec3};
        /// # fn main() {
        /// let (angle, axis) = (3_f32, Vec3::new(1_f32, 3., 7.));
        /// let a = Quaternion::rotation_3d(angle, axis);
        /// let b = Quaternion::from(Mat4::rotation_3d(angle, axis));
        /// assert_relative_eq!(a, b);
        /// # }
        /// ```
        impl<T> From<Mat4<T>> for Quaternion<T>
            where T: Real + Zero + One + Mul<Output=T> + Add<Output=T> + Sub<Output=T>
        {
            fn from(m: Mat4<T>) -> Self {
                // http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/
                // Using the most branchy one, because if you're willing to convert a matrix to
                // quaternion in the first place, surely you don't mind this extra cost.
                let m = Rows4::from(m);
                let Rows4 {
                    rows: CVec4 {
                        x: Vec4 { x: m00, y: m01, z: m02, w: _ },
                        y: Vec4 { x: m10, y: m11, z: m12, w: _ },
                        z: Vec4 { x: m20, y: m21, z: m22, w: _ },
                        w: Vec4 { x:   _, y:   _, z:   _, w: _ },
                    }
                } = m;
                let tr = m00 + m11 + m22;

                let one = T::one();
                let two = one + one;
                let four = two + two;
                if tr > T::zero() {
                    let s = (tr+one).sqrt() * two; // S=4*qw
                    Self {
                        w: s / four,
                        x: (m21 - m12) / s,
                        y: (m02 - m20) / s,
                        z: (m10 - m01) / s,
                    }
                } else if (m00 > m11) && (m00 > m22) {
                    let s = (one + m00 - m11 - m22).sqrt() * two; // S=4*qx
                    Self {
                        w: (m21 - m12) / s,
                        x: s / four,
                        y: (m01 + m10) / s,
                        z: (m02 + m20) / s,
                    }
                } else if m11 > m22 {
                    let s = (one + m11 - m00 - m22).sqrt() * two; // S=4*qy
                    Self {
                        w: (m02 - m20) / s,
                        x: (m01 + m10) / s,
                        y: s / four,
                        z: (m12 + m21) / s,
                    }
                } else {
                    let s = (one + m22 - m00 - m11).sqrt() * two; // S=4*qz
                    Self {
                        w: (m10 - m01) / s,
                        x: (m02 + m20) / s,
                        y: (m12 + m21) / s,
                        z: s / four,
                    }
                }
            }
        } */
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
            /// Returns an element-wise-converted copy of this matrix, using the given conversion
            /// closure.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<f32>::new(
            ///     0.25, 1.25, 5.56, 8.66,
            ///     8.53, 2.92, 3.86, 9.36,
            ///     1.02, 0.28, 5.52, 6.06,
            ///     6.20, 7.01, 4.90, 5.26
            /// );
            /// let m = m.map(|x| x.round() as i32);
            /// assert_eq!(m, Mat4::new(
            ///     0, 1, 6, 9,
            ///     9, 3, 4, 9,
            ///     1, 0, 6, 6,
            ///     6, 7, 5, 5
            /// ));
            /// ```
            pub fn map<D,F>(self, mut f: F) -> Mat3<D> where F: FnMut(T) -> D {
                let m = self.$lines;
                Mat3 {
                    $lines: CVec3::new(
                        Vec3::new(f(m.x.x), f(m.x.y), f(m.x.z)),
                        Vec3::new(f(m.y.x), f(m.y.y), f(m.y.z)),
                        Vec3::new(f(m.z.x), f(m.z.y), f(m.z.z)),
                    )
                }
            }

            /// Returns a memberwise-converted copy of this matrix, using `AsPrimitive`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let v = Vec4::new(0_f32, 1., 2., 3.);
            /// let i: Vec4<i32> = v.as_();
            /// assert_eq!(i, Vec4::new(0, 1, 2, 3));
            /// ```
            ///
            /// # Safety
            ///
            /// **In Rust versions before 1.45.0**, some uses of the `as` operator were not entirely safe.
            /// In particular, it was undefined behavior if
            /// a truncated floating point value could not fit in the target integer
            /// type ([#10184](https://github.com/rust-lang/rust/issues/10184));
            ///
            /// ```ignore
            /// # use num_traits::AsPrimitive;
            /// let x: u8 = (1.04E+17).as_(); // UB
            /// ```
            pub fn as_<D>(self) -> Mat3<D> where T: AsPrimitive<D>, D: 'static + Copy {
                let m = self.$lines;
                Mat3 {
                    $lines: CVec3::new(
                        Vec3::new(m.x.x.as_(), m.x.y.as_(), m.x.z.as_()),
                        Vec3::new(m.y.x.as_(), m.y.y.as_(), m.y.z.as_()),
                        Vec3::new(m.z.x.as_(), m.z.y.as_(), m.z.z.as_()),
                    )
                }
            }

            /// Applies the function f to each element of two matrices, pairwise, and returns the result.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let a = Mat4::<f32>::new(
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99
            /// );
            /// let b = Mat4::<i32>::new(
            ///     0, 1, 0, 0,
            ///     1, 0, 0, 0,
            ///     0, 0, 1, 0,
            ///     0, 0, 0, 1
            /// );
            /// let m = a.map2(b, |a, b| a.round() as i32 + b);
            /// assert_eq!(m, Mat4::new(
            ///     0, 2, 3, 3,
            ///     1, 1, 3, 3,
            ///     0, 1, 4, 3,
            ///     0, 1, 3, 4
            /// ));
            /// ```
            pub fn map2<D,F,S>(self, other: Mat3<S>, mut f: F) -> Mat3<D> where F: FnMut(T, S) -> D {
                let m = self.$lines;
                let o = other.$lines;
                Mat3 {
                    $lines: CVec3::new(
                        Vec3::new(f(m.x.x, o.x.x), f(m.x.y, o.x.y), f(m.x.z, o.x.z)),
                        Vec3::new(f(m.y.x, o.y.x), f(m.y.y, o.y.y), f(m.y.z, o.y.z)),
                        Vec3::new(f(m.z.x, o.z.x), f(m.z.y, o.z.y), f(m.z.z, o.z.z))
                    )
                }
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
            /// # use vek::Mat3;
            /// use std::f32::consts::PI;
            ///
            /// # fn main() {
            /// let m = Mat3::new(
            ///     0, 1, 2,
            ///     4, 5, 6,
            ///     8, 9, 0
            /// );
            /// let t = Mat3::new(
            ///     0, 4, 8,
            ///     1, 5, 9,
            ///     2, 6, 0
            /// );
            /// assert_eq!(m.transposed(), t);
            /// assert_eq!(m, m.transposed().transposed());
            ///
            /// let m = Mat3::rotation_x(PI/7.);
            /// assert_relative_eq!(m * m.transposed(), Mat3::identity());
            /// assert_relative_eq!(m.transposed() * m, Mat3::identity());
            /// # }
            /// ```
            // NOTE: Implemented on a per-matrix basis to avoid a Clone bound on T
            pub fn transposed(self) -> Self {
                let s = self.$lines;
                Self {
                    $lines: CVec3::new(
                        Vec3::new(s.x.x, s.y.x, s.z.x),
                        Vec3::new(s.x.y, s.y.y, s.z.y),
                        Vec3::new(s.x.z, s.y.z, s.z.z)
                    )
                }
            }
            /// Transpose this matrix.
            ///
            /// ```
            /// # use vek::Mat3;
            ///
            /// let mut m = Mat3::new(
            ///     0, 1, 2,
            ///     4, 5, 6,
            ///     8, 9, 0
            /// );
            /// let t = Mat3::new(
            ///     0, 4, 8,
            ///     1, 5, 9,
            ///     2, 6, 0
            /// );
            /// m.transpose();
            /// assert_eq!(m, t);
            /// ```
            // NOTE: Implemented on a per-matrix basis to avoid a Clone bound on T
            pub fn transpose(&mut self) {
                mem::swap(&mut self.$lines.x.y, &mut self.$lines.y.x);
                mem::swap(&mut self.$lines.x.z, &mut self.$lines.z.x);
                mem::swap(&mut self.$lines.y.z, &mut self.$lines.z.y);
            }

            /// Get this matrix's determinant.
            ///
            /// A matrix is invertible if its determinant is non-zero.
            pub fn determinant(self) -> T where T: Copy + Mul<T,Output=T> + Sub<T,Output=T> + Add<T, Output=T> {
                // https://www.mathsisfun.com/algebra/matrix-determinant.html
                use super::row_major::Mat3 as Rows3;
                let CVec3 {
                    x: Vec3 { x: a, y: b, z: c },
                    y: Vec3 { x: d, y: e, z: f },
                    z: Vec3 { x: g, y: h, z: i },
                } = Rows3::from(self).rows;
                a*(e*i - f*h) - b*(d*i - f*g) + c*(d*h - e*g)
            }


            //
            // MULTIPLY BY
            //

            /// Shortcut for `self * Vec3::from_point_2d(rhs)`.
            pub fn mul_point_2d<V: Into<Vec2<T>> + From<Vec3<T>>>(self, rhs: V) -> V
                where T: Real + MulAdd<T,T,Output=T>
            {
                V::from(self * Vec3::from_point_2d(rhs))
            }
            /// Shortcut for `self * Vec3::from_direction_2d(rhs)`.
            pub fn mul_direction_2d<V: Into<Vec2<T>> + From<Vec3<T>>>(self, rhs: V) -> V
                where T: Real + MulAdd<T,T,Output=T>
            {
                V::from(self * Vec3::from_direction_2d(rhs))
            }


            /// Translates this matrix in 2D.
            pub fn translate_2d<V: Into<Vec2<T>>>(&mut self, v: V)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.translated_2d(v);
            }
            /// Returns this matrix translated in 2D.
            pub fn translated_2d<V: Into<Vec2<T>>>(self, v: V) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::translation_2d(v) * self
            }
            /// Creates a 2D translation matrix.
            pub fn translation_2d<V: Into<Vec2<T>>>(v: V) -> Self where T: Zero + One {
                let v = v.into();
                Self::new(
                    T::one() , T::zero(), v.x,
                    T::zero(), T::one() , v.y,
                    T::zero(), T::zero(), T::one()
                )
            }
            /// Scales this matrix in 3D.
            pub fn scale_3d<V: Into<Vec3<T>>>(&mut self, v: V)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.scaled_3d(v);
            }
            /// Returns this matrix scaled in 3D.
            pub fn scaled_3d<V: Into<Vec3<T>>>(self, v: V) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::scaling_3d(v) * self
            }
            /// Creates a 3D scaling matrix.
            pub fn scaling_3d<V: Into<Vec3<T>>>(v: V) -> Self where T: Zero {
                let Vec3 { x, y, z } = v.into();
                Self::new(
                    x, T::zero(), T::zero(),
                    T::zero(), y, T::zero(),
                    T::zero(), T::zero(), z
                )
            }
            /// Rotates this matrix around the X axis.
            pub fn rotate_x(&mut self, angle_radians: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_x(angle_radians);
            }
            /// Returns this matrix rotated around the X axis.
            pub fn rotated_x(self, angle_radians: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::rotation_x(angle_radians) * self
            }
            /// Creates a matrix that rotates around the X axis.
            pub fn rotation_x(angle_radians: T) -> Self where T: Real {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    T::one(), T::zero(), T::zero(),
                    T::zero(), c, -s,
                    T::zero(), s, c
                )
            }
            /// Rotates this matrix around the Y axis.
            pub fn rotate_y(&mut self, angle_radians: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_y(angle_radians);
            }
            /// Returns this matrix rotated around the Y axis.
            pub fn rotated_y(self, angle_radians: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::rotation_y(angle_radians) * self
            }
            /// Creates a matrix that rotates around the Y axis.
            pub fn rotation_y(angle_radians: T) -> Self where T: Real {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, T::zero(), s,
                    T::zero(), T::one(), T::zero(),
                    -s, T::zero(), c
                )
            }
            /// Rotates this matrix around the Z axis.
            pub fn rotate_z(&mut self, angle_radians: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_z(angle_radians);
            }
            /// Returns this matrix rotated around the Z axis.
            pub fn rotated_z(self, angle_radians: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::rotation_z(angle_radians) * self
            }
            /// Creates a matrix that rotates around the Z axis.
            pub fn rotation_z(angle_radians: T) -> Self where T: Real {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, -s, T::zero(),
                    s,  c, T::zero(),
                    T::zero(), T::zero(), T::one()
                )
            }

            /// Rotates this matrix around a 3D axis.
            /// The axis is not required to be normalized.
            pub fn rotate_3d<V: Into<Vec3<T>>>(&mut self, angle_radians: T, axis: V)
                where T: Real + MulAdd<T,T,Output=T> + Add<T, Output=T>
            {
                *self = self.rotated_3d(angle_radians, axis);
            }
            /// Returns this matrix rotated around a 3D axis.
            /// The axis is not required to be normalized.
            pub fn rotated_3d<V: Into<Vec3<T>>>(self, angle_radians: T, axis: V) -> Self
                where T: Real + MulAdd<T,T,Output=T> + Add<T, Output=T>
            {
                Self::rotation_3d(angle_radians, axis) * self
            }
            /// Creates a matrix that rotates around a 3D axis.
            /// The axis is not required to be normalized.
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
            pub fn rotation_3d<V: Into<Vec3<T>>>(angle_radians: T, axis: V) -> Self where T: Real + Add<T, Output=T> {
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
            /// Creates a matrix that would rotate a `from` direction to `to`.
            ///
            /// ```
            /// # extern crate vek;
            /// # #[macro_use] extern crate approx;
            /// # use vek::{Vec3, Mat3};
            ///
            /// # fn main() {
            /// let (from, to) = (Vec3::<f32>::unit_x(), Vec3::<f32>::unit_z());
            /// let m = Mat3::<f32>::rotation_from_to_3d(from, to);
            /// assert_relative_eq!(m * from, to);
            ///
            /// let (from, to) = (Vec3::<f32>::unit_x(), -Vec3::<f32>::unit_x());
            /// let m = Mat3::<f32>::rotation_from_to_3d(from, to);
            /// assert_relative_eq!(m * from, to);
            /// # }
            /// ```
            pub fn rotation_from_to_3d<V: Into<Vec3<T>>>(from: V, to: V) -> Self
                where T: Real + Add<T, Output=T>
            {
                Self::from(Quaternion::rotation_from_to_3d(from, to))
            }
        }

        use super::mat4::Mat4;
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
        use super::mat2::Mat2;
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
        impl<T> From<Quaternion<T>> for Mat3<T>
            where T: Copy + Zero + One + Mul<Output=T> + Add<Output=T> + Sub<Output=T>
        {
            fn from(q: Quaternion<T>) -> Self {
                Mat4::from(q).into()
            }
        }
        /* NOTE: Blocked by From<Mat4<T>> for Quaternion
        /// A quaternion may be obtained from a rotation matrix.
        ///
        /// ```
        /// # extern crate vek;
        /// # #[macro_use] extern crate approx;
        /// # use vek::{Mat3, Quaternion, Vec3};
        /// # fn main() {
        /// let (angle, axis) = (3_f32, Vec3::new(1_f32, 3., 7.));
        /// let a = Quaternion::rotation_3d(angle, axis);
        /// let b = Quaternion::from(Mat3::rotation_3d(angle, axis));
        /// assert_relative_eq!(a, b);
        /// # }
        /// ```
        impl<T> From<Mat3<T>> for Quaternion<T>
            where T: Zero + One + Mul<Output=T> + Add<Output=T> + Sub<Output=T> + Real
        {
            fn from(m: Mat3<T>) -> Self {
                Mat4::from(m).into()
            }
        }
        */

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
            /// Returns an element-wise-converted copy of this matrix, using the given conversion
            /// closure.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let m = Mat4::<f32>::new(
            ///     0.25, 1.25, 5.56, 8.66,
            ///     8.53, 2.92, 3.86, 9.36,
            ///     1.02, 0.28, 5.52, 6.06,
            ///     6.20, 7.01, 4.90, 5.26
            /// );
            /// let m = m.map(|x| x.round() as i32);
            /// assert_eq!(m, Mat4::new(
            ///     0, 1, 6, 9,
            ///     9, 3, 4, 9,
            ///     1, 0, 6, 6,
            ///     6, 7, 5, 5
            /// ));
            /// ```
            pub fn map<D,F>(self, mut f: F) -> Mat2<D> where F: FnMut(T) -> D {
                let m = self.$lines;
                Mat2 {
                    $lines: CVec2::new(
                        Vec2::new(f(m.x.x), f(m.x.y)),
                        Vec2::new(f(m.y.x), f(m.y.y)),
                    )
                }
            }

            /// Returns a memberwise-converted copy of this matrix, using `AsPrimitive`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use vek::vec::Vec4;
            /// let v = Vec4::new(0_f32, 1., 2., 3.);
            /// let i: Vec4<i32> = v.as_();
            /// assert_eq!(i, Vec4::new(0, 1, 2, 3));
            /// ```
            ///
            /// # Safety
            ///
            /// **In Rust versions before 1.45.0**, some uses of the `as` operator were not entirely safe.
            /// In particular, it was undefined behavior if
            /// a truncated floating point value could not fit in the target integer
            /// type ([#10184](https://github.com/rust-lang/rust/issues/10184));
            ///
            /// ```ignore
            /// # use num_traits::AsPrimitive;
            /// let x: u8 = (1.04E+17).as_(); // UB
            /// ```
            pub fn as_<D>(self) -> Mat2<D> where T: AsPrimitive<D>, D: 'static + Copy {
                let m = self.$lines;
                Mat2 {
                    $lines: CVec2::new(
                        Vec2::new(m.x.x.as_(), m.x.y.as_()),
                        Vec2::new(m.y.x.as_(), m.y.y.as_()),
                    )
                }
            }

            /// Applies the function f to each element of two matrices, pairwise, and returns the result.
            ///
            /// ```
            /// use vek::mat::repr_c::row_major::Mat4;
            ///
            /// let a = Mat4::<f32>::new(
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99,
            ///     0.25, 1.25, 2.52, 2.99
            /// );
            /// let b = Mat4::<i32>::new(
            ///     0, 1, 0, 0,
            ///     1, 0, 0, 0,
            ///     0, 0, 1, 0,
            ///     0, 0, 0, 1
            /// );
            /// let m = a.map2(b, |a, b| a.round() as i32 + b);
            /// assert_eq!(m, Mat4::new(
            ///     0, 2, 3, 3,
            ///     1, 1, 3, 3,
            ///     0, 1, 4, 3,
            ///     0, 1, 3, 4
            /// ));
            /// ```
            pub fn map2<D,F,S>(self, other: Mat2<S>, mut f: F) -> Mat2<D> where F: FnMut(T, S) -> D {
                let m = self.$lines;
                let o = other.$lines;
                Mat2 {
                    $lines: CVec2::new(
                        Vec2::new(f(m.x.x, o.x.x), f(m.x.y, o.x.y)),
                        Vec2::new(f(m.y.x, o.y.x), f(m.y.y, o.y.y))
                    )
                }
            }

            /// The matrix's transpose.
            ///
            /// ```
            /// # extern crate vek;
            /// # use vek::Mat2;
            ///
            /// # fn main() {
            /// let m = Mat2::new(
            ///     0, 1,
            ///     4, 5
            /// );
            /// let t = Mat2::new(
            ///     0, 4,
            ///     1, 5
            /// );
            /// assert_eq!(m.transposed(), t);
            /// assert_eq!(m, m.transposed().transposed());
            /// # }
            /// ```
            // NOTE: Implemented on a per-matrix basis to avoid a Clone bound on T
            pub fn transposed(self) -> Self {
                let s = self.$lines;
                Self {
                    $lines: CVec2::new(
                        Vec2::new(s.x.x, s.y.x),
                        Vec2::new(s.x.y, s.y.y)
                    )
                }
            }
            /// Transpose this matrix.
            ///
            /// ```
            /// # use vek::Mat2;
            ///
            /// let mut m = Mat2::new(
            ///     0, 1,
            ///     4, 5
            /// );
            /// let t = Mat2::new(
            ///     0, 4,
            ///     1, 5
            /// );
            /// m.transpose();
            /// assert_eq!(m, t);
            /// ```
            // NOTE: Implemented on a per-matrix basis to avoid a Clone bound on T
            pub fn transpose(&mut self) {
                mem::swap(&mut self.$lines.x.y, &mut self.$lines.y.x);
            }

            /// Get this matrix's determinant.
            ///
            /// A matrix is invertible if its determinant is non-zero.
            ///
            /// ```
            /// # use vek::Mat2;
            /// let (a,b,c,d) = (0,1,2,3);
            /// let m = Mat2::new(a,b,c,d);
            /// assert_eq!(m.determinant(), a*d - c*b);
            /// ```
            pub fn determinant(self) -> T where T: Mul<T,Output=T> + Sub<T,Output=T> {
                // NOTE: This works on row-major as well as column-major.
                let CVec2 {
                    x: Vec2 { x: a, y: b },
                    y: Vec2 { x: c, y: d },
                } = self.$lines;
                a*d - c*b
            }

            /// Rotates this matrix around the Z axis (counter-clockwise rotation in 2D).
            pub fn rotate_z(&mut self, angle_radians: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.rotated_z(angle_radians);
            }
            /// Rotates this matrix around the Z axis (counter-clockwise rotation in 2D).
            pub fn rotated_z(self, angle_radians: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::rotation_z(angle_radians) * self
            }
            /// Creates a matrix that rotates around the Z axis (counter-clockwise rotation in 2D).
            pub fn rotation_z(angle_radians: T) -> Self where T: Real {
                let c = angle_radians.cos();
                let s = angle_radians.sin();
                Self::new(
                    c, -s,
                    s,  c
                )
            }
            /// Scales this matrix in 2D.
            pub fn scale_2d<V: Into<Vec2<T>>>(&mut self, v: V)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.scaled_2d(v);
            }
            /// Returns this matrix scaled in 2D.
            pub fn scaled_2d<V: Into<Vec2<T>>>(self, v: V) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::scaling_2d(v) * self
            }
            /// Creates a 2D scaling matrix.
            pub fn scaling_2d<V: Into<Vec2<T>>>(v: V) -> Self where T: Zero {
                let Vec2 { x, y } = v.into();
                Self::new(
                    x, T::zero(),
                    T::zero(), y
                )
            }
            /// Shears this matrix along the X axis.
            pub fn shear_x(&mut self, k: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.sheared_x(k);
            }
            /// Returns this matrix sheared along the X axis.
            pub fn sheared_x(self, k: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::shearing_x(k) * self
            }
            /// Creates a 2D shearing matrix along the X axis.
            pub fn shearing_x(k: T) -> Self where T: Zero + One {
                Self::new(
                    T::one(), k,
                    T::zero(), T::one()
                )
            }
            /// Shears this matrix along the Y axis.
            pub fn shear_y(&mut self, k: T)
                where T: Real + MulAdd<T,T,Output=T>
            {
                *self = self.sheared_y(k);
            }
            /// Returns this matrix sheared along the Y axis.
            pub fn sheared_y(self, k: T) -> Self
                where T: Real + MulAdd<T,T,Output=T>
            {
                Self::shearing_y(k) * self
            }
            /// Creates a 2D shearing matrix along the Y axis.
            pub fn shearing_y(k: T) -> Self where T: Zero + One {
                Self::new(
                    T::one(), T::zero(),
                    k, T::one()
                )
            }
        }
        use super::mat3::Mat3;
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
        use super::mat4::Mat4;
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
        /// 4x4 matrix.
        pub mod mat4 {
            use super::*;
            /// 4x4 matrix.
            #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq/* , Ord, PartialOrd */)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            #[repr(C)]
            pub struct Mat4<T> {
                #[allow(missing_docs)]
                pub $lines: CVec4<Vec4<T>>,
            }
            mat_impl_mat!{$lines Mat4 RowMatrix4 ColumnMatrix4 CVec4 Vec4 (4 x 4) (x y z w)}
            mat_impl_mat4!{$lines}
        }
        pub use self::mat4::Mat4;

        /// 3x3 matrix.
        pub mod mat3 {
            use super::*;
            /// 3x3 matrix.
            #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq/* , Ord, PartialOrd */)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            #[repr(C)]
            pub struct Mat3<T> {
                #[allow(missing_docs)]
                pub $lines: CVec3<Vec3<T>>,
            }
            mat_impl_mat!{$lines Mat3 RowMatrix3 ColumnMatrix3 CVec3 Vec3 (3 x 3) (x y z)}
            mat_impl_mat3!{$lines}
        }
        pub use self::mat3::Mat3;

        /// 2x2 matrix.
        pub mod mat2 {
            use super::*;
            /// 2x2 matrix.
            #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq/* , Ord, PartialOrd */)]
            #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
            #[repr(C)]
            pub struct Mat2<T> {
                #[allow(missing_docs)]
                pub $lines: CVec2<Vec2<T>>,
            }
            mat_impl_mat!{$lines Mat2 RowMatrix2 ColumnMatrix2 CVec2 Vec2 (2 x 2) (x y)}
            mat_impl_mat2!{$lines}
        }
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
        pub use self::column_major::*;
    }
}

pub mod repr_c {
    //! Matrix types which use `#[repr(C)]` vectors exclusively.
    //!
    //! See also the `repr_simd` neighbour module, which is available on Nightly
    //! with the `repr_simd` feature enabled.

    use super::*;
    #[allow(unused_imports)]
    use super::vec::repr_c::{Vec2, Vec2 as CVec2};
    #[allow(unused_imports)]
    use super::vec::repr_c::{Vec3, Vec3 as CVec3};
    use super::vec::repr_c::{Vec4, Vec4 as CVec4};

    use super::quaternion::repr_c::Quaternion;
    use super::transform::repr_c::Transform;

    mat_declare_modules!{}
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Matrix types which use a `#[repr(C)]` vector of `#[repr(simd)]` vectors.

    use super::*;
    #[allow(unused_imports)]
    use super::vec::repr_simd::{Vec2};
    #[allow(unused_imports)]
    use super::vec::repr_c::{Vec2 as CVec2};
    #[allow(unused_imports)]
    use super::vec::repr_simd::{Vec3};
    #[allow(unused_imports)]
    use super::vec::repr_c::{Vec3 as CVec3};
    use super::vec::repr_simd::{Vec4};
    use super::vec::repr_c::{Vec4 as CVec4};

    use super::quaternion::repr_simd::Quaternion;
    use super::transform::repr_simd::Transform;

    mat_declare_modules!{}
}

pub use self::repr_c::*;


#[cfg(test)]
mod tests {
    macro_rules! for_each_type {
        ($mat:ident $Mat:ident $($T:ident)+) => {
            mod $mat {
                // repr_c matrices are expected to be packed.
                // repr_simd matrices are not necessarily expected to be packed in the first place.
                mod repr_c {
                    $(mod $T {
                        use $crate::mat::repr_c::column_major::$Mat;
                        use $crate::vtest::Rc;

                        #[test]
                        fn is_actually_packed() {
                            let m = $Mat::<$T>::identity();
                            let a = m.clone().into_row_array(); // same as into_col_array(), because identity.
                            let mp = unsafe {
                                ::std::slice::from_raw_parts(&m as *const _ as *const $T, a.len())
                            };
                            assert_eq!(mp, &a);
                        }
                        #[test]
                        fn is_actually_packed_refcell() {
                            let m = $Mat::<$T>::identity().map(::std::cell::RefCell::new);
                            let a = m.clone().into_row_array(); // same as into_col_array(), because identity.
                            let mp = unsafe {
                                ::std::slice::from_raw_parts(&m as *const _ as *const ::std::cell::RefCell<$T>, a.len())
                            };
                            assert_eq!(mp, &a);
                        }
                        #[test] fn claims_to_be_packed() { assert!($Mat::<$T>::default().is_packed()); }
                        #[test] fn claims_to_be_packed_refcell() { assert!($Mat::<$T>::default().map(::std::cell::RefCell::new).is_packed()); }

                        #[test]
                        fn rc_col_array_conversions() {
                            let m: $Mat<Rc<i32>> = $Mat::default().map(Rc::new);
                            let mut a = m.into_col_array();
                            assert_eq!(Rc::strong_count(&a[0]), 1);
                            *Rc::make_mut(&mut a[0]) = 2; // Try to write. If there's a double free, this is supposed to crash.
                            let mut m = $Mat::from_col_array(a);
                            assert_eq!(Rc::strong_count(&m[(0, 0)]), 1);
                            *Rc::make_mut(&mut m[(0, 0)]) = 3; // Try to write. If there's a double free, this is supposed to crash.
                        }
                        #[test]
                        fn rc_row_array_conversions() {
                            let m: $Mat<Rc<i32>> = $Mat::default().map(Rc::new);
                            let mut a = m.into_row_array();
                            assert_eq!(Rc::strong_count(&a[0]), 1);
                            *Rc::make_mut(&mut a[0]) = 2; // Try to write. If there's a double free, this is supposed to crash.
                            let mut m = $Mat::from_row_array(a);
                            assert_eq!(Rc::strong_count(&m[(0, 0)]), 1);
                            *Rc::make_mut(&mut m[(0, 0)]) = 3; // Try to write. If there's a double free, this is supposed to crash.
                        }
                    })+
                }
                #[cfg(all(nightly, feature="repr_simd"))]
                mod repr_simd {
                    mod bool {
                        use $crate::mat::repr_simd::column_major::$Mat;
                        #[test]
                        fn can_monomorphize() {
                            let _: $Mat<bool> = $Mat::<u32>::identity().map(|x| x != 0);
                        }
                    }
                }
            }
        };
    }
    // Vertical editing helps here :)
    for_each_type!{mat2 Mat2 i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    for_each_type!{mat3 Mat3 i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}
    for_each_type!{mat4 Mat4 i8 u8 i16 u16 i32 u32 i64 u64 f32 f64}

    use super::Mat4;
    use super::super::vec::Vec4;


    #[test] fn simple_mat2() {
        use crate::vec::Vec2;
        use crate::mat::row_major::Mat2 as Rows2;
        use crate::mat::column_major::Mat2 as Cols2;
        let a = Rows2::new(
            1, 2,
            3, 4
        );
        let b = Cols2::new(
            1, 2,
            3, 4
        );
        let v = Vec2::new(2, 3);

        // Not available on no_std, but don't worry, it's been long proven.
        // assert_eq!(format!("{}", a), format!("{}", b));
        
        assert_eq!(v * a, v * b);
        assert_eq!(a * v, b * v);
    }

    #[test]
    fn test_model_look_at_rh() {
        let eye = Vec4::new(1_f32, 0., 1., 1.);
        let target = Vec4::new(2_f32, 0., 2., 1.);
        let model = Mat4::<f32>::model_look_at_rh(eye, target, Vec4::unit_y());
        assert_relative_eq!(model * Vec4::unit_w(), Vec4::new(1_f32, 0., 1., 1.));
        assert_relative_eq!(model * Vec4::new(0_f32, 0., -2_f32.sqrt(), 1.), Vec4::new(2_f32, 0., 2., 1.));

        // A "model" look-at essentially undoes a "view" look-at
        let view = Mat4::<f32>::look_at_rh(eye, target, Vec4::unit_y());
        assert_relative_eq!(view * model, Mat4::identity());
        assert_relative_eq!(model * view, Mat4::identity());
    }

    #[test]
    fn test_model_look_at_lh() {
        let eye = Vec4::new(1_f32, 0., 1., 1.);
        let target = Vec4::new(2_f32, 0., 2., 1.);
        let model = Mat4::<f32>::model_look_at_lh(eye, target, Vec4::unit_y());
        assert_relative_eq!(model * Vec4::unit_w(), eye);
        let d = 2_f32.sqrt();
        assert_relative_eq!(model * Vec4::new(0_f32, 0., d, 1.), target);

        // A "model" look-at essentially undoes a "view" look-at
        let view = Mat4::look_at_lh(eye, target, Vec4::unit_y());
        assert_relative_eq!(view * model, Mat4::identity());
        assert_relative_eq!(model * view, Mat4::identity());
    }

    #[test]
    fn test_look_at_rh() {
        let eye = Vec4::new(1_f32, 0., 1., 1.);
        let target = Vec4::new(2_f32, 0., 2., 1.);
        let view = Mat4::<f32>::look_at_rh(eye, target, Vec4::unit_y());
        assert_relative_eq!(view * eye, Vec4::unit_w());
        assert_relative_eq!(view * target, Vec4::new(0_f32, 0., -2_f32.sqrt(), 1.));
    }

    #[test]
    fn test_look_at_lh() {
        let eye = Vec4::new(1_f32, 0., 1., 1.);
        let target = Vec4::new(2_f32, 0., 2., 1.);
        let view = Mat4::<f32>::look_at_lh(eye, target, Vec4::unit_y());
        assert_relative_eq!(view * eye, Vec4::unit_w());
        assert_relative_eq!(view * target, Vec4::new(0_f32, 0., 2_f32.sqrt(), 1.));
    }

}

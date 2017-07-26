
//! Matrix elements are written as `mij`, where i is the row index and j is the column index, independently of storage order.
//! This convention has been chosen because it is the *de facto* standard.

#![no_std]
#![cfg_attr(feature = "repr_simd", feature(cfg_target_feature))]
#![cfg_attr(feature = "repr_simd", feature(repr_simd, simd_ffi))]
#![cfg_attr(feature = "repr_align", feature(repr_align, attr_literals))]
//#![cfg_attr(feature = "repr_simd", allow(improper_ctypes)]
//#![cfg_attr(feature = "repr_simd", feature(link_llvm_intrinsics)]

#[cfg(feature = "serde")]
#[macro_use]
extern crate serde;

macro_rules! declare_vec_types {
    ($(#[$attrs:meta])+) => {
        #[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
        $(#[$attrs])+
        #[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub struct Vec4<T>(pub T, pub T, pub T, pub T);
    }
}

macro_rules! declare_mat_types {
    ({$lines:ident: $CVecN:ident}) => {
        // NOTE: It's likely unclear for users what PartialOrd and Ord would mean on a matrix,
        // but it's honestly completely harmless and might even be frustrating in
        // some rare cases if it wasn't derived.
        // People can implement any comparison operation they want if they need it.
        #[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
        #[cfg_attr(feature = "repr_align", repr(align(16)))] // TODO check if it's necessary
        #[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub struct Mat4<T> { pub $lines: $CVecN<Vec4<T>> }
    }
}

macro_rules! row4_fn_new {
    ($CVecN:ident<$VecN:ident<$T:ty> >) => {
        /// Creates a new row-major 4x4 matrix from individual elements.
        ///
        /// The order of parameters is the same as the `new` method of column-major 4x4 matrices.
        ///
        /// If you want to initialize a row-major 4x4 matrix with a set of rows, consider explicit initialization of the `rows` member.
        /// If you want to initialize a row-major 4x4 matrix with a set of columns, you can either use this or the transpose of these rows.
        pub fn new(
            m00: $T, m01: $T, m02: $T, m03: $T,
            m10: $T, m11: $T, m12: $T, m13: $T,
            m20: $T, m21: $T, m22: $T, m23: $T,
            m30: $T, m31: $T, m32: $T, m33: $T
        ) -> Self 
        {
            Self {
                rows: $CVecN(
                    $VecN(m00, m01, m02, m03),
                    $VecN(m10, m11, m12, m13),
                    $VecN(m20, m21, m22, m23),
                    $VecN(m30, m31, m32, m33),
                )
            }
        }
    }
}
macro_rules! col4_fn_new {
    ($CVecN:ident<$VecN:ident<$T:ty> >) => {
        /// Creates a new column-major 4x4 matrix from individual elements.
        ///
        /// The order of parameters is the same as the `new` method of row-major 4x4 matrices.
        ///
        /// If you want to initialize a column-major 4x4 matrix with a set of columns, consider explicit initialization of the `cols` member.
        /// If you want to initialize a column-major 4x4 matrix with a set of rows, you can either use this or the transpose of these rows.
        pub fn new(
            m00: $T, m01: $T, m02: $T, m03: $T,
            m10: $T, m11: $T, m12: $T, m13: $T,
            m20: $T, m21: $T, m22: $T, m23: $T,
            m30: $T, m31: $T, m32: $T, m33: $T
        ) {
            Self {
                cols: $CVecN(
                    $VecN(m00, m10, m20, m30),
                    $VecN(m01, m11, m21, m31),
                    $VecN(m02, m12, m22, m32),
                    $VecN(m03, m13, m23, m33),
                )
            }
        }
    }
}
/*
        // Keep these definition hard-coded - they are for testing purposes.
        
        pub(crate) fn iota() -> Self {
            Self::new(
                00, 01, 02, 03,
                10, 11, 12, 13,
                20, 21, 22, 23,
                30, 31, 32, 33
            )
        }
        pub(crate) fn transposed_iota() -> Self {
            Self::new(
                00, 10, 20, 30,
                01, 11, 21, 31,
                02, 12, 22, 32,
                03, 13, 23, 33
            )
        }
        // Vec4
        pub(crate) fn iota() -> Self {
            Self::new(0,1,2,3)
        }
        pub(crate) fn mat4_mul_vec4() -> Vec4<$T> {
            let m = Mat::new_debug().rows;
            let v = Vec::new_debug();
            Vec4<$T>(
                (m.0).0 * v.0 + (m.0).1 * v.1 + (m.0).2 * v.2 + (m.0).3 * v.3,
                (m.1).0 * v.0 + (m.1).1 * v.1 + (m.1).2 * v.2 + (m.1).3 * v.3,
                (m.2).0 * v.0 + (m.2).1 * v.1 + (m.2).2 * v.2 + (m.2).3 * v.3,
                (m.3).0 * v.0 + (m.3).1 * v.1 + (m.3).2 * v.2 + (m.3).3 * v.3,
            )
        }
        pub(crate) fn vec4_mul_mat4() -> Vec4<$T> {
            let m = Mat::new_debug().rows;
            let v = Vec::new_debug();
            Vec4(
                (m.0).0 * v.0 + (m.1).0 * v.1 + (m.2).0 * v.2 + (m.3).0 * v.3,
                (m.0).1 * v.0 + (m.1).1 * v.1 + (m.2).1 * v.2 + (m.3).1 * v.3,
                (m.0).2 * v.0 + (m.1).2 * v.1 + (m.2).2 * v.2 + (m.3).2 * v.3,
                (m.0).3 * v.0 + (m.1).3 * v.1 + (m.2).3 * v.2 + (m.3).3 * v.3,
            )
        }
*/
macro_rules! matn_fns_init {
    ($CVecN:ident<$VecN:ident<$T:ty> >, $($get:tt)+) => {
        pub fn from_diagonal(val: $T) -> Self {
            let mut out = unsafe { ::core::mem::uninitialized() };
            $(
                out.$get = $VecN::zero();
                (out.$get).$get = val.clone();
            )+
            out
        }
        pub fn zero() -> Self {
            Self::from_diagonal(T::zero())
        }
        pub fn identity() -> Self {
            Self::from_diagonal(T::one())
        }
    }
}
macro_rules! matn_impl_default {
    ($CVecN:ident<$VecN:ident<$T:ty> >) => {
        impl Default for $Mat<$T> { fn default() -> Self { Self::identity() } }
    }
}

pub mod repr_c {
    declare_vec_types!(#[repr(packed,C)] #[cfg_attr(feature = "repr_align", repr(align(16)))]);
    pub mod row_major {
        use super::*;
        declare_mat_types!({rows: Vec4});
    }
    pub mod column_major {
        use super::*;
        declare_mat_types!({cols: Vec4});
    }
}
/* TODO: Conversions between col-major and row-major
impl From<$ColN<$T>> for $RowN<$T> { fn from(m: $ColN<$T>) -> Self { Self { rows: m.transposed().cols } }}
impl From<$RowN<$T>> for $ColN<$T> { fn from(m: $RowN<$T>) -> Self { Self { cols: m.transposed().rows } }}
impl $RowN<$T> { pub fn into_column_major(self) -> $ColN { self.into() }}
impl $ColN<$T> { pub fn into_row_major(self) -> $RowN { self.into() }}
impl $RowN<$T> { pub fn to_column_major(&self) -> $ColN { self.clone().into() }}
impl $ColN<$T> { pub fn to_row_major(&self) -> $RowN { self.clone().into() }}
use core::fmt::{self, Formatter};
impl<T: Display> Display for $RowN<T> {
    fn fmt(&self, f: &mut Formatter, rhs: Self) -> fmt::Result {
        write!(f, "[")?;
        for row in self.rows.iter().take(self.rows.dimension()-1) {
            writeln!(f, " {}", row)?;
        }
        write!(f, " {}]", self.rows.iter().last())
    }
}
impl<T: Display> Display for $ColN<T> {
    fn fmt(&self, f: &mut Formatter, rhs: Self) -> fmt::Result {
        write!(f, "{}", self.to_row_major())
    }
}
*/

#[cfg(feature = "repr_simd")]
pub mod repr_simd {

    #[cfg(any(target_arch = "x86", target_arch = "x86-64"))]
    extern crate x86intrin;

    #[cfg(any(target_arch = "x86", target_arch = "x86-64"))]
    use self::x86intrin::*;

    macro_rules! mat4_fn_transposed_x86 {
        ({$lines:ident: $CVecN:ident} $_mm_unpacklo_m:ident $_mm_unpackhi_m:ident $_mm_movelh_m:ident $_mm_movehl_m:ident) => {
            #[must_use]
            pub fn transposed(self) -> Self {
                let tmp0 = $_mm_unpacklo_m(self.$lines.0, self.$lines.1); 
                let tmp2 = $_mm_unpacklo_m(self.$lines.2, self.$lines.3);
                let tmp1 = $_mm_unpackhi_m(self.$lines.0, self.$lines.1);
                let tmp3 = $_mm_unpackhi_m(self.$lines.2, self.$lines.3); 
                Self {
                    $lines: $CVecN(
                        $_mm_movelh_m(tmp0, tmp2),
                        $_mm_movehl_m(tmp2, tmp0),
                        $_mm_movelh_m(tmp1, tmp3),
                        $_mm_movehl_m(tmp3, tmp1),
                    )
                }
            }
        }
    }
    #[cfg(any(target_arch = "x86-64", all(target_arch = "x86", target_feature = "sse")))]
    macro_rules! mat4_f32_fn_transposed_sse {
        ({$lines:ident: $CVecN:ident}) => {
            mat4_fn_transposed_x86!({$lines: $CVecN} _mm_unpacklo_ps _mm_unpackhi_ps _mm_movelh_ps _mm_movehl_ps){}
        }
    }
    #[cfg(any(target_arch = "x86-64", all(target_arch = "x86", target_feature = "sse2")))]
    macro_rules! mat4_i32_fn_transposed_sse2 {
        ({$lines:ident: $CVecN:ident}) => {
            mat4_fn_transposed_x86!({$lines: $CVecN} _mm_unpacklo_epi32 _mm_unpackhi_epi32 _mm_movelh_epi32 _mm_movehl_epi32){}
        }
    }


    use super::repr_c::Vec4 as CVec4;

    declare_vec_types!(#[repr(packed,simd)]);

    pub mod row_major {
        use super::*;
        declare_mat_types!({rows: CVec4});

        impl Mat4<f32> {
            #[cfg(any(target_arch = "x86-64", all(target_arch = "x86", target_feature = "sse")))]
            mat4_f32_fn_transposed_sse!{{rows: CVec4}}
        }
        impl Mat4<i32> {
            #[cfg(any(target_arch = "x86-64", all(target_arch = "x86", target_feature = "sse2")))]
            mat4_i32_fn_transposed_sse2!{{rows: CVec4}}
        }
        impl Mat4<u32> {
            #[cfg(any(target_arch = "x86-64", all(target_arch = "x86", target_feature = "sse2")))]
            mat4_i32_fn_transposed_sse2!{{rows: CVec4}}
        }
    }
    pub mod column_major {
        use super::*;
        declare_mat_types!({cols: CVec4});

        impl Mat4<f32> {
            #[cfg(any(target_arch = "x86-64", all(target_arch = "x86", target_feature = "sse")))]
            impl_mat4_f32_transpose_sse!{{cols: CVec4}}
        }
        impl Mat4<i32> {
            #[cfg(any(target_arch = "x86-64", all(target_arch = "x86", target_feature = "sse2")))]
            impl_mat4_i32_transpose_sse2!{{cols: CVec4}}
        }
        impl Mat4<u32> {
            #[cfg(any(target_arch = "x86-64", all(target_arch = "x86", target_feature = "sse2")))]
            impl_mat4_i32_transpose_sse2!{{cols: CVec4}}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::repr_c::row_major::Mat4 as CRow4;
    use super::repr_c::column_major::Mat4 as CCol4;
    use super::repr_simd::row_major::Mat4 as Row4;
    use super::repr_simd::column_major::Mat4 as Col4;

    macro_rules! test_mat_basics {
        ($($Mat:ident)+, $($T:ty)+) => { $($( mod basics { use super::*; mod $Mat { use super::*; mod $T { use super::*;
                #[test] fn default_is_identity() { assert_eq!($Mat::<$T>::default(), $Mat::<$T>::identity()); }
                #[test] fn transposed_identity() { assert_eq!($Mat::<$T>::identity().transposed(), $Mat::<$T>::identity()); }
                #[test] fn transposed_iota() { assert_eq!($Mat::<$T>::iota().transposed(), $Mat::<$T>::transposed_iota()); }
                #[test] fn identity_mul_vec() { assert_eq!($Mat::<$T>::identity() * v, v); }
                #[test] fn vec_mul_identity() { assert_eq!(v * $Mat::<$T>::identity(), v); }
                #[test] fn mat_mul_vec() { 
                    let (m, v) = ($Mat::<$T>::iota(), VecN::iota());
                    assert_eq!(m * v, mat4_mul_vec4(m, v));
                }
                #[test] fn vec_mul_mat() { 
                    let (m, v) = ($Mat::<$T>::iota(), VecN::iota());
                    assert_eq!(v * m, vec4_mul_mat4(v, m));
                }
            }}})+)+
        }
    }

    test_mat_basics!(Row4 Col4 CRow4 CCol4, i8 u8 i16 u16 i32 u32 i64 u64 f32 f64);
    test_mat_basics!(CRow4 CCol4, isize usize);
}

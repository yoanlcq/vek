//! Generic but efficient SIMD vector+matrix library for game engines, with focus on intent and the small bits that make you happier as a user.
//!
//! DO NOT USE (yet). This is very much a work-in progress, breaking changes happen all the time on a whim.
//!
//! The efficiency claim is based on the fact that implementations are specialized according
//! to the target hardware, and the generated assembly is checked to ensure it is optimal.  
//! As you would expect, SSE-enabled x86 CPUs do benefit from this.
//!
//! See the FAQ in the README and the [the roadmap to 1.0](https://github.com/yoanlcq/vek/issues/1) for more info.
//!
//!
//! # Overview
//!
//! Here is what `vek` has to offer:
//!
//! - General-purpose vectors: `Vec2<T>`, `Vec3<T>`, `Vec4<T>`. They are tuple structs and have the
//! same features as spatial vectors.
//! - "Data crunching" vectors: `Vec8<T>`, `Vec16<T>`, `Vec32<T>`, `Vec64<T>`, useful for
//! performing basic operaton on many elements in the best way allowed by CPU features.
//! - Spatial vectors: `Xy<T>`, `Xyz<T>`, `Xyzw<T>`. They are for storing points and directions in
//! euclidian spaces.
//! - RGB vectors: `Rgb<T>`, `Rgba<T>`. They have extended functionality related to color.
//! - Texture coordinate vectors: `Uv<T>`, `Uvw<T>`;
//! - Spatial extent vectors: `Extent2<T>`, `Extent3<T>`, for representing width, height and depth.
//! - Square matrices: `Mat2<T>`, `Mat3<T>`, `Mat4<T>`.
//!
//! Matrices can be row-major or column-major at your option, because there are use cases for both
//! layouts, even though column-major is often better performance-wise.
//!
//! Types share functionality whenever relevant.  
//! Also, there are several (concise) ways to convert from one vector type to another:
//!
//! - Vectors implement `AsRef` and `AsMut` on any lower-dimensioned Vector with the same element type.  
//! - Vectors implement `From` on any Vector or tuple type  with the same element type. When converting to a
//!   higher-dimensioned vector, uninitialized elements are set to the default value of their type.
//!
//! Here's a small taste of what using this crate could look like :
//!
//! ```ignore
//! // TODO make this example work
//! # extern crate vek;
//! use vek::mat::row_major::Mat4 as Rows4;
//! use vek::mat::column_major::Mat4 as Cols4;
//! use vek::vec::Xyzw;
//! use std::f32::consts::PI;
//!
//! let position = Xyzw::new_point(1_f32, 2_f32, 3_f32);
//! let model: Cols4<_> = Rows4::rotation_x(PI) * Cols4::rotation_3d(PI, position);
//! let new_position = model * position;
//! ```
//!
//! You can use straight tuple types for convenience, however since their
//! memory layout is undefined, the compiler can't generate optimal code. `vek`'s vector
//! types are just as convenient (if not more so), but marked `#[repr(simd)]` when enabled.
//!
//! Matrix elements are written as `mij`, where i is the row index and j is the column index, independently of storage order.
//! This convention has been chosen because it is the *de facto* standard.
//!
//! # Cargo features
//!
//! - `serde` makes vectors and matrices derive `Serialize` and `Deserialize`.
//!
//! ***
//!
//! - `repr_simd` allows Rust's `repr_simd` and `simd_ffi` features, which require Nightly, but
//! help a lot to generate high-quality code.
//! - `repr_align` allows Rust's `repr_align` features, which require Nightly. It's always safe to
//!   leave disabled, but does increase code quality a bit when enabled.
//! - `x86intrin` enables x86 intrinsics through the `x86intrin` crate. `vek` doesn't diretcly
//!   depend on it because it won't compile on Stable and there's no way (as of this writing)
//!   to selectively depend on a crate based on the `rustc` version, not even via build scripts.
//!
//! ***
//!
//! - `f32`, `f64`, `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `i128`, `u128`, `isize`, `usize`
//!   implement vectors and matrices for each of these types. Select only those that you need in
//!   order to reduce build times.
//! - `itypes` is a shorthand for `i8`, `i16`, `i32` and `i64` (but neither `i128` nor `isize`).
//! - `utypes` is a shorthand for `u8`, `u16`, `u32` and `u64` (but neither `u128` nor `usize`).
//! - `ftypes` is a shorthand for `f32` and `f64`.
//!
//! ***
//!
//! - `fix` implements vectors and matrices of fixed-point numbers from the `fix` crate.
//! - `fpa` implements vectors and matrices of fixed-point numbers from the `fpa` crate.
//! - `num-bigint` implements vectors and matrices of big integers from the `num-bigint` crate.
//!
//! # `#![no_std]`
//! This crate is `#![no_std]`.

#![no_std]
#![doc(
    test(attr(deny(warnings))),
    html_root_url = "https://docs.rs/vek/0.2.0",
    //html_logo_url = "https://yoanlcq.github.io/vek/logo.png",
    //html_favicon_url = "https://yoanlcq.github.io/vek/favicon.ico",
)]
#![deny(missing_docs)]
#![deny(warnings)]
#![cfg_attr(all(nightly, feature="clippy"), feature(plugin))]
#![cfg_attr(all(nightly, feature="clippy"), plugin(clippy))]
#![cfg_attr(all(nightly, feature="repr_simd" ), feature(cfg_target_feature))]
#![cfg_attr(all(nightly, feature="repr_simd" ), feature(repr_simd, simd_ffi))]
#![cfg_attr(all(nightly, feature="repr_align"), feature(repr_align, attr_literals))]
//#![cfg_attr(feature="repr_simd", allow(improper_ctypes)]
//#![cfg_attr(feature="repr_simd", feature(link_llvm_intrinsics)]
#![cfg_attr(all(nightly,test), feature(test))]

#[cfg(all(nightly,test))]
extern crate test;
/*
#[cfg(feature="serde_derive")]
#[macro_use]
extern crate serde_derive;
*/
#[cfg(feature="serde")]
#[macro_use]
extern crate serde;
#[cfg(feature="fix")]
extern crate fix;
#[cfg(feature="fpa")]
extern crate fpa;
#[cfg(feature="num_bigint")]
extern crate num_bigint;

#[cfg(feature="x86intrin")]
extern crate x86intrin;

// extern crate num_traits;
// extern crate num_integer;

pub mod mat;
pub use mat::*;
pub mod vec;
pub use vec::*;


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

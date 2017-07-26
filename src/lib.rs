//! Generic but efficient SIMD vector+matrix library for game engines, with focus on intent and the small bits that make you happier as a user.
//!
//! DO NOT USE (yet). This is very much a work-in progress, breaking changes happen all the time on a whim.
//!
//! Useful, efficient and colorful vector and matrix types for use in computer graphics, with focus
//! on intent, performance, and reality of the target hardware.
//!
//! The efficiency claim is based on the fact that implementations are specialized according
//! to the target hardware, and the generated assembly is checked to ensure it is optimal.  
//! As you would expect, SSE-enabled x86 CPUs do benefit from this.
//!
//! See the FAQ in the README and the [the roadmap to 1.0](https://github.com/yoanlcq/vek/issues#1) for more info.
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
//!
//! # Cargo features
//!
//! - `serde` makes vectors and matrices derive `Serialize` and `Deserialize`.
//! - `repr_simd` allows Rust's `repr_simd` and `simd_ffi` features, which require Nightly, but
//! help a lot to generate high-quality code.
//! - `repr_align` allows Rust's `repr_align` features, which require Nightly. It's always safe to
//!   leave disabled, but does increase code quality a bit when enabled.
//! - `f32`, `f64`, `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `i128`, `u128`, `isize`, `usize`
//!   implement vectors and matrices for each of these types. Select only those that you need in
//!   order to reduce build times.
//! - `itypes` is a shorthand for `i8`, `i16`, `i32` and `i64` (but neither `i128` nor `isize`).
//! - `utypes` is a shorthand for `u8`, `u16`, `u32` and `u64` (but neither `u128` nor `usize`).
//! - `ftypes` is a shorthand for `f32` and `f64`.
//! - `fix` implements vectors and matrices of fixed-point numbers from the `fix` crate.
//! - `fpa` implements vectors and matrices of fixed-point numbers from the `fpa` crate.
//! - `num-bigint` implements vectors and matrices of big integers from the `num-bigint` crate.
//!
//! # `#![no_std]`
//! This crate is `#![no_std]`.


#![no_std]
#![doc(
    html_root_url = "https://docs.rs/vek/0.1.0",
    test(attr(deny(warnings)))
)]
#![deny(missing_docs)]
#![feature(test)]
#![feature(repr_simd)]
#![feature(repr_align, attr_literals)]

extern crate test;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

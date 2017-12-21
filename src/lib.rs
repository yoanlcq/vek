//! Generic but efficient SIMD vector+matrix library for game engines, with focus on intent and the small bits that make you happier as a user.
//!
//! # Disclaimer
//!
//! **DO NOT USE** (yet). This is very much a work-in progress, breaking changes happen all the time on a whim.
//! Also, the API is a bit cluttered by the `repr(C)` vs `repr(simd)` stuff (I'm still not very
//! confident about that), 
//! and very few operations have been proven to actually work in 3D scenes (as of today).
//!
//! See the FAQ in the README and the [the roadmap to 1.0](https://github.com/yoanlcq/vek/issues/1) for more info.
//!
//! The efficiency claim is based on the fact that implementations are specialized according
//! to the target hardware, and the generated assembly is checked to ensure it is optimal.  
//! As one would expect, SSE-enabled x86 CPUs do benefit from this.
//!
//! However, as of today, my focus is on functionality more than optimizations, which
//! means some operations are not as fast as they ought to be.
//!
//! # Overview
//!
//! Here is what `vek` has to offer:
//!
//! - General-purpose vectors: `Vec2<T>`, `Vec3<T>`, `Vec4<T>`. They have uses for representing
//! points or direction in euclidian spaces.
//! - "Data crunching" vectors: `Vec8<T>`, `Vec16<T>`, `Vec32<T>`, `Vec64<T>`, useful for
//! performing basic operaton on many elements in the best way allowed by CPU features.
//! - RGB vectors: `Rgb<T>`, `Rgba<T>`. They have extended functionality related to color.
//! - Texture coordinate vectors: `Uv<T>`, `Uvw<T>`;
//! - Spatial extent vectors: `Extent2<T>`, `Extent3<T>`, for representing width, height and depth.
//! - Square matrices: `Mat2<T>`, `Mat3<T>`, `Mat4<T>`.
//!
//! Matrices can be row-major or column-major at your option, because there are use cases for both
//! layouts, even though column-major is often better performance-wise, for most use case in
//! computer graphics.
//!
//! Types share functionality whenever relevant.  
//! Also, there are several (concise) ways to convert from one vector type to another:
//!
//! - Vectors implement `AsRef` and `AsMut` on any lower-dimensioned Vector with the same element type.  
//! - Vectors implement `From` on any Vector or tuple type  with the same element type. When converting to a
//!   higher-dimensioned vector, uninitialized elements are set to the default value of their type.
//!
//! Here's a preview of what using this crate looks like :
//!
//! ```
//! # extern crate vek;
//! #[macro_use] 
//! extern crate approx;
//! use vek::{Vec4, Mat4, Lerp};
//! use std::f32::consts::PI;
//!
//! # fn main() {
//! let point = Vec4::new_point(1_f32, 2., 3.); // (1, 2, 3, 1)
//! let direction = Vec4::new_direction(1_f32, 2., 3.); // (1, 2, 3, 0)
//! let model = Mat4::rotation_3d(PI, direction)
//!     .translated_3d(Vec4::unit_x() * 3.)
//!     .scaled_3d(2.);
//! println!("Rotated point: {}", model * point);
//!
//! let four = Vec4::broadcast(2_f32).sqrt().product();
//! assert_relative_eq!(four, 4.);
//! let four = Vec4::from(four); // Same as broadcast()
//! let iota = Vec4::iota();
//! assert_relative_eq!(Vec4::lerp(iota, four, 0.0_f32), iota);
//! assert_relative_eq!(Vec4::lerp(iota, four, 0.5_f32), Vec4::new(2., 2.5, 3., 3.5));
//! assert_relative_eq!(Vec4::lerp(iota, four, 1.0_f32), four);
//! # }
//! ```
//!
//! Because vectors implement `From` tuples, you may directly use tuples
//! in operations that are generic over relevant parameters.
//!
//! In the docs and the code, matrix elements are written as `mij`,
//! where i is the row index and j is the column index, independently of storage layout.
//! This convention has been chosen because it is the mathematical standard.
//!
//! # The deal with `repr_c` and `repr_simd` modules
//!
//! *N.B: If you're on Stable or have disabled this crate's "`repr_simd`"
//! feature, you don't need to worry about this.*
//!
//! The need for convenient SIMD-enabled types was a major motivation for creating `vek`.
//! However, since there are a some issues with Nightly Rust's `#[repr(simd)]` features,
//! we would like to be able to also use regular `#[repr(C)]` vectors.
//!
//! Therefore, `vek` splits main modules into two sub-modules, `repr_c` and `repr_simd`,
//! hoping to make everyone happy. This is a trade-off between functionality and
//! implementation complexity that I am willing to make for now.
//!
//! ## `#[repr(simd)]` caveats
//!
//! You can instantiate any `#[repr(simd)]` type with any type as long as
//! it is a "machine type", like `f32` and `i32`, but not `isize` or newtypes.
//!
//! **Be careful:** the size of a `#[repr(simd)]` vector is never guaranteed to be
//! exactly equal to the sum of its elements.  
//! For instance, an SIMD `Vec3<f32>` actually contains 4 `f32` elements on x86 instead of 3.  
//! **Also, `repr_simd` matrices are affected by this.**
//!
//! Therefore, be extra careful when sending these as raw data, as you may want
//! to do with OpenGL.
//!
//! ## So, which do I pick?
//!
//! Use types of `repr_simd` modules when:
//!
//! - You know how your target hardware handles `#[repr(simd)]`, and therefore
//!   know how much memory SIMD vector structures actually take;
//! - You don't mind alignment requirements and extra empty space;
//!
//! Otherwise, or when in doubt, just pick `repr_c`.
//! This crate always re-exports `repr_c` modules, so this would be the default.
//!
//! # Cargo features
//!
//! - `serde` makes vectors and matrices derive `Serialize` and `Deserialize`.
//! - `image` makes color vectors implement the `Pixel` trait from the `image` crate.
//!
//! ***
//!
//! - `vec2`, `vec3`, `vec8`, `vec16`, `vec32`, `vec64`, `rgba`, `rgb`, `extent3`, `extent2`, `uvw`, `uv`, `mat2`, `mat3`, `quaternion`
//!   Select which types you want. Restricting your selection drastically decreases compile times.  
//!   They are all enabled by default so that they appear in the documentation (and that doc-tests work).  
//! - `geom`, `bezier`  
//!   Other commonly useful and lightweight goodies such as rectangles and Bézier curves.  
//!   The `FrustumPlanes<T>` type, which is required by orthographic and frustum projection
//!   matrices, is enabled with `geom`.
//!
//! ***
//!
//! - `repr_simd` enables Nightly Rust's `repr_simd` and `simd_ffi` features, which
//! help a lot to generate high-quality code.
//! - `repr_align` enables Nightly Rust's `repr_align` features so that `#[repr(c)]`
//!   have the same alignment as their SIMD counterpart.  
//!   Because enabling it may incur some issues, it is not enabled by default (and might become
//!   deprecated).
//! - `x86intrin` enables x86 intrinsics through the `x86intrin` crate. `vek` doesn't directly
//!   depend on it because it won't compile on Stable and there's no way (as of this writing)
//!   to selectively depend on a crate based on the `rustc` version, not even via build scripts.
//!
//! # `#![no_std]`
//! This crate is `#![no_std]`.

#![no_std]
#![doc(
	test(attr(deny(warnings))),
	html_root_url = "https://docs.rs/vek/0.8.0",
	//html_logo_url = "https://yoanlcq.github.io/vek/logo.png",
	//html_favicon_url = "https://yoanlcq.github.io/vek/favicon.ico",
)]
//#![warn(missing_docs)]
//#![deny(warnings)]
//#![allow(unknown_lints)]
//#![deny(incoherent_fundamental_impls)]
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

#[cfg(feature="serde")]
#[macro_use]
extern crate serde;

#[cfg(feature="x86intrin")]
extern crate x86intrin;

extern crate num_traits;
extern crate num_integer;
// NOTE: Allow unused imports here, because usage depends on which features are enabled.
#[allow(unused_imports)]
#[macro_use]
extern crate approx;
#[allow(unused_imports)]
#[macro_use]
extern crate static_assertions;

pub mod mat;
pub use mat::*;
pub mod vec;
pub use vec::*;
pub mod ops;
pub use ops::*;

#[cfg(feature="quaternion")]
pub mod quaternion;
#[cfg(feature="quaternion")]
pub use quaternion::*;

#[cfg(feature="bezier")]
pub mod bezier;
#[cfg(feature="bezier")]
pub use bezier::*;

#[cfg(feature="geom")]
pub mod geom;
#[cfg(feature="geom")]
pub use geom::*;

pub mod frustum;
pub use frustum::*;
pub mod rect;
pub use rect::*;


// 0.9 roadmap:
// TODO: Mat4: Prove that invert() works on projection matrices too.
// TODO: Mat4 is_invertible(), determinant(), etc...
// TODO:
// - Others
//   Document the assumed handedness for rotations
//       Handedness is only human perception. The maths are the same.
//   Document the assumed matrix transform order (matrix * column_vector).
//       If you wanna reverse the order, you have to transpose everything.
//   Document the _zo _no stuff
//   Document what _lh _rh transformations actually do (the tests do it but...)
//   Document that feature requests are welcome!
// NOTE: The point of `fix`, `fpa` and `num-bigint` features was to make them implement vek::ops.
// NOTE: Clamp: rename to `PartialClamp` trait ? Resolved: NO. This would be a mess for an issue nobody really cares abouts.
// - Matrices:
//   - Add 3D shearing.
//   - Add scale_from_point and rotate_about_point
//   - foward_*, right(), unit_x() etc
// - Quaternions:
//   - to_angle_axis()
//   - decompose() for Mat4


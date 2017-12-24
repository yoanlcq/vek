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
//! points or directions in euclidian spaces.
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
//! assert_relative_eq!(Lerp::lerp(iota, four, 0.0_f32), iota);
//! assert_relative_eq!(Lerp::lerp(iota, four, 0.5_f32), Vec4::new(2., 2.5, 3., 3.5));
//! assert_relative_eq!(Lerp::lerp(iota, four, 1.0_f32), four);
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

#![cfg_attr(not(test), no_std)]
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

#[cfg(not(test))]
extern crate core as std;

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
pub mod frustum;
pub use frustum::*;
pub mod rect;
pub use rect::*;
pub mod tween;
pub use tween::*;
#[cfg(feature="quaternion")]
pub mod transform;
#[cfg(feature="quaternion")]
pub use transform::*;

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

// 0.9 roadmap:
//
// # Features
// - Make vectors properly implement AsRef and AsMut to all other kinds of vectors
// - impl FromStr for vectors
// - from_html_hex(s: &str) for Rgb and Rgba
// - Re-provide the `fix`, `fpa` and `num-bigint` optional dependencies.
//   The point is to make their types implement most of vek::ops traits;
// - Matrices:
//   - Add 3D shearing.
//   - Add symmetries.
//   - Add decompose() (steal from GLM) (convert into Transform!)
//   - Add scale_from_point and rotate_about_point
//   - is_diagonal_matrix()
//   - is_symmetric()
//   - FromStr
// - Add Euler Angles ?
//   - I'm not sure, because they suck, and nobody agrees on the order in which
//     rotations are applied. Some say X,Y,Z, others say Z,Y,X.
//     I'd rather leave this to users who know better what they want.
// - Free functions (e.g dot(), cross(), etc).
//   This would require pulling new traits out of existing functionality.
//   - dot()
//   - hadd()
//   - cross()
//   - distance()
//   - normalize()
//   - reflect()
//   - refract()
//   - face_forward()
//   - angle_between()
//   - transpose()
//   - invert()
//   Note that there are no free functions for Lerp, Slerp, etc.
//   It's enough to just write Lerp::lerp(..). Less pain for us to maintain, but I might change my mind.
// - Consider turning the ops module into a vek-ops crate (Like num did with num-traits)
// - Consider a vek-derive crate for Lerp and stuff;
//
// # Soft fixes
// - More assertions for projection matrices;
// - Re-enable #![warn(missing_docs)]
// - Make sure to fullfill the SIMD efficiency promise.
//   - Use platform-intrinsics for operations on repr(simd) vectors.
//   - transposed_sse() for Mat4<f32> (based on _MM_TRANSPOSE4_PS())
//   - transposed_sse2() for Mat4<i32>
//   - dot_sse4_1() for Vec4<f32>
//   - load/store_nontemporal() for Vec4<f32> and Vec4<i32> (!! needs fencing! So there should be
//   associated fence functions! !!)
//   - Many others ???
//
// # Docs and publicity
// - Do fix the README and src/lib.rs's doc.
// - Change the headline to "2D and 3D Math swiss army knife" or something
// - Make a cool logo!
// - "Explain" the reason behind the name;
// - Sort API entries so that documentation is easy to discover
// - Document mixing repr_c and repr_simd (quirks, and From implementations)
// - Document mixing row-major and column-major (new(), Display, public member, multiplication and efficiency)
// - Document which traits aren't needed to be imported (Lerp, Slerp, etc) and why these in
//   particular. (Answer: it's convenient and arbitrary. There's no rule.)
// - Explain why I'm not using `#[repr(packed)]`, and how to deal with this as a user.
// - Document choices for assertions:
//   - assert!() when critical to memory safety;
//   - debug_assert!() otherwise.
//   - Illustrate with perspective projection matrix. We'd rather have the final user see weird behaviour
//     than a sudden unexplained panic.
// - Document the assumed handedness for rotations
//   Handedness is only a matter of human visualization. The maths are the same.
// - Document the assumed matrix transform order (matrix * column_vector).
//   If you wanna reverse the order, you have to transpose everything.
// - Document the _zo _no stuff
// - Document what _lh _rh transformations actually do;
//   For instance, it's hard to picture the difference between look_at_view_rh() and
//   look_at_view_lh().
// - Document that feature requests are welcome!
// - Add these to unwanted-features, wont-fix, whatever :
//   - Clamp WILL NOT see a `PartialClamp` counterpart. This would be a mess for an issue nobody really cares about.
//   - Factor and Progress types (for Lerp, etc) ARE NOT REQUIRED to be Float.
//     Think about fixed-point: They can't implement classify(), infinity(), etc.
//

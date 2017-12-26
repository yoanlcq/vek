//! Generic 2D-3D math swiss army knife for game engines, with SIMD support and focus on convenience.
//!
//! See [the wiki](https://github.com/yoanlcq/vek/wiki) for an overview, FAQ, guides, and other info.
//!
//! This crate is still in its alpha days!
//! The public API is quite close to being stable, but it hasn't been battle-tested enough.  
//! Also, some parts, such as `geom` and `bezier`, definitely miss some functionalities.  
//! Issues and bug reports are very welcome!
//!
//! # Cargo features
//!
//! - `serde` makes vectors and matrices derive `Serialize` and `Deserialize`.
//! - `image` makes color vectors implement the `Pixel` trait from the `image` crate.
//! - `vec8`, `vec16`, `vec32`, `vec64`, `rgba`, `rgb`, `extent3`, `uvw`, `uv`, `mat2`, `mat3`, `quaternion`
//!   Enable these types. Restricting your selection drastically decreases compile times.  
//!   Other types are always enabled for the sake of doc-tests.  
//!   Enabling `quaternion` also unlocks the experimental `Transform` type.
//! - `geom` enables the experimental geom module, which provides simple geometric primitives.
//!   In the future, it will be more feature-complete and provide optimized mass-processing
//!   routines.
//! - `bezier` unlocks Bézier curve types.
//! - `repr_simd` enables Nightly Rust's `repr_simd` and `simd_ffi` features, and unlock
//!   SIMD versions of all appropriate types (though `repr_simd` modules).
//!   On Stable, this feature has no effect.
//! - `x86intrin` enables x86 intrinsics through the `x86intrin` crate. `vek` doesn't directly
//!   depend on it because it won't compile on Stable and there's no way (as of this writing)
//!   to selectively depend on a crate based on the `rustc` version, not even via build scripts.
//!
//! # `#![no_std]`
//! This crate is `#![no_std]`.

#![cfg_attr(not(test), no_std)]
#![doc(
	test(attr(deny(warnings))),
	html_root_url = "https://docs.rs/vek/0.8.1",
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


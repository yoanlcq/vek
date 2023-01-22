//! Generic 2D-3D math swiss army knife for game engines, with SIMD support and focus on convenience.
//!
//! See [the wiki](https://github.com/yoanlcq/vek/wiki) for an overview, FAQ, guides, and other info.
//!
//! Issues and bug reports are very welcome!
//!
//! # Cargo features
//!
//! - `vec8`, `vec16`, `vec32`, `vec64`, `rgba`, `rgb`, `uvw`, `uv`
//!   Enable these types.
//!   Other types are always enabled for the sake of doc-tests.  
//! - `repr_simd` enables Nightly Rust's `repr_simd` and `simd_ffi` features, and unlock
//!   SIMD versions of all appropriate types (though `repr_simd` modules).
//!   On Stable, this feature has no effect.
//! - `serde` makes vectors and matrices derive `Serialize` and `Deserialize`.
//! - `image` makes color vectors implement the `Pixel` trait from the `image` crate.
//! - `mint` enables conversion to the `mint` crate's types.
//!   `mint` is an interoperability layer for math libraries.
//!
//! # `#![no_std]`
//! This crate is `#![no_std]`.

#![no_std]
#![doc(
    test(attr(deny(warnings))),
    html_root_url = "https://docs.rs/vek/0.15.10",
    //html_logo_url = "https://yoanlcq.github.io/vek/logo.png",
    //html_favicon_url = "https://yoanlcq.github.io/vek/favicon.ico",
)]
#![warn(missing_docs)]
#![allow(stable_features)]
#![deny(unconditional_recursion)]
//#![deny(warnings)]
//#![allow(unknown_lints)]
//#![deny(incoherent_fundamental_impls)]
#![cfg_attr(all(nightly, feature = "clippy"), feature(plugin))]
#![cfg_attr(all(nightly, feature = "clippy"), plugin(clippy))]
//#![cfg_attr(all(nightly, feature="repr_simd" ), feature(cfg_target_feature))]
#![cfg_attr(all(nightly, feature = "repr_simd"), feature(repr_simd, simd_ffi))]
#![cfg_attr(all(nightly, feature = "platform_intrinsics"), feature(platform_intrinsics))]
//#![cfg_attr(feature="repr_simd", allow(improper_ctypes)]
//#![cfg_attr(feature="repr_simd", feature(link_llvm_intrinsics)]
#![cfg_attr(all(nightly, test), feature(test))]

// See https://github.com/yoanlcq/vek/pull/84
// Rust 1.59 was complaining: "error: recursion limit reached while expanding reduce_fn! in src/vec.rs:47:69"
#![recursion_limit = "256"]

extern crate core as std;

#[cfg(test)]
mod vtest;

#[cfg(feature = "serde")]
#[macro_use]
pub extern crate serde;

#[cfg(feature = "mint")]
pub extern crate mint;

#[cfg(feature = "bytemuck")]
pub extern crate bytemuck;

#[cfg(feature = "az")]
pub extern crate az;

pub extern crate num_integer;
pub extern crate num_traits;

// NOTE: Allow unused imports here, because usage depends on which features are enabled.
#[allow(unused_imports)]
#[macro_use]
pub extern crate approx;

#[cfg(feature = "platform_intrinsics")]
mod simd_llvm;
// ^ Please do not make this module public; we don't want people to use it, because it could change as the SIMD infrastructure evolves.

pub mod ops;
pub use crate::ops::*;
pub mod vec;
pub use crate::vec::*;
pub mod mat;
pub use crate::mat::*;
pub mod quaternion;
pub use crate::quaternion::*;
pub mod transition;
pub use crate::transition::*;
pub mod transform;
pub use crate::transform::*;
pub mod bezier;
pub use crate::bezier::*;
pub mod geom;
pub use crate::geom::*;
pub mod simd_traits;
pub use crate::simd_traits::*;

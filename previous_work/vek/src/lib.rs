//! Generic linear algebra focused on computer graphics and intent.
//!
//! It does not try to be extensive (e.g rect primitives don't ship with a physics engine.)
//!
//! DO NOT USE (yet). This is very much a work-in progress, breaking changes happen all the time on a whim.
//!
//! TODO have an FAQ which explains the trade-off of this lib and the repr_c/repr_simd fiasco

// Plans for next version :
// - Have free functions (e.g dot(u,v))
//   - dot(u,v) (including quaternions)
//   - normalize()
//   - distance()
//   - reflect()
//   - refract()
//   - face_forward()
//   - cross()

// TODO tests (+tests on fixed-point numbers and bignums)
// TODO investigate splitting into multiple crates
// TODO seriously investigate (and benchmark) column-major vs row-major order benefits
// TODO have a story for using on non-Nightly channels
// TODO: Implement correctness check for FrustumPlanes (i.e left <= right, etc)
// TODO index vectors by range
// TODO angle between two vectors
// TODO put must_use where relevant (seems to work on functions)
// TODO tests
// TODO benchmarks
// TODO doc
// TODO provide efficient functions for AoS and SoA
// TODO serde


#![doc(html_root_url = "https://docs.rs/vek/0.1.0")]
//#![deny(missing_docs)]
#![no_std]
#![doc(test(attr(deny(warnings))))]
#![feature(test)]
#![feature(repr_simd)]
//#![feature(i128, i128_type)]
#![feature(repr_align, attr_literals)]

extern crate test;

pub mod color_component;
pub use color_component::*;
pub mod clamp;
pub use clamp::*;
pub mod wrap;
pub use wrap::*;
pub mod lerp;
pub use lerp::*;
pub mod tween;
pub use tween::*;
pub mod quaternion;
pub use quaternion::*;
pub mod geom;
pub use geom::*;
pub mod mat;
pub use mat::*;
pub mod vec;
pub use vec::*;
pub mod bezier;
pub use bezier::*;

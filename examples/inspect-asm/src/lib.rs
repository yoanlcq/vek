// Check asm with:
//     cargo rustc --release -- -C "llvm-args=-x86-asm-syntax=intel" --emit=asm
//     
// Then find the asm file in target/release/deps.

extern crate vek;

use vek::vec::*;

#[no_mangle] pub fn v4_f32_zero() -> Vec4<f32> { Vec4::zero() }
#[no_mangle] pub fn v4_f32_one () -> Vec4<f32> { Vec4::one()  }
#[no_mangle] pub fn v4_i32_zero() -> Vec4<i32> { Vec4::zero() }
#[no_mangle] pub fn v4_i32_one () -> Vec4<i32> { Vec4::one()  }

#[no_mangle] pub extern "C" fn v2_f32_dot_reprc (a: Vec2<f32>, b: Vec2<f32>) -> f32 { a.dot(b)  }
#[no_mangle] pub extern "C" fn v2_f32_dot_simd (a: repr_simd::Vec2<f32>, b: repr_simd::Vec2<f32>) -> f32 { a.dot(b)  }
#[no_mangle] pub extern "C" fn v2_f32_dot_reprc_handmade (a: Vec2<f32>, b: Vec2<f32>) -> f32 { a.x * b.x + a.y * b.y  }


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


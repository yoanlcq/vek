use vek::vec::repr_simd::Vec4;

const MIX: Vec4<f32> = Vec4::new(0.0f32, 0.0f32, 0.0f32, 0.0f32);
const MAX: Vec4<f32> = Vec4::new(1.0f32, 1.0f32, 1.0f32, 1.0f32);

/// Produces following error when compiled
///
/// error[E0511]: invalid monomorphization of `simd_reduce_all` intrinsic: unsupported simd_reduce_all from `vek::vec::repr_simd::Vec4<bool>` with element `bool` to `bool`
///     --> /mnt/nfs/marcel/code/rust/vek2/src/vec.rs:3402:5
///      |
/// 3402 |     vec_impl_all_vecs!{simd #[repr(simd)] c #[repr(C)] repr_simd}
///      |
///      = note: this error originates in the macro `vec_impl_vec` (in Nightly builds, run with -Z macro-backtrace for more info)
fn failing() {
    assert!(MIX.partial_cmple(&MAX).reduce_and());
}

#[allow(dead_code)]
fn working() {
    println!("This example is deactivated, change main() to call `failing`");
    assert!(MIX.partial_cmple_pbv(MAX).reduce_and());
}

/// use `cargo build` with `failing` to see the llvm error, its optimized away if not called
fn main() {
    working();
}

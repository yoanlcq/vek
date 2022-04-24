use vek::vec::repr_simd::Vec4;

fn main() {
    assert!(Vec4::broadcast(true).reduce_and());
    assert!(Vec4::broadcast(true).reduce_or());
    assert!(Vec4::broadcast(1i32).reduce_and());
    assert!(Vec4::broadcast(1i32).reduce_or());
    assert!(Vec4::broadcast(0f32).partial_cmplt(&Vec4::broadcast(1f32)) != Vec4::broadcast(false));
    assert!(Vec4::broadcast(0f32).partial_cmpgt(&Vec4::broadcast(1f32)) == Vec4::broadcast(false));
    assert!(Vec4::broadcast(0f32).partial_cmplt_simd(Vec4::broadcast(1f32)) != Vec4::zero());
    assert!(Vec4::broadcast(0f32).partial_cmpgt_simd(Vec4::broadcast(1f32)) == Vec4::zero());
}
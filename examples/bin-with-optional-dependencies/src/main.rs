extern crate vek;

fn test_bytemuck() {
    println!("---- test bytemuck ----");
    let v = vek::vec::repr_simd::Vec3::<u32>::iota();
    let vslice = &[v];
    let data: &[u8] = vek::bytemuck::cast_slice(vslice);
    println!("vec : {}", v);
    println!("size: {}", std::mem::size_of_val(vslice));
    println!("data: {:?}", data);
    println!("---- end test bytemuck ----");
}

fn main() {
    test_bytemuck();
}

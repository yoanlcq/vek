//! TODO: move this into two GitHub issues
//!
//! # TO-DO list and feature set
//!
//! Don't miss the `Won't implement` section at the end.
//!
//! **IMPORTANT**: No feature must be marked as implemented until it
//! has been documented, tested, benchmarked, and assembly-checked
//! (i.e there are test source files with `#[no_mangle]` functions which
//! purpose is to inspect the assembly output of typical code).
//! Of course, tests can (and should) be in the documentation itself as examples.
//!
//! # Crate-related
//! - Exhaustive Cargo.toml for `crates.io`
//! - Clippy support
//! - Serde support
//! - Prove its usability on Stable
//! - Add Continuous Integration (e.g Travis CI)
//!
//! # General
//! - Tests prove that the memory alignment of vectors and matrices is correct;
//! - Convenient public exports (e.g `vek::Mat4` resolves to `vek::<repr_best>::column_major::Mat4` 
//!   where `repr_best` is `repr_simd` when possible, or `repr_c` otherwise)
//! - Implementation macros are public such that anybody can use them to make their types implement
//!   functions found in this crate painlessly;
//! - All types are implemented for bigints with the `num-bigint` crate;
//! - All types are implemented for fixed-point numbers with the `fpa` crate;
//! - All types are implemented for fixed-point numbers with the `fix` crate;
//! - All types are shown to work on ARM or any target other than x86;
//!
//! # Free functions
//!
//! The convention for naming them is this:
//!
//! `<cpu>::<featureset>::<M><T>_<operation>`
//!
//! Where `featureset` is an underscore-separated list of required CPU features,
//! roughly sorted for biggest to smallest.
//!
//! For instance, if some operation on x86 requires `AVX512` and `FMA`, then the
//! module would be `x86::avx512_fma`.
//!
//! `T` is the element type. If unspecified, matches any type.  
//! `M` is the matrix type. It can be :
//!
//! - `vec`: Any vector type.
//! - `vecN`: A vector of dimension N.
//! - `mat`: Any square matrix type.
//! - `matN`: A square NxN matrix.
//! - `rows`: Any square row-major matrix type.
//! - `cols`: Any square column-major matrix type.
//! - `rowsN`: A square NxN row-major matrix type.
//! - `colsN`: A square NxN column-major matrix type.
//!
//! ## gen (general implementations)
//! - `mat_transposed()`
//!
//! ## x86
//! - `sse::mat4f32_transposed(Mat4<f32>)`
//! - `sse2::mat4i32_transposed(Mat4<i32>)`
//! - `sse2::mat4u32_transposed(Mat4<u32>)`
//! - `sse4_1::vec4f32_dot(Vec4<f32>, Vec4<f32>)`
//!
//! ## arm
//! *None yet*
//!
//!
//! # Implementation items
//!
//! ## All vector types
//! - `Display`
//! - `FromStr`
//! - `iota`
//! - `broadcast`
//! - `is_broadcast`
//! - `zero`
//! - `is_zero`
//! - `one`
//! - `is_one`
//! - `is_any_negative`
//! - `is_all_positive`
//! - `reduce_sum`
//! - `reduce_product`
//! - `reduce_min`
//! - `reduce_max`
//! - `reduce_average`
//! - `fmadd`
//! - `convert (Closure)`
//! - `cast (NumCast)`
//! - `into_array`
//! - `as_slice`
//! - `as_mut_slice`
//! - `as_ptr`
//! - `as_mut_ptr`
//! - `AsRef<[T; N]>`
//! - `AsMut<[T; N]>`
//! - `Borrow<[T; N]>`
//! - `BorrowMut<[T; N]>`
//! - `Index<usize>`
//! - `Index<RangeFrom<usize>>`
//! - `Index<RangeTo<usize>>`
//! - `Index<RangeFull<usize>>`
//! - `iter`
//! - `iter_mut`
//! - `IntoIterator`
//! - `ExactSizeIterator`
//! - `FromIterator`
//! - `lerp(Self)`
//! - `lerp(scalar)`
//! - `wrap(Self)`
//! - `wrap(scalar)`
//! - `cmpeq`
//! - `cmpge`
//! - `cmpgt`
//! - `cmple`
//! - `cmplt`
//! - `load_nontemporal` (`_mm_stream_load_si128`)
//! - `store_nontemporal` (`_mm_stream_ps`)
//! - `_mm_sqrt_ps`
//! - `_mm_rsqrt_ps`
//! - `_mm_hadd_ps ???`
//! - `_mm_ceil_ps`
//! - `_mm_floor_ps`
//! - `_mm_round_ps(mode)`
//!
//! ## Non-color vectors
//! - `magnitude`
//! - `magnitude_squared`
//! - `distance`
//! - `distance_squared`
//! - `reflect`
//! - `refract`
//! - `face_forward`
//! - `dot` (possibly with `_mm_dp_ps`)
//! - `angle`
//! - `normalized`
//!
//! ## Vec2, Xy
//! - `rotated_z`
//! - `unit_x`
//! - `unit_y`
//! - `left`
//! - `up`
//! - `down`
//! - `right`
//!
//! ## Vec3, Xyz
//! - `unit_z`
//! - `forward_lh`
//! - `forward_rh`
//! - `back_lh`
//! - `back_rh`
//!
//! ## Vec3, Xyz
//! - `new_point(x,y)`
//! - `new_direction(x,y)`
//! - `point(v2)`
//! - `direction(v2)`
//!
//! ## Vec4, Xyzw
//! - `new_point(x,y,z)`
//! - `new_direction(x,y,z)`
//! - `point(v3)`
//! - `direction(v3)`
//!
//! ## Non-color vectors of dimension 3
//! - `cross`
//!
//! ## Rgb, Rgba
//! - `black white red green blue cyan yellow magenta gray(val)`
//!
//! ## Rgba
//! - `new_opaque`
//! - `new_transparent`
//! - `opaque`
//! - `transparent`
//! - `translucent`
//!
//! ## All matrix types
//! - `Display`
//! - `FromStr` (detailed format in documentation)
//! - `Add<M>, Sub<M>, Rem<M>, Neg<M>, (+ ... Assign, + over references)`
//! - `Add<T>, Sub<T>, Rem<T>, (+ ... Assign, + over references)`
//! - `pow_u`
//! - `mul_memberwise`
//! - `row_count, col_count`
//! - `cast (NumCast)`
//! - `convert (Closure)`
//! - `is_invertible`
//! - `inverted -> Option<Self>`
//! - `inverted_unchecked -> Self` (not `unsafe`, but debug_assert!()s)
//! - `transposed`
//! - `transpose_self(&mut self)`
//! - `determinant`
//! - `is_diagonal`
//! - `is_symmetric`
//! - `zero`
//! - `is_zero`
//! - `identity`
//! - `is_identity`
//! - `broadcast_diagonal`
//! - `is_uniform_diagonal`
//! - `from_diagonal(VecN)` (but `broadcast_diagonal` is preferred)
//! - `diagonal` (returns the diagonal vector)
//! - `trace`
//!
//! ## f32 matrix types
//! - `as_gl_uniform_params(&self) -> (GLboolean, *const GLfloat)`
//!
//! ## RowsN
//! - `Mul<Self, Output=Self>`, the most efficient
//! - `Mul<ColsN,Output=ColsN>`
//!
//! ## ColsN
//! - `Mul<Self, Output=Self>`, the most efficient
//! - `Mul<RowsN,Output=RowsN>`
//!
//! ## Mat2
//! - `From<Mat3>`
//! - `From<Mat4>`
//! - `translation_x` (warn that when transforming a vector, its last component should be set to 1.)
//! - `translated_x` (fast because it's only a few adds)
//! - `rotation_z`
//! - `rotated_z` (convenience)
//! - `scaling_xy`
//! - `scaling_xy_uniform`
//! - `scaled_xy` (fast)
//! - `scaled_xy_uniform` (fast)
//! - `shearing_x`
//! - `shearing_y`
//! - `shearing_xy`
//! - `symmetry_x`
//! - `picking_region` (GLM's `pickMatrix()`)
//! - `into_vec4`
//!
//! ## Mat3
//! - `From<Mat2>`
//! - `From<Mat4>`
//! - `From<Quaternion>`
//! - `<all ops from Mat2>`
//! - `translation_xy` (warn that when transforming a vector, its last component should be set to 1.)
//! - `translated_xy` (fast because it's only a few adds)
//! - `rotation_x`
//! - `rotation_y`
//! - `rotation_xyz`
//! - `scaling_xyz`
//! - `scaling_xyz_uniform`
//! - `rotated_x` (convenience)
//! - `rotated_y` (convenience)
//! - `rotated_xyz` (convenience)
//! - `scaled_xyz` (fast)
//! - `scaled_xyz_uniform` (fast)
//! - `shearing_z`
//! - `shearing_xyz`
//! - `symmetry_y`
//! - `symmetry_xy`
//! - `look_at`
//! - `rotation_between_vectors`
//!
//! ## Mat4
//! - `From<Mat2>`
//! - `From<Mat3>`
//! - `<all ops from Mat3, including From<Quaternion>>`
//! - `orthonormalized`
//! - `translation_xyz` (warn that when transforming a vector, its last component should be set to 1.)
//! - `translated_xyz` (fast because it's only a few adds)
//! - `translate_xyz_in_place`
//! - `trs` (translate-rotate-scale (in which order?))
//! - `symmetry_z`
//! - `symmetry_xz`
//! - `symmetry_yz`
//! - `symmetry_xyz`
//! - `frustum[depthrange|gl|d3d|lh|rh]`
//! - `orthographic[depthrange|gl|d3d|lh|rh]`
//! - `perspective_[depthrange|gl|d3d|lh|rh]`
//! - `perspective_fov_[depthrange|gl|d3d|lh|rh]`
//! - `infinite_perspective_[depthrange|gl|d3d|lh|rh]`
//! - `tweaked_infinite_perspective_[depthrange|gl|d3d|lh|rh]`
//! - `screen_to_world`
//! - `world_to_screen`
//! - `into_vec16`
//!
//! ## Quaternions
//! - `from_mat3()`
//! - `from_xyzw(x,y,z,w)`
//! - `from_scalar_and_vector(s,v)`
//! - `from_arc(v3,v3)`
//! - `rotation_x`
//! - `rotation_y`
//! - `rotation_z`
//! - `rotation_xyz(axis, angle)`
//! - `rotated_x` (convenience)
//! - `rotated_y` (convenience)
//! - `rotated_z` (convenience)
//! - `rotated_xyz(axis, angle)` (convenience)
//! - `conjugate`
//! - `zero`
//! - `is_zero`
//! - `identity`
//! - `is_identity`
//! - `look_at`
//! - `lerp`
//! - `nlerp`
//! - `slerp`
//!
//!
//! # Won't implement
//!
//! Any iterator over matrices. Iterate over the public member instead.  
//! Transpose the matrix if needed.
//!
//! ***
//!
//! `FromIterator` for matrices. Collect the public member instead.
//!
//! ***
//!
//! Indexing matrices directly.  
//! Rationale :
//!
//! Most people are tempted to write `m[i][j]` when `i` and `j` are
//! known at compile-time, and might not realize that in Rust this
//! semantically implies bounds checking at runtime.  
//! 
//! Preventing them from being able to do this has these advantages:  
//! First, they'll consider writing e.g `(m.rows.1).3` or `(m.cols.3).1`
//! instead, which is not only checked at compile-time, but also
//! explicit and ensures correctness.
//! If the user suddenly decides to switch layouts, they'll get helpful
//! compiler errors because the matrix's public member has changed, and
//! therefore the meaning of indexing is flipped and requires attention.  
//! We wouldn't have this "correctness check" if writing `m[i][j]` was allowed.
//!
//! If one still wants dynamic indexing, they can write `m.rows[i][j]` or
//! `m.cols[j][i]`.
//!
//! ***
//!
//! Divide matrix by matrix, because it is too confusing (and not always possible).
//! Explicitly mutiply by the inverse (if possible) instead.
//!
//! ***
//!
//! `swap_*` functions for matrices. People can use `mem::swap()` on members
//! directly, bypassing bounds checking.
//!
//! ***
//!
//! `as_ptr()` or `as_mut_ptr()`, because it's important to be explicit about
//! giving an array of rows or columns. Explicitly turn the public member
//! into a pointer instead.  
//! `as_gl_uniform_params()` is the only exception.
//!
//! ***
//!
//! `invert_orthogonal()` or `invert_rotation()`.
//! It's better to explicitly call `transpose()` to invert a matrix that is known to be orthogonal.
//!
//! ***
//!
//! `Rad` or `Deg` newtypes. It's unhandy and worthless - what do you want to check
//! for ? 
//! Also floating-point types already have `to_radians()` and `to_degrees()` in Rust.
//! Radians are used because they are the only sane angular unit. If you want to support 
//! degrees so bad, then write your own wrappers.
//!
//! ***
//!
//! Indexing on Quaternions. Convert them to a `Xyzw` first instead.
//!
//! ***
//!
//! Non-square matrices. It's a bit like EBCDIC, it exists but nobody actually uses it
//! (in the context `vek` would be used, at least).
//! The functionality can be emulated by using higher-order square matrix types and setting
//! appropriate members to zero.
//! If you're concerned about the space it takes in memory, don't forget that you can simply
//! store vectors of vectors and convert them on-the-fly to square matrices, as needed.

//! This module is still a work-in-progress.

// TODO document why euler angles are not supported

use num_traits::{Zero, One, Float};
use core::ops::*;
use core::iter::Sum;
use ops::*;

// TODO:
// is_zero
// is_identity
// from_arc(v3,v3)
// Mul against all (non-color) vector types , by ref
// look_at

macro_rules! impl_mul_by_vec {
    ($Vec3:ident $Vec4:ident) => {
        impl<T: Float + Sum> Mul<$Vec3<T>> for Quaternion<T> {
            type Output = $Vec3<T>;
            fn mul(self, rhs: $Vec3<T>) -> Self::Output {
                let $Vec3 { x, y, z } = rhs;
                let v = Self { x, y, z, w: T::zero() }; // XXX: Is it correct to set w to zero here ??
                let r = v * self.conjugate().normalized();
                (self * r).into()
            }
        }
        impl<T: Float + Sum> Mul<$Vec4<T>> for Quaternion<T> {
            type Output = $Vec4<T>;
            fn mul(self, rhs: $Vec4<T>) -> Self::Output {
                let $Vec4 { x, y, z, .. } = rhs;
                let v = Self { x, y, z, w: T::zero() }; // XXX: Is it correct to set w to zero here ??
                let r = v * self.conjugate().normalized();
                (self * r).into()
            }
        }
    };
}

macro_rules! quaternion_vec3_vec4 {
    ($Vec3:ident $Vec4:ident) => {

        impl_mul_by_vec!{$Vec3 $Vec4}

        impl<T> From<$Vec4<T>> for Quaternion<T> {
            fn from(v: $Vec4<T>) -> Self {
                let $Vec4 { x, y, z, w } = v;
                Self { x, y, z, w }
            }
        }
        impl<T> From<Quaternion<T>> for $Vec4<T> {
            fn from(v: Quaternion<T>) -> Self {
                let Quaternion { x, y, z, w } = v;
                Self { x, y, z, w }
            }
        }
        impl<T> From<Quaternion<T>> for $Vec3<T> {
            fn from(v: Quaternion<T>) -> Self {
                let Quaternion { x, y, z, .. } = v;
                Self { x, y, z }
            }
        }
    };
}

macro_rules! quaternion_complete_mod {
    ($mod:ident #[$attrs:meta]) => {

        use vec::$mod::*;

        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        #[$attrs]
        pub struct Quaternion<T> { pub x: T, pub y: T, pub z: T, pub w: T }

        impl<T: Zero + One> Default for Quaternion<T> {
            fn default() -> Self {
                Self::identity()
            }
        }

        impl<T> Quaternion<T> {
            pub fn from_xyzw(x: T, y: T, z: T, w: T) -> Self {
                Self { x, y, z, w }
            }
            pub fn from_scalar_and_vec3<V: Into<Vec3<T>>>(s: T, v: V) -> Self {
                let Vec3 { x, y, z } = v.into();
                Self { x, y, z, w: s }
            }
            pub fn zero() -> Self where T: Zero {
                Self { 
                    x: T::zero(),
                    y: T::zero(),
                    z: T::zero(),
                    w: T::zero(),
                }
            }
            pub fn identity() -> Self where T: Zero + One {
                Self { 
                    x: T::zero(),
                    y: T::zero(),
                    z: T::zero(),
                    w: T::one(),
                }
            }
            pub fn conjugate(self) -> Self where T: Neg<Output=T> {
                Self {
                    x: -self.x,
                    y: -self.y,
                    z: -self.z,
                    w:  self.w,
                }
            }
            // inner_product
            pub fn dot(self, q: Self) -> T 
                where T: Copy + Sum + Mul<Output=T>
            {
                self.into_vec4().dot(q.into_vec4())
            }
            pub fn normalized(self) -> Self where T: Float + Sum {
                self.into_vec4().normalized().into()
            }

            pub fn rotation_3d<V: Into<Vec3<T>>>(angle_radians: T, axis: V) -> Self
                where T: Float
            {
                let axis = axis.into();
                let two = T::one() + T::one();
                let v = axis * (angle_radians/two).sin();
                let Vec3 { x, y, z } = v;
                let w = (angle_radians/two).cos();
                Self { x, y, z, w }
            }
            pub fn rotation_x(angle_radians: T) -> Self where T: Float + Sum {
                Self::rotation_3d(angle_radians, Vec3::unit_x())
            }
            pub fn rotation_y(angle_radians: T) -> Self where T: Float + Sum {
                Self::rotation_3d(angle_radians, Vec3::unit_y())
            }
            pub fn rotation_z(angle_radians: T) -> Self where T: Float + Sum {
                Self::rotation_3d(angle_radians, Vec3::unit_z())
            }
            pub fn rotated_3d<V: Into<Vec3<T>>>(self, angle_radians: T, axis: V) -> Self where T: Float + Sum {
                Self::rotation_3d(angle_radians, axis) * self
            }
            pub fn rotated_x(self, angle_radians: T) -> Self where T: Float + Sum {
                Self::rotation_x(angle_radians) * self
            }
            pub fn rotated_y(self, angle_radians: T) -> Self where T: Float + Sum {
                Self::rotation_y(angle_radians) * self
            }
            pub fn rotated_z(self, angle_radians: T) -> Self where T: Float + Sum {
                Self::rotation_z(angle_radians) * self
            }
            pub fn rotate_3d<V: Into<Vec3<T>>>(&mut self, angle_radians: T, axis: V) where T: Float + Sum {
                *self = self.rotated_3d(angle_radians, axis);
            }
            pub fn rotate_x(&mut self, angle_radians: T) where T: Float + Sum {
                *self = self.rotated_x(angle_radians);
            }
            pub fn rotate_y(&mut self, angle_radians: T) where T: Float + Sum {
                *self = self.rotated_y(angle_radians);
            }
            pub fn rotate_z(&mut self, angle_radians: T) where T: Float + Sum {
                *self = self.rotated_z(angle_radians);
            }

            /*
            pub fn into_axis_angle(self) -> (T, Vec3<T>) {
                // Q57
            }
             */

            pub fn into_vec4(self) -> Vec4<T> {
                self.into()
            }
            pub fn from_vec4(v: Vec4<T>) -> Self {
                v.into()
            }
            pub fn into_vec3(self) -> Vec3<T> {
                self.into()
            }
        }
        
        impl<T> Lerp<T> for Quaternion<T>
            where T: Copy + Zero + One + Mul<Output=T> + Sub<Output=T> + Add<Output=T> + MulAdd<T,T,Output=T> + Clamp
        {
            fn lerp_unclamped_precise(from: Self, to: Self, factor: T) -> Self {
                Vec4::lerp_unclamped_precise(from.into(), to.into(), factor).into()
            }
            fn lerp_unclamped(from: Self, to: Self, factor: T) -> Self {
                Vec4::lerp_unclamped(from.into(), to.into(), factor).into()
            }
        }
        impl<T> Quaternion<T>
            where T: Copy + Zero + One + Mul<Output=T> + Sub<Output=T> + Add<Output=T> + MulAdd<T,T,Output=T> + Clamp + Sum + Float
        {
            pub fn nlerp(from: Self, to: Self, factor: T) -> Self {
                Self::lerp(from, to, factor).normalized()
            }
            pub fn nlerp_precise(from: Self, to: Self, factor: T) -> Self {
                Self::lerp_precise(from, to, factor).normalized()
            }
            pub fn nlerp_unclamped(from: Self, to: Self, factor: T) -> Self {
                Self::lerp_unclamped(from, to, factor).normalized()
            }
            pub fn nlerp_unclamped_precise(from: Self, to: Self, factor: T) -> Self {
                Self::lerp_unclamped_precise(from, to, factor).normalized()
            }
            // From GLM's source code.
            pub fn slerp_unclamped(from: Self, mut to: Self, factor: T) -> Self 
                where T: Neg<Output=T>
            {
                let mut cos_theta = from.dot(to);
                // If cosTheta < 0, the interpolation will take the long way around the sphere. 
                // To fix this, one quat must be negated.
                if cos_theta < T::zero() {
                    to = -to;
                    cos_theta = -cos_theta;
                }

                // Perform a linear interpolation when cosTheta is close to 1 to avoid side effect of sin(angle) becoming a zero denominator
                if cos_theta > T::one() - T::epsilon() {
                    return Vec4::lerp(from.into(), to.into(), factor).into();
                }
                let angle = cos_theta.acos();
                (from * ((T::one() - factor) * angle).sin() + to * (factor * angle).sin()) / angle.sin()
            }
            pub fn slerp(from: Self, to: Self, factor: T) -> Self {
                Self::slerp_unclamped(from, to, factor.clamped01())
            }
        }


        impl<T> Neg for Quaternion<T> where T: Neg<Output=T> {
            type Output = Self;
            fn neg(self) -> Self::Output {
                Self {
                    x: -self.x,
                    y: -self.y,
                    z: -self.z,
                    w: -self.w,
                }
            }
        }
        impl<T> Div<T> for Quaternion<T> where T: Copy + Div<Output=T> {
            type Output = Self;
            fn div(self, rhs: T) -> Self::Output {
                Self {
                    x: self.x / rhs,
                    y: self.y / rhs,
                    z: self.z / rhs,
                    w: self.w / rhs,
                }
            }
        }


        impl<T> Add for Quaternion<T> where T: Add<Output=T> {
            type Output = Self;
            fn add(self, rhs: Self) -> Self::Output {
                Self {
                    x: self.x + rhs.x,
                    y: self.y + rhs.y,
                    z: self.z + rhs.z,
                    w: self.w + rhs.w,
                }
            }
        }
        impl<T> Sub for Quaternion<T> where T: Sub<Output=T> {
            type Output = Self;
            fn sub(self, rhs: Self) -> Self::Output {
                Self {
                    x: self.x - rhs.x,
                    y: self.y - rhs.y,
                    z: self.z - rhs.z,
                    w: self.w - rhs.w,
                }
            }
        }

        impl<T> Mul for Quaternion<T> 
            where T: Copy + Mul<Output=T> + Sub<Output=T> + Zero + Sum
        {
            type Output = Self;
            fn mul(self, rhs: Self) -> Self::Output {
                let p = self.into_vec3();
                let q =  rhs.into_vec3();
                let r = p.cross(q);
                let w = p * rhs.w;
                let r = r + w;
                let w = q * self.w;
                let r = r + w;
                Self {
                    x: r.x,
                    y: r.y,
                    z: r.z,
                    w: self.w * rhs.w - self.dot(rhs)
                }
            }
        }

        impl<T> Mul<T> for Quaternion<T>
        where T: Mul<Output=T> + Copy
        {
            type Output = Self;
            fn mul(self, rhs: T) -> Self::Output {
                Self {
                    x: self.x * rhs,
                    y: self.y * rhs,
                    z: self.z * rhs,
                    w: self.w * rhs,
                }
            }
        }

        /*
        // NOTE: Only for orthogonal matrices
        static inline void mat4o_mul_quat(mat4 R, mat4 M, quat q)
        {
            quat_mul_vec3(R[0], q, M[0]);
            quat_mul_vec3(R[1], q, M[1]);
            quat_mul_vec3(R[2], q, M[2]);

            R[3][0] = R[3][1] = R[3][2] = 0.f;
            R[3][3] = 1.f;
        }
        */
    }
}


#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    use super::*;
    use super::super::vec::repr_c::{Vec3 as CVec3, Vec4 as CVec4};
    quaternion_complete_mod!(repr_simd #[repr(packed,simd)]);
    quaternion_vec3_vec4!(Vec3 Vec4);
    quaternion_vec3_vec4!(CVec3 CVec4);
}
pub mod repr_c {
    use super::*;
    quaternion_complete_mod!(repr_c #[repr(packed,C)]);
    quaternion_vec3_vec4!(Vec3 Vec4);

    #[cfg(all(nightly, feature="repr_simd"))]
    use super::super::vec::repr_simd::{Vec3 as SimdVec3, Vec4 as SimdVec4};
    #[cfg(all(nightly, feature="repr_simd"))]
    quaternion_vec3_vec4!(SimdVec3 SimdVec4);
}
pub use self::repr_c::*;

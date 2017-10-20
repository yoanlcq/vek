// TODO document why euler angles are not supported

use num_traits::{Zero, One, Float};
use core::ops::*;
use core::iter::Sum;

macro_rules! quaternion_complete_mod {
    ($mod:ident #[$attrs:meta] $($MulVec:ident)+) => {

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
            pub fn new(x: T, y: T, z: T, w: T) -> Self {
                Self { x, y, z, w }
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
                where T: Clone + Sum + Mul<Output=T>
            {
                self.into_xyzw().dot(q.into_xyzw())
            }
            pub fn normalized(self) -> Self where T: Float + Sum {
                Self::from_xyzw(self.into_xyzw().normalized())
            }

            pub fn rotation(angle_radians: T, axis: Xyz<T>) -> Self
                where T: Float
            {
                let two = T::one() + T::one();
                let v = axis * (angle_radians/two).sin();
                let Xyz { x, y, z } = v;
                let w = (angle_radians/two).cos();
                Self { x, y, z, w }
            }
            /*
            pub fn into_axis_angle(self) -> (T, Vec3<T>) {
                // Q57
            }
             */

            pub fn into_xyzw(self) -> Xyzw<T> {
                self.into()
            }
            pub fn from_xyzw(v: Xyzw<T>) -> Self {
                Self::from(v)
            }
            pub fn into_xyz(self) -> Xyz<T> {
                self.into()
            }
        }

        impl<T> From<Xyzw<T>> for Quaternion<T> {
            fn from(v: Xyzw<T>) -> Self {
                let Xyzw { x, y, z, w } = v;
                Self { x, y, z, w }
            }
        }
        impl<T> From<Quaternion<T>> for Xyzw<T> {
            fn from(v: Quaternion<T>) -> Self {
                let Quaternion { x, y, z, w } = v;
                Self { x, y, z, w }
            }
        }
        impl<T> From<Quaternion<T>> for Xyz<T> {
            fn from(v: Quaternion<T>) -> Self {
                let Quaternion { x, y, z, .. } = v;
                Self { x, y, z }
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
                let p = self.into_xyz();
                let q =  rhs.into_xyz();
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
        where T: Mul<Output=T> + Clone
        {
            type Output = Self;
            fn mul(self, rhs: T) -> Self::Output {
                Self {
                    x: self.x * rhs.clone(),
                    y: self.y * rhs.clone(),
                    z: self.z * rhs.clone(),
                    w: self.w * rhs.clone(),
                }
            }
        }

        // NOTE: The following two were supposed to use `$MulVec` but we miss
        // the required conversion functions right now.

        impl<T: Float> Mul<Xyz<T>> for Quaternion<T> 
            where T: Sum
        {
            type Output = Xyz<T>;
            fn mul(self, rhs: Xyz<T>) -> Self::Output {
                let Xyz { x, y, z } = rhs;
                let v = Self { x, y, z, w: T::zero() };

                let r = v * self.conjugate().normalized();
                (self * r).into_xyz()
            }
        }
        impl<T: Float> Mul<Xyzw<T>> for Quaternion<T> 
            where T: Sum
        {
            type Output = Xyzw<T>;
            fn mul(self, rhs: Xyzw<T>) -> Self::Output {
                let Xyzw { x, y, z, .. } = rhs;
                let v = Self { x, y, z, w: T::zero() }; // XXX: Is it correct to set w to zero here ??

                let r = v * self.conjugate().normalized();
                (self * r).into_xyzw()
            }
        }



        /*

        static inline void mat4_from_quat(mat4 M, quat q)
        {
            float a = q[3];
            float b = q[0];
            float c = q[1];
            float d = q[2];
            float a2 = a*a;
            float b2 = b*b;
            float c2 = c*c;
            float d2 = d*d;
            
            M[0][0] = a2 + b2 - c2 - d2;
            M[0][1] = 2.f*(b*c + a*d);
            M[0][2] = 2.f*(b*d - a*c);
            M[0][3] = 0.f;

            M[1][0] = 2*(b*c - a*d);
            M[1][1] = a2 - b2 + c2 - d2;
            M[1][2] = 2.f*(c*d + a*b);
            M[1][3] = 0.f;

            M[2][0] = 2.f*(b*d + a*c);
            M[2][1] = 2.f*(c*d - a*b);
            M[2][2] = a2 - b2 - c2 + d2;
            M[2][3] = 0.f;

            M[3][0] = M[3][1] = M[3][2] = 0.f;
            M[3][3] = 1.f;
        }

        // NOTE: Only for orthogonal matrices
        static inline void mat4o_mul_quat(mat4 R, mat4 M, quat q)
        {
            quat_mul_vec3(R[0], q, M[0]);
            quat_mul_vec3(R[1], q, M[1]);
            quat_mul_vec3(R[2], q, M[2]);

            R[3][0] = R[3][1] = R[3][2] = 0.f;
            R[3][3] = 1.f;
        }
        static inline void quat_from_mat4(quat q, mat4 M)
        {
            float r=0.f;
            int i;

            int perm[] = { 0, 1, 2, 0, 1 };
            int *p = perm;

            for(i = 0; i<3; i++) {
                float m = M[i][i];
                if( m < r )
                    continue;
                m = r;
                p = &perm[i];
            }

            r = sqrtf(1.f + M[p[0]][p[0]] - M[p[1]][p[1]] - M[p[2]][p[2]] );

            if(r < 1e-6) {
                q[0] = 1.f;
                q[1] = q[2] = q[3] = 0.f;
                return;
            }

            q[0] = r/2.f;
            q[1] = (M[p[0]][p[1]] - M[p[1]][p[0]])/(2.f*r);
            q[2] = (M[p[2]][p[0]] - M[p[0]][p[2]])/(2.f*r);
            q[3] = (M[p[2]][p[1]] - M[p[1]][p[2]])/(2.f*r);
        }
        */
    }
}
#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    use super::*;
    quaternion_complete_mod!(repr_simd #[repr(packed,simd)] Vec3 Xyz);
}
pub mod repr_c {
    use super::*;
    quaternion_complete_mod!(repr_c #[repr(packed,C)] Vec3 Xyz);
}
#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

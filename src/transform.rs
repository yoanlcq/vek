//! A convenient position + orientation + scale container, backed by two `Vec3` and a `Quaternion.`

macro_rules! transform_complete_mod {
    ($mod:ident) => {

        // WISH:
        // forward_rh, forward_lh, etc etc
        // look_at
        // rotate_around

        use std::ops::Add;
        use $crate::num_traits::{Zero, One, real::Real};
        use $crate::ops::*;
        use crate::vec::$mod::*;
        use crate::quaternion::$mod::*;

        /// A convenient position + orientation + scale container, backed by two `Vec3` and a `Quaternion.`
        ///
        /// It can be easily interpolated and converted to a `Mat4` of any layout.
        ///
        /// ```
        /// # extern crate vek;
        /// # #[macro_use] extern crate approx;
        /// # use vek::{Transform, Mat4, Quaternion, Vec3};
        /// # fn main() {
        /// let (p, rz, s) = (Vec3::unit_x(), 3.0_f32, 5.0_f32);
        /// let a = Mat4::scaling_3d(s).rotated_z(rz).translated_3d(p);
        /// let b = Mat4::from(Transform {
        ///     position: p, 
        ///     orientation: Quaternion::rotation_z(rz),
        ///     scale: Vec3::broadcast(s),
        /// });
        /// assert_relative_eq!(a, b);
        /// # }
        /// ```
        // Name decided by looking at this thread:
        // https://www.gamedev.net/forums/topic/611925-is-there-a-name-for-position-orientation/
        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Transform<P,O,S> {
            /// Local position.
            pub position: Vec3<P>,
            /// Local orientation; It is not named `rotation` because `rotation` denotes an
            /// operation, but not a current state.
            pub orientation: Quaternion<O>,
            /// Local scale.
            pub scale: Vec3<S>,
        }

        /// The default `Transform` has a zero position, identity orientation and unit scale.
        ///
        /// ```
        /// # use vek::{Transform, Quaternion, Vec3};
        /// let a = Transform {
        ///     position: Vec3::<f32>::zero(),
        ///     orientation: Quaternion::<f32>::identity(),
        ///     scale: Vec3::<f32>::one(),
        /// };
        /// assert_eq!(a, Transform::default());
        /// ```
        impl<P: Zero, O: Zero + One, S: One> Default for Transform<P,O,S> {
            fn default() -> Self {
                Self {
                    position: Vec3::zero(),
                    orientation: Quaternion::identity(),
                    scale: Vec3::one(),
                }
            }
        }

        /// LERP on a `Transform` is defined as LERP-ing between the positions and scales, 
        /// and performing SLERP between the orientations.
        impl<P,O,S,Factor> Lerp<Factor> for Transform<P,O,S> 
            where Factor: Copy + Into<O>,
                  P: Lerp<Factor,Output=P>,
                  S: Lerp<Factor,Output=S>,
                  O: Lerp<O,Output=O> + Real + Add<Output=O>,
        {
            type Output = Self;
            fn lerp_unclamped(a: Self, b: Self, t: Factor) -> Self {
                Transform {
                    position: Lerp::lerp_unclamped(a.position, b.position, t),
                    orientation: Slerp::slerp_unclamped(a.orientation, b.orientation, t),
                    scale: Lerp::lerp_unclamped(a.scale, b.scale, t),
                }
            }
            fn lerp_unclamped_precise(a: Self, b: Self, t: Factor) -> Self {
                Transform {
                    position: Lerp::lerp_unclamped_precise(a.position, b.position, t),
                    orientation: Slerp::slerp_unclamped(a.orientation, b.orientation, t),
                    scale: Lerp::lerp_unclamped_precise(a.scale, b.scale, t),
                }
            }
        }

        /// LERP on a `Transform` is defined as LERP-ing between the positions and scales, 
        /// and performing SLERP between the orientations.
        impl<'a,P,O,S,Factor> Lerp<Factor> for &'a Transform<P,O,S> 
            where Factor: Copy + Into<O>,
                  &'a P: Lerp<Factor,Output=P>,
                  &'a S: Lerp<Factor,Output=S>,
                  O: Lerp<O,Output=O> + Real + Add<Output=O>,
        {
            type Output = Transform<P,O,S>;
            fn lerp_unclamped(a: Self, b: Self, t: Factor) -> Self::Output {
                Transform {
                    position: Lerp::lerp_unclamped(&a.position, &b.position, t),
                    orientation: Slerp::slerp_unclamped(&a.orientation, &b.orientation, t),
                    scale: Lerp::lerp_unclamped(&a.scale, &b.scale, t),
                }
            }
            fn lerp_unclamped_precise(a: Self, b: Self, t: Factor) -> Self::Output {
                Transform {
                    position: Lerp::lerp_unclamped_precise(&a.position, &b.position, t),
                    orientation: Slerp::slerp_unclamped(&a.orientation, &b.orientation, t),
                    scale: Lerp::lerp_unclamped_precise(&a.scale, &b.scale, t),
                }
            }
        }
    }     
}         

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! `Transform` struct that uses `#[repr(simd)]` vectors and quaternions.
    transform_complete_mod!(repr_simd);
}
pub mod repr_c {
    //! `Transform` struct that uses `#[repr(C)]` vectors and quaternions.
    transform_complete_mod!(repr_c);
}
pub use self::repr_c::*;

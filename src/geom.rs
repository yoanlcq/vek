//! Common and trivial geometric primitives.
//!
//! This module is still a work-in-progress. A lot of useful operations remain to be implemented.

// WISH add useful impls to this module (inclusing basic conversions from rect to vec pairs)

// NOTE: in this module, the type parameters <P,E> usually stand for Position and Extent.

extern crate num_traits;

use self::num_traits::{/*Float, Zero*/FloatConst, One};
use std::ops::*;
//use clamp::partial_max;

macro_rules! geom_complete_mod {
    ($mod:ident) => {
        // use mat::$mod::Mat2;
        use vec::$mod::*;

        /// A `Rect` extended to 3D.
        ///
        /// This would have been named `Box`, but it was "taken" by the standard library already.
        ///
        /// You should probably use `Aabb` because it is less confusing.
        ///
        /// Rect3 is only useful when using extra precise integer coordinates where `Aabb` would only
        /// allow for representing half the possible values for the extent. 
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Rect3<P,E> {
            /// X position of the top-left-near corner.
            pub x: P,
            /// Y position of the top-left-near corner.
            pub y: P,
            /// Z position of the top-left-near corner.
            pub z: P,
            /// Width.
            pub w: E,
            /// Height, with Y axis going downwards.
            pub h: E,
            /// Depth, with Z axis going forwards.
            pub depth: E,
        }
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Aabb<P,E> {
            pub center: Vec3<P>,
            pub half_extent: Extent3<E>,
        }

        // NOTE: Only implement axis-aligned primitives (a.k.a don't go on a rampage).
        // 
        // Don't write, e.g a "Disk in 3D-space" structure, because users would rather
        // represent it with a (Disk, z, orientation) tuple or anything else that suits their particular needs.
        //
        // On the other hand, everybody agrees that a minimal "Disk" struct is a position+radius pair.
        // (even if it's just expressed as a radius with no
        // position, then fine, just use the radius as-is, without making it it a new struct).
        // 
        // Any other info, such as fill color, border thickness, etc. are just extras that users can
        // put on top (see composition over inheritance, etc).

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Disk<P,E> {
            pub center: Vec2<P>,
            pub radius: E,
        }

        impl<P,E> Disk<P,E> {
            pub fn area(self) -> E where E: Copy + FloatConst + Mul<Output=E> { 
                let r = || self.radius; 
                E::PI()*r()*r()
            }
            pub fn diameter(self) -> E where E: Copy + Add<Output=E> { 
                self.radius + self.radius
            }
            /*
            /// Returns the magnitude of the direction vector that can push or pull
            /// the other disk such that they both collide exactly.
            ///
            /// A negative value indicates that both disks collide by this amount.  
            /// Otherwise, it's the distance such that they would collide.
            pub fn disk_distance_magnitude(self, other: Self) -> Vec2<P> 
                where E: Sub<Output=E>, P: Float
            {
                Vec2::distance(self.center, other.center) - self.radius - other.radius
            }
            /// How much this disk penetrates another.
            pub fn disk_penetration_vector(self, other: Self) -> Vec2<P>
                where P: Copy, E: Copy + Zero
            {
                let len = self.disk_collision_magnitude(other);
                (self.center - other.center) * partial_max(E::zero(), len)
            }
            pub fn disk_distance_vector(self, other: Self) -> Vec2<P>
                where P: Copy + Sub<Output=P>, E: Copy 
            {
                let len = self.disk_collision_magnitude(other);
                (self.center - other.center) * len
            }
            /// Returns the direction vector such that moving `p` by it pushes it
            /// exactly outside of the disk.
            pub fn point_distance_magnitude(self, p: Vec2<P>) -> Vec2<P> {
                Vec2::distance(self.center, p) - self.radius
            }
            */
        }

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Sphere<P,E> {
            pub center: Vec3<P>,
            pub radius: E,
        }
        impl<P,E> Sphere<P,E> {
            pub fn surface_area(self) -> E where E: Copy + One + FloatConst + Add<Output=E> + Mul<Output=E> {
                let four = E::one() + E::one() + E::one() + E::one();
                let r = self.radius;
                four*E::PI()*r*r
            }
            pub fn volume(self) -> E where E: Copy + One + FloatConst + Add<Output=E> + Mul<Output=E> + Div<Output=E> {
                let four = E::one() + E::one() + E::one() + E::one();
                let three = E::one() + E::one() + E::one();
                let r = self.radius;
                (four*E::PI()*r*r*r)/three
            }
            pub fn diameter(self) -> E where E: Copy + Add<Output=E> {
                self.radius + self.radius
            }
            /*
            pub fn collision(self, _other: Self) -> Vec3<P> { unimplemented!() }
            pub fn collision_with_point(self, _p: Vec3<P>) -> Vec3<P> { unimplemented!() }
            */
        }

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Ellipsis<P,E> {
            pub center: Vec2<P>,
            pub radius: Extent2<E>,
        }
        /// Nobody can possibly use this ???
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Potato<P,E> {
            pub center: Vec3<P>,
            pub radius: Extent3<E>,
        }

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct LineSegment2<T> {
            pub a: Vec2<T>,
            pub b: Vec2<T>,
        }
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct LineSegment3<T> {
            pub a: Vec3<T>,
            pub b: Vec3<T>,
        }
    }
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    use super::*;
    geom_complete_mod!(repr_simd);
}

pub mod repr_c {
    use super::*;
    geom_complete_mod!(repr_c);
}

pub use self::repr_c::*;

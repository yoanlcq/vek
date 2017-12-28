//! Common and trivial geometric primitives.
//!
//! This module is still a work-in-progress. A lot of useful operations remain to be implemented.

// WISH add useful impls to this module (inclusing basic conversions from rect to vec pairs)

// NOTE: in this module, the type parameters <P,E> usually stand for Position and Extent.

extern crate num_traits;

use self::num_traits::{Float, FloatConst, Zero, One};
use std::ops::*;
use std::iter::Sum;
//use clamp::partial_max;

// NOTE: There's never a sane Default for this, so don't implement or derive it!!
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct FrustumPlanes<T> {
    pub left: T,
    pub right: T,
    pub bottom: T,
    pub top: T,
    pub near: T,
    pub far: T,
}


macro_rules! geom_complete_mod {
    ($mod:ident) => {

        use vec::$mod::*;

        // XXX: Beware when using code that assumes that Y points downards.
        // Luckily, our matrix functions (those that receive a viewport) do not!
        /// 2D rectangle, by a bottom-left position, and extents.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Rect<P, E> {
            /// X position of the top-left corner.
            pub x: P,
            /// Y position of the top-left corner.
            pub y: P,
            /// Width.
            pub w: E,
            /// Height, with Y axis going upwards.
            pub h: E,
        }

        geom_impl_rect_or_rect3!{
            Rect Vec2 Extent2 (x split_at_x y split_at_y) (w h)
            Aabr into_aabr
            collides_with_rect: collides_with_rect
            collides_with_aab: collides_with_aabr
            collision_vector_with_rect: collision_vector_with_rect
            collision_vector_with_aab: collision_vector_with_aabr
        }


        /// Axis-aligned Bounding Rectangle.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Aabr<T> {
            pub min: Vec2<T>,
            pub max: Vec2<T>,
        }

        geom_impl_aabr_or_aabb!{
            Aabr Vec2 (x split_at_x y split_at_y)
            Rect into_rect
            collides_with_aab: collides_with_aabr
            collision_vector_with_aab: collision_vector_with_aabr
        }


        /// A `Rect` extended to 3D.
        ///
        /// This would have been named `Box`, but it was "taken" by the standard library already.
        ///
        /// You should probably use `Aabb` because it is less confusing.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Rect3<P,E> {
            /// X position of the bottom-left-near corner.
            pub x: P,
            /// Y position of the bottom-left-near corner.
            pub y: P,
            /// Z position of the bottom-left-near corner.
            pub z: P,
            /// Width.
            pub w: E,
            /// Height, with Y axis going upwards.
            pub h: E,
            /// Depth, with Z axis going forwards.
            pub d: E,
        }

        geom_impl_rect_or_rect3!{
            Rect3 Vec3 Extent3 (x split_at_x y split_at_y z split_at_z) (w h d)
            Aabb into_aabb
            collides_with_rect: collides_with_rect3
            collides_with_aab: collides_with_aabb
            collision_vector_with_rect: collision_vector_with_rect3
            collision_vector_with_aab: collision_vector_with_aabb
        }


        /// Axis-aligned Bounding Box.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Aabb<T> {
            pub min: Vec3<T>,
            pub max: Vec3<T>,
        }

        geom_impl_aabr_or_aabb!{
            Aabb Vec3 (x split_at_x y split_at_y z split_at_z)
            Rect3 into_rect3
            collides_with_aab: collides_with_aabb
            collision_vector_with_aab: collision_vector_with_aabb
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
            pub fn circumference(self) -> E 
                where E: Copy + FloatConst + Mul<Output=E> + Add<Output=E>
            {
                (E::PI() + E::PI()) * self.radius
            }
            pub fn area(self) -> E where E: Copy + FloatConst + Mul<Output=E> { 
                let r = self.radius; 
                E::PI()*r*r
            }
        }

        geom_impl_disk_or_sphere!{
            Disk Vec2 (x y)
            Extent2 Rect Aabr aabr
            collides_with_other: collides_with_disk
            collision_vector_with_other: collision_vector_with_disk
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
        }

        geom_impl_disk_or_sphere!{
            Sphere Vec3 (x y z)
            Extent3 Rect3 Aabb aabb
            collides_with_other: collides_with_sphere
            collision_vector_with_other: collision_vector_with_sphere
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
            pub start: Vec2<T>,
            pub end: Vec2<T>,
        }
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct LineSegment3<T> {
            pub start: Vec3<T>,
            pub end: Vec3<T>,
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

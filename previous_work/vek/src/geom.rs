// WISH add useful impls to this module (inclusing basic conversions from rect to vec pairs)

// NOTE: in this module, the type parameters <P,E> usually stand for Position and Extent.

extern crate num_traits;

use self::num_traits::{/*Float, Zero*/FloatConst, One};
use core::mem;
use core::ops::*;
//use clamp::partial_max;

macro_rules! geom_complete_mod {
    ($mod:ident) => {
        // use mat::$mod::Mat2;
        use vec::$mod::*;

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Rect<P,E> {
            /// Commonly assumed to be the top-left corner, because it is the de facto standard for this kind of struct.
            pub position: Xy<P>,
            /// Extent, with Y axis going downwards.
            pub extent: Extent2<E>,
        }
        /// A `Rect` extended to 3D.
        ///
        /// This would have been named `Box`, but it was "taken" by the standard library already.
        ///
        /// You should probably use `Aabb` because it is less confusing.
        ///
        /// Rect3 is only useful when using extra precise integer coordinates where `Aabb` would only
        /// allow for representing half the possible values for the extent. 
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Rect3<P,E> {
            /// Commonly assumed to be the top-left-near corner.
            pub position: Xyz<P>,
            /// Extent, with Y axis going downwards.
            pub extent: Extent3<E>,
        }
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Aabb<P,E> {
            pub center: Xyz<P>,
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

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Disk<P,E> {
            pub center: Xy<P>,
            pub radius: E,
        }

        impl<P,E> Disk<P,E> {
            pub fn area(self) -> E where E: Clone + FloatConst + Mul<Output=E> { 
                let r = || self.radius.clone(); 
                E::PI()*r()*r()
            }
            pub fn diameter(self) -> E where E: Clone + Add<Output=E> { 
                self.radius.clone() + self.radius.clone()
            }
            /*
            /// Returns the magnitude of the direction vector that can push or pull
            /// the other disk such that they both collide exactly.
            ///
            /// A negative value indicates that both disks collide by this amount.  
            /// Otherwise, it's the distance such that they would collide.
            pub fn disk_distance_magnitude(self, other: Self) -> Xy<P> 
                where E: Sub<Output=E>, P: Float
            {
                Xy::distance(self.center, other.center) - self.radius - other.radius
            }
            /// How much this disk penetrates another.
            pub fn disk_penetration_vector(self, other: Self) -> Xy<P>
                where P: Clone, E: Clone + Zero
            {
                let len = self.clone().disk_collision_magnitude(other.clone());
                (self.center - other.center) * partial_max(E::zero(), len)
            }
            pub fn disk_distance_vector(self, other: Self) -> Xy<P>
                where P: Clone + Sub<Output=P>, E: Clone 
            {
                let len = self.clone().disk_collision_magnitude(other.clone());
                (self.center - other.center) * len
            }
            /// Returns the direction vector such that moving `p` by it pushes it
            /// exactly outside of the disk.
            pub fn point_distance_magnitude(self, p: Xy<P>) -> Xy<P> {
                Xy::distance(self.center, p) - self.radius
            }
            */
        }

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Sphere<P,E> {
            pub center: Xyz<P>,
            pub radius: E,
        }
        impl<P,E> Sphere<P,E> {
            pub fn surface_area(self) -> E where E: Clone + One + FloatConst + Add<Output=E> + Mul<Output=E> {
                let four = E::one() + E::one() + E::one() + E::one();
                let r = || self.radius.clone();
                four*E::PI()*r()*r()
            }
            pub fn volume(self) -> E where E: Clone + One + FloatConst + Add<Output=E> + Mul<Output=E> + Div<Output=E> {
                let four = E::one() + E::one() + E::one() + E::one();
                let three = E::one() + E::one() + E::one();
                let r = || self.radius.clone();
                (four/three)*E::PI()*r()*r()*r()
            }
            pub fn diameter(self) -> E where E: Clone + Add<Output=E> {
                self.radius.clone() + self.radius.clone()
            }
            /*
            pub fn collision(self, _other: Self) -> Xyz<P> { unimplemented!() }
            pub fn collision_with_point(self, _p: Xyz<P>) -> Xyz<P> { unimplemented!() }
            */
        }

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Ellipsis<P,E> {
            pub center: Xy<P>,
            pub radius: Extent2<E>,
        }
        /// Nobody can possibly use this ???
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Potato<P,E> {
            pub center: Xyz<P>,
            pub radius: Extent3<E>,
        }

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Line2<T> {
            pub a: Xy<T>,
            pub b: Xy<T>,
        }
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct Line3<T> {
            pub a: Xyz<T>,
            pub b: Xyz<T>,
        }


        impl<P,E> Rect<P,E> {
            pub fn new(x: P, y: P, w: E, h: E) -> Self {
                Self { position: Xy { x, y }, extent: Extent2 { w, h } }
            }
            pub fn into_pair(self) -> (Xy<P>, Extent2<E>) {
                ( Xy { x: self.position.x, y: self.position.y }, Extent2 { w: self.extent.w, h: self.extent.h })
            }
            pub fn to_pair(&self) -> (Xy<P>, Extent2<E>) where P: Clone, E: Clone {
                let s = self.clone();
                ( Xy { x: s.position.x, y: s.position.y }, Extent2 { w: s.extent.w, h: s.extent.h })
            }
            // Might look silly, but it's actually better then the other way around, because
            // there is less loss of information. A rect is actually a position and extent.
            // Direct acces to their components is only a shortcut.
            pub fn x(self) -> P { self.position.x }
            pub fn y(self) -> P { self.position.y }
            pub fn w(self) -> E { self.extent.w   }
            pub fn h(self) -> E { self.extent.h   }
            pub fn convert<DP,DE,PF,EF>(self, pf: PF, ef: EF) -> Rect<DP,DE>
                where PF: Fn(P) -> DP, EF: Fn(E) -> DE
            {
                let mut out: Rect<DP,DE> = unsafe { mem::uninitialized() };
                out.position = self.position.convert(pf);
                out.extent   = self.extent  .convert(ef);
                out
            }
            /*
            pub fn collision(self, _other: Self) -> Xy<P> {
                unimplemented!()    
            }
            pub fn split_v(self, _from_left: E) -> (Self, Self) {
                unimplemented!()
            }
            pub fn split_h(self, _from_top: E) -> (Self, Self) {
                unimplemented!()
            }
            pub fn split(self, _from_topleft: Extent2<E>) -> Mat2<Self> {
                unimplemented!()
            }
            */
        }

        impl<P,E> From<(Xy<P>, Extent2<E>)> for Rect<P,E> {
            fn from(t: (Xy<P>, Extent2<E>)) -> Self {
                let position = t.0;
                let extent = t.1;
                Self { position, extent }
            }
        }

        // NOTE: Don't implement Default
        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub struct FrustumPlanes<T> {
            pub left: T,
            pub right: T,
            pub bottom: T,
            pub top: T,
            pub near: T,
            pub far: T,
        }
    }
}

pub mod repr_simd {
    use super::*;
    geom_complete_mod!(repr_simd);
}

pub mod repr_c {
    use super::*;
    geom_complete_mod!(repr_c);
}

pub use self::repr_simd::*;

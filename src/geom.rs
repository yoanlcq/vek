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

// TODO:
// lerp for all shapes
// More intersections (e.g line_segment vs box, etc)

macro_rules! geom_impl_line_segment {
    ($LineSegment:ident $Vec:ident) => {
        impl<T> From<Range<$Vec<T>>> for $LineSegment<T> {
            fn from(range: Range<$Vec<T>>) -> Self {
                let Range { start, end } = range;
                Self { start, end }
            }
        }
        impl<T> $LineSegment<T> {
            pub fn into_range(self) -> Range<$Vec<T>> {
                let Self { start, end } = self;
                Range { start, end }
            }
        }
    };
}

macro_rules! geom_impl_rect_or_rect3 {
    (
        $Rect:ident $Vec:ident $Extent:ident ($(($p_s:expr) $p:ident $split_at_p:ident)+) ($($e:ident)+)
        $Aab:ident $into_aab:ident
        contains_rect: $contains_rect:ident
        contains_aab: $contains_aab:ident
        collides_with_rect: $collides_with_rect:ident
        collides_with_aab: $collides_with_aab:ident
        collision_vector_with_rect: $collision_vector_with_rect:ident
        collision_vector_with_aab: $collision_vector_with_aab:ident
    ) => {
        impl<P,E> $Rect<P,E> {
            pub fn new($($p: P,)+ $($e: E),+) -> Self {
                Self { $($p,)+ $($e),+ }
            }
            pub fn position(self) -> $Vec<P> {
                let Self { $($p,)+ .. } = self;
                $Vec { $($p,)+ }
            }
            pub fn extent(self) -> $Extent<E> {
                let Self { $($e,)+ .. } = self;
                $Extent { $($e,)+ }
            }
            pub fn into_pair(self) -> ($Vec<P>, $Extent<E>) {
                let Self { $($p,)+ $($e,)+ } = self;
                ($Vec { $($p,)+ }, $Extent { $($e,)+ })
            }
            pub fn map<DP,DE,PF,EF>(self, pf: PF, ef: EF) -> $Rect<DP,DE>
                where PF: FnMut(P) -> DP, EF: FnMut(E) -> DE
            {
                let Self { $($p,)+ $($e,)+ } = self;
                let $Vec { $($p,)+ } = $Vec { $($p,)+ }.map(pf);
                let $Extent { $($e,)+ } = $Extent { $($e,)+ }.map(ef);
                $Rect { $($p,)+ $($e,)+ }
            }
        }
        impl<T> $Rect<T,T> where T: Copy + Add<T, Output=T> {
            pub fn $into_aab(self) -> $Aab<T> {
                self.into()
            }
            pub fn contains_point(self, p: $Vec<T>) -> bool where T: PartialOrd {
                self.$into_aab().contains_point(p)
            }
            pub fn $contains_rect(self, other: Self) -> bool where T: PartialOrd {
                self.$into_aab().$contains_aab(other.into())
            }
            pub fn $collides_with_rect(self, other: Self) -> bool where T: PartialOrd {
                self.$into_aab().$collides_with_aab(other.into())
            }
            pub fn center(self) -> $Vec<T> where T: One + Div<T,Output=T> {
                self.$into_aab().center()
            }
        }
        impl<T> $Rect<T,T> where T: Copy + PartialOrd + Sub<T, Output=T> + Add<T, Output=T> {
            /// Gets a copy of this shape so that it contains the given point.
            pub fn expanded_to_contain_point(self, p: $Vec<T>) -> Self where T: PartialOrd {
                self.$into_aab().expanded_to_contain_point(p).into()
            }
            /// Expands this shape so that it contains the given point.
            pub fn expand_to_contain_point(&mut self, p: $Vec<T>) where T: PartialOrd {
                *self = self.expanded_to_contain_point(p);
            }
            pub fn union(self, other: Self) -> Self {
                self.$into_aab().union(other.into()).into()
            }
            pub fn intersection(self, other: Self) -> Self {
                self.$into_aab().intersection(other.into()).into()
            }
            /// Gets a vector that tells how much `self` penetrates `other`.
            pub fn $collision_vector_with_rect(self, other: Self) -> $Vec<T> 
                where T: One + Div<T,Output=T>
            {
                self.$into_aab().$collision_vector_with_aab(other.into())
            }
            $(
            /// Splits this shape in two, by a straight plane along the
            #[doc=$p_s]
            /// axis.  
            /// The returned tuple is `(low, high)`.
            ///
            /// # Panics
            /// `sp` is assumed to be a position along the
            #[doc=$p_s]
            /// axis that is within this shape's bounds.
            pub fn $split_at_p(self, sp: T) -> (Self, Self) {
                let (low, high) = self.$into_aab().$split_at_p(sp);
                (low.into(), high.into())
            }
            )+
        }
        impl<P,E> From<($Vec<P>, $Extent<E>)> for $Rect<P,E> {
            fn from(t: ($Vec<P>, $Extent<E>)) -> Self {
                let ($Vec { $($p,)+ }, $Extent { $($e,)+ }) = t;
                Self { $($p,)+ $($e,)+ }
            }
        }
        impl<T> From<$Aab<T>> for $Rect<T,T> 
            where T: Copy + Sub<T, Output=T>
        {
            fn from(aab: $Aab<T>) -> Self {
                let $Extent { $($e,)+ } = (aab.max - aab.min).into();
                Self {
                    $($p: aab.min.$p,)+
                    $($e,)+
                }
            }
        }
        impl<T> From<$Rect<T,T>> for $Aab<T> 
            where T: Copy + Add<T, Output=T>
        {
            fn from(rect: $Rect<T,T>) -> Self {
                Self {
                    min: rect.position(),
                    max: rect.position() + rect.extent(),
                }
            }
        }
    };
}
macro_rules! geom_impl_aabr_or_aabb {
    (
        $Aab:ident $Vec:ident $Extent:ident ($(($p_s:expr) $p:ident $split_at_p:ident)+)
        $Rect:ident $into_rect:ident
        contains_aab: $contains_aab:ident
        collides_with_aab: $collides_with_aab:ident
        collision_vector_with_aab: $collision_vector_with_aab:ident
    ) => {
        impl<T> $Aab<T> {
            /// Creates a new bounding shape from a single point.
            pub fn new_empty(p: $Vec<T>) -> Self where T: Copy {
                let (min, max) = (p, p);
                Self { min, max }
            }
            pub fn $into_rect(self) -> $Rect<T,T> 
                where T: Copy + Sub<T, Output=T>
            {
                self.into()
            }
            /// Gets this bounding shape's center.
            pub fn center(self) -> $Vec<T>
                where T: Copy + One + Add<T,Output=T> + Div<T,Output=T>
            {
                (self.min + self.max) / (T::one() + T::one())
            }
            /// Gets this bounding shape's total size.
            pub fn size(self) -> $Extent<T>
                where T: Copy + Sub<T, Output=T>
            {
                self.$into_rect().extent()
            }
            /// Gets this bounding shape's half size.
            pub fn half_size(self) -> $Extent<T>
                where T: Copy + Sub<T, Output=T> + One + Div<T,Output=T> + Add<T, Output=T>
            {
                self.size() / (T::one() + T::one())
            }
            /// Gets the smallest bounding shape that contains both this one and another.
            pub fn union(self, other: Self) -> Self where T: PartialOrd {
                Self {
                    min: $Vec::partial_min(self.min, other.min),
                    max: $Vec::partial_max(self.max, other.max),
                }
            }
            /// Gets the largest bounding shape contained by both this one and another.
            pub fn intersection(self, other: Self) -> Self where T: PartialOrd {
                Self {
                    min: $Vec::partial_max(self.min, other.min),
                    max: $Vec::partial_min(self.max, other.max),
                }
            }
            /// Gets a copy of this shape so that it contains the given point.
            pub fn expanded_to_contain_point(self, p: $Vec<T>) -> Self where T: Copy + PartialOrd {
                self.union(Self::new_empty(p))
            }
            /// Expands this shape so that it contains the given point.
            pub fn expand_to_contain_point(&mut self, p: $Vec<T>) where T: Copy + PartialOrd {
                *self = self.expanded_to_contain_point(p);
            }
            pub fn contains_point(self, p: $Vec<T>) -> bool 
                where T: PartialOrd
            {
                true $(&& self.min.$p <= p.$p && p.$p <= self.max.$p)+
            }
            pub fn $contains_aab(self, other: Self) -> bool 
                where T: PartialOrd
            {
                true $(&& self.min.$p <= other.min.$p && other.max.$p <= self.max.$p)+
            }
            pub fn $collides_with_aab(self, other: Self) -> bool 
                where T: PartialOrd
            {
                true $(&& self.max.$p > other.min.$p && self.min.$p < other.max.$p)+
            }
            /// Gets a vector that tells how much `self` penetrates `other`.
            pub fn $collision_vector_with_aab(self, other: Self) -> $Vec<T> 
                where T: Copy + PartialOrd + Sub<T, Output=T> + One + Add<T,Output=T> + Div<T,Output=T>
            {
                let (b1, b2) = (self, other);
                let (c1, c2) = (b1.center(), b2.center());
                $Vec { $($p: if c1.$p < c2.$p {
                    b1.max.$p - b2.min.$p
                } else {
                    b1.min.$p - b2.max.$p
                }),+}
            }
            $(
            /// Splits this shape in two, by a straight plane along the
            #[doc=$p_s]
            /// axis.  
            /// The returned tuple is `(low, high)`.
            ///
            /// # Panics
            /// `sp` is assumed to be a position along the
            #[doc=$p_s]
            /// axis that is within this shape's bounds.
            pub fn $split_at_p(self, sp: T) -> (Self, Self) where T: Copy + PartialOrd {
                debug_assert!(sp >= self.min.$p);
                debug_assert!(sp <= self.max.$p);
                let low = Self {
                    min: self.min,
                    max: { let mut v = self.max; v.$p = sp; v },
                };
                let high = Self {
                    min: { let mut v = self.min; v.$p = sp; v },
                    max: self.max,
                };
                (low, high)
            }
            )+

            pub fn map<D,F>(self, mut f: F) -> $Aab<D> where F: FnMut(T) -> D
            {
                let Self { min, max } = self;
                let $Vec { $($p,)+ } = min;
                let min = $Vec { $($p: f($p),)+ };
                let $Vec { $($p,)+ } = max;
                let max = $Vec { $($p: f($p),)+ };
                $Aab { min, max }
            }
        }
    };
}
macro_rules! geom_impl_disk_or_sphere {
    (
        $Shape:ident ($Shape_s:expr) $Vec:ident ($($p:ident)+)
        $Extent:ident
        $Rect:ident $rect:ident
        $Aab:ident $aab:ident
        collides_with_other: $collides_with_other:ident
        collision_vector_with_other: $collision_vector_with_other:ident
    ) => {
        impl<P,E> $Shape<P,E> {
            /// Creates a new
            #[doc=$Shape_s]
            /// from `center` and `radius`.
            pub fn new(center: $Vec<P>, radius: E) -> Self {
                Self { center, radius }
            }
            /// Creates a new
            #[doc=$Shape_s]
            /// from `center` and a `radius` equal to one.
            pub fn unit(center: $Vec<P>) -> Self where E: One {
                Self { center, radius: One::one() }
            }
            /// Creates a new
            #[doc=$Shape_s]
            /// from `center` and a `radius` equal to zero.
            pub fn point(center: $Vec<P>) -> Self where E: Zero {
                Self { center, radius: Zero::zero() }
            }
            /// Gets the value of twice the radius.
            pub fn diameter(self) -> E where E: Copy + Add<Output=E> {
                self.radius + self.radius
            }
            /// Gets the bounding rectangle.
            pub fn $rect(self) -> $Rect<P,E> 
                where P: Sub<P,Output=P> + From<E> + Copy, E: Copy + Add<E,Output=E>
            {
                $Rect::from((
                    self.center - P::from(self.radius),
                    $Extent::broadcast(self.diameter())
                ))
            }
        }

        impl<T> $Shape<T,T> where T: Copy + Add<T,Output=T> + Sub<T,Output=T> {
            /// Gets this shape's bounds.
            pub fn $aab(self) -> $Aab<T> {
                $Aab {
                    min: self.center - self.radius,
                    max: self.center + self.radius,
                }
            }
        }
        impl<T: Float + Sum> $Shape<T,T> {
            /// Does this shape contain the given point ?
            pub fn contains_point(self, p: $Vec<T>) -> bool where T: PartialOrd {
                self.center.distance(p) <= self.radius
            }
            /// Does this shape collide with another ?
            pub fn $collides_with_other(self, other: Self) -> bool where T: PartialOrd {
                self.center.distance(other.center) <= (self.radius + other.radius)
            }
            // XXX: This remains to be tested!
            /// Gets a vector that tells how much this shape penetrates another.
            pub fn $collision_vector_with_other(self, other: Self) -> $Vec<T> {
                let v = other.center - self.center;
                let mag = self.radius + other.radius - v.magnitude();
                v.normalized() * mag
            }
        }
    };
}


// NOTE: There's never a sane Default for this, so don't implement or derive it!!
/// Data that represents distance offsets of frustum planes from an origin.
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
        /// 2D rectangle, represented by a bottom-left position, and 2D extents.
        ///
        /// Most operations on a `Rect` are better done by converting it to
        /// `Aabr` form. In fact, most existing code in the wild implicitly does this
        /// and so does this crate.
        ///
        /// `Aabr` structs are often more convenient, faster and probably less confusing.  
        /// The `Rect` type is provided because it exists for a lot of APIs (including
        /// some system APIs, OpenGL, and others).
        ///
        /// Please note that in most APIs in the wild (but **NOT** in `vek`), the Y axis
        /// is treated as pointing **downwards** because it's the one of window space.
        ///
        /// Keeping the Y axis upwards is a lot more convenient implementation-wise,
        /// and still matches the convention used in 3D spaces.
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
            Rect Vec2 Extent2 (("x") x split_at_x ("y") y split_at_y) (w h)
            Aabr into_aabr
            contains_rect: contains_rect
            contains_aab: contains_aabr
            collides_with_rect: collides_with_rect
            collides_with_aab: collides_with_aabr
            collision_vector_with_rect: collision_vector_with_rect
            collision_vector_with_aab: collision_vector_with_aabr
        }


        /// Axis-aligned Bounding Rectangle (2D), represented by `min` and `max` points.
        ///
        /// **N.B:** You are responsible for ensuring that all respective elements of
        /// `min` are indeed less than or equal to those of `max`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Aabr<T> {
            pub min: Vec2<T>,
            pub max: Vec2<T>,
        }

        geom_impl_aabr_or_aabb!{
            Aabr Vec2 Extent2 (("x") x split_at_x ("y") y split_at_y)
            Rect into_rect
            contains_aab: contains_aabr
            collides_with_aab: collides_with_aabr
            collision_vector_with_aab: collision_vector_with_aabr
        }


        /// A `Rect` extended to 3D.
        ///
        /// This would have been named `Box`, but it was "taken" by the standard library already.
        ///
        /// You should probably use `Aabb` because it is less confusing.  
        /// See also `Rect` for a short discussion on the topic.
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
            Rect3 Vec3 Extent3 (("x") x split_at_x ("y") y split_at_y ("z") z split_at_z) (w h d)
            Aabb into_aabb
            contains_rect: contains_rect3
            contains_aab: contains_aabb
            collides_with_rect: collides_with_rect3
            collides_with_aab: collides_with_aabb
            collision_vector_with_rect: collision_vector_with_rect3
            collision_vector_with_aab: collision_vector_with_aabb
        }


        /// Axis-aligned Bounding Box (3D), represented by `min` and `max` points.
        ///
        /// **N.B:** You are responsible for ensuring that all respective elements of
        /// `min` are indeed less than or equal to those of `max`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Aabb<T> {
            pub min: Vec3<T>,
            pub max: Vec3<T>,
        }

        geom_impl_aabr_or_aabb!{
            Aabb Vec3 Extent3 (("x") x split_at_x ("y") y split_at_y ("z") z split_at_z)
            Rect3 into_rect3
            contains_aab: contains_aabb
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

        /// Disk (2D), represented by center and radius.
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
                let pi = E::PI();
                (pi + pi) * self.radius
            }
            pub fn area(self) -> E where E: Copy + FloatConst + Mul<Output=E> { 
                let r = self.radius; 
                E::PI()*r*r
            }
        }

        geom_impl_disk_or_sphere!{
            Disk ("Disk") Vec2 (x y)
            Extent2 Rect rect Aabr aabr
            collides_with_other: collides_with_disk
            collision_vector_with_other: collision_vector_with_disk
        }


        /// Sphere (3D), represented by center and radius.
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
            Sphere ("Sphere") Vec3 (x y z)
            Extent3 Rect3 rect3 Aabb aabb
            collides_with_other: collides_with_sphere
            collision_vector_with_other: collision_vector_with_sphere
        }


        /// Ellipsis (2D), represented by center and radius in both axii.
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

        /// 2D Line segment, represented by two points, `start` and `end`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct LineSegment2<T> {
            pub start: Vec2<T>,
            pub end: Vec2<T>,
        }
        /// 3D Line segment, represented by two points, `start` and `end`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct LineSegment3<T> {
            pub start: Vec3<T>,
            pub end: Vec3<T>,
        }

        geom_impl_line_segment!{LineSegment2 Vec2}
        geom_impl_line_segment!{LineSegment3 Vec3}
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

#[cfg(test)]
mod tests {
    use super::*;
    use ::vec::{Vec2, Vec3};

    #[test] fn rect_center() {
        let min = Vec2::new(-1_f32, -1.);
        let max = -min;
        let aabr = Aabr { min, max };
        assert_relative_eq!(aabr.center(), Vec2::zero());
        let rect = aabr.into_rect();
        assert_relative_eq!(rect.center(), Vec2::zero());
    }
    #[test] fn rect3_center() {
        let min = Vec3::new(-1_f32, -1., -1.);
        let max = -min;
        let aabb = Aabb { min, max };
        assert_relative_eq!(aabb.center(), Vec3::zero());
        let rect3 = aabb.into_rect3();
        assert_relative_eq!(rect3.center(), Vec3::zero());
    }

}

//! Common and trivial geometric primitives.

// NOTE: in this module, the type parameters <P,E> usually stand for Position and Extent.

use num_traits::{real::Real, FloatConst, Zero, One, AsPrimitive};
use approx::RelativeEq;
use std::ops::*;
use std::ops::Add;
use crate::ops::Clamp;

// WISH: add useful impls to this module (inclusing basic conversions from rect to vec pairs)
// WISH: lerp for all shapes
// WISH: More intersections (e.g line_segment vs box, etc)

macro_rules! geom_impl_line_segment {
    ($LineSegment:ident $Vec:ident) => {
        impl<T> From<Range<$Vec<T>>> for $LineSegment<T> {
            fn from(range: Range<$Vec<T>>) -> Self {
                let Range { start, end } = range;
                Self { start, end }
            }
        }
        impl<T> $LineSegment<T> {
            /// Converts this line segment into a range of points.
            pub fn into_range(self) -> Range<$Vec<T>> {
                let Self { start, end } = self;
                Range { start, end }
            }

            /// Project the given point onto the line segment (equivalent to 'snapping' the point
            /// to the closest point on the line segment).
            pub fn projected_point(self, p: $Vec<T>) -> $Vec<T> where T: Real + Add<T, Output=T> + RelativeEq {
                let len_sq = self.start.distance_squared(self.end);

                if len_sq.relative_eq(&Zero::zero(), T::default_epsilon(), T::default_max_relative()) {
                    self.start
                } else {
                    let t = ((p - self.start).dot(self.end - self.start) / len_sq)
                        .max(Zero::zero())
                        .min(One::one());
                    self.start + (self.end - self.start) * t
                }
            }

            /// Get the smallest distance between the line segment and a point.
            pub fn distance_to_point(self, p: $Vec<T>) -> T where T: Real + Add<T, Output=T> + RelativeEq {
                self.projected_point(p).distance(p)
            }

            /// Converts this line to a line of another type, using the `as` conversion.
            pub fn as_<D>(self) -> $LineSegment<D> where T: AsPrimitive<D>, D: 'static + Copy {
                let Self { start, end } = self;
                $LineSegment { start: start.as_(), end: end.as_() }
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
            /// Creates a new rectangle from position elements and extent elements.
            pub fn new($($p: P,)+ $($e: E),+) -> Self {
                Self { $($p,)+ $($e),+ }
            }
            /// Gets this rectangle's position.
            pub fn position(self) -> $Vec<P> {
                let Self { $($p,)+ .. } = self;
                $Vec { $($p,)+ }
            }
            /// Sets this rectangle's position.
            pub fn set_position(&mut self, p: $Vec<P>) {
                $(self.$p = p.$p;)*
            }
            /// Gets this rectangle's extent (size).
            pub fn extent(self) -> $Extent<E> {
                let Self { $($e,)+ .. } = self;
                $Extent { $($e,)+ }
            }
            /// Sets this rectangle's extent (size).
            pub fn set_extent(&mut self, e: $Extent<E>) {
                $(self.$e = e.$e;)*
            }
            /// Gets this rectangle's position and extent (size).
            pub fn position_extent(self) -> ($Vec<P>, $Extent<E>) {
                let Self { $($p,)+ $($e,)+ } = self;
                ($Vec { $($p,)+ }, $Extent { $($e,)+ })
            }
            /// Returns this rectangle, converted with the given closures (one for position
            /// elements, the other for extent elements).
            pub fn map<DP,DE,PF,EF>(self, pf: PF, ef: EF) -> $Rect<DP,DE>
                where PF: FnMut(P) -> DP, EF: FnMut(E) -> DE
            {
                let Self { $($p,)+ $($e,)+ } = self;
                let $Vec { $($p,)+ } = $Vec { $($p,)+ }.map(pf);
                let $Extent { $($e,)+ } = $Extent { $($e,)+ }.map(ef);
                $Rect { $($p,)+ $($e,)+ }
            }

            /// Converts this rectangle to a rectangle of another type, using the `as` conversion.
            pub fn as_<DP,DE>(self) -> $Rect<DP,DE>
                where P: AsPrimitive<DP>, DP: 'static + Copy,
                      E: AsPrimitive<DE>, DE: 'static + Copy
            {
                let Self { $($p,)+ $($e,)+ } = self;
                let $Vec { $($p,)+ } = $Vec { $($p,)+ }.as_();
                let $Extent { $($e,)+ } = $Extent { $($e,)+ }.as_();
                $Rect { $($p,)+ $($e,)+ }
            }
        }
        impl<T> $Rect<T,T> where T: Copy + Add<T, Output=T> {
            /// Converts this into the matching axis-aligned bounding shape representation.
            pub fn $into_aab(self) -> $Aab<T> {
                self.into()
            }
            /// Does this rectangle contain the given point ?
            pub fn contains_point(self, p: $Vec<T>) -> bool where T: PartialOrd {
                self.$into_aab().contains_point(p)
            }
            /// Does this rectangle fully contain the given one ?
            pub fn $contains_rect(self, other: Self) -> bool where T: PartialOrd {
                self.$into_aab().$contains_aab(other.into())
            }
            /// Does this rectangle collide with another ?
            pub fn $collides_with_rect(self, other: Self) -> bool where T: PartialOrd {
                self.$into_aab().$collides_with_aab(other.into())
            }
            /// Gets this rectangle's center.
            pub fn center(self) -> $Vec<T> where T: One + Div<T,Output=T> {
                self.$into_aab().center()
            }
        }
        impl<T> $Rect<T,T> where T: Copy + PartialOrd + Sub<T, Output=T> + Add<T, Output=T> {
            /// Returns this shape so that it contains the given point.
            pub fn expanded_to_contain_point(self, p: $Vec<T>) -> Self where T: PartialOrd {
                self.$into_aab().expanded_to_contain_point(p).into()
            }
            /// Expands this shape so that it contains the given point.
            pub fn expand_to_contain_point(&mut self, p: $Vec<T>) where T: PartialOrd {
                *self = self.expanded_to_contain_point(p);
            }
            /// Gets the smallest rectangle that contains both this one and another.
            pub fn union(self, other: Self) -> Self {
                self.$into_aab().union(other.into()).into()
            }
            /// Gets the largest rectangle contained by both this one and another.
            pub fn intersection(self, other: Self) -> Self {
                self.$into_aab().intersection(other.into()).into()
            }
            /// Sets this rectangle to the union of itself with another.
            pub fn expand_to_contain(&mut self, other: Self) {
                *self = self.union(other);
            }
            /// Sets this rectangle to the intersection of itself with another.
            pub fn intersect(&mut self, other: Self) {
                *self = self.intersection(other);
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
            pub fn $split_at_p(self, sp: T) -> [Self; 2] {
                let s = self.$into_aab().$split_at_p(sp);
                [s[0].into(), s[1].into()]
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
            /// Is this bounding shape valid ?
            /// True only if all elements of `self.min` are less than or equal to those of `self.max`.
            pub fn is_valid(&self) -> bool where T: PartialOrd {
                self.min.partial_cmple(&self.max).reduce_and()
            }
            /// Makes this bounding shape valid by swapping elements of `self.min` with `self.max` as needed.
            pub fn make_valid(&mut self) where T: PartialOrd {
                $(if self.min.$p > self.max.$p { std::mem::swap(&mut self.min.$p, &mut self.max.$p); })+
            }
            /// Returns this bounding shape made valid by swapping elements of `self.min` with `self.max` as needed.
            pub fn made_valid(mut self) -> Self where T: PartialOrd {
                self.make_valid();
                self
            }
            /// Creates a new bounding shape from a single point.
            pub fn new_empty(p: $Vec<T>) -> Self where T: Copy {
                let (min, max) = (p, p);
                Self { min, max }
            }
            /// Converts this bounding shape to the matching rectangle representation.
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
            /// Sets this bounding shape to the union of itself with another.
            pub fn expand_to_contain(&mut self, other: Self) where T: Copy + PartialOrd {
                *self = self.union(other);
            }
            /// Sets this bounding shape to the intersection of itself with another.
            pub fn intersect(&mut self, other: Self) where T: Copy + PartialOrd {
                *self = self.intersection(other);
            }
            /// Gets a copy of this shape so that it contains the given point.
            pub fn expanded_to_contain_point(self, p: $Vec<T>) -> Self where T: Copy + PartialOrd {
                self.union(Self::new_empty(p))
            }
            /// Expands this shape so that it contains the given point.
            pub fn expand_to_contain_point(&mut self, p: $Vec<T>) where T: Copy + PartialOrd {
                *self = self.expanded_to_contain_point(p);
            }
            /// Does this bounding shape contain the given point ?
            pub fn contains_point(self, p: $Vec<T>) -> bool
                where T: PartialOrd
            {
                true $(&& self.min.$p <= p.$p && p.$p <= self.max.$p)+
            }
            /// Does this bounding shape fully contain another ?
            pub fn $contains_aab(self, other: Self) -> bool
                where T: PartialOrd
            {
                true $(&& self.min.$p <= other.min.$p && other.max.$p <= self.max.$p)+
            }
            /// Does this bounding shape collide with another ?
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
            /// Project the given point into the bounding shape (equivalent to 'snapping' the point
            /// to the closest point in the bounding shape).
            pub fn projected_point(self, p: $Vec<T>) -> $Vec<T>
                where T: Clamp
            {
                p.clamped(self.min, self.max)
            }
            /// Get the smallest distance between the bounding shape and a point.
            pub fn distance_to_point(self, p: $Vec<T>) -> T where T: Clamp + Real + Add<T, Output=T> + RelativeEq {
                self.projected_point(p).distance(p)
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
            pub fn $split_at_p(self, sp: T) -> [Self; 2] where T: Copy + PartialOrd {
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
                [low, high]
            }
            )+

            /// Returns this bounding shape, converted element-wise using the given closure.
            pub fn map<D,F>(self, mut f: F) -> $Aab<D> where F: FnMut(T) -> D
            {
                let Self { min, max } = self;
                let $Vec { $($p,)+ } = min;
                let min = $Vec { $($p: f($p),)+ };
                let $Vec { $($p,)+ } = max;
                let max = $Vec { $($p: f($p),)+ };
                $Aab { min, max }
            }

            /// Converts this rectangle to a rectangle of another type, using the `as` conversion.
            pub fn as_<D>(self) -> $Aab<D> where T: AsPrimitive<D>, D: 'static + Copy {
                let Self { min, max } = self;
                $Aab { min: min.as_(), max: max.as_() }
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
            /// Gets the bounding rectangle for this shape.
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
        impl<T: Real + Add<T, Output=T>> $Shape<T,T> {
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
#[allow(missing_docs)]
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

        use crate::vec::$mod::*;

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
            /// X position of the **bottom-left** corner.
            pub x: P,
            /// Y position of the **bottom-left** corner.
            pub y: P,
            /// Width.
            pub w: E,
            /// Height, **with Y axis going upwards**.
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
        /// The `is_valid()`, `make_valid()` and `made_valid()` methods are designed to help you
        /// with this.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Aabr<T> {
            /// Minimum coordinates of bounds.
            pub min: Vec2<T>,
            /// Maximum coordinates of bounds.
            pub max: Vec2<T>,
        }

        impl<T> From<Aabb<T>> for Aabr<T> {
            fn from(aabb: Aabb<T>) -> Self {
                Self {
                    min: aabb.min.into(),
                    max: aabb.max.into(),
                }
            }
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
            /// X position of the **bottom-left-near** corner.
            pub x: P,
            /// Y position of the **bottom-left-near** corner.
            pub y: P,
            /// Z position of the **bottom-left-near** corner.
            pub z: P,
            /// Width.
            pub w: E,
            /// Height, **with Y axis going upwards**.
            pub h: E,
            /// Depth, **with Z axis going forwards**.
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
        /// The `is_valid()`, `make_valid()` and `made_valid()` methods are designed to help you
        /// with this.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Aabb<T> {
            /// Minimum coordinates of bounds.
            pub min: Vec3<T>,
            /// Maximum coordinates of bounds.
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
        #[allow(missing_docs)]
        pub struct Disk<P,E> {
            pub center: Vec2<P>,
            pub radius: E,
        }

        impl<P,E> Disk<P,E> {
            /// Gets this disk's circumference.
            pub fn circumference(self) -> E
                where E: Copy + FloatConst + Mul<Output=E> + Add<Output=E>
            {
                let pi = E::PI();
                (pi + pi) * self.radius
            }
            /// Gets this disk's area.
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
        #[allow(missing_docs)]
        pub struct Sphere<P,E> {
            pub center: Vec3<P>,
            pub radius: E,
        }
        impl<P,E> Sphere<P,E> {
            /// Gets this sphere's surface area.
            pub fn surface_area(self) -> E where E: Copy + One + FloatConst + Add<Output=E> + Mul<Output=E> {
                let four = E::one() + E::one() + E::one() + E::one();
                let r = self.radius;
                four*E::PI()*r*r
            }
            /// Gets this sphere's volume.
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
        #[allow(missing_docs)]
        pub struct Ellipsis<P,E> {
            pub center: Vec2<P>,
            pub radius: Extent2<E>,
        }
        /// Nobody can possibly use this ???
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[allow(missing_docs)]
        pub struct Potato<P,E> {
            pub center: Vec3<P>,
            pub radius: Extent3<E>,
        }

        /// 2D Line segment, represented by two points, `start` and `end`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[allow(missing_docs)]
        pub struct LineSegment2<T> {
            pub start: Vec2<T>,
            pub end: Vec2<T>,
        }
        /// 3D Line segment, represented by two points, `start` and `end`.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        #[allow(missing_docs)]
        pub struct LineSegment3<T> {
            pub start: Vec3<T>,
            pub end: Vec3<T>,
        }

        geom_impl_line_segment!{LineSegment2 Vec2}
        geom_impl_line_segment!{LineSegment3 Vec3}

        /// 3D ray, represented by a starting point and a normalized direction vector.
        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Ray<T> {
            /// The ray's starting point.
            pub origin: Vec3<T>,
            /// The ray's direction. **Methods expect it to be normalized**.
            pub direction: Vec3<T>,
        }

        impl<T: Real + Add<T, Output=T>> Ray<T> {
            /// Creates a `Ray` from a starting point and direction.
            ///
            /// This doesn't check if `direction` is normalized, because either you know it is, or
            /// it isn't and maybe it doesn't matter for your use case.
            pub fn new(origin: Vec3<T>, direction: Vec3<T>) -> Self {
                Self { origin, direction }
            }
            /// Tests if this ray intersects the given triangle, returning the distance from
            /// the ray's origin along its direction where the intersection lies.
            ///
            /// If the returned value is `Some(x)` where `x < EPSILON`, then you should
            /// assume there was a line intersection, **NOT** a ray intersection.
            ///
            /// This uses the [Möller–Trumbore intersection algorithm](https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm).
            pub fn triangle_intersection(&self, tri: [Vec3<T>; 3]) -> Option<T> {
                let (v0, v1, v2) = (tri[0], tri[1], tri[2]);
                let edge1 = v1 - v0;
                let edge2 = v2 - v0;
                let h = self.direction.cross(edge2);
                let a = edge1.dot(h);
                if a > -T::epsilon() && a < T::epsilon() {
                    return None;
                }
                let f = a.recip();
                let s = self.origin - v0;
                let u = f * s.dot(h);
                if u < T::zero() || u > T::one() {
                    return None;
                }
                let q = s.cross(edge1);
                let v = f * self.direction.dot(q);
                if v < T::zero() || u + v > T::one() {
                    return None;
                }
                Some(f * edge2.dot(q))
            }
        }
    }
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Basic geometric primitives that use `#[repr(simd)]` vectors.
    use super::*;
    geom_complete_mod!(repr_simd);
}

pub mod repr_c {
    //! Basic geometric primitives that use `#[repr(C)]` vectors.
    use super::*;
    geom_complete_mod!(repr_c);
}

pub use self::repr_c::*;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vec::{Vec2, Vec3};

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
    #[test] fn projected_point() {
        let segment = LineSegment2 { start: Vec2::new(-5_f32, 5.), end: Vec2::new(5., -5.0) };
        assert_relative_eq!(segment.start, segment.projected_point(Vec2::new(-4., 7.)));
        assert_relative_eq!(segment.end, segment.projected_point(Vec2::new(7., -4.)));
        assert_relative_eq!(Vec2::zero(), segment.projected_point(Vec2::new(1., 1.)));
        let segment = LineSegment3 { start: Vec3::new(-5_f32, -0., 5.), end: Vec3::new(5., 0., -5.0) };
        assert_relative_eq!(Vec3::zero(), segment.projected_point(Vec3::new(-0., -1., -0.)));
    }
    #[test] fn distance_to_point() {
        let segment = LineSegment2 { start: Vec2::new(-5_f32, 5.), end: Vec2::new(5., -5.0) };
        assert_relative_eq!(2.0f32.sqrt(), segment.distance_to_point(Vec2::new(-1., -1.)));
        let segment = LineSegment3 { start: Vec3::new(-5_f32, 0., 5.), end: Vec3::new(5., 0., -5.) };
        assert_relative_eq!(2.0f32.sqrt(), segment.distance_to_point(Vec3::new(-1., 0., -1.)));
    }
}

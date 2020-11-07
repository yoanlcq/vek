//! Low-order (quadratic and cubic) Bézier curves.
// NOTE: Most info from https://pomax.github.io/bezierinfo

use num_traits::{Zero, real::Real};
use crate::ops::*;
use std::ops::*;
use std::ops::Add;
use crate::vec::repr_c::{
    Vec3 as CVec3,
    Vec4 as CVec4,
};

// WISH: OOBBs from beziers
// WISH: "Tracing a curve at fixed distance intervals"
// WISH: Line-curve intersection (Especially straight horizontal and straight vertical)

macro_rules! bezier_impl_any {
    (3 $Bezier:ident $Point:ident) => {

        bezier_impl_any!{$Bezier $Point}

        impl<T: Real> $Bezier<T> {
            /// Gets the Axis-Aligned Bounding Box for this curve.
            pub fn aabb(self) -> Aabb<T> {
                let (min_x, max_x) = self.x_bounds();
                let (min_y, max_y) = self.y_bounds();
                let (min_z, max_z) = self.z_bounds();
                Aabb {
                    min: Vec3::new(min_x, min_y, min_z),
                    max: Vec3::new(max_x, max_y, max_z),
                }
            }
            /// Returns this curve, flipping the `y` coordinate of each of its points.
            pub fn flipped_z(self) -> Self {
                self.into_vector().map(|mut p| {p.z = -p.z; p}).into()
            }
            /// Flips the `x` coordinate of all points of this curve.
            pub fn flip_z(&mut self) {
                *self = self.flipped_z();
            }
        }
        impl<T> Mul<$Bezier<T>> for Rows3<T> where T: Real + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self * p).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Cols3<T> where T: Real + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self * p).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Rows4<T> where T: Real + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self.mul_point(p).into()).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Cols4<T> where T: Real + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self.mul_point(p).into()).into()
            }
        }
    };
    (2 $Bezier:ident $Point:ident) => {

        bezier_impl_any!{$Bezier $Point}

        impl<T> Mul<$Bezier<T>> for Rows2<T> where T: Real + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self * p).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Cols2<T> where T: Real + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self * p).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Rows3<T> where T: Real + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self.mul_point_2d(p).into()).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Cols3<T> where T: Real + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self.mul_point_2d(p).into()).into()
            }
        }
    };
    ($Bezier:ident $Point:ident) => {
        impl<T: Real> $Bezier<T> {
            /// Evaluates the normalized tangent at interpolation factor `t`.
            pub fn normalized_tangent(self, t: T) -> Point<T> where T: Add<T, Output=T> {
                self.evaluate_derivative(t).normalized()
            }
            // WISH: better length approximation estimations (e.g see https://math.stackexchange.com/a/61796)
            /// Approximates the curve's length by subdividing it into step_count+1 segments.
            pub fn length_by_discretization(self, step_count: u16) -> T
                where T: Add<T, Output=T> + From<u16>
            {
                let mut length = T::zero();
                let mut prev_point = self.evaluate(T::zero());
                for i in 1..(step_count+2) {
                    let t = <T as From<u16>>::from(i)/(<T as From<u16>>::from(step_count)+T::one());
                    let next_point = self.evaluate(t);
                    length = length + (next_point - prev_point).magnitude();
                    prev_point = next_point;
                }
                length
            }

            /// Gets the Axis-Aligned Bounding Rectangle for this curve.
            ///
            /// On 3D curves, this discards the `z` values.
            pub fn aabr(self) -> Aabr<T> {
                let (min_x, max_x) = self.x_bounds();
                let (min_y, max_y) = self.y_bounds();
                Aabr {
                    min: Vec2::new(min_x, min_y),
                    max: Vec2::new(max_x, max_y),
                }
            }
            /// Returns this curve, flipping the `x` coordinate of each of its points.
            pub fn flipped_x(self) -> Self {
                self.into_vector().map(|mut p| {p.x = -p.x; p}).into()
            }
            /// Returns this curve, flipping the `y` coordinate of each of its points.
            pub fn flipped_y(self) -> Self {
                self.into_vector().map(|mut p| {p.y = -p.y; p}).into()
            }
            /// Flips the `x` coordinate of all points of this curve.
            pub fn flip_x(&mut self) {
                *self = self.flipped_x();
            }
            /// Flips the `y` coordinate of all points of this curve.
            pub fn flip_y(&mut self) {
                *self = self.flipped_y();
            }

            // TODO: Test this! binary_search_point_by_steps
            /// Searches for the point lying on this curve that is closest to `p`.
            ///
            /// `steps` is the number of points to sample in the curve for the "broad phase"
            /// that takes place before the binary search.
            ///
            /// `epsilon` denotes the desired precision for the result. The higher it is, the
            /// sooner the algorithm will finish, but the result would be less satisfactory.
            ///
            /// # Panics
            /// Panics if `epsilon` is less than or equal to `T::epsilon()`.  
            /// `epsilon` must be positive and not approximately equal to zero.
            pub fn binary_search_point_by_steps(self, p: Point<T>, steps: u16, epsilon: T) -> (T, Point<T>) 
                where T: Add<T, Output=T> + From<u16>
            {
                let steps_f = <T as From<u16>>::from(steps);
                let it = (0..steps).map(|i| {
                    let i = <T as From<u16>>::from(i);
                    let t = i / steps_f;
                    (t, self.evaluate(t))
                });
                // half_interval = 1/(2*steps)
                let h = (steps_f + steps_f).recip();
                self.binary_search_point(p, it, h, epsilon)
            }
            // TODO: Test this! binary_search_point
            /// Searches for the point lying on this curve that is closest to `p`.
            ///
            /// For an example usage, see the source code of `binary_search_point_by_steps()`.
            ///
            /// `coarse` is an iterator over pairs of `(interpolation_value, point)` that are
            /// assumed to, together, represent a discretization of the curve.  
            /// This parameter is used for a "broad phase" - the point yielded by `coarse` that is
            /// closest to `p` is the starting point for the binary search.  
            /// `coarse` may very well yield a single pair; Also, it was designed so that,
            /// if you already have the values handy, there is no need to recompute them.  
            /// This function doesn't panic if `coarse` yields no element, but then it would be
            /// very unlikely for the result to be satisfactory.
            ///
            /// `half_interval` is the starting value for the half of the binary search interval.
            ///
            /// `epsilon` denotes the desired precision for the result. The higher it is, the
            /// sooner the algorithm will finish, but the result would be less satisfactory.
            ///
            /// # Panics
            /// Panics if `epsilon` is less than or equal to `T::epsilon()`.  
            /// `epsilon` must be positive and not approximately equal to zero.
            pub fn binary_search_point<I>(self, p: Point<T>, coarse: I, half_interval: T, epsilon: T) -> (T, Point<T>)
                where T: Add<T, Output=T>, I: IntoIterator<Item=(T, Point<T>)>
            {
                debug_assert!(epsilon > T::epsilon());
                let mut t = T::one();
                let mut pt = self.end;
                let mut d = pt.distance_squared(p);
                for (t_, pt_) in coarse {
                    let d_ = pt_.distance_squared(p);
                    if d_ < d {
                        d = d_; pt = pt_; t = t_;
                    }
                }
                let mut h = half_interval;
                while h >= epsilon {
                    let (p1, p2) = (self.evaluate(t-h), self.evaluate(t+h));
                    let (d1, d2) = (p.distance_squared(p1), p.distance_squared(p2));
                    if d1 < d || d2 < d {
                        if d1 < d2 {
                            d = d1; pt = p1; t = t - h;
                        } else {
                            d = d2; pt = p2; t = t + h;
                        }
                        continue;
                    }
                    h = h / (T::one() + T::one());
                }
                (t, pt)
            }
        }
    };
}

macro_rules! bezier_impl_quadratic_axis {
    ($QuadraticBezier:ident $Point:ident ($x_s:expr) $x:ident $x_inflection:ident $x_min:ident $x_max:ident $x_bounds:ident) => {
        impl<T: Real> $QuadraticBezier<T> {
            /// Returns the evaluation factor that gives an inflection point along the
            #[doc=$x_s]
            /// axis, if any.
            // Code in part taken from `lyon` crate, geom.
            // Also explained at https://pomax.github.io/bezierinfo/#extremities
            pub fn $x_inflection(self) -> Option<T> {
                let div = self.start.$x - (self.ctrl.$x + self.ctrl.$x) + self.end.$x;
                if div.abs() <= T::epsilon() {
                    return None;
                }
                let t = (self.start.$x - self.ctrl.$x) / div;
                if T::zero() <= t && t <= T::one() {
                    return Some(t);
                }
                return None;
            }
            /// Returns the evaluation factor that gives the point on the curve which
            #[doc=$x_s]
            /// coordinate is the minimum.
            pub fn $x_min(self) -> T {
                if let Some(t) = self.$x_inflection() {
                    let p = self.evaluate(t);
                    if p.$x < self.start.$x && p.$x < self.end.$x {
                        return t;
                    }
                }
                if self.start.$x < self.end.$x { T::zero() } else { T::one() }
            }
            /// Returns the evaluation factor that gives the point on the curve which
            #[doc=$x_s]
            /// coordinate is the maximum.
            pub fn $x_max(self) -> T {
                if let Some(t) = self.$x_inflection() {
                    let p = self.evaluate(t);
                    if p.$x > self.start.$x && p.$x > self.end.$x {
                        return t;
                    }
                }
                if self.start.$x > self.end.$x { T::zero() } else { T::one() }
            }
            /// Returns the evaluation factors that give the points on the curve which
            #[doc=$x_s]
            /// coordinates are the respective minimum and maximum.
            pub fn $x_bounds(self) -> (T, T) {
                // PERF: We don't need to compute $x_inflections twice!
                (self.$x_min(), self.$x_max())
            }
        }
    };
}

macro_rules! bezier_impl_cubic_axis {
    ($CubicBezier:ident $Point:ident ($x_s:expr) $x:ident $x_inflections:ident $x_min:ident $x_max:ident $x_bounds:ident) => {
        impl<T: Real> $CubicBezier<T> {
            /// Returns the evaluation factor that gives an inflection point along the
            #[doc=$x_s]
            /// axis, if any.
            // Code in part taken from `lyon` crate, geom.
            // Also explained at https://pomax.github.io/bezierinfo/#extremities
            pub fn $x_inflections(self) -> Option<(T, Option<T>)> {
                // See www.faculty.idc.ac.il/arik/quality/appendixa.html for an explanation
                // The derivative of a cubic bezier curve is a curve representing a second degree polynomial function
                // f(x) = a * x² + b * x + c such as :
                let two = T::one() + T::one();
                let three = two + T::one();
                let four = three + T::one();
                let six = three + three;
                let a = three * (self.end.$x - three * self.ctrl1.$x + three * self.ctrl0.$x - self.start.$x);
                let b = six * (self.ctrl1.$x - two * self.ctrl0.$x + self.start.$x);
                let c = three * (self.ctrl0.$x - self.start.$x);

                // If the derivative is a linear function
                if a.abs() <= T::epsilon() {
                    return if b.abs() <= T::epsilon() {
                        // If the derivative is a constant function
                        if c.abs() <= T::epsilon() {
                            Some((T::zero(), None))
                        } else {
                            None
                        }
                    } else {
                        Some((-c / b, None))
                    };
                }

                // Wants to use IsBetween01, but that would annoyingly propagate the trait bound.
                let is_between01 = |t| { T::zero() < t && t < T::one() };

                let discriminant = b * b - four * a * c;

                // There is no Real solution for the equation
                if discriminant < T::zero() {
                    return None;
                }

                // There is one Real solution for the equation
                if discriminant.abs() <= T::epsilon() {
                    let t = -b / (a + a);
                    return if is_between01(t) {
                        Some((t, None))
                    } else {
                        None
                    };
                }

                // There are two Real solutions for the equation
                let discriminant_sqrt = discriminant.sqrt();

                let first_extremum = (-b - discriminant_sqrt) / (a + a);
                let second_extremum = (-b + discriminant_sqrt) / (a + a);

                if is_between01(first_extremum) {
                    if is_between01(second_extremum) {
                        Some((first_extremum, Some(second_extremum)))
                    } else {
                        Some((first_extremum, None))
                    }
                } else {
                    if is_between01(second_extremum) {
                        Some((second_extremum, None))
                    } else {
                        None
                    }
                }
            }
            /// Returns the evaluation factor that gives the point on the curve which
            #[doc=$x_s]
            /// coordinate is the minimum.
            pub fn $x_min(self) -> T {
                if let Some((t1, t2)) = self.$x_inflections() {
                    let p1 = self.evaluate(t1);
                    if let Some(t2) = t2 {
                        let p2 = self.evaluate(t2);
                        if (p1.$x < self.start.$x && p1.$x < self.end.$x)
                        || (p2.$x < self.start.$x && p2.$x < self.end.$x)
                        {
                            return if p1.$x < p2.$x { t1 } else { t2 };
                        }
                    } else {
                        if p1.$x < self.start.$x && p1.$x < self.end.$x {
                            return t1;
                        }
                    }
                }
                if self.start.$x < self.end.$x { T::zero() } else { T::one() }
            }
            /// Returns the evaluation factor that gives the point on the curve which
            #[doc=$x_s]
            /// coordinate is the maximum.
            pub fn $x_max(self) -> T {
                if let Some((t1, t2)) = self.$x_inflections() {
                    let p1 = self.evaluate(t1);
                    if let Some(t2) = t2 {
                        let p2 = self.evaluate(t2);
                        if (p1.$x > self.start.$x && p1.$x > self.end.$x)
                        || (p2.$x > self.start.$x && p2.$x > self.end.$x)
                        {
                            return if p1.$x > p2.$x { t1 } else { t2 };
                        }
                    } else {
                        if p1.$x > self.start.$x && p1.$x > self.end.$x {
                            return t1;
                        }
                    }
                }
                if self.start.$x > self.end.$x { T::zero() } else { T::one() }
            }

            /// Returns the evaluation factors that give the points on the curve which
            #[doc=$x_s]
            /// coordinates are the respective minimum and maximum.
            pub fn $x_bounds(self) -> (T, T) {
                // PERF: We don't need to compute $x_inflections twice!
                (self.$x_min(), self.$x_max())
            }
        }
    };
}
macro_rules! bezier_impl_quadratic {
    ($(#[$attrs:meta])* 3 $QuadraticBezier:ident $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        bezier_impl_quadratic!{$(#[$attrs])* $QuadraticBezier $CubicBezier $Point $LineSegment}
        bezier_impl_quadratic_axis!{$QuadraticBezier $Point ("Z") z z_inflection min_z max_z z_bounds}
        bezier_impl_any!(3 $QuadraticBezier $Point);
    };
    ($(#[$attrs:meta])* 2 $QuadraticBezier:ident $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        bezier_impl_quadratic!{$(#[$attrs])* $QuadraticBezier $CubicBezier $Point $LineSegment}
        bezier_impl_any!(2 $QuadraticBezier $Point);
    };
    ($(#[$attrs:meta])* $QuadraticBezier:ident $CubicBezier:ident $Point:ident $LineSegment:ident) => {

        type Point<T> = $Point<T>;
        
        $(#[$attrs])*
        #[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, /*PartialOrd, Ord*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        ///
        /// Also note that quadratic Bézier curves are quite bad at approximating circles.
        /// See [the relevant section of "A Primer on Bézier Curves"](https://pomax.github.io/bezierinfo/#circles)
        /// for an explanation.
        pub struct $QuadraticBezier<T> {
            /// Starting point of the curve.
            pub start: Point<T>,
            /// Control point of the curve.
            pub ctrl: Point<T>, 
            /// End point of the curve.
            pub end: Point<T>,
        }
        
        impl<T: Real> $QuadraticBezier<T> {
            /// Evaluates the position of the point lying on the curve at interpolation factor `t`.
            ///
            /// This is one of the most important Bézier curve operations,
            /// because, in one way or another, it is used to render a curve
            /// to the screen.
            /// The common use case is to successively evaluate a curve at a set of values
            /// that range from 0 to 1, to approximate the curve as an array of
            /// line segments which are then rendered.
            pub fn evaluate(self, t: T) -> Point<T> {
                let l = T::one();
                let two = l+l;
                self.start*(l-t)*(l-t) + self.ctrl*two*(l-t)*t + self.end*t*t
            }
            /// Evaluates the derivative tangent at interpolation factor `t`, which happens to give
            /// a non-normalized tangent vector.
            ///
            /// See also `normalized_tangent()`.
            pub fn evaluate_derivative(self, t: T) -> Point<T> {
                let l = T::one();
                let n = l+l;
                (self.ctrl-self.start)*(l-t)*n + (self.end-self.ctrl)*t*n
            }
            /// Returns the constant matrix M such that,
            /// given `T = [1, t*t, t*t*t]` and `P` the vector of control points,
            /// `dot(T * M, P)` evalutes the Bezier curve at 't'.
            ///
            /// This function name is arguably dubious.
            pub fn matrix() -> Mat3<T> {
                let zero = T::zero();
                let one = T::one();
                let two = one+one;
                Mat3 {
                    rows: CVec3::new(
                        Vec3::new( one,  zero, zero),
                        Vec3::new(-two,  two, zero),
                        Vec3::new( one, -two, one),
                    )
                }
            }
            /// Splits this quadratic Bézier curve into two curves, at interpolation factor `t`.
            // NOTE that some computations may be reused, but the compiler can
            // reason about these. Clarity wins here IMO.
            pub fn split(self, t: T) -> [Self; 2] {
                let l = T::one();
                let two = l+l;
                let first = $QuadraticBezier {
                    start: self.start,
                    ctrl:  self.ctrl*t - self.start*(t-l),
                    end:   self.end*t*t - self.ctrl*two*t*(t-l) + self.start*(t-l)*(t-l),
                };
                let second = $QuadraticBezier {
                    start: self.end*t*t - self.ctrl*two*t*(t-l) + self.start*(t-l)*(t-l),
                    ctrl:  self.end*t - self.ctrl*(t-l),
                    end:   self.end,
                };
                [first, second]
            }
            /// Elevates this curve into a cubic Bézier curve.
            pub fn into_cubic(self) -> $CubicBezier<T> {
                self.into()
            }
        }
        impl<T> $QuadraticBezier<T> {
            /// Gets this curve reversed, i.e swaps `start` with `end`.
            pub fn reversed(mut self) -> Self {
                self.reverse();
                self
            }
            /// Reverses this curve, i.e swaps `start` with `end`.
            pub fn reverse(&mut self) {
                std::mem::swap(&mut self.start, &mut self.end);
            }
            // Convenience for this module
            pub(crate) fn into_vector(self) -> Vec3<Point<T>> {
                self.into_vec3()
            }
            /// Converts this curve into a `Vec3` of points.
            pub fn into_vec3(self) -> Vec3<Point<T>> {
                self.into()
            }
            /// Converts this curve into a tuple of points.
            pub fn into_tuple(self) -> (Point<T>, Point<T>, Point<T>) {
                self.into_vec3().into_tuple()
            }
            /// Converts this curve into an array of points.
            pub fn into_array(self) -> [Point<T>; 3] {
                self.into_vec3().into_array()
            }
        }
        
        impl<T> From<Vec3<Point<T>>> for $QuadraticBezier<T> {
            fn from(v: Vec3<Point<T>>) -> Self {
                $QuadraticBezier {
                    start: v.x, 
                    ctrl: v.y, 
                    end: v.z
                }
            }
        }
        impl<T> From<$QuadraticBezier<T>> for Vec3<Point<T>> {
            fn from(v: $QuadraticBezier<T>) -> Self {
                Vec3::new(v.start, v.ctrl, v.end)
            }
        }

        impl<T: Real> From<$LineSegment<T>> for $QuadraticBezier<T> {
            fn from(line_segment: $LineSegment<T>) -> Self {
                let ctrl = (line_segment.start + line_segment.end) / (T::one() + T::one());
                Self {
                    start: line_segment.start, 
                    ctrl, 
                    end:   line_segment.end
                }
            }
        }
        impl<T: Real> From<Range<Point<T>>> for $QuadraticBezier<T> {
            fn from(range: Range<Point<T>>) -> Self {
                Self::from($LineSegment::from(range))
            }
        }
        
        bezier_impl_quadratic_axis!{$QuadraticBezier $Point ("X") x x_inflection min_x max_x x_bounds}
        bezier_impl_quadratic_axis!{$QuadraticBezier $Point ("Y") y y_inflection min_y max_y y_bounds}
    }
}

// NOTE: The reason to split 2D-3D conversion into two macros is
// to make sure that each is displayed in the correct documentation page.
macro_rules! bezier_impl_2d_into_3d {
    ($Bezier2:ident $Bezier3:ident) => {
        impl<T: Zero> $Bezier2<T> {
            /// Converts this 2D curve to a 3D one, setting the `z` elements to zero.
            pub fn into_3d(self) -> $Bezier3<T> {
                self.into()
            }
        }
        impl<T: Zero> From<$Bezier2<T>> for $Bezier3<T> {
            fn from(c: $Bezier2<T>) -> Self {
                c.into_vector().map(Into::into).into()
            }
        }
    };
}
macro_rules! bezier_impl_3d_into_2d {
    ($Bezier3:ident $Bezier2:ident) => {
        impl<T> $Bezier3<T> {
            /// Converts this 3D curve to a 2D one, dropping the `z` elements.
            pub fn into_2d(self) -> $Bezier2<T> { 
                self.into() 
            }
        }
        impl<T> From<$Bezier3<T>> for $Bezier2<T> {
            fn from(c: $Bezier3<T>) -> Self {
                c.into_vector().map(Into::into).into()
            }
        }
    };
}

macro_rules! bezier_impl_cubic {
    ($(#[$attrs:meta])* 3 $QuadraticBezier:ident $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        bezier_impl_cubic!{$(#[$attrs])* $QuadraticBezier $CubicBezier $Point $LineSegment}
        bezier_impl_cubic_axis!{$CubicBezier $Point ("Z") z z_inflections min_z max_z z_bounds}
        bezier_impl_any!(3 $CubicBezier $Point);
    };
    ($(#[$attrs:meta])* 2 $QuadraticBezier:ident $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        bezier_impl_cubic!{$(#[$attrs])* $QuadraticBezier $CubicBezier $Point $LineSegment}
        bezier_impl_any!(2 $CubicBezier $Point);
    };
    ($(#[$attrs:meta])* $QuadraticBezier:ident $CubicBezier:ident $Point:ident $LineSegment:ident) => {

        type Point<T> = $Point<T>;
        
        $(#[$attrs])*
        #[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, /*PartialOrd, Ord*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct $CubicBezier<T> {
            /// Starting point of the curve.
            pub start: Point<T>, 
            /// First control point of the curve, associated with `start`.
            pub ctrl0: Point<T>, 
            /// Second control point of the curve, associated with `end`.
            pub ctrl1: Point<T>,
            /// End point of the curve.
            pub end: Point<T>,
        }

        impl<T: Real> $CubicBezier<T> {
            /// Evaluates the position of the point lying on the curve at interpolation factor `t`.
            ///
            /// This is one of the most important Bézier curve operations,
            /// because, in one way or another, it is used to render a curve
            /// to the screen.
            /// The common use case is to successively evaluate a curve at a set of values
            /// that range from 0 to 1, to approximate the curve as an array of
            /// line segments which are then rendered.
            pub fn evaluate(self, t: T) -> Point<T> {
                let l = T::one();
                let three = l+l+l;
                self.start*(l-t)*(l-t)*(l-t) + self.ctrl0*three*(l-t)*(l-t)*t + self.ctrl1*three*(l-t)*t*t + self.end*t*t*t
            }
            /// Evaluates the derivative tangent at interpolation factor `t`, which happens to give
            /// a non-normalized tangent vector.
            ///
            /// See also `normalized_tangent()`.
            pub fn evaluate_derivative(self, t: T) -> Point<T> {
                let l = T::one();
                let n = l+l+l;
                let two = l+l;
                (self.ctrl0-self.start)*(l-t)*(l-t)*n + (self.ctrl1-self.ctrl0)*two*(l-t)*t*n + (self.end-self.ctrl1)*t*t*n
            }
            /// Returns the constant matrix M such that,
            /// given `T = [1, t*t, t*t*t, t*t*t*t]` and `P` the vector of control points,
            /// `dot(T * M, P)` evalutes the Bezier curve at 't'.
            ///
            /// This function name is arguably dubious.
            pub fn matrix() -> Mat4<T> {
                let zero = T::zero();
                let one = T::one();
                let three = one+one+one;
                let six = three + three;
                Mat4 {
                    rows: CVec4::new(
                        Vec4::new( one,  zero,  zero, zero),
                        Vec4::new(-three,  three,  zero, zero),
                        Vec4::new( three, -six,  three, zero),
                        Vec4::new(-one,  three, -three, one),
                    )
                }
            }
            /// Splits this cubic Bézier curve into two curves, at interpolation factor `t`.
            // NOTE that some computations may be reused, but the compiler can
            // reason about these. Clarity wins here IMO.
            pub fn split(self, t: T) -> [Self; 2] {
                let l = T::one();
                let two = l+l;
                let three = l+l+l;
                let first = $CubicBezier {
                    start: self.start,
                    ctrl0: self.ctrl0*t - self.start*(t-l),
                    ctrl1: self.ctrl1*t*t - self.ctrl0*two*t*(t-l) + self.start*(t-l)*(t-l),
                    end:   self.end*t*t*t - self.ctrl1*three*t*t*(t-l) + self.ctrl0*three*t*(t-l)*(t-l) - self.start*(t-l)*(t-l)*(t-l),
                };
                let second = $CubicBezier {
                    start: self.end*t*t*t - self.ctrl1*three*t*t*(t-l) + self.ctrl0*three*t*(t-l)*(t-l) - self.start*(t-l)*(t-l)*(t-l),
                    ctrl0: self.end*t*t - self.ctrl1*two*t*(t-l) + self.ctrl0*(t-l)*(t-l),
                    ctrl1: self.end*t - self.ctrl1*(t-l),
                    end:   self.end,
                };
                [first, second]
            }
            /// Gets the cubic Bézier curve that approximates a unit quarter circle.
            ///
            /// You can build a good-looking circle out of 4 curves by applying
            /// symmetries to this curve.
            pub fn unit_quarter_circle() -> Self {
                let (two, three) = (T::one()+T::one(), T::one()+T::one()+T::one());
                let coeff = (two+two)*(two.sqrt()-T::one())/three;
                Self {
                    start: Vec2::unit_x().into(),
                    ctrl0: (Vec2::unit_x() + Vec2::unit_y()*coeff).into(),
                    ctrl1: (Vec2::unit_x()*coeff + Vec2::unit_y()).into(),
                    end: Vec2::unit_y().into(),
                }
            }
            /// Gets the 4 cubic Bézier curves that, used together, approximate a unit quarter circle.
            ///
            /// The returned tuple is `(north-east, north-west, south-west, south-east)`.
            pub fn unit_circle() -> [Self; 4] {
                let a = Self::unit_quarter_circle();
                let b = a.flipped_x();
                let c = b.flipped_y();
                let d = a.flipped_y();
                [a, b, c, d]
            }
        }
        impl<T> $CubicBezier<T> {
            /// Gets this curve reversed, i.e swaps `start` with `end` and `ctrl0` with `ctrl1`.
            pub fn reversed(mut self) -> Self {
                self.reverse();
                self
            }
            /// Reverses this curve, i.e swaps `start` with `end` and `ctrl0` with `ctrl1`.
            pub fn reverse(&mut self) {
                std::mem::swap(&mut self.start, &mut self.end);
                std::mem::swap(&mut self.ctrl0, &mut self.ctrl1);
            }
            // Convenience for this module
            pub(crate) fn into_vector(self) -> Vec4<Point<T>> {
                self.into_vec4()
            }
            /// Converts this curve into a `Vec4` of points.
            pub fn into_vec4(self) -> Vec4<Point<T>> {
                self.into()
            }
            /// Converts this curve into a tuple of points.
            pub fn into_tuple(self) -> (Point<T>, Point<T>, Point<T>, Point<T>) {
                self.into_vec4().into_tuple()
            }
            /// Converts this curve into an array of points.
            pub fn into_array(self) -> [Point<T>; 4] {
                self.into_vec4().into_array()
            }
        }
        
        impl<T> From<Vec4<Point<T>>> for $CubicBezier<T> {
            fn from(v: Vec4<Point<T>>) -> Self {
                $CubicBezier {
                    start: v.x, 
                    ctrl0: v.y,
                    ctrl1: v.z, 
                    end: v.w
                }
            }
        }
        impl<T> From<$CubicBezier<T>> for Vec4<Point<T>> {
            fn from(v: $CubicBezier<T>) -> Self {
                Vec4::new(v.start, v.ctrl0, v.ctrl1, v.end)
            }
        }
        impl<T: Real + Lerp<T,Output=T>> From<$LineSegment<T>> for $CubicBezier<T> 
        {
            fn from(line_segment: $LineSegment<T>) -> Self {
                let three = T::one() + T::one() + T::one();
                let t = three.recip();
                let ctrl0 = Lerp::lerp_unclamped(line_segment.start, line_segment.end, t);
                let ctrl1 = Lerp::lerp_unclamped(line_segment.start, line_segment.end, t+t);
                Self {
                    start: line_segment.start, 
                    ctrl0, 
                    ctrl1, 
                    end:   line_segment.end
                }
            }
        }
        impl<T: Real + Lerp<T, Output=T>> From<Range<Point<T>>> for $CubicBezier<T> {
            fn from(range: Range<Point<T>>) -> Self {
                Self::from($LineSegment::from(range))
            }
        }
        impl<T: Real> From<$QuadraticBezier<T>> for $CubicBezier<T> {
            fn from(b: $QuadraticBezier<T>) -> Self {
                let three = T::one() + T::one() + T::one();
                $CubicBezier {
                    start: b.start,
                    ctrl0: (b.start + b.ctrl + b.ctrl) / three,
                    ctrl1: (b.end + b.ctrl + b.ctrl) / three,
                    end: b.end,
                }
            }
        }
        
        bezier_impl_cubic_axis!{$CubicBezier $Point ("X") x x_inflections min_x max_x x_bounds}
        bezier_impl_cubic_axis!{$CubicBezier $Point ("Y") y y_inflections min_y max_y y_bounds}
    }
}

macro_rules! impl_all_beziers {
    ($mod:ident) => {
        use  crate::vec::$mod::{Vec3, Vec4, Vec2};
        use  crate::mat::$mod::row_major::{Mat2 as Rows2, Mat3 as Rows3, Mat4 as Rows4};
        use  crate::mat::$mod::column_major::{Mat2 as Cols2, Mat3 as Cols3, Mat4 as Cols4};
        use crate::geom::$mod::{LineSegment2, LineSegment3, Aabr, Aabb};
        use self::Rows4 as Mat4;
        use self::Rows3 as Mat3;

        mod quadratic_bezier2 {
            use super::*;
            bezier_impl_quadratic!{
                /// A 2D Bézier curve with one control point.
                ///
                /// 2x2 and 3x3 matrices can be multiplied by a Bézier curve to transform all of its points.
                2 QuadraticBezier2 CubicBezier2 Vec2 LineSegment2
            }
            bezier_impl_2d_into_3d!{QuadraticBezier2 QuadraticBezier3}
        }

        mod quadratic_bezier3 {
            use super::*;
            bezier_impl_quadratic!{
                /// A 3D Bézier curve with one control point.
                ///
                /// 3x3 and 4x4 matrices can be multiplied by a Bézier curve to transform all of its points.
                3 QuadraticBezier3 CubicBezier3 Vec3 LineSegment3
            }
            bezier_impl_3d_into_2d!{QuadraticBezier3 QuadraticBezier2}
        }


        mod cubic_bezier2 {
            use super::*;
            bezier_impl_cubic!{
                /// A 2D Bézier curve with two control points.
                ///
                /// 2x2 and 3x3 matrices can be multiplied by a Bézier curve to transform all of its points.
                2 QuadraticBezier2 CubicBezier2 Vec2 LineSegment2
            }
            bezier_impl_2d_into_3d!{CubicBezier2 CubicBezier3}
        }


        mod cubic_bezier3 {
            use super::*;
            bezier_impl_cubic!{
                /// A 3D Bézier curve with two control points.
                ///
                /// 3x3 and 4x4 matrices can be multiplied by a Bézier curve to transform all of its points.
                3 QuadraticBezier3 CubicBezier3 Vec3 LineSegment3
            }
            bezier_impl_3d_into_2d!{CubicBezier3 CubicBezier2}
        }

        pub use quadratic_bezier2::*;
        pub use quadratic_bezier3::*;
        pub use cubic_bezier2::*;
        pub use cubic_bezier3::*;
    };
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    //! Bézier curve structs that use `#[repr(simd)]` vectors.
    use super::*;
    impl_all_beziers!{repr_simd}
}
pub mod repr_c {
    //! Bézier curve structs that use `#[repr(C)]` vectors.
    use super::*;
    impl_all_beziers!{repr_c}
}

pub use self::repr_c::*;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vec::{Vec2, Vec3};

    macro_rules! test {
        ($Bezier:ident $bezier:ident $Vec:ident) => {
            mod $bezier {
                use super::*;
                #[test] fn lerp_from_line_segment() {
                    let count = 32;
                    let t_iter = (0..(count+1)).into_iter().map(|i| i as f32 / (count as f32));
                    let l = || $Vec::<f32>::unit_x() .. $Vec::<f32>::unit_y();
                    let c = $Bezier::from(l());
                    for t in t_iter {
                        assert_relative_eq!(c.evaluate(t), Lerp::lerp_unclamped_precise(l().start, l().end, t))
                    }
                }
            }
        };
    }
    test!{QuadraticBezier2 quadratic2 Vec2}
    test!{QuadraticBezier3 quadratic3 Vec3}
    test!{CubicBezier2 cubic2 Vec2}
    test!{CubicBezier3 cubic3 Vec3}
}

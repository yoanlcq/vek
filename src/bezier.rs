//! Bézier curves of low order.
// NOTE: Most info from https://pomax.github.io/bezierinfo

use num_traits::Float;
use std::fmt::Debug;
use ops::*;
use std::ops::*;
use std::iter::Sum;
use vec::repr_c::{
    Vec3 as CVec3,
    Vec4 as CVec4,
};

// WISH: OOBBs from beziers
// WISH: "Tracing a curve at fixed distance intervals"

macro_rules! bezier_impl_any {
    (3 $Bezier:ident $Point:ident) => {
        impl<T: Float> $Bezier<T> {
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
        bezier_impl_any!{$Bezier $Point}
    };
    ($Bezier:ident $Point:ident) => {
        impl<T: Float> $Bezier<T> {
            /// Evaluates the normalized tangent at interpolation factor `t`.
            pub fn normalized_tangent(self, t: T) -> $Point<T> where T: Sum {
                self.evaluate_derivative(t).normalized()
            }
            // WISH: better length approximation estimations (e.g see https://math.stackexchange.com/a/61796)
            /// Approximates the curve's length by subdividing it into step_count+1 segments.
            pub fn length_by_discretization(self, step_count: u32) -> T
                where T: Sum
            {
	            let mut length = T::zero();
	            let mut prev_point = self.evaluate(T::zero());
                for i in 1..step_count+2 {
    		        let t = T::from(i).unwrap()/(T::from(step_count).unwrap()+T::one());
    		        let next_point = self.evaluate(t);
                    length = length + (next_point - prev_point).magnitude();
    		        prev_point = next_point;
                }
	            length
            }

            /// Gets the Axis-Aligned Bounding Rectangle for this curve.
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

            // TODO: Test this! binary_search_point_easy
            pub fn binary_search_point_easy(self, p: $Point<T>, steps: u16, epsilon: T) -> (T, $Point<T>) 
                where T: Sum + From<u16> + Debug
            {
                let it = (0..(steps+1)).map(|i| {
                    let t = <T as From<u16>>::from(i) / <T as From<u16>>::from(steps);
                    (t, self.evaluate(t))
                });
                let h = <T as From<u16>>::from(steps);
                let h = (h+h).recip();
                self.binary_search_point(p, it, h, epsilon)
            }
            // TODO: Test this! binary_search_point
            pub fn binary_search_point<I>(self, p: $Point<T>, coarse: I, half_interval: T, epsilon: T) -> (T, $Point<T>)
                where T: Sum + Debug, I: IntoIterator<Item=(T, $Point<T>)>
            {
                debug_assert_ne!(epsilon, T::zero());
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
        impl<T> Mul<$Bezier<T>> for Rows4<T> where T: Float + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self.mul_point(p).into()).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Cols4<T> where T: Float + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| self.mul_point(p).into()).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Rows3<T> where T: Float + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| (self * Vec3::from(p)).into()).into()
            }
        }
        impl<T> Mul<$Bezier<T>> for Cols3<T> where T: Float + MulAdd<T,T,Output=T> {
            type Output = $Bezier<T>;
            fn mul(self, rhs: $Bezier<T>) -> $Bezier<T> {
                rhs.into_vector().map(|p| (self * Vec3::from(p)).into()).into()
            }
        }
    };
}

macro_rules! bezier_impl_quadratic_axis {
    ($QuadraticBezier:ident $Point:ident $x:ident $x_inflection:ident $x_min:ident $x_max:ident $x_bounds:ident) => {
        impl<T: Float> $QuadraticBezier<T> {
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
            pub fn $x_min(self) -> T {
                if let Some(t) = self.$x_inflection() {
                    let p = self.evaluate(t);
                    if p.$x < self.start.$x && p.$x < self.end.$x {
                        return t;
                    }
                }
                if self.start.$x < self.end.$x { T::zero() } else { T::one() }
            }
            pub fn $x_max(self) -> T {
                if let Some(t) = self.$x_inflection() {
                    let p = self.evaluate(t);
                    if p.$x > self.start.$x && p.$x > self.end.$x {
                        return t;
                    }
                }
                if self.start.$x > self.end.$x { T::zero() } else { T::one() }
            }
            pub fn $x_bounds(self) -> (T, T) {
                // PERF: We don't need to compute $x_inflections twice!
                (self.$x_min(), self.$x_max())
            }
        }
    };
}

macro_rules! bezier_impl_cubic_axis {
    ($CubicBezier:ident $Point:ident $x:ident $x_inflections:ident $x_min:ident $x_max:ident $x_bounds:ident) => {
        impl<T: Float> $CubicBezier<T> {
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
        bezier_impl_quadratic_axis!{$QuadraticBezier $Point z z_inflection min_z max_z z_bounds}
        bezier_impl_any!(3 $QuadraticBezier $Point);
    };
    ($(#[$attrs:meta])* 2 $QuadraticBezier:ident $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        bezier_impl_quadratic!{$(#[$attrs])* $QuadraticBezier $CubicBezier $Point $LineSegment}
        bezier_impl_any!($QuadraticBezier $Point);
    };
    ($(#[$attrs:meta])* $QuadraticBezier:ident $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        
        $(#[$attrs])*
        #[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, /*PartialOrd, Ord*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct $QuadraticBezier<T> {
            pub start: $Point<T>,
            pub ctrl: $Point<T>, 
            pub end: $Point<T>,
        }
        
        impl<T: Float> $QuadraticBezier<T> {
            /// Evaluates the position of the point lying on the curve at interpolation factor `t`.
            ///
            /// This is one of the most important Bézier curve operations,
            /// because, in one way or another, it is used to render a curve
            /// to the screen.
            /// The common use case is to successively evaluate a curve at a set of values
            /// that range from 0 to 1, to approximate the curve as an array of
            /// line segments which are then rendered.
            pub fn evaluate(self, t: T) -> $Point<T> {
                let l = T::one();
                let two = l+l;
                self.start*(l-t)*(l-t) + self.ctrl*two*(l-t)*t + self.end*t*t
            }
            /// Evaluates the derivative tangent at interpolation factor `t`, which happens to give
            /// a non-normalized tangent vector.
            ///
            /// See also `normalized_tangent()`.
            pub fn evaluate_derivative(self, t: T) -> $Point<T> {
                let l = T::one();
                let n = l+l;
                (self.ctrl-self.start)*(l-t)*n + (self.end-self.ctrl)*t*n
            }
            /// Creates a quadratic Bézier curve from a single segment.
            pub fn from_line_segment(line: $LineSegment<T>) -> Self {
                $QuadraticBezier {
                    start: line.start, 
                    ctrl: line.start, 
                    end: line.end
                }
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
            pub fn split(self, t: T) -> (Self, Self) {
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
                (first, second)
            }
            /// Gets this curve reversed, i.e swaps `start` with `end`.
            pub fn reversed(self) -> Self {
                Self {
                    start: self.end,
                    ctrl: self.ctrl,
                    end: self.start,
                }
            }
            /// Reverses this curve, i.e swaps `start` with `end`.
            pub fn reverse(&mut self) {
                *self = self.reversed();
            }

            pub fn into_cubic(self) -> $CubicBezier<T> {
                let three = T::one() + T::one() + T::one();
                $CubicBezier {
                    start: self.start,
                    ctrl0: (self.start + self.ctrl + self.ctrl) / three,
                    ctrl1: (self.end + self.ctrl + self.ctrl) / three,
                    end: self.end,
                }
            }

            /// Converts this curve into a `Vec3` of points.
            pub fn into_vec3(self) -> Vec3<$Point<T>> {
                self.into()
            }
            /// Converts this curve into a tuple of points.
            pub fn into_tuple(self) -> ($Point<T>, $Point<T>, $Point<T>) {
                self.into_vec3().into_tuple()
            }
            /// Converts this curve into an array of points.
            pub fn into_array(self) -> [$Point<T>; 3] {
                self.into_vec3().into_array()
            }
            // Convenience for this module
            pub(crate) fn into_vector(self) -> Vec3<$Point<T>> {
                self.into_vec3()
            }
        }
        
        impl<T> From<Vec3<$Point<T>>> for $QuadraticBezier<T> {
            fn from(v: Vec3<$Point<T>>) -> Self {
                $QuadraticBezier {
                    start: v.x, 
                    ctrl: v.y, 
                    end: v.z
                }
            }
        }
        impl<T> From<$QuadraticBezier<T>> for Vec3<$Point<T>> {
            fn from(v: $QuadraticBezier<T>) -> Self {
                Vec3::new(v.start, v.ctrl, v.end)
            }
        }
        
        bezier_impl_quadratic_axis!{$QuadraticBezier $Point x x_inflection min_x max_x x_bounds}
        bezier_impl_quadratic_axis!{$QuadraticBezier $Point y y_inflection min_y max_y y_bounds}
    }
}

macro_rules! bezier_impl_cubic {
    ($(#[$attrs:meta])* 3 $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        bezier_impl_cubic!{$(#[$attrs])* $CubicBezier $Point $LineSegment}
        bezier_impl_cubic_axis!{$CubicBezier $Point z z_inflections min_z max_z z_bounds}
        bezier_impl_any!(3 $CubicBezier $Point);
    };
    ($(#[$attrs:meta])* 2 $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        bezier_impl_cubic!{$(#[$attrs])* $CubicBezier $Point $LineSegment}
        bezier_impl_any!($CubicBezier $Point);
    };
    ($(#[$attrs:meta])* $CubicBezier:ident $Point:ident $LineSegment:ident) => {
        
        $(#[$attrs])*
        #[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, /*PartialOrd, Ord*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct $CubicBezier<T> {
            pub start: $Point<T>, 
            pub ctrl0: $Point<T>, 
            pub ctrl1: $Point<T>,
            pub end: $Point<T>,
        }

        impl<T: Float> $CubicBezier<T> {
            /// Evaluates the position of the point lying on the curve at interpolation factor `t`.
            ///
            /// This is one of the most important Bézier curve operations,
            /// because, in one way or another, it is used to render a curve
            /// to the screen.
            /// The common use case is to successively evaluate a curve at a set of values
            /// that range from 0 to 1, to approximate the curve as an array of
            /// line segments which are then rendered.
            pub fn evaluate(self, t: T) -> $Point<T> {
                let l = T::one();
                let three = l+l+l;
		        self.start*(l-t)*(l-t)*(l-t) + self.ctrl0*three*(l-t)*(l-t)*t + self.ctrl1*three*(l-t)*t*t + self.end*t*t*t
            }
            /// Evaluates the derivative tangent at interpolation factor `t`, which happens to give
            /// a non-normalized tangent vector.
            ///
            /// See also `normalized_tangent()`.
            pub fn evaluate_derivative(self, t: T) -> $Point<T> {
                let l = T::one();
        	    let n = l+l+l;
                let two = l+l;
        		(self.ctrl0-self.start)*(l-t)*(l-t)*n + (self.ctrl1-self.ctrl0)*two*(l-t)*t*n + (self.end-self.ctrl1)*t*t*n
        	}
            /// Creates a cubic Bézier curve from a single segment.
            pub fn from_line_segment(line: $LineSegment<T>) -> Self {
                $CubicBezier {
                    start: line.start, 
                    ctrl0: line.start, 
                    ctrl1: line.end, 
                    end:   line.end
                }
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
            pub fn split(self, t: T) -> (Self, Self) {
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
                (first, second)
            }
            /// Gets this curve reversed, i.e swaps `start` with `end` and `ctrl0` with `ctrl1`.
            pub fn reversed(self) -> Self {
                Self {
                    start: self.end,
                    ctrl0: self.ctrl1,
                    ctrl1: self.ctrl0,
                    end: self.start,
                }
            }
            /// Reverses this curve, i.e swaps `start` with `end` and `ctrl0` with `ctrl1`.
            pub fn reverse(&mut self) {
                *self = self.reversed();
            }
            /// Converts this curve into a `Vec4` of points.
            pub fn into_vec4(self) -> Vec4<$Point<T>> {
                self.into()
            }
            /// Converts this curve into a tuple of points.
            pub fn into_tuple(self) -> ($Point<T>, $Point<T>, $Point<T>, $Point<T>) {
                self.into_vec4().into_tuple()
            }
            /// Converts this curve into an array of points.
            pub fn into_array(self) -> [$Point<T>; 4] {
                self.into_vec4().into_array()
            }
            // Convenience for this module
            pub(crate) fn into_vector(self) -> Vec4<$Point<T>> {
                self.into_vec4()
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
            pub fn unit_circle() -> (Self, Self, Self, Self) {
                let a = Self::unit_quarter_circle();
                let b = a.flipped_x();
                let c = b.flipped_y();
                let d = a.flipped_y();
                (a, b, c, d)
            }
        }
        
        impl<T> From<Vec4<$Point<T>>> for $CubicBezier<T> {
            fn from(v: Vec4<$Point<T>>) -> Self {
                $CubicBezier {
                    start: v.x, 
                    ctrl0: v.y,
                    ctrl1: v.z, 
                    end: v.w
                }
            }
        }
        impl<T> From<$CubicBezier<T>> for Vec4<$Point<T>> {
            fn from(v: $CubicBezier<T>) -> Self {
                Vec4::new(v.start, v.ctrl0, v.ctrl1, v.end)
            }
        }
        
        bezier_impl_cubic_axis!{$CubicBezier $Point x x_inflections min_x max_x x_bounds}
        bezier_impl_cubic_axis!{$CubicBezier $Point y y_inflections min_y max_y y_bounds}
    }
}

macro_rules! impl_all_beziers {
    () => {
        bezier_impl_quadratic!{
            /// A 2D curve with one control point.
            2 QuadraticBezier2 CubicBezier2 Vec2 LineSegment2
        }
        bezier_impl_quadratic!{
            /// A 3D curve with one control point.
            3 QuadraticBezier3 CubicBezier3 Vec3 LineSegment3
        }
        bezier_impl_cubic!{
            /// A 2D curve with two control points.
            2 CubicBezier2 Vec2 LineSegment2
        }
        bezier_impl_cubic!{
            /// A 3D curve with two control points.
            3 CubicBezier3 Vec3 LineSegment3
        }
    };
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    use super::*;
    use  vec::repr_simd::{Vec3, Vec4, Vec2};
    use  mat::repr_simd::row_major::{Mat3 as Rows3, Mat4 as Rows4};
    use  mat::repr_simd::column_major::{Mat3 as Cols3, Mat4 as Cols4};
    use geom::repr_simd::{LineSegment2, LineSegment3, Aabr, Aabb};
    use self::Rows4 as Mat4;
    use self::Rows3 as Mat3;
    impl_all_beziers!{}
}
pub mod repr_c {
    use super::*;
    use  vec::repr_c::{Vec3, Vec4, Vec2};
    use  mat::repr_c::row_major::{Mat3 as Rows3, Mat4 as Rows4};
    use  mat::repr_c::column_major::{Mat3 as Cols3, Mat4 as Cols4};
    use geom::repr_c::{LineSegment2, LineSegment3, Aabr, Aabb};
    use self::Rows4 as Mat4;
    use self::Rows3 as Mat3;
    impl_all_beziers!{}
}

pub use self::repr_c::*;

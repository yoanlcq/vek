// https://pomax.github.io/bezierinfo

use num_traits::Float;
use core::ops::*;
use core::iter::Sum;
use vec::repr_c::{
    Vec3 as CVec3,
    Vec4 as CVec4,
};

// WISH: into_iter, iter_mut, etc (for concisely applying the same xform to all points)
// WISH: AABBs from beziers
// WISH: OOBBs from beziers
// WISH: "Tracing a curve at fixed distance intervals"
// WISH: project a point on a curve using e.g binary search after a coarse linear search

macro_rules! bezier_impl_any {
    ($Bezier:ident $Point:ident) => {
        impl<T> $Bezier<T> {
            pub fn normalized_tangent(self, t: T) -> $Point<T> where T: Float + Sum {
                self.evaluate_derivative(t).normalized()
            }
            // WISH: better length approximation estimations (e.g see https://math.stackexchange.com/a/61796)
            /// Approximates the curve's length by subdividing it into step_count+1 segments.
            pub fn length_by_discretization(self, step_count: u32) -> T
                where T: Float + AddAssign + Sum
            {
	            let mut length = T::zero();
	            let mut prev_point = self.evaluate(T::zero());
                for i in 1..step_count+2 {
    		        let t = T::from(i).unwrap()/(T::from(step_count).unwrap()+T::one());
    		        let next_point = self.evaluate(t);
                    length += (next_point - prev_point).magnitude();
    		        prev_point = next_point;
                }
	            length
            }

        }
    }
}

macro_rules! bezier_impl_quadratic {
    ($QuadraticBezier:ident $Point:ident $Line:ident) => {
        
        #[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, /*PartialOrd, Ord*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct $QuadraticBezier<T>(pub $Point<T>, pub $Point<T>, pub $Point<T>);
        
        impl<T: Float> $QuadraticBezier<T> {
            pub fn evaluate(self, t: T) -> $Point<T> {
                let l = T::one();
                let two = l+l;
                self.0*(l-t)*(l-t) + self.1*two*(l-t)*t + self.2*t*t
            }
            pub fn evaluate_derivative(self, t: T) -> $Point<T> {
                let l = T::one();
                let n = l+l;
                (self.1-self.0)*(l-t)*n + (self.2-self.1)*t*n
            }
            pub fn from_line(line: $Line<T>) -> Self {
                $QuadraticBezier(line.a, line.a, line.b)
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
                        Vec3( one,  zero, zero),
                        Vec3(-two,  two, zero),
                        Vec3( one, -two, one),
                    )
                }
            }
            // NOTE that some computations may be reused, but the compiler can
            // reason about these. Clarity wins here IMO.
            pub fn split(self, t: T) -> (Self, Self) {
                let l = T::one();
                let two = l+l;
                let first = $QuadraticBezier(
                    self.0,
                    self.1*t - self.0*(t-l),
                    self.2*t*t - self.1*two*t*(t-l) + self.0*(t-l)*(t-l),
                );
                let second = $QuadraticBezier(
                    self.2*t*t - self.1*two*t*(t-l) + self.0*(t-l)*(t-l),
                    self.2*t - self.1*(t-l),
                    self.2,
                );
                (first, second)
            }
        }
        
        impl<T> From<Vec3<$Point<T>>> for $QuadraticBezier<T> {
            fn from(v: Vec3<$Point<T>>) -> Self {
                $QuadraticBezier(v.0, v.1, v.2)
            }
        }
        impl<T> From<$QuadraticBezier<T>> for Vec3<$Point<T>> {
            fn from(v: $QuadraticBezier<T>) -> Self {
                Vec3(v.0, v.1, v.2)
            }
        }
        
        bezier_impl_any!($QuadraticBezier $Point);
    }
}

macro_rules! bezier_impl_cubic {
    ($CubicBezier:ident $Point:ident $Line:ident) => {
        
        #[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, /*PartialOrd, Ord*/)]
		#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct $CubicBezier<T>(pub $Point<T>, pub $Point<T>, pub $Point<T>, pub $Point<T>);

        impl<T: Float> $CubicBezier<T> {
            pub fn evaluate(self, t: T) -> $Point<T> {
                let l = T::one();
                let three = l+l+l;
		        self.0*(l-t)*(l-t)*(l-t) + self.1*three*(l-t)*(l-t)*t + self.2*three*(l-t)*t*t + self.3*t*t*t
            }
            pub fn evaluate_derivative(self, t: T) -> $Point<T> {
                let l = T::one();
        	    let n = l+l+l;
                let two = l+l;
        		(self.1-self.0)*(l-t)*(l-t)*n + (self.2-self.1)*two*(l-t)*t*n + (self.3-self.2)*t*t*n
        	}
            pub fn from_line(line: $Line<T>) -> Self {
                $CubicBezier(line.a, line.a, line.b, line.b)
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
                        Vec4( one,  zero,  zero, zero),
                        Vec4(-three,  three,  zero, zero),
                        Vec4( three, -six,  three, zero),
                        Vec4(-one,  three, -three, one),
                    )
                }
            }
            // NOTE that some computations may be reused, but the compiler can
            // reason about these. Clarity wins here IMO.
            pub fn split(self, t: T) -> (Self, Self) {
                let l = T::one();
                let two = l+l;
                let three = l+l+l;
                let first = $CubicBezier(
                    self.0,
                    self.1*t - self.0*(t-l),
                    self.2*t*t - self.1*two*t*(t-l) + self.0*(t-l)*(t-l),
                    self.3*t*t*t - self.2*three*t*t*(t-l) + self.1*three*t*(t-l)*(t-l) - self.0*(t-l)*(t-l)*(t-l),
                );
                let second = $CubicBezier(
                    self.3*t*t*t - self.2*three*t*t*(t-l) + self.1*three*t*(t-l)*(t-l) - self.0*(t-l)*(t-l)*(t-l),
                    self.3*t*t - self.2*two*t*(t-l) + self.1*(t-l)*(t-l),
                    self.3*t - self.2*(t-l),
                    self.3,
                );
                (first, second)
            }
            // WISH: CubicBezier::circle(radius)
            // pub fn circle(radius: T, curve_count: u32) ->
        }
        
        impl<T> From<Vec4<$Point<T>>> for $CubicBezier<T> {
            fn from(v: Vec4<$Point<T>>) -> Self {
                $CubicBezier(v.0, v.1, v.2, v.3)
            }
        }
        impl<T> From<$CubicBezier<T>> for Vec4<$Point<T>> {
            fn from(v: $CubicBezier<T>) -> Self {
                Vec4(v.0, v.1, v.2, v.3)
            }
        }
        
        bezier_impl_any!($CubicBezier $Point);
    }
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    use super::*;
    use vec::repr_simd::{Vec3, Vec4, Xy, Xyz};
    use mat::repr_simd::row_major::{Mat3, Mat4};
    use geom::repr_simd::{Line2, Line3};
    bezier_impl_quadratic!(QuadraticBezier2 Xy Line2);
    bezier_impl_quadratic!(QuadraticBezier3 Xyz Line3);
    bezier_impl_cubic!(CubicBezier2 Xy Line2);
    bezier_impl_cubic!(CubicBezier3 Xyz Line3);
}
pub mod repr_c {
    use super::*;
    use  vec::repr_c::{Vec3, Vec4, Xy, Xyz};
    use  mat::repr_c::row_major::{Mat3, Mat4};
    use geom::repr_c::{Line2, Line3};
    bezier_impl_quadratic!(QuadraticBezier2 Xy Line2);
    bezier_impl_quadratic!(QuadraticBezier3 Xyz Line3);
    bezier_impl_cubic!(CubicBezier2 Xy Line2);
    bezier_impl_cubic!(CubicBezier3 Xyz Line3);
}

#[cfg(all(nightly, feature="repr_simd"))]
pub use self::repr_simd::*;
#[cfg(not(all(nightly, feature="repr_simd")))]
pub use self::repr_c::*;

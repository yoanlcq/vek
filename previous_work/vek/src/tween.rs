extern crate num_traits;

use self::num_traits::{Zero};
use core::ops::{Range, Sub};

use clamp::Clamp01;
use lerp::Lerp;

/// A convenience structure for storing a progression from one value to another.
#[derive(Debug, Default, Clone, Copy, Hash)]
pub struct Tween<T, Progress=f32> {
    /// The value when progress gets close to 0.
    pub start: T,
    /// The value when progress gets close to 1.
    pub end: T,
    /// Expected to be between 0 and 1.
    pub progress: Progress
}
impl<T, Progress> From<Range<T>> for Tween<T, Progress> where Progress: Zero {
    fn from(r: Range<T>) -> Self {
        Tween { start: r.start, end: r.end, progress: Zero::zero() }
    }
}
impl<T, Progress> Tween<T, Progress> where T: Lerp<Progress>, Progress: Clone + Clamp01 + Sub<Output=Progress> {
    /// Gets the transition's current state, clamping progress to [0;1].
    pub fn current(self) -> T {
        T::lerp(self.start, self.end, self.progress)
    }
    /// Gets the transition's current state using the `progress` value as-is.
    pub fn current_unclamped(self) -> T {
        T::lerp_unclamped(self.start, self.end, self.progress)
    }
    pub fn into_range(self) -> Range<T> {
        let Tween { start, end, .. } = self;
        Range { start, end }
    }
    pub fn to_range(&self) -> Range<T> where T: Clone {
        let &Tween { ref start, ref end, .. } = self;
        Range { start: start.clone(), end: end.clone() }
    }
}

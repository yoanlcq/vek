//! A convenience structure for storing a progression from one value to another.

use num_traits::{Zero, One};
use std::ops::Range;
use ops::{Lerp, Clamp};

/// A convenience structure for storing a progression from one value to another.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Tween<T, Progress=f32> {
    /// The value when progress gets close to 0.
    pub start: T,
    /// The value when progress gets close to 1.
    pub end: T,
    /// Expected to be between 0 and 1.
    pub progress: Progress
}
impl<T: Default, Progress: Zero> Default for Tween<T, Progress> {
    fn default() -> Self {
        Self {
            start: T::default(),
            end: T::default(),
            progress: Progress::zero(),
        }
    }
}
impl<T, Progress: Zero> From<Range<T>> for Tween<T, Progress> {
    fn from(r: Range<T>) -> Self {
        Tween { start: r.start, end: r.end, progress: Zero::zero() }
    }
}
impl<T, Progress> Tween<T, Progress> {
    /// Creates a new `Tween` from `start` and `end` values, setting `progress` to zero.
    pub fn new(start: T, end: T) -> Self where Progress: Zero {
        Self { start, end, progress: Zero::zero() }
    }
    /// Creates a new `Tween` from `start`, `end` and `progress` values.
    pub fn with_progress(start: T, end: T, progress: Progress) -> Self {
        Self { start, end, progress }
    }
    /// Gets the transition's current state, clamping progress to [0;1].
    pub fn get(self) -> T where T: Lerp<Progress,Output=T>, Progress: Clamp + Zero + One {
        T::lerp(self.start, self.end, self.progress)
    }
    /// Gets the transition's current state using the `progress` value as-is.
    pub fn get_unclamped(self) -> T where T: Lerp<Progress,Output=T> {
        T::lerp_unclamped(self.start, self.end, self.progress)
    }
    /// Converts this into a `Range`, dropping the `progress` value.
    pub fn into_range(self) -> Range<T> {
        let Tween { start, end, .. } = self;
        Range { start, end }
    }
}

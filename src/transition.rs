//! Convenience structures for representing a transition from one value to another.
//!
//! The `Transition` struct is useful in animation, especially since it allows
//! you to customize the interpolation function via what's called a "Progress Mapper", i.e
//! an object that also acts as a function which, given a "progress" value, returns
//! the LERP factor to use.
//!
//! This is especially powerful since it allows for any kind of interpolation curve :
//! exponential, logarithmic, sines, squares... One could even imagine a Progress Mapper based
//! on Bézier curves.

// WISH: Another kind of Transition that not only takes two ends, but any
// number of them. There should be two :
// - TransitionFrames, that take I: IntoIterator<Item=T>
// - RefTransitionFrames, that take I: IntoIterator<Item=&T>

use num_traits::{Zero, One};
use std::ops::Range;
use crate::ops::{Lerp, Clamp};

/// A functor that maps a progress value to a LERP factor.
pub trait ProgressMapper<Progress=f32> {
    /// Maps an unconstrained progress value to a LERP factor.
    ///
    /// The input progress value should be between zero and one, but is not required to.  
    /// The returned LERP factor should be between zero and one, but is not required to.
    fn map_progress(&self, progress: Progress) -> Progress;
}

/// A pass-through functor that returns progress values directly as LERP factors.
#[derive(Debug, Copy, Clone, Default, Hash, PartialEq, Eq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct IdentityProgressMapper;

impl<Progress> ProgressMapper<Progress> for IdentityProgressMapper {
    #[inline]
    fn map_progress(&self, progress: Progress) -> Progress {
        progress
    }
}

/// A function pointer container that can map a progress value to a LERP factor.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
// #[cfg_attr(feature="serde", derive(Serialize, Deserialize))] NOTE: Fails with cargo test.
pub struct ProgressMapperFn<T>(pub fn(T) -> T);

/// The default value for a `ProgressMapperFn` is the identity function.
impl<T> Default for ProgressMapperFn<T> {
    fn default() -> Self {
        fn identity<T>(x: T) -> T { x }
        ProgressMapperFn(identity)
    }
}

impl<T> From<fn(T) -> T> for ProgressMapperFn<T> {
    fn from(f: fn(T) -> T) -> Self {
        ProgressMapperFn(f)
    }
}

// Many progress
// Much wow
impl<Progress> ProgressMapper<Progress> for ProgressMapperFn<Progress> {
    fn map_progress(&self, progress: Progress) -> Progress {
        self.0(progress)
    }
}



/// A convenience structure for storing a progression from one value to another.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct Transition<T, F, Progress=f32> {
    /// The value when progress gets close to 0.
    pub start: T,
    /// The value when progress gets close to 1.
    pub end: T,
    /// Expected to be between 0 and 1.
    pub progress: Progress,
    /// Functor that maps the current progress value to a LERP factor.
    pub progress_mapper: F,
}
impl<T: Default, F: Default, Progress: Zero> Default for Transition<T, F, Progress> {
    fn default() -> Self {
        Self {
            start: T::default(),
            end: T::default(),
            progress: Progress::zero(),
            progress_mapper: F::default(),
        }
    }
}
impl<T, F: Default, Progress: Zero> From<Range<T>> for Transition<T, F, Progress> {
    fn from(r: Range<T>) -> Self {
        Transition { start: r.start, end: r.end, progress: Zero::zero(), progress_mapper: F::default() }
    }
}
impl<T, F, Progress> Transition<T, F, Progress> {
    /// Creates a new `Transition` from `start` and `end` values and `progress_mapper`, setting `progress` to zero.
    pub fn with_mapper(start: T, end: T, progress_mapper: F) -> Self where Progress: Zero {
        Self { start, end, progress_mapper, progress: Zero::zero() }
    }
    /// Creates a new `Transition` from `start`, `end`, `progress_mapper` and `progress` values.
    pub fn with_mapper_and_progress(start: T, end: T, progress_mapper: F, progress: Progress) -> Self {
        Self { start, end, progress_mapper, progress }
    }
    /// Converts this into a `Range`, dropping the `progress` and `progress_mapper` values.
    pub fn into_range(self) -> Range<T> {
        let Self { start, end, .. } = self;
        Range { start, end }
    }
}
impl<T, F, Progress> Transition<T, F, Progress> where F: ProgressMapper<Progress> {
    /// Gets the transition's current state, clamping progress to [0;1].
    pub fn into_current(self) -> T where T: Lerp<Progress,Output=T>, Progress: Clamp + Zero + One {
        Lerp::lerp(self.start, self.end, self.progress_mapper.map_progress(self.progress))
    }
    /// Gets the transition's current state using the `progress` value as-is.
    pub fn into_current_unclamped(self) -> T where T: Lerp<Progress,Output=T> {
        Lerp::lerp_unclamped(self.start, self.end, self.progress_mapper.map_progress(self.progress))
    }
    /// Gets the transition's current state, clamping progress to [0;1].
    pub fn into_current_precise(self) -> T where T: Lerp<Progress,Output=T>, Progress: Clamp + Zero + One {
        Lerp::lerp_precise(self.start, self.end, self.progress_mapper.map_progress(self.progress))
    }
    /// Gets the transition's current state using the `progress` value as-is.
    pub fn into_current_unclamped_precise(self) -> T where T: Lerp<Progress,Output=T> {
        Lerp::lerp_unclamped_precise(self.start, self.end, self.progress_mapper.map_progress(self.progress))
    }
    /// Gets the transition's current state, clamping progress to [0;1].
    pub fn current<'a>(&'a self) -> T where &'a T: Lerp<Progress,Output=T>, Progress: Copy + Clamp + Zero + One {
        Lerp::lerp(&self.start, &self.end, self.progress_mapper.map_progress(self.progress))
    }
    /// Gets the transition's current state using the `progress` value as-is.
    pub fn current_unclamped<'a>(&'a self) -> T where &'a T: Lerp<Progress,Output=T>, Progress: Copy {
        Lerp::lerp_unclamped(&self.start, &self.end, self.progress_mapper.map_progress(self.progress))
    }
    /// Gets the transition's current state, clamping progress to [0;1].
    pub fn current_precise<'a>(&'a self) -> T where &'a T: Lerp<Progress,Output=T>, Progress: Copy + Clamp + Zero + One {
        Lerp::lerp_precise(&self.start, &self.end, self.progress_mapper.map_progress(self.progress))
    }
    /// Gets the transition's current state using the `progress` value as-is.
    pub fn current_unclamped_precise<'a>(&'a self) -> T where &'a T: Lerp<Progress,Output=T>, Progress: Copy {
        Lerp::lerp_unclamped_precise(&self.start, &self.end, self.progress_mapper.map_progress(self.progress))
    }
}

/// A convenience structure for storing a linear progression from one value to another.
pub type LinearTransition<T,Progress=f32> = Transition<T, IdentityProgressMapper, Progress>;

impl<T, Progress> LinearTransition<T, Progress> {
    /// Creates a new `LinearTransition` from `start` and `end` values, setting `progress` to zero.
    pub fn new(start: T, end: T) -> Self where Progress: Zero {
        Self { start, end, progress: Zero::zero(), progress_mapper: Default::default() }
    }
    /// Creates a new `LinearTransition` from `start`, `end` and `progress` values.
    pub fn with_progress(start: T, end: T, progress: Progress) -> Self {
        Self { start, end, progress, progress_mapper: Default::default() }
    }
}

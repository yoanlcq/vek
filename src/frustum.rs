//! The `FrustumPlanes` type.
//!
//! Other functionality could be added to this module later.  
//! It is not part of `geom` because it is required by some of `Mat4`'s functions.

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


// NOTE: Don't implement Default
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


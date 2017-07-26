extern crate num_traits;

use self::num_traits::Zero;
use core::num::Wrapping;

/// Trait for types that are suitable for representing a color component value.
pub trait ColorComponent : Zero {
    /// The minimum value such that the color is at its maximum.
    ///
    /// In pratice, it yields `T::MAX` for integers and `1` for real number types.
    fn full() -> Self;
}

impl ColorComponent for f32 { fn full() -> Self { 1f32 } }
impl ColorComponent for f64 { fn full() -> Self { 1f64 } }
impl ColorComponent for u8  { fn full() -> Self { ::core::u8  ::MAX } }
impl ColorComponent for u16 { fn full() -> Self { ::core::u16 ::MAX } }
impl ColorComponent for u32 { fn full() -> Self { ::core::u32 ::MAX } }
impl ColorComponent for u64 { fn full() -> Self { ::core::u64 ::MAX } }
//impl ColorComponent for u128{ fn full() -> Self { ::core::u128::MAX } }
impl ColorComponent for i8  { fn full() -> Self { ::core::i8  ::MAX } }
impl ColorComponent for i16 { fn full() -> Self { ::core::i16 ::MAX } }
impl ColorComponent for i32 { fn full() -> Self { ::core::i32 ::MAX } }
impl ColorComponent for i64 { fn full() -> Self { ::core::i64 ::MAX } }
//impl ColorComponent for i128{ fn full() -> Self { ::core::i128::MAX } }
impl ColorComponent for Wrapping<u8 >  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<u16>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<u32>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<u64>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
//impl ColorComponent for Wrapping<u128> { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<i8 >  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<i16>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<i32>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
impl ColorComponent for Wrapping<i64>  { fn full() -> Self { Wrapping(ColorComponent::full()) } }
//impl ColorComponent for Wrapping<i128> { fn full() -> Self { Wrapping(ColorComponent::full()) } }


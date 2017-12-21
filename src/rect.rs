macro_rules! rect_complete_mod {
    ($mod:ident) => {

        use vec::$mod::*;

        #[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, /*Ord, PartialOrd*/)]
        #[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
        pub struct Rect<P, E> {
            /// X position of the top-left corner.
            pub x: P,
            /// Y position of the top-left corner.
            pub y: P,
            /// Width.
            pub w: E,
            /// Height, with Y axis going downwards.
            pub h: E,
        }

        impl<P,E> Rect<P,E> {
            pub fn new(x: P, y: P, w: E, h: E) -> Self {
                Self { x, y, w, h }
            }
            pub fn position(self) -> Vec2<P> {
                let Self { x, y, .. } = self;
                Vec2 { x, y }
            }
            pub fn extent(self) -> Extent2<E> {
                let Self { w, h, .. } = self;
                Extent2 { w, h }
            }
            pub fn into_pair(self) -> (Vec2<P>, Extent2<E>) {
                let Self { x, y, w, h } = self;
                (Vec2 { x, y }, Extent2 { w, h })
            }
            pub fn convert<DP,DE,PF,EF>(self, pf: PF, ef: EF) -> Rect<DP,DE>
                where PF: Fn(P) -> DP, EF: Fn(E) -> DE
            {
                let Self { x, y, w, h } = self;
                let Vec2 { x, y } = Vec2 { x, y }.map(pf);
                let Extent2 { w, h } = Extent2 { w, h }.map(ef);
                Rect { x, y, w, h }
            }
            /*
            pub fn collision(self, _other: Self) -> Vec2<P> {
                unimplemented!()    
            }
            pub fn split_v(self, _from_left: E) -> (Self, Self) {
                unimplemented!()
            }
            pub fn split_h(self, _from_top: E) -> (Self, Self) {
                unimplemented!()
            }
            pub fn split(self, _from_topleft: Extent2<E>) -> Mat2<Self> {
                unimplemented!()
            }
            */
        }

        impl<P,E> From<(Vec2<P>, Extent2<E>)> for Rect<P,E> {
            fn from(t: (Vec2<P>, Extent2<E>)) -> Self {
                let (Vec2 { x, y }, Extent2 { w, h }) = t;
                Self { x, y, w, h }
            }
        }
        impl<P,E> From<(P,P,E,E)> for Rect<P,E> {
            fn from(t: (P,P,E,E)) -> Self {
                Self::new(t.0, t.1, t.2, t.3)
            }
        }
    }
}

#[cfg(all(nightly, feature="repr_simd"))]
pub mod repr_simd {
    rect_complete_mod!(repr_simd);
}

pub mod repr_c {
    rect_complete_mod!(repr_c);
}
pub use self::repr_c::*;

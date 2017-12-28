// TODO:
// lerp
// union
// intersection


macro_rules! geom_impl_rect_or_rect3 {
    (
        $Rect:ident $Vec:ident $Extent:ident ($($p:ident $split_at_p:ident)+) ($($e:ident)+)
        $Aab:ident $into_aab:ident
        collides_with_rect: $collides_with_rect:ident
        collides_with_aab: $collides_with_aab:ident
        collision_vector_with_rect: $collision_vector_with_rect:ident
        collision_vector_with_aab: $collision_vector_with_aab:ident
    ) => {
        impl<P,E> $Rect<P,E> {
            pub fn new($($p: P,)+ $($e: E),+) -> Self {
                Self { $($p,)+ $($e),+ }
            }
            pub fn position(self) -> $Vec<P> {
                let Self { $($p,)+ .. } = self;
                $Vec { $($p,)+ }
            }
            pub fn extent(self) -> $Extent<E> {
                let Self { $($e,)+ .. } = self;
                $Extent { $($e,)+ }
            }
            pub fn into_pair(self) -> ($Vec<P>, $Extent<E>) {
                let Self { $($p,)+ $($e,)+ } = self;
                ($Vec { $($p,)+ }, $Extent { $($e,)+ })
            }
            pub fn map<DP,DE,PF,EF>(self, pf: PF, ef: EF) -> $Rect<DP,DE>
                where PF: FnMut(P) -> DP, EF: FnMut(E) -> DE
            {
                let Self { $($p,)+ $($e,)+ } = self;
                let $Vec { $($p,)+ } = $Vec { $($p,)+ }.map(pf);
                let $Extent { $($e,)+ } = $Extent { $($e,)+ }.map(ef);
                $Rect { $($p,)+ $($e,)+ }
            }
        }
        impl<T: Copy + Add<T, Output=T>> $Rect<T,T> {
            pub fn $into_aab(self) -> $Aab<T> {
                self.into()
            }
            pub fn contains_point(self, p: $Vec<T>) -> bool where T: PartialOrd {
                self.$into_aab().contains_point(p)
            }
            pub fn $collides_with_rect(self, other: Self) -> bool where T: PartialOrd {
                self.$into_aab().$collides_with_aab(other.into())
            }
            pub fn center(self) -> $Vec<T> where T: One + Div<T,Output=T> {
                self.$into_aab().center()
            }
            /// Gets a vector that tells how much `self` penetrates `other`.
            pub fn $collision_vector_with_rect(self, other: Self) -> $Vec<T> 
                where T: PartialOrd + Sub<T, Output=T> + One + Div<T,Output=T>
            {
                self.$into_aab().$collision_vector_with_aab(other.into())
            }
            $(
            pub fn $split_at_p(self, sp: T) -> (Self, Self) where T: PartialOrd + Sub<T, Output=T> {
                let (low, high) = self.$into_aab().$split_at_p(sp);
                (low.into(), high.into())
            }
            )+
        }
        impl<P,E> From<($Vec<P>, $Extent<E>)> for $Rect<P,E> {
            fn from(t: ($Vec<P>, $Extent<E>)) -> Self {
                let ($Vec { $($p,)+ }, $Extent { $($e,)+ }) = t;
                Self { $($p,)+ $($e,)+ }
            }
        }
        impl<T> From<$Aab<T>> for $Rect<T,T> 
            where T: Copy + Sub<T, Output=T>
        {
            fn from(aab: $Aab<T>) -> Self {
                let $Extent { $($e,)+ } = (aab.max - aab.min).into();
                Self {
                    $($p: aab.min.$p,)+
                    $($e,)+
                }
            }
        }
        impl<T> From<$Rect<T,T>> for $Aab<T> 
            where T: Copy + Add<T, Output=T>
        {
            fn from(rect: $Rect<T,T>) -> Self {
                Self {
                    min: rect.position(),
                    max: rect.position() + rect.extent(),
                }
            }
        }
    };
}
macro_rules! geom_impl_aabr_or_aabb {
    (
        $Aab:ident $Vec:ident ($($p:ident $split_at_p:ident)+)
        $Rect:ident $into_rect:ident
        collides_with_aab: $collides_with_aab:ident
        collision_vector_with_aab: $collision_vector_with_aab:ident
    ) => {
        impl<T> $Aab<T> {
            pub fn $into_rect(self) -> $Rect<T,T> 
                where T: Copy + Sub<T, Output=T>
            {
                self.into()
            }
            pub fn center(self) -> $Vec<T>
                where T: Copy + One + Add<T,Output=T> + Div<T,Output=T>
            {
                (self.min + self.max) / (T::one() + T::one())
            }
            pub fn contains_point(self, p: $Vec<T>) -> bool 
                where T: PartialOrd
            {
                true $(&& self.min.$p <= p.$p && p.$p <= self.max.$p)+
            }
            pub fn $collides_with_aab(self, other: Self) -> bool 
                where T: PartialOrd
            {
                true $(&& self.max.$p > other.min.$p && self.min.$p < other.max.$p)+
            }
            /// Gets a vector that tells how much `self` penetrates `other`.
            pub fn $collision_vector_with_aab(self, other: Self) -> $Vec<T> 
                where T: Copy + PartialOrd + Sub<T, Output=T> + One + Add<T,Output=T> + Div<T,Output=T>
            {
                let (b1, b2) = (self, other);
                let (c1, c2) = (b1.center(), b2.center());
                $Vec { $($p: if c1.$p < c2.$p {
                    b1.max.$p - b2.min.$p
                } else {
                    b1.min.$p - b2.max.$p
                }),+}
            }
            $(
            pub fn $split_at_p(self, sp: T) -> (Self, Self) where T: Copy + PartialOrd {
                debug_assert!(sp >= self.min.$p);
                debug_assert!(sp <= self.max.$p);
                let low = Self {
                    min: self.min,
                    max: { let mut v = self.max; v.$p = sp; v },
                };
                let high = Self {
                    min: { let mut v = self.min; v.$p = sp; v },
                    max: self.max,
                };
                (low, high)
            }
            )+

            pub fn map<D,F>(self, mut f: F) -> $Aab<D> where F: FnMut(T) -> D
            {
                let Self { min, max } = self;
                let $Vec { $($p,)+ } = min;
                let min = $Vec { $($p: f($p),)+ };
                let $Vec { $($p,)+ } = max;
                let max = $Vec { $($p: f($p),)+ };
                $Aab { min, max }
            }
        }
    };
}
macro_rules! geom_impl_disk_or_sphere {
    (
        $Shape:ident $Vec:ident ($($p:ident)+)
        $Extent:ident
        $Rect:ident
        $Aab:ident $aab:ident
        collides_with_other: $collides_with_other:ident
        collision_vector_with_other: $collision_vector_with_other:ident
    ) => {
        impl<P,E> $Shape<P,E> {
            pub fn new(center: $Vec<P>, radius: E) -> Self {
                Self { center, radius }
            }
            pub fn unit(center: $Vec<P>) -> Self where E: One {
                Self { center, radius: One::one() }
            }
            pub fn point(center: $Vec<P>) -> Self where E: Zero {
                Self { center, radius: Zero::zero() }
            }
            pub fn diameter(self) -> E where E: Copy + Add<Output=E> {
                self.radius + self.radius
            }
            pub fn rect(self) -> $Rect<P,E> 
                where P: Sub<P,Output=P> + From<E> + Copy, E: Copy + Add<E,Output=E>
            {
                $Rect::from((
                    self.center - P::from(self.radius),
                    $Extent::broadcast(self.diameter())
                ))
            }
        }

        impl<T> $Shape<T,T> where T: Copy + Add<T,Output=T> + Sub<T,Output=T> {
            pub fn $aab(self) -> $Aab<T> {
                $Aab {
                    min: self.center - self.radius,
                    max: self.center + self.radius,
                }
            }
        }
        impl<T: Float + Sum> $Shape<T,T> {
            pub fn contains_point(self, p: $Vec<T>) -> bool where T: PartialOrd {
                self.center.distance(p) <= self.radius
            }
            pub fn $collides_with_other(self, other: Self) -> bool where T: PartialOrd {
                self.center.distance(other.center) <= (self.radius + other.radius)
            }
            // XXX: This remains to be tested!
            /// Gets a vector that tells how much this shape penetrates another.
            pub fn $collision_vector_with_other(self, other: Self) -> $Vec<T> {
                let v = other.center - self.center;
                let mag = self.radius + other.radius - v.magnitude();
                v.normalized() * mag
            }
        }
    };
}

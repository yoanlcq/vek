// This file is not named "test" in order to avoid conflicts with the "test" crate.

#[derive(Debug, Default)]
pub struct Rc<T> {
    val: T,
    refcount_minus_one: isize,
}

impl<T> Drop for Rc<T> {
    fn drop(&mut self) {
        self.refcount_minus_one -= 1;
    }
}

impl<T> Rc<T> {
    pub fn new(val: T) -> Self {
        Self { val, refcount_minus_one: 0 }
    }
    pub fn strong_count(s: &Self) -> usize {
        (s.refcount_minus_one + 1) as usize
    }
    pub fn make_mut(s: &mut Self) -> &mut T {
        &mut s.val
    }
}
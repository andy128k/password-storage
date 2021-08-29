pub trait Also: Sized {
    fn also(self, also: impl FnMut(&mut Self)) -> Self;
}

impl<T> Also for T {
    fn also(mut self, mut also: impl FnMut(&mut Self)) -> Self {
        (also)(&mut self);
        self
    }
}

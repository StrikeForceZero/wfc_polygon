pub trait IsSomeAndSame {
    fn is_some_and_same(&self, other: &Self) -> bool;
}

impl<T> IsSomeAndSame for Option<T>
where
    T: PartialEq,
{
    fn is_some_and_same(&self, other: &Self) -> bool {
        match (self, other) {
            (None, _) | (_, None) => false,
            (Some(a), Some(b)) => a == b,
        }
    }
}

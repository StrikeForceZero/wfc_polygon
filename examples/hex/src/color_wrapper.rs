/// Wrapper to allow hashing `Color` with up to 2 decimals places of precision for each f32 value in equality checks
use std::hash::{Hash, Hasher};

use bevy::prelude::Color;

#[derive(Debug, Copy, Clone)]
pub(crate) struct ColorWrapper(pub(crate) Color);

impl PartialEq for ColorWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for ColorWrapper {}

impl ColorWrapper {
    pub fn id(&self) -> [usize; 4] {
        let r = (self.0.r() * 100.0) as usize;
        let g = (self.0.g() * 100.0) as usize;
        let b = (self.0.b() * 100.0) as usize;
        let a = (self.0.a() * 100.0) as usize;
        [r, g, b, a]
    }
}

impl Hash for ColorWrapper {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

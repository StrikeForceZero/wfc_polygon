use bevy::prelude::*;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
pub enum HexSegmentId {
    #[default]
    None,
    Grass,
    Mountain,
    MountainPeak,
    River,
    Ocean,
    Sand,
}

impl HexSegmentId {
    pub fn all() -> Vec<Self> {
        vec![
            Self::Grass,
            Self::Mountain,
            Self::MountainPeak,
            Self::River,
            Self::Ocean,
            Self::Sand,
        ]
    }
    pub fn compatible(&self) -> Vec<Self> {
        match self {
            Self::None => vec![],
            Self::Grass => vec![Self::Grass, Self::Sand, Self::Mountain],
            Self::Mountain => vec![Self::Mountain, Self::MountainPeak, Self::Grass],
            Self::MountainPeak => vec![Self::Mountain],
            Self::River => vec![Self::River, Self::Ocean, Self::Sand],
            Self::Ocean => vec![Self::Ocean, Self::River],
            Self::Sand => vec![Self::Sand, Self::Grass, Self::River],
        }
    }
    pub fn probability(&self) -> f64 {
        match self {
            Self::None => 0.0,
            Self::Grass => 0.8,
            Self::Mountain => 0.25,
            Self::MountainPeak => 0.5,
            Self::River => 0.6,
            Self::Ocean => 0.7,
            Self::Sand => 0.35,
        }
    }
    pub fn distribution(&self) -> f64 {
        match self {
            Self::None => 0.0,
            Self::Grass => 0.8,
            Self::Mountain => 0.25,
            Self::MountainPeak => 0.01,
            Self::River => 0.3,
            Self::Ocean => 0.7,
            Self::Sand => 0.35,
        }
    }
    pub fn as_color(&self) -> Color {
        match self {
            Self::None => Color::BLACK,
            Self::Grass => Color::DARK_GREEN,
            Self::Mountain => Color::GRAY,
            Self::MountainPeak => Color::WHITE,
            Self::River => Color::BLUE,
            Self::Ocean => Color::MIDNIGHT_BLUE,
            Self::Sand => Color::rgb(0.82, 0.70, 0.55), // Tan
        }
    }
}

use bevy::app::{App, Plugin, PostUpdate, Startup, Update};
use bevy::prelude::IntoSystemConfigs;

use crate::component::{HexData, HexGrid, HexInvalid, HexPos, HexPossibilities, InnerHex};
use crate::resource::{GenMapSystemId, HexPossibilitiesCache};
use crate::system;

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        let gen_map_res = GenMapSystemId(app.world.register_system(system::gen_map));
        app
            /* rustfmt next line chain */
            .insert_resource(gen_map_res)
            .register_type::<HexPos>()
            .register_type::<HexData>()
            .register_type::<HexPossibilities>()
            .register_type::<InnerHex>()
            .register_type::<HexInvalid>()
            .register_type::<HexPossibilitiesCache>()
            .init_resource::<HexPossibilitiesCache>()
            .init_resource::<HexGrid>()
            .add_systems(Startup, system::setup)
            .add_systems(Update,
                         (
                             system::ui,
                             system::update,
                             system::input_handler,
                         ).chain(),
            )
            .add_systems(PostUpdate, system::invalid_hex_handler)
        /* rustfmt next line semi-colon */
        ;
    }
}

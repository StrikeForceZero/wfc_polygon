use bevy::prelude::*;

use crate::component::{HexData, HexInvalid, HexPos, HexPossibilities, InnerHex};
use crate::event::{ChangeHexMode, ClearCache, MapGenerated, RegenerateMap};
use crate::resource::{GenMapSystemId, HexPossibilitiesCache, HexScale};
use crate::system;

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        let gen_map_res = GenMapSystemId(app.world.register_system(system::gen_map));
        app
            /* rustfmt next line chain */
            .insert_resource(gen_map_res)
            .insert_resource(HexScale(20.0))
            .register_type::<HexPos>()
            .register_type::<HexData>()
            .register_type::<HexPossibilities>()
            .register_type::<InnerHex>()
            .register_type::<HexInvalid>()
            .register_type::<HexPossibilitiesCache>()
            .init_resource::<HexPossibilitiesCache>()
            .add_event::<RegenerateMap>()
            .add_event::<ClearCache>()
            .add_event::<MapGenerated>()
            .add_event::<ChangeHexMode>()
            .add_systems(Startup, system::setup)
            .add_systems(Update,
                         (
                             system::change_hex_mode_event_handler,
                             system::ui,
                             system::cache_update_on_hex_selected_handler,
                             system::input_handler,
                             system::regen_map_event_handler,
                             system::clear_cache_event_handler,
                             system::map_generated_event_handler,
                         ).chain(),
            )
            .add_systems(PostUpdate, system::invalid_hex_handler)
        /* rustfmt next line semi-colon */
        ;
    }
}

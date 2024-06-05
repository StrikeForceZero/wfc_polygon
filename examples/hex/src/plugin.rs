use bevy::prelude::*;

use wfc_polygon::wfc::WrapMode;

use crate::component::{HexData, HexInvalid, HexPos, HexPossibilities, HexText, InnerHex};
use crate::event::{ChangeHexMode, ClearCache, GridCellSet, MapGenerated, RegenerateMap, WfcStep};
use crate::resource::{
    ColorMaterialMap, GenMapSystemId, GridSize, HexPossibilitiesCache, HexScale, HexTextEnabled,
    WfcAnimate, WfcWrapMode,
};
use crate::system;

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        let gen_map_res = GenMapSystemId(app.world.register_system(system::gen_map));
        app
            /* rustfmt next line chain */
            .insert_resource(gen_map_res)
            .insert_resource(HexScale(20.0))
            .insert_resource(GridSize(UVec2::splat(40)))
            .insert_resource(HexTextEnabled(false))
            .insert_resource(WfcAnimate(false))
            .insert_resource(WfcWrapMode(Some(WrapMode::Both)))
            .register_type::<HexPos>()
            .register_type::<HexData>()
            .register_type::<HexPossibilities>()
            .register_type::<InnerHex>()
            .register_type::<HexInvalid>()
            .register_type::<HexText>()
            .register_type::<HexPossibilitiesCache>()
            .register_type::<WfcAnimate>()
            .init_resource::<ColorMaterialMap>()
            .init_resource::<HexPossibilitiesCache>()
            .add_event::<RegenerateMap>()
            .add_event::<ClearCache>()
            .add_event::<MapGenerated>()
            .add_event::<ChangeHexMode>()
            .add_event::<GridCellSet>()
            .add_event::<WfcStep>()
            .add_systems(Startup, system::setup)
            .add_systems(Update,
                         (
                             system::change_hex_mode_event_handler,
                             // system::ui,
                             system::cache_update_on_hex_selected_handler,
                             system::clear_cache_event_handler,
                             system::map_generated_event_handler,
                         ).chain(),
            )
            .add_systems(Update, system::input_handler)
            .add_systems(Update, system::regen_map_event_handler)
            .add_systems(Update, system::wfc_step_handler)
            .add_systems(Update, system::grid_cell_set_event_handler)
            .add_systems(PostUpdate, system::invalid_hex_handler)
        /* rustfmt next line semi-colon */
        ;
    }
}

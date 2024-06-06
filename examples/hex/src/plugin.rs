use bevy::prelude::*;
use rand::prelude::StdRng;
use rand::rngs::mock::StepRng;
use rand::SeedableRng;

use wfc_polygon::wfc::WrapMode;

use crate::{AnimateMode, INITIAL_SEED, system, TextMode};
use crate::component::{
    HexData, HexInvalid, HexInvalidPossibilities, HexIx, HexPos, HexPossibilities, HexText,
    InnerHex,
};
use crate::event::{
    ChangeHexMode, ClearCache, GridCellSet, GridCellUpdate, MapGenerated, RegenerateMap, WfcStep,
};
use crate::resource::{
    ColorMaterialMap, CustomRng, GenMapSystemId, GridSize, HexPossibilitiesCache, HexScale,
    HexTextMode, Seed, WfcAnimate, WfcWrapMode,
};

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        let gen_map_res = GenMapSystemId(app.world.register_system(system::gen_map));
        let (custom_seed, custom_rng) = if let Some(initial_seed) = INITIAL_SEED {
            (
                Some(initial_seed),
                Some(
                    StdRng::from_rng(StepRng::new(initial_seed, 1)).expect("failed to create rng"),
                ),
            )
        } else {
            (None, None)
        };
        app
            /* rustfmt next line chain */
            .insert_resource(gen_map_res)
            .insert_resource(HexScale(20.0))
            .insert_resource(GridSize(UVec2::splat(40)))
            .insert_resource(HexTextMode(Some(TextMode::PossibilityCount)))
            .insert_resource(WfcAnimate(AnimateMode::SingleManual))
            .insert_resource(WfcWrapMode(Some(WrapMode::Both)))
            .insert_resource(Seed(custom_seed))
            .insert_resource(CustomRng(custom_rng))
            .register_type::<HexPos>()
            .register_type::<HexData>()
            .register_type::<HexPossibilities>()
            .register_type::<InnerHex>()
            .register_type::<HexInvalid>()
            .register_type::<HexIx>()
            .register_type::<HexText>()
            .register_type::<HexPossibilitiesCache>()
            .register_type::<WfcAnimate>()
            .register_type::<AnimateMode>()
            .register_type::<HexInvalidPossibilities>()
            .init_resource::<ColorMaterialMap>()
            .init_resource::<HexPossibilitiesCache>()
            .add_event::<RegenerateMap>()
            .add_event::<ClearCache>()
            .add_event::<MapGenerated>()
            .add_event::<ChangeHexMode>()
            .add_event::<GridCellSet>()
            .add_event::<WfcStep>()
            .add_event::<GridCellUpdate>()
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
            .add_systems(Update, (
                system::on_cell_update,
                system::grid_cell_set_event_handler,
                system::on_hex_text_added,
            ).chain())
            .add_systems(PostUpdate, system::invalid_hex_handler)
        /* rustfmt next line semi-colon */
        ;
    }
}

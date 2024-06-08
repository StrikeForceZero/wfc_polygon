use bevy::prelude::*;
use bevy_inspector_egui::bevy_egui::EguiContexts;
use bevy_inspector_egui::egui;

use crate::config::GridSize;
use crate::hex::tile_id::HexTileId;
use crate::input::InputMap;
use crate::wfc::SelectedGridCellData;

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        app
            /* rustfmt next line chain */
            .insert_resource(UiState {
                global_enable: true,
                ..default()
            })
            .add_systems(Update, ui)
        /* rustfmt next line semi-colon */
        ;
    }
}

#[derive(Debug, Default, Clone, Reflect, Resource)]
pub struct UiState {
    pub show_config: bool,
    pub global_enable: bool,
    pub show_possibilities: bool,
    pub show_help: bool,
    pub show_wfc_config: bool,
}

pub fn ui(
    mut ui_state: ResMut<UiState>,
    mut egui_contexts: EguiContexts,
    input_map: Res<InputMap>,
    hex_possibility_data: Res<SelectedGridCellData<HexTileId>>,
    mut grid_size: ResMut<GridSize>,
) {
    if ui_state.show_config {
        egui::Window::new("UI Config").show(egui_contexts.ctx_mut(), |ui| {
            egui::ScrollArea::vertical()
                .auto_shrink([false; 2])
                .show(ui, |ui| {
                    macro_rules! ui_state_toggle {
                        ($target:expr, $label:literal) => {{
                            if ui
                                .add(egui::RadioButton::new($target == true, $label))
                                .clicked()
                            {
                                $target = !$target
                            }
                        }};
                    }
                    ui_state_toggle!(ui_state.global_enable, "Global Enable");
                    ui_state_toggle!(ui_state.show_possibilities, "Show Possibilities");
                    ui_state_toggle!(ui_state.show_help, "Show Help");
                    ui_state_toggle!(ui_state.show_wfc_config, "Show WFC Config");
                });
        });
    }
    if !ui_state.global_enable {
        return;
    }
    if ui_state.show_possibilities {
        egui::Window::new("Possibilities").show(egui_contexts.ctx_mut(), |ui| {
            egui::ScrollArea::vertical()
                .auto_shrink([false; 2])
                .show(ui, |ui| {
                    ui.heading("Click tile to view possibilities");
                    for (pos, (data, possibilities)) in hex_possibility_data.0.iter() {
                        ui.add(egui::Label::new(format!(
                            "pos: {}\ndata: {data:#?}\npossibilities:{:#?}",
                            pos.0, possibilities.0
                        )));
                    }
                });
        });
    }
    if ui_state.show_help {
        egui::Window::new("Help").show(egui_contexts.ctx_mut(), |ui| {
            egui::ScrollArea::vertical()
                .auto_shrink([false; 2])
                .show(ui, |ui| {
                    ui.heading("Controls");
                    ui.add(egui::Label::new(
                        ron::ser::to_string_pretty(
                            &input_map.clone(),
                            ron::ser::PrettyConfig::default(),
                        )
                        .unwrap(),
                    ));
                });
        });
    }
    if ui_state.show_wfc_config {
        egui::Window::new("WFC Config").show(egui_contexts.ctx_mut(), |ui| {
            egui::ScrollArea::vertical()
                .auto_shrink([false; 2])
                .show(ui, |ui| {
                    ui.heading("Grid Size");

                    ui.add(egui::Label::new("x"));
                    let mut grid_size_x_str = grid_size.0.x.to_string();
                    ui.add(egui::text_edit::TextEdit::singleline(&mut grid_size_x_str));
                    if let Ok(grid_size_x) = grid_size_x_str.parse::<u32>() {
                        if grid_size_x != grid_size.0.x {
                            grid_size.0.x = grid_size_x;
                        }
                    }

                    ui.add(egui::Label::new("y"));
                    let mut grid_size_y_str = grid_size.0.y.to_string();
                    ui.add(egui::text_edit::TextEdit::singleline(&mut grid_size_y_str));
                    if let Ok(grid_size_y) = grid_size_y_str.parse::<u32>() {
                        if grid_size_y != grid_size.0.y {
                            grid_size.0.y = grid_size_y;
                        }
                    }
                });
        });
    }
}

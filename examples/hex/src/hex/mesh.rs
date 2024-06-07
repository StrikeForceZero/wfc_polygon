use bevy::prelude::*;
use bevy::render::mesh::Indices;
use bevy::render::mesh::PrimitiveTopology::TriangleList;
use bevy::render::render_asset::RenderAssetUsages;

use crate::hex::map::FlatTopHexagonalSegmentIdMap;

pub fn hex_mesh(sides: Option<FlatTopHexagonalSegmentIdMap>) -> [Mesh; 6] {
    // pointy top
    // let vertices = &[
    //     // Center vertex
    //     [0.0, 0.0, 0.0],
    //     // Vertices of the hexagon
    //     [0.0, 1.0, 0.0],     // top
    //     [0.866, 0.5, 0.0],   // top-right
    //     [0.866, -0.5, 0.0],  // bottom-right
    //     [0.0, -1.0, 0.0],    // bottom
    //     [-0.866, -0.5, 0.0], // bottom-left
    //     [-0.866, 0.5, 0.0],  // top-left
    // ];

    // flat top
    let vertices = &[
        // Center vertex
        [0.0, 0.0, 0.0],
        // Vertices of the hexagon
        [1.0, 0.0, 0.0],     // right
        [0.5, 0.866, 0.0],   // top-right
        [-0.5, 0.866, 0.0],  // top-left
        [-1.0, 0.0, 0.0],    // left
        [-0.5, -0.866, 0.0], // bottom-left
        [0.5, -0.866, 0.0],  // bottom-right
    ];

    // Define the indices for the 6 triangles
    let indices = Indices::U32(vec![
        /* rustfmt keep multi line */
        0, 1, 2, // right
        0, 2, 3, // top-right
        0, 3, 4, // top-left
        0, 4, 5, // left
        0, 5, 6, // bottom-left
        0, 6, 1, // bottom-right
    ]);

    // Create the mesh
    let mut base_mesh = Mesh::new(TriangleList, RenderAssetUsages::default());
    base_mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices.to_vec());
    base_mesh.insert_indices(indices);

    // Triangle 0: Top side
    // Triangle 1: Top-right side
    // Triangle 2: Bottom-right side
    // Triangle 3: Bottom side
    // Triangle 4: Bottom-left side
    // Triangle 5: Top-left side

    [0, 1, 2, 3, 4, 5].map(|n| {
        let mut mesh = base_mesh.clone();
        mesh.insert_indices(Indices::U32(vec![0, (n + 1) % 6 + 1, (n + 2) % 6 + 1]));
        mesh
    })
}

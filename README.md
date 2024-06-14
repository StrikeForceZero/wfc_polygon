# WFC Polygon (Name TBD)

This is a **naive** implementation of wave function collapse written in Rust.

The primary goal is to create an equilateral polygon-agnostic interface for wave function collapse, while many other libraries only support standard square
grids.

Instead of backtracking it employs a method where it removes the conflicted tile and its neighbors and resets their entropy based on what valid sets are
available given the current state.

**NOTE: as the current version is `0.0.0` as it's still being iterated on and breaking changes are common.**

## Equilateral Polygons Supported

| Polygon            | Supported |
|--------------------|-----------|
| Triangle           | Untested* |
| Square             | Yes       |
| Flat Top Hexagon   | Yes       |
| Pointy Top Hexagon | Untested  |

*Triangles have their own unique logic but have not been explicitly tested for accuracy. The hex example demonstrates how hexagons can be divided into equal
segments (triangles), each with its own compatibility rules as a side of the hexagon. It is important to note that generating all valid permutations of a
hexagon and its segments is the user's responsibility, as this is not included in the collapse process itself. (Adjacent hex segments of the same hex are not
checked for compatibility.)

## Examples

Right now the examples folder contains a single (albeit messy) example that showcases complex hexagon rules and simple hexagon rules.
The eventual goal is to break it up and reuse it for triangle and square examples.

## License

All code in this repository is dual-licensed under either:

- MIT License ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)

at your option. This means you can select the license you prefer.

### Your Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual-licensed as above, without any additional terms or conditions.

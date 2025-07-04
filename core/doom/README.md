# `core/doom`

This directory contains a tiny ray‑casting renderer implemented entirely in Mochi. The goal is to provide a very small, self‑contained example of how a "Doom"‑style 3D view can be produced without any FFI calls.

## Architecture

- **Map** – a list of strings where `#` represents a wall and `.` or space is empty.
- **Player** – position (`x`, `y`) and viewing angle in radians.
- **Renderer** – `render(map, player, width, height)` casts rays and returns an ASCII image as a list of lines.

The implementation uses simple trigonometry approximations written in pure Mochi. Rendering is done in text only and meant for educational purposes rather than performance.

## Example

```mochi
import "core/doom/doom.mochi" as doom

let map = [
  "########",
  "#......#",
  "#..##..#",
  "#......#",
  "########",
]

var p = doom.Player{ x: 3.5, y: 3.5, angle: 0.0 }
let screen = doom.render(map, p, 40, 12)
for line in screen { print(line) }
```

Running this program prints a simple first‑person view of the scene. Move the player by updating `p.x`, `p.y`, and `p.angle` and calling `render` again.

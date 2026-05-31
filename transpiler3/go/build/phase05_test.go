package build

import (
	"testing"
)

// TestPhase5Sums is the MEP-54 Phase 5 gate for the Go transpiler.
//
// Phase 5 lowering contract:
//
//   - Sum type declarations lower to a Go interface with a private marker
//     method, plus one concrete struct per variant:
//
//       type Shape = Circle(r: float) | Rect(w: float, h: float)
//
//     → interface Shape { mochiShape() }
//       type CircleShape struct { R float64 }; func (*CircleShape) mochiShape() {}
//       type RectShape   struct { W, H float64 }; func (*RectShape)   mochiShape() {}
//
//   - Variant construction: `Circle(3.0)` → `&CircleShape{R: 3.0}`.
//
//   - Match expression/statement lowers to a Go type-switch:
//
//       match s { Circle(r) => r*r*3 | Rect(w, h) => w*h }
//
//     → switch __m := s.(type) {
//           case *CircleShape: r := __m.R; return r*r*3
//           case *RectShape:   w, h := __m.W, __m.H; return w*h
//           default: panic("MOCHI_ERR_MATCH")
//       }
//
//   - The exhaustiveness guard `default: panic("MOCHI_ERR_MATCH")` is always
//     emitted; the type checker guarantees exhaustiveness so it is unreachable.
func TestPhase5Sums(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"sum_",
		"option_",
		"result_",
	)
}

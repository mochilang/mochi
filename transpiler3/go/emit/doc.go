// Package emit renders a gotree.File to a .go source file on
// disk after running its bytes through go/format.Source.
//
// Design contract: MEP-54 §Phase 0+. See
// website/docs/mep/mep-0054.md.
//
// Public entry point (introduced incrementally by phase):
//
//	func Emit(file *gotree.File, outDir, fileName string) error
//
// Determinism: the output is byte-identical across runs given
// the same input gotree.File. gotree.File.Render is itself
// deterministic; go/format.Source is too.
//
// Phase 0 ships only the package skeleton.
package emit

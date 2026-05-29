// Package lower translates an aotir.Program into an rtree.SourceFile.
// Entry point: Lower(prog, className) → *rtree.SourceFile.
//
// Phase 0 scope: hello world only. The lowerer handles the entry function's
// statements when they are exclusively mochi_print_str / mochi_print_i64 /
// mochi_print_f64 / mochi_print_bool calls. Anything else returns an error
// describing the unsupported aotir node so later phases can extend the switch.
package lower

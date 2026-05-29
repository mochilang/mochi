// Package ptree is the PHP syntax tree model used by the
// Mochi-to-PHP transpiler (MEP-55).
//
// The tree is a Mochi-side shadow of the PHP 8.4 syntax surface
// needed by the lowering passes. The emitter walks this tree to
// produce typed-PHP source. We do not use nikic/php-parser to
// emit (decision documented in MEP-55 §Rationale and in the
// research note research/0055/codegen-design.md): emitting from
// a parser AST optimised for round-trip is heavier than emitting
// from a tree shaped to the IR we are lowering from.
package ptree

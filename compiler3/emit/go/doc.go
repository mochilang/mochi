// Package gogen is the compiler3 IR-to-Go-source emitter (MEP-43
// Phase 4). It consumes one `*ir.Function` at a time and produces a
// Go source file.
//
// The emitter walks blocks in reverse-postorder and dispatches on
// ir.OpCode for value lowering, ir.TerminatorKind for control flow.
// Every dispatch arm is a single line of printf, never a per-Mochi-
// syntactic-construct case (the whole point of MEP-43 §3.2): the
// emitter scales with the IR op set, not with the Mochi grammar.
//
// Phase 4 ships the integer/float arithmetic, the i64 comparisons,
// the OpCall recursion path, the string ops, and the list/map/F64-
// array ops that exist in compiler3/ir/types.go today. The OpPhi join
// is lowered to assigned-once Go variables at the head of each block,
// the same trick LLVM's MachineSink uses: phis become "the receiving
// block writes a temp at its head, predecessors write through that
// temp before jumping".
//
// The emitter is deliberately separate from `compiler3/emit/` which
// is the vm3 bytecode emitter; both consume `*ir.Function` but their
// output targets are different (one is binary, one is .go source).
package gogen

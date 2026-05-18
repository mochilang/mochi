// Package ir is compiler3's typed SSA intermediate representation.
//
// Every Value carries a static Type (i64, f64, bool, str, list, map,
// struct, ...). Passes preserve type; lowering picks the opcode by
// type. See MEP-40 §7.1, §10 Phase 4.1.
//
// Phase 4.1 lands the core IR shape: Function / Block / Value /
// Terminator plus an op set rich enough to express every kernel in
// compiler3/corpus. Hand-built IR fixtures (one per corpus kernel)
// live in compiler3/ir/fixture.go and are consumed by Phase 4.2
// (opt passes), Phase 4.3 (regalloc), and Phase 4.4 (emit). Phase
// 4.5 wires typed AST -> ir.Function via compiler3/build.
//
// Conventions:
//
//   - Every Value has a unique ID across its Function. The ID is the
//     Function.Values slice index.
//   - A Block names the Values it owns in Block.Values, in producer
//     order. The last "Value" is the terminator's source, if any.
//   - Phi nodes are first-class: Value.Op == OpPhi, and Value.Args
//     carries pairs of (predecessor-block-ID, incoming-value-ID).
//   - Terminators name their static successors via Terminator.Target /
//     IfTrue / IfFalse and carry one operand via Terminator.Value
//     (the conditional for TermBranch, the returned value for
//     TermReturn).
//   - Parameter values use Op == OpParam with no Args; their Type is
//     the parameter's declared type.
//   - Constants embed their payload in Value.Const (sign-extended for
//     i64, bit-cast for f64; the bool variant uses 0/1).
//
// The IR validates as SSA: every Value is defined in exactly one
// Block, every use refers to a previously-defined Value (per the
// dominator tree, with Phi as the explicit join). Validate(fn)
// enforces these properties and is called by every fixture test.
package ir

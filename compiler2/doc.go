// Package compiler2 is the from-scratch typed bytecode compiler for vm2.
//
// Design contract: MEP-21 v2 (Type-Directed Bytecode and Compiler/VM
// Co-Design). See website/docs/mep/mep-0021.md.
//
// Pipeline: typed AST -> typed SSA IR (compiler2/ir) -> optimization
// passes (compiler2/opt) -> linear-scan register allocation
// (compiler2/regalloc) -> typed monomorphic bytecode emission
// (compiler2/emit) -> runtime/vm2 Program.
//
// The existing compiler/ package continues to target runtime/vm; the two
// pipelines coexist while compiler2 + vm2 are brought up to parity.
package compiler2

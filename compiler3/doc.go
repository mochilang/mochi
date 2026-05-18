// Package compiler3 is the from-scratch typed bytecode compiler for vm3.
//
// Design contract: MEP-40 (vm3 + compiler3). See
// website/docs/mep/mep-0040.md.
//
// Pipeline:
//
//	typed AST (existing types/ pass)
//	-> typed SSA IR (compiler3/ir)
//	-> optimization passes (compiler3/opt: ConstFold, DCE, branch
//	   threading, LICM, TailCall)
//	-> linear-scan register allocation per bank (compiler3/regalloc)
//	-> typed monomorphic bytecode emission (compiler3/emit)
//	-> runtime/vm3 Function.
//
// compiler3 co-exists with compiler2 until Phase 7 cut-over.
package compiler3

// Package c is the Mochi-to-C ahead-of-time transpiler.
//
// Design contract: MEP-45 (Mochi-to-C transpiler). See
// website/docs/mep/mep-0045.md. Phase tracking lives at
// website/docs/implementation/0045/.
//
// Pipeline:
//
//	Mochi source
//	-> parser (reused MEP-1/2/3 frontend)
//	-> typed AST (reused MEP-4/5/6 type checker + inference)
//	-> monomorphisation (Mochi forbids polymorphic recursion;
//	   the instantiation set is finite)
//	-> aotir (transpiler3/c/aotir): purpose-built lowering IR
//	   for C emission
//	-> match-to-decision-tree (Maranget, ML Workshop 2008)
//	-> closure-convert (closures lower to fat pointers:
//	   code + environment)
//	-> emit (transpiler3/c/emit): ISO C23 source string
//	-> build (transpiler3/c/build): driver invokes a C compiler
//	   (host cc with vendored zig fallback) and links against
//	   libmochi.a built from transpiler3/c/runtime/
//	-> single statically-linked native binary
//
// Surface: additive. mochi run keeps the vm3 path. mochi build
// --target=c (and every native triple) routes through this
// pipeline. The master correctness gate is byte-equal stdout
// against vm3 on the full fixture corpus.
//
// Boundary with vm3: vm3 is the recording oracle for fixture
// expect.txt goldens. transpiler3/c/ does not import, link, or
// otherwise depend on runtime/vm3 or compiler3/ir; the only
// shared code with prior backends is the parser and the type
// checker.
//
// Subpackages:
//
//   - aotir:        typed lowering IR (instructions, blocks,
//                   functions, verifier).
//   - lower:        typed AST -> aotir pass tree (monomorphise,
//                   match-to-decision-tree, closure-convert).
//   - emit:         aotir -> ISO C23 source.
//   - build:        driver, host-cc discovery, .mochi/cache,
//                   link.
//   - toolchain/zig: vendored zig cross-compiler bootstrap
//                   (download + SHA-256 pin + invoke).
//   - runtime:      C runtime (libmochi.a) headers + sources.
package c

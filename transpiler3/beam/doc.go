// Package beam is the Mochi-to-Erlang/BEAM transpiler.
//
// Design contract: MEP-46 (Mochi-to-Erlang/BEAM transpiler). See
// website/docs/mep/mep-0046.md. Phase tracking lives at
// website/docs/implementation/0046/.
//
// Pipeline:
//
//	Mochi source
//	-> parser (reused MEP-1/2/3 frontend)
//	-> typed AST (reused MEP-4/5/6 type checker + inference)
//	-> monomorphisation (MEP-45 pass, reused unchanged)
//	-> aotir.Program (MEP-45 IR, reused unchanged)
//	-> match-to-decision-tree (MEP-45 Maranget pass, reused)
//	-> closure-convert (MEP-45 pass, reused)
//	-> beam/lower: aotir.Program -> cerl.Module
//	-> beam/emit: cerl.Module -> .beam bytes
//	   (drives compile:forms/2 via erl -noshell subprocess)
//	-> beam/build: .beam bytes -> escript archive or OTP release
//
// Surface: additive. mochi run keeps the vm3 path. mochi build
// --target=beam-escript and --target=beam-release route through
// this pipeline. The master correctness gate is byte-equal stdout
// against vm3 on the full fixture corpus across OTP 27 and OTP 28
// on x86_64-linux-gnu and aarch64-darwin.
//
// Boundary with MEP-45: the aotir IR is consumed read-only; no
// changes to transpiler3/c/ are required or permitted by this
// pipeline. The fork happens at the lowering stage.
//
// Subpackages:
//
//   - cerl:    Go-side Core Erlang AST types and ETF serialiser.
//   - lower:   aotir.Program -> cerl.Module lowering pass.
//   - emit:    cerl.Module -> .beam files via compile:forms/2.
//   - build:   build driver, escript packing, OTP release target,
//              cache layer.
//   - runtime: the mochi OTP application (Erlang source + rebar3).
package beam

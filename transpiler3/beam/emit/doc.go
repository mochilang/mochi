// Package emit compiles a cerl.Module to a .beam file.
//
// Design contract: MEP-46 §1 "Pipeline and IR reuse". See
// website/docs/mep/mep-0046.md.
//
// Public entry point:
//
//	func Emit(mod *cerl.Module, outDir string) ([]BeamFile, error)
//
// Emit serialises the cerl.Module to an Erlang External Term Format
// (ETF) binary, writes it to a temporary file, then invokes:
//
//	erl -noshell -eval '
//	  {ok, Bin} = file:read_file(ETFPath),
//	  Forms = binary_to_term(Bin),
//	  {ok, Mod, BeamBytes, _Warns} =
//	      compile:forms(Forms, [from_core, debug_info, return_errors, return_warnings]),
//	  file:write_file(OutPath, BeamBytes),
//	  halt(0).'
//
// The subprocess is invoked once per module. The generated .beam
// file is written to outDir/<ModName>.beam. A BeamFile record holds
// the module name and the absolute path to the .beam file.
//
// Error handling: if compile:forms/2 returns {error, Errors, _}
// the raw Erlang error terms are captured from stderr and returned
// as a Go error. This provides actionable diagnostics even when the
// Core Erlang AST is invalid.
//
// Reproducibility: the cerl.Module's function definitions are
// sorted by mangled name before serialisation so that two
// independent Emit calls on the same logical module produce
// bit-identical .beam files (modulo the CInf timestamp chunk,
// which Phase 18 strips).
package emit

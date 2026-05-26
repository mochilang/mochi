package emit

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"mochi/transpiler3/beam/cerl"
)

// BeamFile holds the result of compiling one cerl.Module.
type BeamFile struct {
	// ModName is the Erlang module name (e.g. "mochi_main").
	ModName string
	// Path is the absolute path to the generated .beam file.
	Path string
}

// Emit serialises mod to ETF, drives compile:forms/2 [from_core] via an
// erl -noshell subprocess, and writes the resulting .beam file to
// outDir/<ModName>.beam.
//
// The erl binary must be on PATH. OTP 27 or later is required.
func Emit(mod *cerl.Module, outDir string) ([]BeamFile, error) {
	if mod == nil {
		return nil, fmt.Errorf("beam/emit: nil module")
	}
	if outDir == "" {
		return nil, fmt.Errorf("beam/emit: empty outDir")
	}

	etfBytes, err := mod.MarshalBinary()
	if err != nil {
		return nil, fmt.Errorf("beam/emit: marshal %s: %w", mod.Name, err)
	}

	etfPath := filepath.Join(outDir, mod.Name+".core.etf")
	if err := os.WriteFile(etfPath, etfBytes, 0o644); err != nil {
		return nil, fmt.Errorf("beam/emit: write etf %s: %w", etfPath, err)
	}

	beamPath := filepath.Join(outDir, mod.Name+".beam")

	// compile:forms/2 with from_core compiles a Core Erlang form
	// (as produced by MarshalBinary's c_module tuple) to a .beam binary.
	// return_warnings ensures a 4-tuple {ok,Mod,Bin,Warnings} on success.
	erlExpr := fmt.Sprintf(
		`{ok,B}=file:read_file("%s"),`+
			`Form=binary_to_term(B),`+
			`case compile:forms(Form,[from_core,debug_info,return_errors,return_warnings]) of `+
			`{ok,_,BeamBin,_}->file:write_file("%s",BeamBin),halt(0);`+
			`{error,Es,_}->io:format(standard_error,"compile error: ~p~n",[Es]),halt(1)`+
			`end.`,
		etfPath, beamPath,
	)

	cmd := exec.Command("erl", "-noshell", "-eval", erlExpr)
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("beam/emit: compile:forms for %s: %w\n%s", mod.Name, err, out)
	}

	return []BeamFile{{ModName: mod.Name, Path: beamPath}}, nil
}

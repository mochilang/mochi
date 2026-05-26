package build

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
	beamlower "mochi/transpiler3/beam/lower"
	"mochi/transpiler3/beam/emit"
	clower "mochi/transpiler3/c/lower"
	"mochi/types"
)

// Target selects the output artifact produced by Driver.Build.
type Target int

const (
	// TargetEscript produces a single-file escript executable.
	// Requires erl on PATH at runtime. Cold-start ~50ms. Size ~2-10MB.
	TargetEscript Target = iota

	// TargetRelease produces a self-contained OTP release tarball
	// with ERTS bundled. No erl required at runtime. Cold-start ~300ms.
	// Size ~30-80MB. Supports hot reload and supervision.
	TargetRelease

	// TargetAtomVM produces a .avm bundle for embedded targets
	// (ESP32, STM32). Only Phases 1-5 are supported; pg, gun, and
	// crypto are not available on AtomVM.
	TargetAtomVM
)

// Driver is the top-level build orchestrator for the BEAM transpiler.
// Call Build to compile a Mochi source file to the target artifact.
type Driver struct {
	// CacheDir is the directory for .beam file caches keyed by
	// BLAKE3 hash of the source + compiler version.
	// Defaults to .mochi/beam-cache/ in the source file's directory.
	CacheDir string
}

// escriptShebang is the two-line header that makes a .beam file
// directly executable as an escript. The escript VM reads the
// module name from the .beam binary metadata and calls ModName:main/1.
const escriptShebang = "#!/usr/bin/env escript\n%%!\n"

// Build compiles the Mochi source at src, writes the output artifact
// to out, and returns any error. The pipeline is:
//
//  1. Parse + type-check (compiler3 frontend, shared with MEP-45).
//  2. Lower to aotir (transpiler3/c/lower, reused from MEP-45).
//  3. beam/lower: aotir -> cerl.Module.
//  4. beam/emit: cerl.Module -> .beam file via compile:forms/2.
//  5. Pack as escript (TargetEscript) or return error for other targets.
//
// Phase 1 supports TargetEscript only. TargetRelease and TargetAtomVM
// return an error until their respective phases land.
func (d *Driver) Build(src, out string, target Target) error {
	if target != TargetEscript {
		return fmt.Errorf("beam/build: only TargetEscript is supported in Phase 1 (got %d)", target)
	}

	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("beam/build: parse %s: %w", src, err)
	}
	if errs := types.Check(prog, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("beam/build: type-check %s: %w", src, errs[0])
	}
	ir, err := clower.Lower(prog)
	if err != nil {
		return fmt.Errorf("beam/build: lower %s: %w", src, err)
	}

	// Phase 1 uses a fixed module name. Phase N will derive the name
	// from the source file's package path.
	const modName = "mochi_main"

	mod, err := beamlower.Lower(ir, modName)
	if err != nil {
		return fmt.Errorf("beam/build: beam lower %s: %w", src, err)
	}

	workDir, err := os.MkdirTemp("", "mochi-beam-")
	if err != nil {
		return fmt.Errorf("beam/build: mkdtemp: %w", err)
	}
	defer os.RemoveAll(workDir)

	beamFiles, err := emit.Emit(mod, workDir)
	if err != nil {
		return fmt.Errorf("beam/build: emit %s: %w", src, err)
	}
	if len(beamFiles) == 0 {
		return fmt.Errorf("beam/build: emit produced no .beam files")
	}

	beamBytes, err := os.ReadFile(beamFiles[0].Path)
	if err != nil {
		return fmt.Errorf("beam/build: read %s: %w", beamFiles[0].Path, err)
	}

	// Write escript: shebang header + raw .beam bytes.
	// escript reads the module name from the .beam binary and calls main/1.
	content := append([]byte(escriptShebang), beamBytes...)
	if err := os.WriteFile(out, content, 0o755); err != nil {
		return fmt.Errorf("beam/build: write escript %s: %w", out, err)
	}

	_ = filepath.Dir(out) // ensure import used
	return nil
}

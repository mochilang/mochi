package build

import (
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"lukechampine.com/blake3"

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

// runtimeSrcDir returns the absolute path to transpiler3/beam/runtime/src/
// by walking up from this Go source file using runtime.Caller.
func runtimeSrcDir() (string, error) {
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		return "", fmt.Errorf("runtime.Caller failed")
	}
	// thisFile is .../transpiler3/beam/build/build.go
	// runtime src is  .../transpiler3/beam/runtime/src/
	repoRoot := filepath.Dir(filepath.Dir(filepath.Dir(filepath.Dir(thisFile))))
	dir := filepath.Join(repoRoot, "transpiler3", "beam", "runtime", "src")
	if _, err := os.Stat(dir); err != nil {
		return "", fmt.Errorf("runtime src dir not found at %s: %w", dir, err)
	}
	return dir, nil
}

// compileRuntime compiles all .erl files in the runtime src/ directory
// to .beam files in outDir, returning the list of .beam file paths.
func compileRuntime(outDir string) ([]string, error) {
	srcDir, err := runtimeSrcDir()
	if err != nil {
		return nil, err
	}

	erlFiles, err := filepath.Glob(filepath.Join(srcDir, "*.erl"))
	if err != nil {
		return nil, fmt.Errorf("glob runtime erl: %w", err)
	}

	if len(erlFiles) == 0 {
		return nil, nil
	}

	// Compile with erlc.
	args := append([]string{"-o", outDir}, erlFiles...)
	cmd := exec.Command("erlc", args...)
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("erlc runtime: %w\n%s", err, out)
	}

	var beamFiles []string
	for _, erl := range erlFiles {
		name := strings.TrimSuffix(filepath.Base(erl), ".erl")
		beamFiles = append(beamFiles, filepath.Join(outDir, name+".beam"))
	}
	return beamFiles, nil
}

// cacheKey returns the BLAKE3 hex digest of the source file content combined
// with the current compiler version string. This uniquely identifies a build
// artifact so that unchanged source files skip recompilation (Phase 1.2).
func (d *Driver) cacheKey(src string) (string, error) {
	content, err := os.ReadFile(src)
	if err != nil {
		return "", err
	}
	h := blake3.New(32, nil)
	h.Write(content)
	h.Write([]byte(compilerVersion))
	return hex.EncodeToString(h.Sum(nil)), nil
}

// compilerVersion is a sentinel that changes whenever the compiler pipeline
// changes in a way that affects generated output. Bump this string whenever a
// new phase changes code-gen semantics.
const compilerVersion = "mep46-v1"

// cacheDir returns the effective cache directory for this driver, defaulting
// to `.mochi/cache/beam/` adjacent to the source file when CacheDir is empty.
func (d *Driver) cacheDir(src string) string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	return filepath.Join(filepath.Dir(src), ".mochi", "cache", "beam")
}

// copyFile copies src to dst, creating dst's parent directories as needed.
func copyFile(dst, src string) error {
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return err
	}
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o755)
	if err != nil {
		return err
	}
	defer out.Close()
	_, err = io.Copy(out, in)
	return err
}

// Build compiles the Mochi source at src, writes the output artifact
// to out, and returns any error. The pipeline is:
//
//  1. Parse + type-check (compiler3 frontend, shared with MEP-45).
//  2. Lower to aotir (transpiler3/c/lower, reused from MEP-45).
//  3. beam/lower: aotir -> cerl.Module.
//  4. beam/emit: cerl.Module -> .beam file via compile:forms/2.
//  5. Compile runtime .erl files to .beam.
//  6. Pack as archive escript (TargetEscript).
//
// If CacheDir is set (or the default .mochi/cache/beam/ directory exists),
// Build checks a BLAKE3 content-addressed cache before recompiling.
// Phase 1.2: rebuild on unchanged source is a file-copy no-op.
func (d *Driver) Build(src, out string, target Target) error {
	if target != TargetEscript {
		return fmt.Errorf("beam/build: only TargetEscript is supported in Phase 2 (got %d)", target)
	}

	// Phase 1.2: check cache before doing any compilation work.
	cacheKey, err := d.cacheKey(src)
	if err == nil {
		cacheFile := filepath.Join(d.cacheDir(src), cacheKey+".escript")
		if _, err := os.Stat(cacheFile); err == nil {
			return copyFile(out, cacheFile)
		}
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

	// Compile the runtime .erl files.
	runtimeBeams, err := compileRuntime(workDir)
	if err != nil {
		return fmt.Errorf("beam/build: compile runtime: %w", err)
	}

	// Pack as archive escript.
	if err := packArchiveEscript(out, beamFiles[0].Path, runtimeBeams); err != nil {
		return err
	}

	// Phase 1.2: store in cache so the next identical build is a copy.
	if cacheKey != "" {
		cacheFile := filepath.Join(d.cacheDir(src), cacheKey+".escript")
		// Best-effort: ignore errors so a read-only cache dir doesn't break builds.
		_ = copyFile(cacheFile, out)
	}
	return nil
}

// packArchiveEscript creates a zip-archive escript using escript:create/2
// via an erl -noshell subprocess. This ensures the zip format is compatible
// with OTP's escript module.
func packArchiveEscript(out, userBeam string, runtimeBeams []string) error {
	allBeams := append([]string{userBeam}, runtimeBeams...)

	// Build the Erlang expression to pass all beam file paths and the output
	// path to escript:create/2.
	binariesExpr := "["
	for i, bp := range allBeams {
		if i > 0 {
			binariesExpr += ","
		}
		name := filepath.Base(bp)
		binariesExpr += fmt.Sprintf("{%q,element(2,file:read_file(%q))}", name, bp)
	}
	binariesExpr += "]"

	erlExpr := fmt.Sprintf(
		`Bins = %s,`+
			`Sections = [shebang, {emu_args, "-escript main mochi_main"}, {archive, Bins, []}],`+
			`case escript:create(%q, Sections) of`+
			`  ok -> file:change_mode(%q, 8#755), halt(0);`+
			`  {error, E} -> io:format(standard_error, "escript:create error: ~p~n", [E]), halt(1)`+
			`end.`,
		binariesExpr, out, out,
	)

	cmd := exec.Command("erl", "-noshell", "-eval", erlExpr)
	if result, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("escript:create for %s: %w\n%s", out, err, result)
	}
	return nil
}

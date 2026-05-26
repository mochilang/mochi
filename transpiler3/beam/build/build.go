package build

import (
	"bufio"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
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

	// TargetRebar3Project emits a standalone rebar3 project directory
	// containing rebar.config, src/ with runtime .erl files, and
	// ebin/ with pre-compiled .beam files. The user can run
	// `rebar3 compile` and `rebar3 dialyzer` against the output.
	// Phase 15.1.
	TargetRebar3Project

	// TargetMixProject emits a standalone Mix (Elixir) project directory
	// containing mix.exs and ebin/ with pre-compiled .beam files.
	// The user can run `mix compile` against the output.
	// Phase 15.2.
	TargetMixProject

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
//  6. Pack as archive escript (TargetEscript) or emit project dir.
//
// For TargetEscript, out is a file path.
// For TargetRebar3Project and TargetMixProject, out is a directory path.
//
// If CacheDir is set (or the default .mochi/cache/beam/ directory exists),
// Build checks a BLAKE3 content-addressed cache before recompiling.
// Phase 1.2: rebuild on unchanged source is a file-copy no-op.
func (d *Driver) Build(src, out string, target Target) error {
	switch target {
	case TargetRebar3Project:
		return d.buildRebar3Project(src, out)
	case TargetMixProject:
		return d.buildMixProject(src, out)
	case TargetRelease:
		return d.buildRelease(src, out)
	case TargetEscript:
		// handled below
	default:
		return fmt.Errorf("beam/build: unsupported target %d", target)
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

	// Phase 12.2: if mochi.toml exists next to src, emit rebar.config.
	if err := maybeEmitRebarConfig(src, out); err != nil {
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

// depEntry holds a single Hex.pm dependency parsed from mochi.toml.
type depEntry struct {
	Name    string
	Version string
}

// parseMochiToml reads a mochi.toml file and extracts the [dependencies]
// section. It only parses key = "value" lines under [dependencies]; a full
// TOML library is not needed for this minimal format.
//
// Phase 12.2.
func parseMochiToml(path string) ([]depEntry, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	// Pattern for a dep line: identifier = "version"
	depLine := regexp.MustCompile(`^\s*([A-Za-z][A-Za-z0-9_]*)\s*=\s*"([^"]+)"\s*$`)

	var deps []depEntry
	inDeps := false
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		trimmed := strings.TrimSpace(line)

		// Section header.
		if strings.HasPrefix(trimmed, "[") {
			inDeps = strings.TrimSpace(strings.Trim(trimmed, "[]")) == "dependencies"
			continue
		}
		if !inDeps {
			continue
		}
		// Skip blank lines and comments.
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}
		if m := depLine.FindStringSubmatch(line); m != nil {
			deps = append(deps, depEntry{Name: m[1], Version: m[2]})
		}
	}
	return deps, scanner.Err()
}

// emitRebarConfig writes a rebar.config file to outPath containing
// {erl_opts, [debug_info]} and {deps, [...]} entries for the given deps.
//
// Phase 12.2.
func emitRebarConfig(outPath string, deps []depEntry) error {
	var sb strings.Builder
	sb.WriteString("{erl_opts, [debug_info]}.\n")
	sb.WriteString("{deps, [\n")
	for i, d := range deps {
		sb.WriteString(fmt.Sprintf("    {%s, %q}", d.Name, d.Version))
		if i < len(deps)-1 {
			sb.WriteString(",")
		}
		sb.WriteString("\n")
	}
	sb.WriteString("]}.\n")
	return os.WriteFile(outPath, []byte(sb.String()), 0o644)
}

// maybeEmitRebarConfig checks whether a mochi.toml exists in the same
// directory as src. If it does, it parses the [dependencies] section and
// writes a rebar.config next to out. This is a best-effort step: failures
// are returned so tests can catch them.
//
// Phase 12.2.
func maybeEmitRebarConfig(src, out string) error {
	tomlPath := filepath.Join(filepath.Dir(src), "mochi.toml")
	if _, err := os.Stat(tomlPath); err != nil {
		// No mochi.toml — nothing to do.
		return nil
	}
	deps, err := parseMochiToml(tomlPath)
	if err != nil {
		return fmt.Errorf("beam/build: parse mochi.toml: %w", err)
	}
	rebarPath := filepath.Join(filepath.Dir(out), "rebar.config")
	return emitRebarConfig(rebarPath, deps)
}

// compileToBeams is a shared helper that compiles src through the full
// Mochi pipeline and returns the workDir (caller must clean up) plus
// all .beam file paths (user module first, then runtime).
func (d *Driver) compileToBeams(src string) (workDir string, beams []string, cleanup func(), err error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return "", nil, nil, fmt.Errorf("beam/build: parse %s: %w", src, err)
	}
	if errs := types.Check(prog, types.NewEnv(nil)); len(errs) > 0 {
		return "", nil, nil, fmt.Errorf("beam/build: type-check %s: %w", src, errs[0])
	}
	ir, err := clower.Lower(prog)
	if err != nil {
		return "", nil, nil, fmt.Errorf("beam/build: lower %s: %w", src, err)
	}

	const modName = "mochi_main"
	mod, err := beamlower.Lower(ir, modName)
	if err != nil {
		return "", nil, nil, fmt.Errorf("beam/build: beam lower %s: %w", src, err)
	}

	wd, err := os.MkdirTemp("", "mochi-beam-")
	if err != nil {
		return "", nil, nil, fmt.Errorf("beam/build: mkdtemp: %w", err)
	}
	cl := func() { os.RemoveAll(wd) }

	emitted, err := emit.Emit(mod, wd)
	if err != nil {
		cl()
		return "", nil, nil, fmt.Errorf("beam/build: emit %s: %w", src, err)
	}
	if len(emitted) == 0 {
		cl()
		return "", nil, nil, fmt.Errorf("beam/build: emit produced no .beam files")
	}

	runtimeBeams, err := compileRuntime(wd)
	if err != nil {
		cl()
		return "", nil, nil, fmt.Errorf("beam/build: compile runtime: %w", err)
	}

	var all []string
	for _, ef := range emitted {
		all = append(all, ef.Path)
	}
	all = append(all, runtimeBeams...)
	return wd, all, cl, nil
}

// buildRebar3Project emits a standalone rebar3 project into the directory out.
// The layout is:
//
//	out/
//	  rebar.config
//	  src/
//	    mochi_app.app.src
//	    *.erl  (runtime sources)
//	  ebin/
//	    *.beam  (pre-compiled user + runtime modules)
//
// Phase 15.1.
func (d *Driver) buildRebar3Project(src, out string) error {
	_, beams, cleanup, err := d.compileToBeams(src)
	if err != nil {
		return err
	}
	defer cleanup()

	ebinDir := filepath.Join(out, "ebin")
	srcDir := filepath.Join(out, "src")
	if err := os.MkdirAll(ebinDir, 0o755); err != nil {
		return err
	}
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		return err
	}

	// Copy .beam files into ebin/.
	for _, bp := range beams {
		dst := filepath.Join(ebinDir, filepath.Base(bp))
		if err := copyFile(dst, bp); err != nil {
			return err
		}
	}

	// Copy runtime .erl sources into src/.
	rtSrcDir, err := runtimeSrcDir()
	if err == nil {
		erlFiles, _ := filepath.Glob(filepath.Join(rtSrcDir, "*.erl"))
		for _, ef := range erlFiles {
			dst := filepath.Join(srcDir, filepath.Base(ef))
			if err := copyFile(dst, ef); err != nil {
				return err
			}
		}
	}

	// Write src/mochi_app.app.src.
	appSrc := `{application, mochi_app,
  [{description, "Generated by Mochi"},
   {vsn, "0.1.0"},
   {modules, []},
   {registered, []},
   {applications, [kernel, stdlib, inets, ssl]}]}.
`
	if err := os.WriteFile(filepath.Join(srcDir, "mochi_app.app.src"), []byte(appSrc), 0o644); err != nil {
		return err
	}

	// Determine deps from mochi.toml (if present).
	deps := loadDeps(src)

	// Write rebar.config.
	return emitRebarConfig(filepath.Join(out, "rebar.config"), deps)
}

// buildMixProject emits a standalone Mix project into the directory out.
// The layout is:
//
//	out/
//	  mix.exs
//	  ebin/
//	    *.beam  (pre-compiled user + runtime modules)
//
// Phase 15.2.
func (d *Driver) buildMixProject(src, out string) error {
	_, beams, cleanup, err := d.compileToBeams(src)
	if err != nil {
		return err
	}
	defer cleanup()

	ebinDir := filepath.Join(out, "ebin")
	if err := os.MkdirAll(ebinDir, 0o755); err != nil {
		return err
	}

	// Copy .beam files into ebin/.
	for _, bp := range beams {
		dst := filepath.Join(ebinDir, filepath.Base(bp))
		if err := copyFile(dst, bp); err != nil {
			return err
		}
	}

	// Determine deps from mochi.toml (if present).
	deps := loadDeps(src)

	// Render mix.exs deps list.
	var mixDeps strings.Builder
	for i, d := range deps {
		if i > 0 {
			mixDeps.WriteString(",\n      ")
		}
		mixDeps.WriteString(fmt.Sprintf("{:%s, %q}", d.Name, d.Version))
	}

	mixExs := fmt.Sprintf(`defmodule MochiApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :mochi_app,
      version: "0.1.0",
      elixir: "~> 1.14",
      deps: deps()
    ]
  end

  defp deps do
    [
      %s
    ]
  end
end
`, mixDeps.String())

	return os.WriteFile(filepath.Join(out, "mix.exs"), []byte(mixExs), 0o644)
}

// loadDeps reads mochi.toml next to src and returns its [dependencies] entries.
// Returns nil if no mochi.toml exists or it cannot be parsed.
func loadDeps(src string) []depEntry {
	tomlPath := filepath.Join(filepath.Dir(src), "mochi.toml")
	deps, err := parseMochiToml(tomlPath)
	if err != nil {
		return nil
	}
	return deps
}

// buildRelease emits a standalone rebar3 project (same as buildRebar3Project)
// and then runs `rebar3 release` inside it to produce an OTP release. The
// release tarball is left at out/_build/default/rel/mochi_app/. rebar3 must
// be on PATH; callers should check with exec.LookPath("rebar3") first.
//
// Phase 15.0.
func (d *Driver) buildRelease(src, out string) error {
	// Emit the rebar3 project first.
	if err := d.buildRebar3Project(src, out); err != nil {
		return err
	}

	// Add a minimal relx configuration to rebar.config.
	rebarPath := filepath.Join(out, "rebar.config")
	existing, err := os.ReadFile(rebarPath)
	if err != nil {
		return fmt.Errorf("beam/build: read rebar.config for relx: %w", err)
	}
	relxEntry := `
{relx, [{release, {mochi_app, "0.1.0"}, [mochi_app, kernel, stdlib, inets, ssl]},
         {dev_mode, false},
         {include_erts, true}]}.
`
	if err := os.WriteFile(rebarPath, append(existing, []byte(relxEntry)...), 0o644); err != nil {
		return fmt.Errorf("beam/build: write relx config: %w", err)
	}

	// Run rebar3 release.
	cmd := exec.Command("rebar3", "release")
	cmd.Dir = out
	if result, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("beam/build: rebar3 release: %w\n%s", err, result)
	}
	return nil
}

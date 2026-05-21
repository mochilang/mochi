// Package gobuild is the Go-target build driver for MEP-43 Phase 7/8.
// It owns the user-facing options ('mochi build --target=go' has today
// no shell because the Mochi-to-IR frontend isn't wired in yet, but
// every option that shell will accept lives here as a typed field).
// Build takes a fully-formed Program plus Options and writes the
// emitter's output to disk; when KeepEmit is false and the caller asks
// for a binary build, the gen file is removed after `go build` returns.
//
// The CLI subcommand in cmd/mochi/main.go is the missing piece between
// 'mochi build --target=go file.mochi' and this driver: once the Mochi
// parser + IR-builder pipeline lands, the subcommand parses the source
// into a Program, calls Build with the right Options, and reports the
// result. Until then, internal callers (tests, the A/B harness) drive
// the driver directly via the Go API.
package gobuild

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"

	gogen "mochi/compiler3/emit/go"
	"mochi/compiler3/frontend"
	"mochi/parser"
)

// Mode controls the shape of the emitter's output. ModeExecutable emits
// a runnable Go program (with package main + a `func main`).
// ModeLibrary emits a multi-file Go package suitable for `import`-ing
// from another Go module; this is the --emit=go-library mode.
type Mode int

const (
	// ModeExecutable produces a single .go file plus go.mod that, when
	// `go run` is invoked, prints the program's stdout. Default.
	ModeExecutable Mode = iota
	// ModeLibrary produces a multi-file Go package (pkg.go + go.mod)
	// the consumer imports. Matches MEP-43 §Phase-8 --emit=go-library.
	ModeLibrary
)

// Options captures every flag the user-facing CLI will eventually
// expose for `mochi build --target=go`. Each field is the typed home
// of one --flag.
type Options struct {
	// Mode selects executable vs library output.
	Mode Mode
	// OutDir is the directory the driver writes generated files into.
	// Required.
	OutDir string
	// KeepEmit retains the emitted .go file(s) on disk after the build
	// finishes. When false, the driver removes generated files after a
	// successful Compile (CLI flag: --keep-emit).
	KeepEmit bool
	// ModulePath is the Go module path baked into go.mod (--module=).
	// Required when Mode == ModeLibrary; ignored otherwise.
	ModulePath string
	// PkgName overrides the package name in the emitted source. When
	// empty, the driver defaults to the module path's last segment in
	// library mode or "main" in executable mode.
	PkgName string
	// RuntimeReplace, when non-empty, adds a `replace mochi => <path>`
	// directive to the generated go.mod so the consumer can resolve
	// `mochi/runtime/mochi/...` without publishing the runtime. Library
	// mode only.
	RuntimeReplace string
}

// Result is what the driver returns on success: the list of files
// written and a hint to the caller (e.g. the executable to run, or the
// import path of the produced library).
type Result struct {
	// Files lists the absolute paths the driver wrote, in sorted
	// order so callers and tests can assert determinism.
	Files []string
	// Mode is the Mode the driver ran in. Echoed back so callers don't
	// have to consult their original Options.
	Mode Mode
	// EntryPoint is the path of the main .go file in executable mode
	// (callers can pass it to `go run`), or the empty string in
	// library mode.
	EntryPoint string
}

// Build emits the Program under the given Options.
//
// In ModeExecutable, Build writes one .go file under OutDir + an
// optional go.mod (only when the source references `runtime/mochi/`).
// In ModeLibrary, Build delegates to gogen.EmitLibrary and writes
// every produced file.
//
// When KeepEmit is false and the caller has marked the build as
// transient (via the returned Result), the caller is responsible for
// `os.RemoveAll(OutDir)` once the produced binary is no longer
// needed; the driver does not delete files itself because it does not
// know whether the downstream `go build`/`go run` step has finished.
func Build(p *gogen.Program, opts Options) (Result, error) {
	if opts.OutDir == "" {
		return Result{}, errors.New("gobuild.Build: OutDir is required")
	}
	if err := os.MkdirAll(opts.OutDir, 0o755); err != nil {
		return Result{}, fmt.Errorf("gobuild.Build: mkdir %s: %w", opts.OutDir, err)
	}

	switch opts.Mode {
	case ModeExecutable:
		return buildExecutable(p, opts)
	case ModeLibrary:
		return buildLibrary(p, opts)
	default:
		return Result{}, fmt.Errorf("gobuild.Build: unknown Mode %d", opts.Mode)
	}
}

func buildExecutable(p *gogen.Program, opts Options) (Result, error) {
	if opts.PkgName != "" {
		p.PkgName = opts.PkgName
	}
	src, err := gogen.Emit(p)
	if err != nil {
		return Result{}, fmt.Errorf("gobuild.Build: emit: %w", err)
	}
	out := filepath.Join(opts.OutDir, "gen.go")
	if err := os.WriteFile(out, src, 0o644); err != nil {
		return Result{}, fmt.Errorf("gobuild.Build: write %s: %w", out, err)
	}
	return Result{Files: []string{out}, Mode: ModeExecutable, EntryPoint: out}, nil
}

func buildLibrary(p *gogen.Program, opts Options) (Result, error) {
	if opts.ModulePath == "" {
		return Result{}, errors.New("gobuild.Build: ModulePath is required in ModeLibrary")
	}
	lib := &gogen.Library{
		ModulePath:     opts.ModulePath,
		PkgName:        opts.PkgName,
		Funcs:          p.Funcs,
		RuntimeReplace: opts.RuntimeReplace,
	}
	files, err := gogen.EmitLibrary(lib)
	if err != nil {
		return Result{}, fmt.Errorf("gobuild.Build: emit library: %w", err)
	}
	var written []string
	for name, content := range files {
		path := filepath.Join(opts.OutDir, name)
		if err := os.WriteFile(path, content, 0o644); err != nil {
			return Result{}, fmt.Errorf("gobuild.Build: write %s: %w", path, err)
		}
		written = append(written, path)
	}
	sort.Strings(written)
	return Result{Files: written, Mode: ModeLibrary}, nil
}

// Cleanup removes the OutDir if KeepEmit is false. Callers invoke this
// once they no longer need the generated source (typically after the
// downstream `go build`/`go run` succeeds).
func Cleanup(r Result, opts Options) error {
	if opts.KeepEmit {
		return nil
	}
	for _, f := range r.Files {
		_ = os.Remove(f)
	}
	return os.Remove(opts.OutDir)
}

// BuildSource is the single-call frontend-plus-emitter pipeline used by
// the `mochi build --target=go` CLI subcommand and the A/B harness.
// It parses srcPath as Mochi, lowers it via compiler3/frontend, then
// hands the resulting Program to Build with opts.
//
// Errors are returned verbatim so a callsite can distinguish parse
// failures (parser package errors) from frontend MVP-unsupported
// surfaces (frontend package errors) from emit / write errors.
func BuildSource(srcPath string, opts Options) (Result, error) {
	prog, err := parser.Parse(srcPath)
	if err != nil {
		return Result{}, fmt.Errorf("gobuild.BuildSource: parse %s: %w", srcPath, err)
	}
	p, err := frontend.Lower(prog)
	if err != nil {
		return Result{}, fmt.Errorf("gobuild.BuildSource: lower %s: %w", srcPath, err)
	}
	return Build(p, opts)
}

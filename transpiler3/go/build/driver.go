package build

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	gemit "mochi/transpiler3/go/emit"
	glower "mochi/transpiler3/go/lower"
	"mochi/types"
)

// Target enumerates the supported MEP-54 build targets.
// Phase 1 wires only the host go-binary target; the rest are
// declared here so the CLI surface stays stable.
type Target string

const (
	TargetGoBinaryLinuxAmd64   Target = "go-linux-amd64"
	TargetGoBinaryLinuxArm64   Target = "go-linux-arm64"
	TargetGoBinaryDarwinAmd64  Target = "go-darwin-amd64"
	TargetGoBinaryDarwinArm64  Target = "go-darwin-arm64"
	TargetGoBinaryWindowsAmd64 Target = "go-windows-amd64"
	TargetGoBinaryFreeBSDAmd64 Target = "go-freebsd-amd64"
	TargetGoModule             Target = "go-module"
	TargetGoWasmJS             Target = "go-wasm-js"
	TargetGoWasiP1             Target = "go-wasip1"
)

// Profile enumerates build profiles. Phase 16 wires the names
// to `go build` flag sets; Phase 1 treats them as informational.
type Profile string

const (
	ProfileDebug   Profile = "debug"
	ProfileRelease Profile = "release"
)

// Driver is the entry point for source-to-binary builds.
// Callers construct one Driver per build and call Build.
//
// Phase 1 surface: parse, type-check, lower (aotir), lower
// (gotree), emit, write go.mod, invoke `go build` on the host
// tuple, copy binary to the caller's out path.
type Driver struct {
	// CacheDir is the directory under which a fresh work
	// sub-directory is created per Build. Empty defaults to
	// os.TempDir().
	CacheDir string

	// GoBin overrides `go` binary discovery. Empty falls back
	// to $GOROOT/bin/go or PATH `go`.
	GoBin string

	// ModulePath is the module name written into go.mod. Empty
	// defaults to "mochi_userprog".
	ModulePath string

	// KeepWorkDir, when true, leaves the per-build work
	// directory in place after Build returns. Useful for tests
	// that want to inspect the generated main.go and go.mod.
	KeepWorkDir bool

	// WorkDirPath, set by Build on success, is the absolute
	// path of the work directory the build used. Always set so
	// callers can inspect it when KeepWorkDir is true.
	WorkDirPath string
}

// Build parses src, type-checks it, lowers it through the
// shared C-transpiler aotir lowerer, lowers aotir to a
// gotree.File, renders it via emit, writes go.mod, invokes
// `go build`, and writes the produced binary to out.
//
// When target == TargetGoModule, Build skips `go build` and
// instead copies the work directory (go.mod + main.go) into
// out as a publish-ready Go module. profile is informational.
func (d *Driver) Build(src, out string, target, profile string) error {
	if src == "" {
		return errors.New("transpiler3/go/build: source path is required")
	}
	if out == "" {
		return errors.New("transpiler3/go/build: output path is required")
	}

	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("transpiler3/go/build: parse %s: %w", src, err)
	}
	if errs := types.Check(prog, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("transpiler3/go/build: type check %s: %w", src, errs[0])
	}
	ir, err := clower.Lower(prog)
	if err != nil {
		return fmt.Errorf("transpiler3/go/build: aotir lower %s: %w", src, err)
	}
	file, err := glower.Lower(ir)
	if err != nil {
		return fmt.Errorf("transpiler3/go/build: gotree lower %s: %w", src, err)
	}

	cacheRoot := d.CacheDir
	if cacheRoot == "" {
		cacheRoot = os.TempDir()
	}
	if err := os.MkdirAll(cacheRoot, 0o755); err != nil {
		return fmt.Errorf("transpiler3/go/build: mkdir cache: %w", err)
	}
	workDir, err := os.MkdirTemp(cacheRoot, "mochi-go-")
	if err != nil {
		return fmt.Errorf("transpiler3/go/build: mkdtemp: %w", err)
	}
	d.WorkDirPath = workDir
	if !d.KeepWorkDir {
		defer func() { _ = os.RemoveAll(workDir) }()
	}

	if err := writeGoMod(workDir, d.ModulePath); err != nil {
		return fmt.Errorf("transpiler3/go/build: write go.mod: %w", err)
	}
	if err := gemit.Emit(file, workDir, "main.go"); err != nil {
		return fmt.Errorf("transpiler3/go/build: emit main.go: %w", err)
	}

	absOut, err := filepath.Abs(out)
	if err != nil {
		return fmt.Errorf("transpiler3/go/build: abs %s: %w", out, err)
	}

	if Target(target) == TargetGoModule {
		if err := copyModule(workDir, absOut); err != nil {
			return fmt.Errorf("transpiler3/go/build: copy module: %w", err)
		}
		return nil
	}

	if err := goBuild(d.GoBin, workDir, absOut, nil); err != nil {
		return err
	}
	return nil
}

// copyModule copies the work directory's go.mod + *.go files
// into outDir as a publish-ready Go module. No `go build` is
// invoked; the caller can `cd outDir && go build .` themselves.
//
// The destination is created if missing. Existing files with
// the same name are overwritten so re-runs are idempotent.
func copyModule(workDir, outDir string) error {
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return fmt.Errorf("mkdir out: %w", err)
	}
	entries, err := os.ReadDir(workDir)
	if err != nil {
		return fmt.Errorf("read work dir: %w", err)
	}
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		name := e.Name()
		src := filepath.Join(workDir, name)
		dst := filepath.Join(outDir, name)
		b, err := os.ReadFile(src)
		if err != nil {
			return fmt.Errorf("read %s: %w", src, err)
		}
		if err := os.WriteFile(dst, b, 0o644); err != nil {
			return fmt.Errorf("write %s: %w", dst, err)
		}
	}
	return nil
}

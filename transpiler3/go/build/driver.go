package build

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	gemit "mochi/transpiler3/go/emit"
	glower "mochi/transpiler3/go/lower"
	"mochi/types"
)

// goEnvGoRoot returns the GOROOT reported by the configured
// `go` binary. Empty goBin falls back to resolveGoBin.
func goEnvGoRoot(goBin string) (string, error) {
	if goBin == "" {
		goBin = resolveGoBin()
	}
	cmd := exec.Command(goBin, "env", "GOROOT")
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("go env GOROOT: %w\n%s", err, stderr.String())
	}
	root := strings.TrimSpace(stdout.String())
	if root == "" {
		return "", errors.New("go env GOROOT returned empty")
	}
	return root, nil
}

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
	// Phase 10.2 Go FFI: copy any sibling *.go files next to the
	// .mochi source into the work dir as `package main` companions.
	// `extern go fun NAME(...)` declarations in the .mochi resolve
	// against these copied files at link time.
	if err := copySiblingGoFiles(src, workDir); err != nil {
		return fmt.Errorf("transpiler3/go/build: copy sibling go files: %w", err)
	}

	absOut, err := filepath.Abs(out)
	if err != nil {
		return fmt.Errorf("transpiler3/go/build: abs %s: %w", out, err)
	}

	switch Target(target) {
	case TargetGoWasiP1:
		if err := goBuild(d.GoBin, workDir, absOut, []string{"GOOS=wasip1", "GOARCH=wasm"}); err != nil {
			return err
		}
		return nil
	case TargetGoWasmJS:
		if err := goBuild(d.GoBin, workDir, absOut, []string{"GOOS=js", "GOARCH=wasm"}); err != nil {
			return err
		}
		if err := copyWasmExec(d.GoBin, filepath.Dir(absOut)); err != nil {
			return fmt.Errorf("transpiler3/go/build: copy wasm_exec.js: %w", err)
		}
		return nil
	case TargetGoModule:
		if err := os.MkdirAll(absOut, 0o755); err != nil {
			return fmt.Errorf("transpiler3/go/build: mkdir module out: %w", err)
		}
		for _, name := range []string{"main.go", "go.mod"} {
			data, err := os.ReadFile(filepath.Join(workDir, name))
			if err != nil {
				return fmt.Errorf("transpiler3/go/build: read %s: %w", name, err)
			}
			if err := os.WriteFile(filepath.Join(absOut, name), data, 0o644); err != nil {
				return fmt.Errorf("transpiler3/go/build: write %s to module out: %w", name, err)
			}
		}
		return nil
	}

	if err := goBuild(d.GoBin, workDir, absOut, nil); err != nil {
		return err
	}
	return nil
}

// copyWasmExec copies $(go env GOROOT)/lib/wasm/wasm_exec.js
// into dstDir so a browser host has the standard JS glue
// alongside the generated .wasm. Go 1.26 ships the file at
// $GOROOT/lib/wasm/wasm_exec.js; older layouts (misc/wasm)
// are not supported by this phase.
func copyWasmExec(goBin, dstDir string) error {
	root, err := goEnvGoRoot(goBin)
	if err != nil {
		return fmt.Errorf("resolve GOROOT: %w", err)
	}
	src := filepath.Join(root, "lib", "wasm", "wasm_exec.js")
	b, err := os.ReadFile(src)
	if err != nil {
		return fmt.Errorf("read %s: %w", src, err)
	}
	if err := os.MkdirAll(dstDir, 0o755); err != nil {
		return fmt.Errorf("mkdir %s: %w", dstDir, err)
	}
	return os.WriteFile(filepath.Join(dstDir, "wasm_exec.js"), b, 0o644)
}

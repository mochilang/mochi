package cbuild

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	cgen "mochi/compiler3/emit/c"
	"mochi/compiler3/frontend"
	"mochi/parser"
)

// Options captures every flag `mochi build --target=c` will
// eventually accept.
type Options struct {
	// OutDir is the directory the driver writes the emitted .c file
	// into. Required. The produced binary is also placed here unless
	// BinaryPath is set.
	OutDir string
	// BinaryPath, if non-empty, names the output executable. When
	// empty, the driver writes "$OutDir/a.out".
	BinaryPath string
	// KeepEmit retains the emitted .c file on disk after the build
	// finishes. When false, the driver removes the .c file after cc
	// succeeds (the binary is the build's artifact, the .c file is a
	// transient intermediate).
	KeepEmit bool
	// CC is the path to the C compiler. When empty, the driver looks
	// at the MOCHI_CC environment variable, then falls back to "cc".
	CC string
	// CCFlags is a per-call list of flags appended after the driver's
	// own (`-std=c99 -O2`). Empty by default.
	CCFlags []string
	// Static, when true, appends `-static` so the produced binary has
	// no dynamic libc dependency. Required by the §11.7 portable mode.
	Static bool
	// Main, when non-empty, names the Mochi function to invoke from
	// the generated `int main(void)`. When empty, the driver emits no
	// main and the produced object cannot link as a standalone
	// executable (suitable for the future --emit=c-library mode).
	Main string
}

// Result is what the driver returns on success.
type Result struct {
	// SourcePath is the path to the emitted .c file. Set even when
	// KeepEmit=false (the file may have been deleted by the time the
	// caller looks; the path is still useful for diagnostics).
	SourcePath string
	// BinaryPath is the path to the produced executable, ready to be
	// run by the caller.
	BinaryPath string
}

// Build emits p as C, invokes cc, and returns the path to the
// produced binary. It is the AOT-side analog of compiler3/build/go's
// Build, scoped to MEP-42 Phase 4.0 (single-binary `mochi build`).
func Build(p *cgen.Program, opts Options) (Result, error) {
	if opts.OutDir == "" {
		return Result{}, errors.New("cbuild.Build: OutDir is required")
	}
	if err := os.MkdirAll(opts.OutDir, 0o755); err != nil {
		return Result{}, fmt.Errorf("cbuild.Build: mkdir %s: %w", opts.OutDir, err)
	}
	if opts.Main != "" {
		p.Main = opts.Main
	}

	src, err := cgen.Emit(p)
	if err != nil {
		return Result{}, fmt.Errorf("cbuild.Build: emit: %w", err)
	}
	srcPath := filepath.Join(opts.OutDir, "gen.c")
	if err := os.WriteFile(srcPath, src, 0o644); err != nil {
		return Result{}, fmt.Errorf("cbuild.Build: write %s: %w", srcPath, err)
	}

	binPath := opts.BinaryPath
	if binPath == "" {
		binPath = filepath.Join(opts.OutDir, "a.out")
	}

	cc := resolveCC(opts.CC)
	args := []string{"-std=c99", "-O2"}
	if opts.Static {
		args = append(args, "-static")
	}
	args = append(args, opts.CCFlags...)
	args = append(args, "-o", binPath, srcPath)

	cmd := exec.Command(cc, args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return Result{SourcePath: srcPath}, fmt.Errorf("cbuild.Build: %s %s: %w\n%s",
			cc, strings.Join(args, " "), err, out)
	}

	if !opts.KeepEmit {
		_ = os.Remove(srcPath)
	}
	return Result{SourcePath: srcPath, BinaryPath: binPath}, nil
}

// BuildSource is the single-call frontend-plus-emitter pipeline used
// by `mochi build --target=c`. It parses srcPath as Mochi, lowers it
// via compiler3/frontend, then hands the resulting Program to Build
// with opts. The Mochi-side main is wired through the frontend's
// program-level entry naming (the frontend names the top-level
// function "main" when the source is a script, or the named function
// when the source is a module).
func BuildSource(srcPath string, opts Options) (Result, error) {
	prog, err := parser.Parse(srcPath)
	if err != nil {
		return Result{}, fmt.Errorf("cbuild.BuildSource: parse %s: %w", srcPath, err)
	}
	p, err := frontend.Lower(prog)
	if err != nil {
		return Result{}, fmt.Errorf("cbuild.BuildSource: lower %s: %w", srcPath, err)
	}
	cp := &cgen.Program{Funcs: p.Funcs}
	if opts.Main == "" {
		// Default: invoke the first emitted function. The frontend's
		// scripts-have-main convention puts main first; modules with
		// no main leave Main empty so the binary cannot link (the
		// caller is asking for a build artifact without an entry).
		if len(p.Funcs) > 0 {
			cp.Main = p.Funcs[0].Name
		}
	} else {
		cp.Main = opts.Main
	}
	return Build(cp, opts)
}

// resolveCC returns the C compiler to invoke. Priority: explicit
// Options.CC, then $MOCHI_CC, then "cc".
func resolveCC(explicit string) string {
	if explicit != "" {
		return explicit
	}
	if env := os.Getenv("MOCHI_CC"); env != "" {
		return env
	}
	return "cc"
}

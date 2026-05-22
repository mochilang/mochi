package cbuild

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	cgen "mochi/compiler3/emit/c"
	"mochi/compiler3/frontend"
	"mochi/parser"
	cruntime "mochi/runtime/c"
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
	// Triple, when non-empty, requests a cross-compile to the given
	// target triple (e.g. "x86_64-linux-musl", "aarch64-linux-musl",
	// "wasm32-wasi"). The driver passes `-target=<triple>` to cc and,
	// when CC and $MOCHI_CC are both empty, prefers `zig cc` as the
	// default cross-compiler because Zig ships every musl + wasi-libc
	// sysroot in-process, satisfying the §Top-line objective (single
	// foreign-arch binary from any §9 host with no extra toolchain
	// install). MEP-42 Phase 5.0.
	Triple string
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

	// Drop the C-target runtime next to gen.c. The set is small
	// (Phase 4.1: print.h, print.c) and grows as later phases land
	// (str.{h,c}, list.{h,c}, ...). The driver writes all of them
	// unconditionally so cc's link step can pick up whichever the
	// emitted source actually references; unused TUs get dead-stripped
	// by the linker. Embed-then-write avoids a per-install "where is
	// the runtime?" path search.
	runtimeSources, err := writeRuntime(opts.OutDir)
	if err != nil {
		return Result{SourcePath: srcPath}, fmt.Errorf("cbuild.Build: write runtime: %w", err)
	}

	binPath := opts.BinaryPath
	if binPath == "" {
		binPath = filepath.Join(opts.OutDir, "a.out")
	}

	ccExe, ccPrefix := resolveCC(opts.CC, opts.Triple)
	args := append([]string{}, ccPrefix...)
	args = append(args, "-std=c99", "-O2", "-I", opts.OutDir)
	if opts.Triple != "" {
		args = append(args, "-target", opts.Triple)
	}
	if opts.Static {
		args = append(args, "-static")
	}
	args = append(args, opts.CCFlags...)
	args = append(args, "-o", binPath, srcPath)
	args = append(args, runtimeSources...)
	// Always link with libm. The C target's emit.go unconditionally
	// includes <math.h>, and Phase 4.3.5 onward emits real math
	// builtins (sqrt and friends). On glibc/musl Linux libm is a
	// separate archive; on macOS / *BSD the symbols live in libSystem
	// so -lm is a no-op there. Either way it is harmless. On wasm32-wasi
	// the wasi-libc archive already carries the math symbols, so -lm
	// resolves to a vacuous archive and is still safe.
	args = append(args, "-lm")

	cmd := exec.Command(ccExe, args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return Result{SourcePath: srcPath}, fmt.Errorf("cbuild.Build: %s %s: %w\n%s",
			ccExe, strings.Join(args, " "), err, out)
	}

	if !opts.KeepEmit {
		_ = os.Remove(srcPath)
		for _, rs := range runtimeSources {
			_ = os.Remove(rs)
		}
		// Header files sit next to the .c sources; remove them too.
		_ = removeHeaders(opts.OutDir)
	}
	return Result{SourcePath: srcPath, BinaryPath: binPath}, nil
}

// writeRuntime copies every embedded runtime/c/src file into dir
// and returns the absolute paths of the .c TUs cc must link in.
// Headers (.h) are written but not returned (they are included by
// gen.c via `-I dir`, not compiled directly).
func writeRuntime(dir string) ([]string, error) {
	var csources []string
	err := fs.WalkDir(cruntime.Runtime, "src", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		data, err := cruntime.Runtime.ReadFile(path)
		if err != nil {
			return err
		}
		base := filepath.Base(path)
		out := filepath.Join(dir, base)
		if err := os.WriteFile(out, data, 0o644); err != nil {
			return err
		}
		if strings.HasSuffix(base, ".c") {
			csources = append(csources, out)
		}
		return nil
	})
	return csources, err
}

// removeHeaders strips any .h files writeRuntime placed in dir.
// Called only on KeepEmit=false. Best-effort: errors are ignored
// because the binary is already produced and intermediates leaking
// is not a build-failure-grade concern.
func removeHeaders(dir string) error {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return err
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".h") {
			continue
		}
		_ = os.Remove(filepath.Join(dir, e.Name()))
	}
	return nil
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
		// Prefer the synthesized "main" function (the frontend emits
		// it for any script that has top-level statements). Modules
		// without a main produce no script-entry function; in that
		// case fall through to Funcs[0] as a best-effort entry so
		// `mochi build --target=c module.mochi` still produces a
		// runnable artifact for a single-function module. Callers
		// who want a different entry pass Options.Main explicitly.
		for _, f := range p.Funcs {
			if f.Name == "main" {
				cp.Main = "main"
				break
			}
		}
		if cp.Main == "" && len(p.Funcs) > 0 {
			cp.Main = p.Funcs[0].Name
		}
	} else {
		cp.Main = opts.Main
	}
	return Build(cp, opts)
}

// resolveCC returns the C compiler executable plus any leading
// argument prefix the caller needs to splice in before the compiler's
// own flags. Priority: explicit Options.CC, then $MOCHI_CC, then a
// triple-aware default. When triple is non-empty and no explicit CC
// is configured, the driver prefers `zig cc` (Zig ships every musl
// and wasi-libc sysroot in-process, so cross-compiling needs no extra
// install). When triple is empty the driver falls back to the host
// `cc`. The argument prefix is the second return value: callers that
// invoke `zig cc` need "cc" as the first argv element after the zig
// binary path; host `cc` needs no prefix.
func resolveCC(explicit, triple string) (string, []string) {
	if explicit != "" {
		exe, prefix := splitCC(explicit)
		return exe, prefix
	}
	if env := os.Getenv("MOCHI_CC"); env != "" {
		exe, prefix := splitCC(env)
		return exe, prefix
	}
	if triple != "" {
		return "zig", []string{"cc"}
	}
	return "cc", nil
}

// splitCC accepts either a bare executable ("cc", "/usr/bin/clang") or
// a wrapper-form string with whitespace ("zig cc", "ccache clang") and
// returns the executable plus any leading argv tokens. This lets users
// pass `--cc="zig cc"` or `MOCHI_CC="ccache cc"` without the driver
// having to special-case wrapper compilers.
func splitCC(s string) (string, []string) {
	fields := strings.Fields(s)
	if len(fields) == 0 {
		return "cc", nil
	}
	if len(fields) == 1 {
		return fields[0], nil
	}
	return fields[0], fields[1:]
}

package build

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	gort "runtime"
	"strings"

	"mochi/parser"
	"mochi/transpiler3/c/emit"
	"mochi/transpiler3/c/lower"
	"mochi/transpiler3/c/runtime"
	"mochi/transpiler3/c/toolchain/zig"
	"mochi/types"
)

// Driver is the entry-point for source-to-binary builds.
// Callers construct one Driver per build and call Build.
//
// Phase 1 surface: parse, type-check, lower, emit, compile +
// link. Sub-phase 1.1 adds CLI flag wiring (--out, --emit=c).
// Sub-phase 1.2 adds the .mochi/cache content-addressed
// layer. Sub-phase 1.3 adds the vendored zig fallback for
// missing host cc.
type Driver struct {
	// CC overrides host-cc discovery. Empty means: discover via
	// $CC, then "cc", "clang", "gcc" on PATH; then (Phase 1.3)
	// fall back to the vendored zig.
	CC string

	// KeepEmit, when true, leaves the emitted C source next to
	// the produced binary as <out>.c. Phase 1.1 wires this from
	// --emit=c on the CLI.
	KeepEmit bool

	// EmittedCPath, set by Build on success when KeepEmit=true,
	// is the absolute path the emitted C source was written to.
	EmittedCPath string

	// NoCache disables both cache lookup and cache store. Used by
	// tests that want to exercise the full pipeline deterministically
	// without depending on per-host cache state.
	NoCache bool

	// CacheDir overrides the default cache root (see CacheRoot()).
	// Tests set this to a per-test tempdir to avoid leaking state.
	CacheDir string

	// CacheHit is set by Build on success and indicates whether
	// the output binary came from the cache (true) or was freshly
	// compiled (false). The CLI exposes this on the `cached <path>`
	// output line.
	CacheHit bool

	// NoZigFallback, when true, makes resolveCC fail rather than
	// downloading the vendored Zig if no host cc is found. Used by
	// tests that want to assert host-cc-only behaviour without
	// triggering a network fetch.
	NoZigFallback bool

	// ExtraFlags appends additional cc flags after the standard
	// compile arguments. Phase 16 uses this to inject sanitiser
	// flags (e.g. []string{"-fsanitize=address"}).
	ExtraFlags []string
}

// Build is the source-to-binary entry point. src is a Mochi
// source file; out is the desired binary path; target is a
// triple (empty for host) and profile is one of "debug",
// "release", "" (treated as "debug" for Phase 1).
//
// Phase 1 ignores target (host triple only; Phase 11 wires it)
// and ignores profile (debug-equivalent; Phase 1.2 wires it
// into the cache key).
func (d *Driver) Build(src, out, target, profile string) error {
	if src == "" {
		return errors.New("transpiler3/c/build: source path is required")
	}
	if out == "" {
		return errors.New("transpiler3/c/build: output path is required")
	}

	d.CacheHit = false
	d.EmittedCPath = ""

	srcBytes, srcErr := os.ReadFile(src)
	if srcErr != nil {
		return fmt.Errorf("transpiler3/c/build: read %s: %w", src, srcErr)
	}

	absOut, err := filepath.Abs(out)
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: abs %s: %w", out, err)
	}
	if err := os.MkdirAll(filepath.Dir(absOut), 0o755); err != nil {
		return fmt.Errorf("transpiler3/c/build: mkdir out: %w", err)
	}

	var cacheRoot, key string
	useCache := !d.NoCache && !d.KeepEmit
	if useCache {
		root := d.CacheDir
		if root == "" {
			root, err = CacheRoot()
			if err != nil {
				return fmt.Errorf("transpiler3/c/build: %w", err)
			}
		}
		rtHash, err := runtimeFingerprint()
		if err != nil {
			return fmt.Errorf("transpiler3/c/build: runtime fingerprint: %w", err)
		}
		cacheRoot = root
		key = cacheKey(src, srcBytes, profile, target, rtHash)
		hit, err := cacheLookup(cacheRoot, key, absOut)
		if err != nil {
			return fmt.Errorf("transpiler3/c/build: cache lookup: %w", err)
		}
		if hit {
			d.CacheHit = true
			return nil
		}
	}

	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: parse %s: %w", src, err)
	}
	if errs := types.Check(prog, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("transpiler3/c/build: type check %s: %w", src, errs[0])
	}
	ir, err := lower.Lower(prog)
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: lower %s: %w", src, err)
	}
	csrc, err := emit.Emit(ir)
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: emit %s: %w", src, err)
	}

	workDir, err := os.MkdirTemp("", "mochi-aot-")
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: mkdtemp: %w", err)
	}
	defer func() {
		if !d.KeepEmit {
			_ = os.RemoveAll(workDir)
		}
	}()

	if err := os.MkdirAll(filepath.Join(workDir, "include", "mochi"), 0o755); err != nil {
		return fmt.Errorf("transpiler3/c/build: mkdir include: %w", err)
	}
	if err := os.MkdirAll(filepath.Join(workDir, "src"), 0o755); err != nil {
		return fmt.Errorf("transpiler3/c/build: mkdir src: %w", err)
	}
	if err := writeRuntimeFiles(workDir); err != nil {
		return fmt.Errorf("transpiler3/c/build: stage runtime: %w", err)
	}
	genPath := filepath.Join(workDir, "main.c")
	if err := os.WriteFile(genPath, []byte(csrc), 0o644); err != nil {
		return fmt.Errorf("transpiler3/c/build: write gen: %w", err)
	}

	cc, ccPrefix, err := d.resolveCC()
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: %w", err)
	}

	rtSrcs, err := collectRuntimeSources(filepath.Join(workDir, "src"))
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: collect runtime sources: %w", err)
	}
	ccArgs := append([]string{}, ccPrefix...)
	ccArgs = append(ccArgs,
		"-std=c2x",
		"-Wall", "-Wextra", "-pedantic",
		// Phase 17.1: strip absolute paths from debug info so binaries
		// built in different tempdirs produce identical DWARF sections.
		"-ffile-prefix-map="+workDir+"=.",
		"-fdebug-prefix-map="+workDir+"=.",

		"-I", filepath.Join(workDir, "include"),
		"-o", absOut,
		genPath,
	)
	ccArgs = append(ccArgs, rtSrcs...)
	// Phase 17.1 (macOS): suppress the random UUID that Apple's linker
	// embeds in Mach-O binaries. Without this, two identical builds
	// produce different LC_UUID values, breaking binary reproducibility.
	if gort.GOOS == "darwin" {
		ccArgs = append(ccArgs, "-Wl,-no_uuid")
	}
	ccArgs = append(ccArgs, d.ExtraFlags...)
	args := ccArgs
	cmd := exec.Command(cc, args...)
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: cc %s %s: %w\n%s",
			cc, strings.Join(args, " "), err, output)
	}

	if d.KeepEmit {
		final := absOut + ".c"
		if err := os.Rename(genPath, final); err != nil {
			if data, rerr := os.ReadFile(genPath); rerr == nil {
				_ = os.WriteFile(final, data, 0o644)
			}
		}
		d.EmittedCPath = final
	}
	if useCache {
		if storeErr := cacheStore(cacheRoot, key, absOut); storeErr != nil {
			return fmt.Errorf("transpiler3/c/build: cache store: %w", storeErr)
		}
	}
	return nil
}

// collectRuntimeSources lists every *.c file under srcDir in
// lexicographic order. The build driver passes the result to cc
// alongside the generated translation unit; each new runtime
// module added to the embed FS rides in automatically once its
// file lands on disk.
func collectRuntimeSources(srcDir string) ([]string, error) {
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		return nil, err
	}
	var srcs []string
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		if !strings.HasSuffix(e.Name(), ".c") {
			continue
		}
		srcs = append(srcs, filepath.Join(srcDir, e.Name()))
	}
	return srcs, nil
}

func writeRuntimeFiles(workDir string) error {
	return fs.WalkDir(runtime.Files, ".", func(path string, e fs.DirEntry, err error) error {
		if err != nil || e.IsDir() {
			return err
		}
		data, err := runtime.Files.ReadFile(path)
		if err != nil {
			return err
		}
		dst := filepath.Join(workDir, path)
		if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
			return err
		}
		return os.WriteFile(dst, data, 0o644)
	})
}

// resolveCC looks up the C compiler to invoke and returns its
// executable path plus any argv prefix that must precede the
// compile arguments (e.g. {"cc"} for "zig", since "zig cc ..." is
// the invocation form).
//
// Discovery order:
//  1. Driver.CC (caller override). Whitespace-split: first token is
//     the executable, the rest is the prefix. Lets callers pass
//     CC="zig cc" or CC="ccache clang" without special casing.
//  2. $CC env var. Same whitespace-split semantics.
//  3. "cc", "clang", "gcc" on PATH (in that order).
//  4. Vendored Zig (downloaded on first use, SHA-256-verified
//     against the pinned manifest). Returned as `<path>/zig` with
//     prefix `["cc"]`.
//
// If discovery exhausts every step the function returns the
// combined error chain so the caller can see why each candidate
// was rejected.
func (d *Driver) resolveCC() (string, []string, error) {
	if d.CC != "" {
		exe, prefix := splitCC(d.CC)
		return exe, prefix, nil
	}
	if env := strings.TrimSpace(os.Getenv("CC")); env != "" {
		exe, prefix := splitCC(env)
		return exe, prefix, nil
	}
	for _, name := range []string{"cc", "clang", "gcc"} {
		if path, err := exec.LookPath(name); err == nil {
			return path, nil, nil
		}
	}
	if d.NoZigFallback {
		return "", nil, errors.New("no C compiler found and --no-zig-fallback set: install cc/clang/gcc on PATH or unset NoZigFallback")
	}
	zigExe, err := zig.Install()
	if err != nil {
		return "", nil, fmt.Errorf("no C compiler found on PATH and vendored zig install failed: %w", err)
	}
	return zigExe, []string{"cc"}, nil
}

// splitCC accepts either a bare executable ("cc", "/usr/bin/clang")
// or a wrapper-form string with whitespace ("zig cc", "ccache cc")
// and returns the executable plus any leading argv tokens.
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

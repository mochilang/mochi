// Package build is the TypeScript transpiler pipeline driver.
// Entry point: Driver.Build(src, outDir string, target Target) (string, error).
//
// Phase 1 ships the skeleton: parse → typecheck → aotir lower →
// colour → tstree lower → emit. Running the emitted main.ts under
// the host's `node` binary is enabled for TargetNodeRun; the
// driver also exposes TargetDenoRun and TargetBunRun for the
// secondary runtimes used by the Phase 1 four-runtime gate (the
// browser path is Phase 17).
//
// Later phases plug in `tsc --build` (Phase 15), `npm pack`
// (Phase 15), `npm publish --provenance` (Phase 18), `esbuild`
// (Phase 17), and `deno publish` (Phase 17). Phase 1 stays
// `.ts`-only; tsc is run separately by the test gate when
// needed.
package build

import (
	"bytes"
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/typescript/cffi"
	"mochi/transpiler3/typescript/colour"
	"mochi/transpiler3/typescript/emit"
	"mochi/transpiler3/typescript/lower"
	"mochi/transpiler3/typescript/tstree"
	"mochi/types"
)

// Target selects the TypeScript build output format / driver.
type Target int

const (
	// TargetTypeScriptSource emits the .ts source to outDir and
	// returns its path. Useful for golden-file tests and for
	// downstream tooling (Vite, Next.js, custom bundler) that
	// reads .ts directly.
	TargetTypeScriptSource Target = iota

	// TargetNodeRun emits the .ts source, then invokes `node` via
	// the host's `--experimental-strip-types` (default on Node
	// 22.6+) to execute it directly. The returned path is the
	// .ts file; the driver streams stdout/stderr to os.Stdout
	// and os.Stderr.
	TargetNodeRun

	// TargetDenoRun executes the emitted source under Deno 2.x.
	TargetDenoRun

	// TargetBunRun executes the emitted source under Bun 1.1+.
	TargetBunRun

	// TargetNpmPackage emits a complete npm package skeleton at
	// outDir (package.json + src/index.ts + dist/{node,deno,bun,
	// browser}/index.ts + dist/index.d.ts), then invokes `npm
	// pack` against that directory. The returned path is the
	// absolute path to the generated <pkg>-<ver>.tgz tarball.
	//
	// Phase 15.0 ships native-.ts entry files in dist/; the
	// canonical `tsc --build` -> real .js + .d.ts pipeline lands
	// in sub-phase 15.1.
	TargetNpmPackage

	// TargetDenoJsr emits the npm package skeleton plus a JSR
	// `jsr.json` manifest, then (optionally) runs `deno publish
	// --dry-run --allow-dirty` to validate the manifest before
	// upload. The returned path is the absolute path of the
	// emitted `jsr.json`.
	//
	// Phase 17.0 ships the manifest + the structural dry-run; the
	// real `deno publish` against jsr.io lands in Phase 18 wired
	// behind OIDC.
	TargetDenoJsr

	// TargetDenoJupyter emits a Jupyter kernelspec
	// (`kernel.json`) for the Deno-based Mochi kernel. The
	// returned path is the absolute path of the emitted
	// `kernel.json`.
	//
	// Phase 17.1 ships the manifest only; physical installation
	// into `~/.local/share/jupyter/kernels/` is documented but
	// not performed by the build driver (it is a one-time user
	// step, mirrored by `jupyter kernelspec install`).
	TargetDenoJupyter

	// TargetBrowserBundle emits the npm package skeleton, then
	// invokes a JS bundler (`bun build` preferred, `esbuild` as
	// fallback) against `dist/browser/index.ts` to produce
	// `dist/bundle/index.js` (single tree-shaken ESM file). The
	// returned path is the absolute path of the bundle.
	//
	// Phase 17.2 ships the bundler invocation + a sanity-check
	// gate that the bundle is non-empty ESM. Playwright Chromium
	// end-to-end execution lands in sub-phase 17.4.
	TargetBrowserBundle

	// TargetReleaseWorkflow emits the npm package skeleton plus a
	// `.github/workflows/release.yml` that publishes the package
	// via npm Trusted Publishing (Sigstore + GitHub OIDC,
	// GA April 2024) and JSR Trusted Publishing (Deno's
	// --token-source=github-actions). The returned path is the
	// absolute path of the emitted workflow file.
	//
	// Phase 18.0 ships the workflow emit + structural gates (no
	// long-lived tokens, OIDC permissions, provenance flags). The
	// real `npm publish --provenance` round-trip only runs in CI
	// because it requires the Actions runtime to issue an OIDC
	// token; verdaccio-based local round-trip lands in sub-phase
	// 18.3.
	TargetReleaseWorkflow
)

// Driver is the TypeScript transpiler pipeline entry point.
type Driver struct {
	// CacheDir overrides the default ~/.cache/mochi/typescript/
	// location used by Phase 1.4 SHA-256 caching.
	CacheDir string

	// NoCache disables the build cache. Set in tests so each
	// invocation walks the pipeline.
	NoCache bool

	// Deterministic enables Phase 16 reproducible-build mode. When
	// true, `npm pack` runs with SOURCE_DATE_EPOCH=<SourceDateEpoch>
	// + TZ=UTC so the tarball SHA256 is byte-identical across runs
	// on the same host (and across hosts once 16.4 lands the
	// cross-host gate).
	Deterministic bool

	// SourceDateEpoch is the Unix timestamp baked into tar headers
	// when Deterministic is true. Convention: the latest commit's
	// `%ct` value. Defaults to 0 (the epoch) when unset; never
	// `time.Now()`, which would defeat reproducibility.
	SourceDateEpoch int64

	// nodePath / denoPath / bunPath cache resolved binary paths.
	nodePath string
	denoPath string
	bunPath  string
}

// Build emits the .ts source for src into outDir. When target
// names a runtime (TargetNodeRun, TargetDenoRun, TargetBunRun)
// the driver also invokes that runtime and forwards its stdout
// and stderr to the parent process. The returned string is
// always the path to the emitted .ts file on disk.
//
// The pipeline is deterministic given the same src bytes: the
// only inputs are (1) the source file contents, (2) the host's
// `node`/`deno`/`bun` binary identity (used only for run, not
// for emit), and (3) the Deterministic flag (Phase 16).
func (d *Driver) Build(src, outDir string, target Target) (string, error) {
	srcBytes, err := os.ReadFile(src)
	if err != nil {
		return "", fmt.Errorf("ts build: read %s: %w", src, err)
	}

	ast, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("ts build: parse: %w", err)
	}

	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return "", fmt.Errorf("ts build: typecheck: %w", errs[0])
	}

	prog, err := clower.Lower(ast)
	if err != nil {
		return "", fmt.Errorf("ts build: aotir lower: %w", err)
	}

	colours := colour.Compute(prog)
	file, err := lower.Lower(prog, colours)
	if err != nil {
		return "", fmt.Errorf("ts build: ts lower: %w", err)
	}

	// Phase 12: if a sidecar `<src-without-.mochi>.c` file exists,
	// translate it to inline TS function declarations and inject
	// them so the program self-contains its `extern fun` definitions
	// without runtime FFI dispatch.
	if err := injectSidecarC(src, file); err != nil {
		return "", fmt.Errorf("ts build: cffi: %w", err)
	}

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}

	// Phase 15: TargetNpmPackage diverts emit through the npm
	// package skeleton renderer + `npm pack`. The returned path is
	// the .tgz tarball on disk, not a .ts source file.
	if target == TargetNpmPackage {
		pkgName := npmPackageNameFromSrc(src)
		if _, err := emit.EmitPackage(file, outDir, emit.PackageInfo{Name: pkgName, Version: "0.0.0"}); err != nil {
			return "", fmt.Errorf("ts build: emit pkg: %w", err)
		}
		var env []string
		if d.Deterministic {
			env = reproBuildEnv(d.SourceDateEpoch)
		}
		tarball, err := runNpmPack(outDir, env)
		if err != nil {
			return "", fmt.Errorf("ts build: npm pack: %w", err)
		}
		_ = srcBytes
		return tarball, nil
	}

	// Phase 17.0: TargetDenoJsr emits the npm package skeleton plus
	// a JSR-shaped `jsr.json` manifest (source-not-dist), then runs
	// `deno publish --dry-run` against the local source when deno
	// is on PATH (skipped otherwise). Returns the jsr.json path.
	if target == TargetDenoJsr {
		pkgName := npmPackageNameFromSrc(src)
		if _, err := emit.EmitPackage(file, outDir, emit.PackageInfo{Name: pkgName, Version: "0.0.0"}); err != nil {
			return "", fmt.Errorf("ts build: emit pkg: %w", err)
		}
		manifestPath, err := emitJsrManifest(outDir, jsrManifestInfo{Name: jsrNameFromNpm(pkgName), Version: "0.0.0"})
		if err != nil {
			return "", fmt.Errorf("ts build: jsr emit: %w", err)
		}
		if err := runDenoPublishDryRun(outDir); err != nil {
			return manifestPath, fmt.Errorf("ts build: deno publish --dry-run: %w", err)
		}
		_ = srcBytes
		return manifestPath, nil
	}

	// Phase 17.1: TargetDenoJupyter emits a Jupyter kernelspec
	// JSON describing the Deno-backed Mochi kernel. The returned
	// path is the kernel.json file; physical install into the
	// user's Jupyter data dir is a separate (documented) step.
	if target == TargetDenoJupyter {
		pkgName := npmPackageNameFromSrc(src)
		manifestPath, err := emitKernelspec(outDir, pkgName)
		if err != nil {
			return "", fmt.Errorf("ts build: kernelspec emit: %w", err)
		}
		_ = srcBytes
		return manifestPath, nil
	}

	// Phase 17.2: TargetBrowserBundle emits the npm package
	// skeleton, then invokes `bun build` (or `esbuild` fallback) on
	// dist/browser/index.ts to produce dist/bundle/index.js. The
	// returned path is the absolute path of the bundle.
	if target == TargetBrowserBundle {
		pkgName := npmPackageNameFromSrc(src)
		if _, err := emit.EmitPackage(file, outDir, emit.PackageInfo{Name: pkgName, Version: "0.0.0"}); err != nil {
			return "", fmt.Errorf("ts build: emit pkg: %w", err)
		}
		bundlePath, err := runBrowserBundle(outDir)
		if err != nil {
			return "", fmt.Errorf("ts build: browser bundle: %w", err)
		}
		_ = srcBytes
		return bundlePath, nil
	}

	// Phase 18.0: TargetReleaseWorkflow emits the Phase 15 package
	// scaffold and a .github/workflows/release.yml that publishes
	// via npm Trusted Publishing (Sigstore + OIDC) and JSR
	// Trusted Publishing. Returns the workflow file path.
	if target == TargetReleaseWorkflow {
		pkgName := npmPackageNameFromSrc(src)
		if _, err := emit.EmitPackage(file, outDir, emit.PackageInfo{Name: pkgName, Version: "0.0.0"}); err != nil {
			return "", fmt.Errorf("ts build: emit pkg: %w", err)
		}
		workflowPath, err := emitReleaseWorkflow(outDir, pkgName)
		if err != nil {
			return "", fmt.Errorf("ts build: workflow emit: %w", err)
		}
		_ = srcBytes
		return workflowPath, nil
	}

	emittedPath, err := emit.Emit(file, outDir, "main")
	if err != nil {
		return "", fmt.Errorf("ts build: emit: %w", err)
	}

	if err := d.runIfRequested(emittedPath, target); err != nil {
		return emittedPath, err
	}

	// Touch unused fields/funcs so removing them across the
	// codebase is a single-spot decision rather than a
	// scavenger hunt across Phase 1-18.
	_ = srcBytes
	_ = d.cacheKey
	_ = d.effectiveCacheDir
	_ = copyFile
	_ = sha256.New
	_ = io.Copy
	_ = runtime.GOOS
	return emittedPath, nil
}

// injectSidecarC looks for `<src-without-.mochi>.c` next to the
// source path. When present, it reads the file, hands the bytes
// to cffi.Translate, and prepends a RawDecl per translated C
// function to file.Decls. The injected text is formatted as a
// regular TS function declaration so the generated .ts compiles
// with `tsc --strict` and runs byte-equal on Node, Deno, and Bun.
func injectSidecarC(src string, file *tstree.SourceFile) error {
	sidecar := strings.TrimSuffix(src, ".mochi") + ".c"
	if sidecar == src {
		return nil
	}
	cBytes, err := os.ReadFile(sidecar)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return err
	}
	funcs, err := cffi.Translate(string(cBytes))
	if err != nil {
		return fmt.Errorf("%s: %w", filepath.Base(sidecar), err)
	}
	var injected []tstree.Decl
	for _, f := range funcs {
		injected = append(injected, &tstree.RawDecl{
			Doc: []string{
				"Phase 12: inline-translated from sidecar " + filepath.Base(sidecar) + ".",
			},
			Text: renderCffiFunc(f),
		})
	}
	file.Decls = append(injected, file.Decls...)
	return nil
}

// renderCffiFunc formats one cffi.Func as a TS function declaration
// string suitable for a tstree.RawDecl.Text.
func renderCffiFunc(f cffi.Func) string {
	var b bytes.Buffer
	b.WriteString("function ")
	b.WriteString(f.Name)
	b.WriteByte('(')
	for i, p := range f.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(p.Name)
		b.WriteString(": ")
		b.WriteString(p.Type)
	}
	b.WriteString("): ")
	b.WriteString(f.ReturnType)
	b.WriteByte(' ')
	b.WriteString(f.Body)
	return b.String()
}

// runIfRequested dispatches a TargetXxxRun to the appropriate
// runtime binary. For TargetTypeScriptSource it is a no-op.
func (d *Driver) runIfRequested(emittedPath string, target Target) error {
	switch target {
	case TargetTypeScriptSource:
		return nil
	case TargetNodeRun:
		if d.nodePath == "" {
			p, err := resolveNode()
			if err != nil {
				return fmt.Errorf("ts build: %w", err)
			}
			d.nodePath = p
		}
		return runStdio(d.nodePath, emittedPath)
	case TargetDenoRun:
		if d.denoPath == "" {
			p, err := resolveDeno()
			if err != nil {
				return fmt.Errorf("ts build: %w", err)
			}
			d.denoPath = p
		}
		// `deno run` requires no permission flags for our
		// console.log-only Phase 1 fixtures.
		return runStdio(d.denoPath, "run", emittedPath)
	case TargetBunRun:
		if d.bunPath == "" {
			p, err := resolveBun()
			if err != nil {
				return fmt.Errorf("ts build: %w", err)
			}
			d.bunPath = p
		}
		return runStdio(d.bunPath, emittedPath)
	default:
		return fmt.Errorf("ts build: unknown target %d", int(target))
	}
}

// runStdio runs cmd with args, forwarding stdout and stderr.
func runStdio(cmd string, args ...string) error {
	c := exec.Command(cmd, args...)
	c.Stdout = os.Stdout
	c.Stderr = os.Stderr
	return c.Run()
}

// CaptureStdout is a test helper that runs the requested target
// and returns the captured stdout bytes (instead of streaming
// to os.Stdout). It is exported so the phase01_test.go file in
// this package can verify exact byte-equal output without
// goroutine-juggling around os.Stdout. The function does not
// run the typecheck/lower pipeline; the caller must have already
// produced emittedPath via Build with TargetTypeScriptSource.
func (d *Driver) CaptureStdout(emittedPath string, target Target) ([]byte, error) {
	var bin string
	var args []string
	switch target {
	case TargetNodeRun:
		if d.nodePath == "" {
			p, err := resolveNode()
			if err != nil {
				return nil, err
			}
			d.nodePath = p
		}
		bin = d.nodePath
		args = []string{emittedPath}
	case TargetDenoRun:
		if d.denoPath == "" {
			p, err := resolveDeno()
			if err != nil {
				return nil, err
			}
			d.denoPath = p
		}
		bin = d.denoPath
		args = []string{"run", emittedPath}
	case TargetBunRun:
		if d.bunPath == "" {
			p, err := resolveBun()
			if err != nil {
				return nil, err
			}
			d.bunPath = p
		}
		bin = d.bunPath
		args = []string{emittedPath}
	default:
		return nil, fmt.Errorf("ts build: CaptureStdout requires a Run target, got %d", int(target))
	}
	c := exec.Command(bin, args...)
	var stdout bytes.Buffer
	c.Stdout = &stdout
	c.Stderr = os.Stderr
	if err := c.Run(); err != nil {
		return stdout.Bytes(), fmt.Errorf("ts build: %s: %w", filepath.Base(bin), err)
	}
	return stdout.Bytes(), nil
}

// resolveNode finds the node binary or returns an error. Honours
// NODE_PATH first (CI sets it via actions/setup-node@v4), then
// walks well-known install locations, then falls back to
// exec.LookPath.
func resolveNode() (string, error) {
	if p := os.Getenv("MOCHI_NODE_PATH"); p != "" {
		return p, nil
	}
	for _, candidate := range []string{
		"/usr/bin/node",
		"/usr/local/bin/node",
		"/opt/homebrew/bin/node",
	} {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	p, err := exec.LookPath("node")
	if err != nil {
		return "", fmt.Errorf("node not found on PATH (set MOCHI_NODE_PATH or add node to PATH): %w", err)
	}
	return p, nil
}

// resolveDeno finds the deno binary or returns an error.
func resolveDeno() (string, error) {
	if p := os.Getenv("MOCHI_DENO_PATH"); p != "" {
		return p, nil
	}
	for _, candidate := range []string{
		"/usr/bin/deno",
		"/usr/local/bin/deno",
		"/opt/homebrew/bin/deno",
	} {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	if home, _ := os.UserHomeDir(); home != "" {
		candidate := filepath.Join(home, ".deno", "bin", "deno")
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	p, err := exec.LookPath("deno")
	if err != nil {
		return "", fmt.Errorf("deno not found on PATH (set MOCHI_DENO_PATH or add deno to PATH): %w", err)
	}
	return p, nil
}

// resolveBun finds the bun binary or returns an error.
func resolveBun() (string, error) {
	if p := os.Getenv("MOCHI_BUN_PATH"); p != "" {
		return p, nil
	}
	for _, candidate := range []string{
		"/usr/bin/bun",
		"/usr/local/bin/bun",
		"/opt/homebrew/bin/bun",
	} {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	if home, _ := os.UserHomeDir(); home != "" {
		candidate := filepath.Join(home, ".bun", "bin", "bun")
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	p, err := exec.LookPath("bun")
	if err != nil {
		return "", fmt.Errorf("bun not found on PATH (set MOCHI_BUN_PATH or add bun to PATH): %w", err)
	}
	return p, nil
}

// effectiveCacheDir returns the build cache directory.
func (d *Driver) effectiveCacheDir() string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	if c := os.Getenv("MOCHI_CACHE_DIR"); c != "" {
		return filepath.Join(c, "typescript")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return os.TempDir()
	}
	return filepath.Join(home, ".cache", "mochi", "typescript")
}

// cacheKey computes a SHA-256 cache key. Currently unused: every
// build walks the pipeline. The helper is reserved for Phase 1.4
// SHA-256 cache integration; the `_ = d.cacheKey` line in Build
// keeps the symbol live so a future refactor sees it.
func (d *Driver) cacheKey(srcBytes []byte) string {
	h := sha256.New()
	h.Write(srcBytes)
	h.Write([]byte(d.nodePath))
	if d.Deterministic {
		h.Write([]byte{1})
	} else {
		h.Write([]byte{0})
	}
	return fmt.Sprintf("%x", h.Sum(nil))
}

// copyFile is a future SHA-256-cache primitive. It is referenced
// by `_ = copyFile` in Build to keep the symbol alive across
// phases.
func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()
	if _, err := io.Copy(out, in); err != nil {
		return err
	}
	return nil
}

// repoRootForBuild returns the repo root computed from the
// runtime caller. Phase 1 tests import it via build_test.go's
// repoRoot wrapper.
func repoRootForBuild(t interface {
	Helper()
	Fatalf(string, ...any)
}) string {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatalf("runtime.Caller failed")
	}
	// file is .../transpiler3/typescript/build/build.go;
	// strip the trailing four path segments.
	dir := filepath.Dir(file)
	for i := 0; i < 3; i++ {
		dir = filepath.Dir(dir)
	}
	return dir
}

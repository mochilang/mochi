package build

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase17JsrManifest asserts the emitted jsr.json has the
// correct shape for the JSR registry's minimal manifest:
//
//   - "name" is JSR-scoped (starts with "@")
//   - "version" is a non-empty semver-shaped string
//   - "exports" points at the source TS entry (./src/index.ts),
//     not the dist/ tree (JSR transpiles server-side)
//   - "publish.include" whitelists src/**/*.ts + README + LICENSE
//
// The Phase 17.0 spec mandates source-not-dist: uploading dist/
// would double the payload and the server would re-transpile
// anyway. This gate fails if a future emit accidentally points at
// the dist/ tree.
func TestPhase17JsrManifest(t *testing.T) {
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	manifestPath, err := d.Build(mochiPath, pkgDir, TargetDenoJsr)
	if err != nil {
		t.Fatalf("Build TargetDenoJsr: %v", err)
	}
	raw, err := os.ReadFile(manifestPath)
	if err != nil {
		t.Fatalf("read jsr.json: %v", err)
	}
	var manifest struct {
		Name    string `json:"name"`
		Version string `json:"version"`
		License string `json:"license"`
		Exports string `json:"exports"`
		Publish struct {
			Include []string `json:"include"`
		} `json:"publish"`
	}
	if err := json.Unmarshal(raw, &manifest); err != nil {
		t.Fatalf("parse jsr.json: %v", err)
	}
	if !strings.HasPrefix(manifest.Name, "@") {
		t.Errorf(`jsr.json name not JSR-scoped: %q`, manifest.Name)
	}
	if !strings.Contains(manifest.Name, "/") {
		t.Errorf(`jsr.json name missing slash: %q`, manifest.Name)
	}
	if manifest.Version == "" {
		t.Errorf("jsr.json missing version")
	}
	if manifest.License == "" {
		t.Errorf("jsr.json missing license (deno publish refuses to upload without one)")
	}
	if manifest.Exports != "./src/index.ts" {
		t.Errorf(`jsr.json exports = %q, want "./src/index.ts" (source-not-dist invariant)`, manifest.Exports)
	}
	wantInclude := map[string]bool{"src/**/*.ts": true, "README.md": true, "LICENSE": true}
	gotInclude := map[string]bool{}
	for _, e := range manifest.Publish.Include {
		gotInclude[e] = true
	}
	for k := range wantInclude {
		if !gotInclude[k] {
			t.Errorf("jsr.json publish.include missing %q (got %v)", k, manifest.Publish.Include)
		}
	}
	for _, e := range manifest.Publish.Include {
		if strings.HasPrefix(e, "dist/") {
			t.Errorf("jsr.json publish.include leaks dist/: %q", e)
		}
	}
}

// TestPhase17JsrSrcExists asserts the JSR target also produced
// the src/index.ts file that jsr.json points at (Phase 15
// scaffold reuse). If the scaffold drifts and stops emitting
// src/, the JSR publish would silently upload an empty payload.
func TestPhase17JsrSrcExists(t *testing.T) {
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase02-scalars", "arith_add.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if _, err := d.Build(mochiPath, pkgDir, TargetDenoJsr); err != nil {
		t.Fatalf("Build TargetDenoJsr: %v", err)
	}
	srcEntry := filepath.Join(pkgDir, "src", "index.ts")
	body, err := os.ReadFile(srcEntry)
	if err != nil {
		t.Fatalf("read %s: %v", srcEntry, err)
	}
	if len(body) == 0 {
		t.Errorf("src/index.ts is empty; JSR publish would upload nothing")
	}
}

// TestPhase17JsrDryRun runs `deno publish --dry-run --allow-dirty`
// against the emitted package directory. The dry-run validates
// the manifest, walks the include list, and reports any errors
// without uploading; this is the structural Phase 17.0 gate.
//
// Skipped if deno is not on PATH (CI without the Deno setup
// action). The TestPhase17JsrManifest gate above still runs.
func TestPhase17JsrDryRun(t *testing.T) {
	if _, err := resolveDeno(); err != nil {
		t.Skipf("deno not on PATH: %v", err)
	}
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if _, err := d.Build(mochiPath, pkgDir, TargetDenoJsr); err != nil {
		t.Fatalf("Build TargetDenoJsr (with deno on PATH): %v", err)
	}
}

// TestPhase17JupyterKernelspec asserts the emitted Jupyter
// kernel.json has the structural fields required by the Jupyter
// kernelspec protocol:
//
//   - "argv" is a non-empty array starting with the runtime name
//     (deno) and contains the {connection_file} placeholder
//   - "display_name" is non-empty (otherwise JupyterLab shows
//     a blank entry in the kernel chooser)
//   - "language" is "mochi" (so notebook front-ends can route
//     syntax highlighting through a Mochi codemirror mode)
//
// This is the Phase 17.1 structural gate. The real install gate
// (jupyter kernelspec install ...) is deferred to sub-phase
// 17.4 because it requires a Jupyter install on the host.
func TestPhase17JupyterKernelspec(t *testing.T) {
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	specPath, err := d.Build(mochiPath, outDir, TargetDenoJupyter)
	if err != nil {
		t.Fatalf("Build TargetDenoJupyter: %v", err)
	}
	if filepath.Base(specPath) != "kernel.json" {
		t.Errorf("kernelspec basename = %q, want %q", filepath.Base(specPath), "kernel.json")
	}
	raw, err := os.ReadFile(specPath)
	if err != nil {
		t.Fatalf("read kernel.json: %v", err)
	}
	var spec struct {
		Argv        []string `json:"argv"`
		DisplayName string   `json:"display_name"`
		Language    string   `json:"language"`
	}
	if err := json.Unmarshal(raw, &spec); err != nil {
		t.Fatalf("parse kernel.json: %v", err)
	}
	if len(spec.Argv) == 0 || spec.Argv[0] != "deno" {
		t.Errorf("kernel.json argv must start with deno, got %v", spec.Argv)
	}
	hasConnectionFile := false
	for _, a := range spec.Argv {
		if a == "{connection_file}" {
			hasConnectionFile = true
			break
		}
	}
	if !hasConnectionFile {
		t.Errorf("kernel.json argv missing {connection_file} placeholder: %v", spec.Argv)
	}
	if spec.DisplayName == "" {
		t.Errorf("kernel.json display_name is empty")
	}
	if spec.Language != "mochi" {
		t.Errorf(`kernel.json language = %q, want "mochi"`, spec.Language)
	}
}

// TestPhase17BrowserBundle is the Phase 17.2 gate: the build
// driver produces a non-empty ESM bundle at dist/bundle/index.js
// that's syntactically valid JavaScript.
//
// Skipped if neither bun nor esbuild is on PATH. The bundle's
// runtime correctness (Playwright Chromium loads it and stdout
// matches vm3) is deferred to sub-phase 17.4 because the
// Playwright harness adds heavy CI infra.
func TestPhase17BrowserBundle(t *testing.T) {
	if _, bunErr := resolveBun(); bunErr != nil {
		if _, esbErr := lookEsbuild(); esbErr != nil {
			t.Skipf("neither bun nor esbuild on PATH: %v / %v", bunErr, esbErr)
		}
	}
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	bundlePath, err := d.Build(mochiPath, pkgDir, TargetBrowserBundle)
	if err != nil {
		t.Fatalf("Build TargetBrowserBundle: %v", err)
	}
	body, err := os.ReadFile(bundlePath)
	if err != nil {
		t.Fatalf("read bundle: %v", err)
	}
	if len(body) == 0 {
		t.Errorf("browser bundle is empty: %s", bundlePath)
	}
	text := string(body)
	if !strings.Contains(text, "console.log") {
		t.Errorf("bundle missing expected console.log call (hello_int prints via console.log):\n%s", text)
	}
	// Browser bundles must never carry CommonJS require() calls.
	// Both bun and esbuild emit ESM imports; a `require(` in the
	// output is a regression that would silently fail in the
	// browser (no `require` global).
	if strings.Contains(text, "require(") {
		t.Errorf("bundle contains require() call (CommonJS in browser bundle is a regression):\n%s", text)
	}
}

// TestPhase17BundleSize is a sanity check on tree-shaking: the
// hello-world bundle should be under 50 KB minified-or-unminified.
// A regression that pulls in a heavy runtime dep (whole stdlib,
// whole observability harness) would blow past this.
func TestPhase17BundleSize(t *testing.T) {
	if _, bunErr := resolveBun(); bunErr != nil {
		if _, esbErr := lookEsbuild(); esbErr != nil {
			t.Skipf("neither bun nor esbuild on PATH: %v / %v", bunErr, esbErr)
		}
	}
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	bundlePath, err := d.Build(mochiPath, pkgDir, TargetBrowserBundle)
	if err != nil {
		t.Fatalf("Build TargetBrowserBundle: %v", err)
	}
	info, err := os.Stat(bundlePath)
	if err != nil {
		t.Fatalf("stat bundle: %v", err)
	}
	const maxSize = 50 * 1024
	if info.Size() > maxSize {
		t.Errorf("hello bundle is %d bytes, want <= %d (tree-shaking regression?)", info.Size(), maxSize)
	}
}

// TestPhase17JsrNameFromNpm asserts the npm-to-JSR name mapping
// matches the Mochi project's reserved scope convention:
// `mochi-<basename>` (npm) maps to `@mochi/<basename>` (JSR).
func TestPhase17JsrNameFromNpm(t *testing.T) {
	cases := []struct{ npm, jsr string }{
		{"mochi-hello", "@mochi/hello"},
		{"mochi-arith-add", "@mochi/arith-add"},
		{"mochi-list-for-each-bool", "@mochi/list-for-each-bool"},
		{"not-prefixed", "@mochi/not-prefixed"},
	}
	for _, tc := range cases {
		got := jsrNameFromNpm(tc.npm)
		if got != tc.jsr {
			t.Errorf("jsrNameFromNpm(%q) = %q, want %q", tc.npm, got, tc.jsr)
		}
	}
}

// lookEsbuild reports whether esbuild is on PATH. Returns the
// resolved path or an error.
func lookEsbuild() (string, error) {
	return exec.LookPath("esbuild")
}

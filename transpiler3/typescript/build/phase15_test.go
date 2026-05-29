package build

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase15NpmPackageBun is the Phase 15 primary gate. For each
// fixture in the curated cross-phase corpus, the driver emits a
// complete npm package skeleton (package.json + src/ + dist/),
// runs `npm pack` to produce a .tgz, installs that tarball into a
// fresh temp directory, and runs the installed package entry with
// Bun 1.1. The captured stdout is byte-equal against the recorded
// .out from the corresponding phase fixture.
//
// The corpus is 6 fixtures, one per major lowering category (hello,
// scalars, lists, closures, query DSL, agents). This exercises the
// emit -> pack -> install -> run pipeline across the breadth of
// language features the transpiler supports through Phase 14
// without paying the cost of a full ~400-fixture run.
//
// Why Bun and not Node: Node 23+ refuses to strip TS types from
// files under node_modules/ (ERR_UNSUPPORTED_NODE_MODULES_TYPE_
// STRIPPING). Phase 15.0 dist/ ships `.ts` files; the Node install
// gate waits for sub-phase 15.1 which adds the `tsc --build` step
// that produces real `.js` + `.d.ts`. Bun runs `.ts` from
// node_modules natively, so the Phase 15.0 gate uses Bun.
//
// Per MEP-52 §Phase 15, the spec gate is the full Phase 1-14 corpus
// on Node + Deno + Bun + Chromium. That's deferred to sub-phase
// 15.5 (full-corpus scaling) + 15.1 (Node via tsc) + 15.2 (Deno via
// npm: specifier) + 15.4 (Chromium via Playwright).
//
// Floor: 5 fixtures per the curated corpus. Below that, structural
// coverage of the lowering surface stops being meaningful.
func TestPhase15NpmPackageBun(t *testing.T) {
	fixtures := []struct {
		dir  string
		name string
	}{
		{"phase01-hello", "hello_int"},
		{"phase02-scalars", "arith_add"},
		{"phase03.1-lists", "list_for_each_bool"},
		{"phase06-closures", "req_capture_adder"},
		{"phase07-query", "req_filter_int"},
		{"phase09-agents", "agent_basic"},
	}
	if len(fixtures) < 5 {
		t.Fatalf("Phase 15 curated corpus has %d fixtures, expected at least 5", len(fixtures))
	}
	for _, fx := range fixtures {
		t.Run(fx.dir+"/"+fx.name, func(t *testing.T) {
			runPhase15NpmFixture(t, fx.dir, fx.name)
		})
	}
}

// runPhase15NpmFixture runs the end-to-end Phase 15 pipeline for
// one fixture: build npm package -> pack -> install -> run on Bun
// -> diff stdout against the recorded .out.
func runPhase15NpmFixture(t *testing.T, fixtureDir, fixtureName string) {
	t.Helper()
	if _, ok := resolveRuntime("bun"); !ok {
		t.Skip("bun not on PATH (Phase 15.0 uses Bun for native .ts node_modules execution)")
	}
	if _, err := resolveNpm(); err != nil {
		t.Skipf("npm not on PATH: %v", err)
	}
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", fixtureDir, fixtureName+".mochi")
	wantPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", fixtureDir, fixtureName+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want %s: %v", wantPath, err)
	}
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	tarball, err := d.Build(mochiPath, pkgDir, TargetNpmPackage)
	if err != nil {
		t.Fatalf("Build TargetNpmPackage: %v", err)
	}
	if !strings.HasSuffix(tarball, ".tgz") {
		t.Fatalf("expected .tgz tarball, got %s", tarball)
	}
	if info, err := os.Stat(tarball); err != nil || info.Size() == 0 {
		t.Fatalf("tarball %s missing/empty: %v", tarball, err)
	}
	installDir := t.TempDir()
	got, err := installAndRunBun(installDir, tarball)
	if err != nil {
		t.Fatalf("install+run: %v", err)
	}
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}

// TestPhase15PackageJSONShape asserts the emitted package.json has
// the load-bearing fields per MEP-52 §Phase 15.2: name + version +
// type=module + exports."."."types"/"node"/"default" + files
// whitelist + engines.node. These are the invariants that a future
// refactor must preserve for the `npm pack` + install path to
// keep working across runtimes.
func TestPhase15PackageJSONShape(t *testing.T) {
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if _, err := d.Build(mochiPath, pkgDir, TargetNpmPackage); err != nil {
		if strings.Contains(err.Error(), "npm pack") || strings.Contains(err.Error(), "npm not found") {
			t.Skipf("npm pack unavailable: %v", err)
		}
		t.Fatalf("Build: %v", err)
	}
	raw, err := os.ReadFile(filepath.Join(pkgDir, "package.json"))
	if err != nil {
		t.Fatalf("read package.json: %v", err)
	}
	var pkg map[string]any
	if err := json.Unmarshal(raw, &pkg); err != nil {
		t.Fatalf("parse package.json: %v\n%s", err, raw)
	}
	if got := pkg["name"]; got != "mochi-hello_int" {
		t.Errorf("name = %v, want mochi-hello_int", got)
	}
	if got := pkg["type"]; got != "module" {
		t.Errorf("type = %v, want module", got)
	}
	if got := pkg["main"]; got != "./dist/node/index.ts" {
		t.Errorf("main = %v, want ./dist/node/index.ts", got)
	}
	if got := pkg["types"]; got != "./dist/index.d.ts" {
		t.Errorf("types = %v, want ./dist/index.d.ts", got)
	}
	exp, ok := pkg["exports"].(map[string]any)
	if !ok {
		t.Fatalf("exports not a map: %T", pkg["exports"])
	}
	dot, ok := exp["."].(map[string]any)
	if !ok {
		t.Fatalf(`exports["."] not a map: %T`, exp["."])
	}
	for _, k := range []string{"types", "node", "deno", "bun", "browser", "default"} {
		if _, present := dot[k]; !present {
			t.Errorf(`exports["."]: missing conditional %q`, k)
		}
	}
	files, ok := pkg["files"].([]any)
	if !ok {
		t.Fatalf("files not an array: %T", pkg["files"])
	}
	hasDist := false
	for _, f := range files {
		if s, ok := f.(string); ok && s == "dist/" {
			hasDist = true
		}
	}
	if !hasDist {
		t.Errorf(`files whitelist missing "dist/": %v`, files)
	}
	engines, ok := pkg["engines"].(map[string]any)
	if !ok {
		t.Fatalf("engines not a map: %T", pkg["engines"])
	}
	if got := engines["node"]; got != ">=22" {
		t.Errorf(`engines.node = %v, want ">=22"`, got)
	}
}

// TestPhase15FilesWhitelist asserts the emitted tarball ships dist/
// only: it never contains src/, tsconfig*.json, or node_modules/.
// The npm `files` whitelist is the security boundary here: anything
// not on it gets stripped from the tarball regardless of whether
// the package directory contains it.
//
// Reads the tarball with the standard library's tar + gzip readers
// (no external dep) and walks every entry name. The npm spec puts
// every file under a top-level `package/` prefix, which we strip
// before matching.
func TestPhase15FilesWhitelist(t *testing.T) {
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	tarball, err := d.Build(mochiPath, pkgDir, TargetNpmPackage)
	if err != nil {
		if strings.Contains(err.Error(), "npm pack") || strings.Contains(err.Error(), "npm not found") {
			t.Skipf("npm pack unavailable: %v", err)
		}
		t.Fatalf("Build: %v", err)
	}
	names, err := readTarballNames(tarball)
	if err != nil {
		t.Fatalf("read tarball: %v", err)
	}
	forbidden := []string{
		"src/",
		"tsconfig.json",
		"tsconfig.base.json",
		"node_modules/",
		".eslintrc",
		".prettierrc",
	}
	for _, n := range names {
		stripped := strings.TrimPrefix(n, "package/")
		for _, bad := range forbidden {
			if strings.HasPrefix(stripped, bad) || strings.Contains(stripped, "/"+bad) {
				t.Errorf("tarball contains forbidden entry %q (matches %q)", n, bad)
			}
		}
	}
	// Positive assertions: dist/ + package.json must be present.
	hasDist := false
	hasPkgJSON := false
	for _, n := range names {
		stripped := strings.TrimPrefix(n, "package/")
		if strings.HasPrefix(stripped, "dist/") {
			hasDist = true
		}
		if stripped == "package.json" {
			hasPkgJSON = true
		}
	}
	if !hasDist {
		t.Errorf("tarball missing dist/ entries: %v", names)
	}
	if !hasPkgJSON {
		t.Errorf("tarball missing package.json: %v", names)
	}
}

// TestPhase15ExportsKeyOrder asserts the canonical key order of
// exports."." in the emitted package.json. TypeScript's resolver
// picks the first matching condition, so `types` must come first.
// MEP-52 §Phase 15.2 mandates the order
// types, deno, bun, browser, node, default.
//
// json.Marshal does not preserve map key order in Go, so the
// emitter writes the inner object as a json.RawMessage with hand-
// crafted ordering. This test guards that invariant by reading
// the raw bytes (not the parsed map) and asserting the substring
// pattern.
func TestPhase15ExportsKeyOrder(t *testing.T) {
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if _, err := d.Build(mochiPath, pkgDir, TargetNpmPackage); err != nil {
		if strings.Contains(err.Error(), "npm pack") || strings.Contains(err.Error(), "npm not found") {
			t.Skipf("npm pack unavailable: %v", err)
		}
		t.Fatalf("Build: %v", err)
	}
	raw, err := os.ReadFile(filepath.Join(pkgDir, "package.json"))
	if err != nil {
		t.Fatalf("read package.json: %v", err)
	}
	// MarshalIndent re-indents the embedded json.RawMessage so
	// every conditional key appears on its own line. The order
	// invariant we need: within the exports."." object, "types"
	// is the first key listed. Scoping to the exports map (locate
	// the `"exports": {` opener, then the `".":` opener) avoids
	// false hits on the top-level `"types"` mirror.
	text := string(raw)
	exportsAt := strings.Index(text, `"exports":`)
	if exportsAt < 0 {
		t.Fatalf(`package.json missing "exports": %s`, text)
	}
	dotAt := strings.Index(text[exportsAt:], `".":`)
	if dotAt < 0 {
		t.Fatalf(`package.json missing exports["."]: %s`, text[exportsAt:])
	}
	region := text[exportsAt+dotAt:]
	idx := func(s string) int { return strings.Index(region, s) }
	typesIdx := idx(`"types"`)
	if typesIdx < 0 {
		t.Fatalf(`exports["."] missing "types" key: %s`, region)
	}
	for _, after := range []string{`"deno"`, `"bun"`, `"browser"`, `"node"`, `"default"`} {
		i := idx(after)
		if i < 0 {
			t.Errorf(`exports["."] missing %s\n%s`, after, region)
			continue
		}
		if i < typesIdx {
			t.Errorf(`exports["."]: %s appears before "types" (order violation):\n%s`, after, region)
		}
	}
}

// readTarballNames walks a gzip-compressed tarball and returns the
// list of every entry's Name field (in archive order).
func readTarballNames(tarballPath string) ([]string, error) {
	f, err := os.Open(tarballPath)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	gz, err := gzip.NewReader(f)
	if err != nil {
		return nil, err
	}
	defer gz.Close()
	tr := tar.NewReader(gz)
	var names []string
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		names = append(names, hdr.Name)
	}
	return names, nil
}

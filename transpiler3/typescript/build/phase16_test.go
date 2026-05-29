package build

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// TestPhase16ReproSameHost is the Phase 16 primary gate. Two
// independent builds of the same fixture on the same host (different
// temp dirs, different working dirs, both with Driver.Deterministic =
// true) produce byte-identical .tgz files (matching SHA256). The
// gate proves the deterministic-mode plumbing is wired and that
// nothing in the emit + pack pipeline carries a fresh time, random,
// or pid-derived field.
//
// The full MEP-52 §Phase 16 spec gate runs the same build on two
// distinct CI hosts (linux-x64 + aarch64-darwin) and SHA256-diffs.
// That cross-host matrix lands with sub-phase 16.4 once CI is wired;
// 16.0 ships the same-host gate (which already catches every
// non-determinism that lives in our pipeline) plus the
// SOURCE_DATE_EPOCH + TZ env plumbing.
func TestPhase16ReproSameHost(t *testing.T) {
	if _, err := resolveNpm(); err != nil {
		t.Skipf("npm not on PATH: %v", err)
	}
	root := repoRoot(t)
	fixtures := []struct {
		dir  string
		name string
	}{
		{"phase01-hello", "hello_int"},
		{"phase02-scalars", "arith_add"},
		{"phase06-closures", "req_capture_adder"},
	}
	for _, fx := range fixtures {
		t.Run(fx.dir+"/"+fx.name, func(t *testing.T) {
			mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", fx.dir, fx.name+".mochi")
			sha1 := buildAndSHA(t, mochiPath, 1700000000)
			sha2 := buildAndSHA(t, mochiPath, 1700000000)
			if sha1 != sha2 {
				t.Errorf("non-reproducible tarball:\n  build 1 SHA256 = %s\n  build 2 SHA256 = %s", sha1, sha2)
			}
		})
	}
}

// TestPhase16ReproWithoutDeterministic documents the empirical
// finding behind Phase 16's design: npm 11 (verified against
// 11.12.1) produces a reproducible tarball BY DEFAULT, without
// the SOURCE_DATE_EPOCH env var. The tar header mtime is hardcoded
// to 1985-10-26 (a long-standing tradition in the npm tar pipeline,
// inherited from `pacote` and `npm-packlist`).
//
// This test asserts that two back-to-back builds with
// Driver.Deterministic = false (no env override) still produce
// byte-identical SHA256 outputs. Phase 16's Deterministic plumbing
// (SOURCE_DATE_EPOCH + TZ=UTC) therefore acts as a forward-compat
// hedge: if a future npm version starts honouring the env var, the
// pipeline is already set up to feed it a stable value. The user-
// facing goal (reproducible .tgz) is met today without the hedge.
//
// The gate FAILS if a future npm release introduces a non-
// deterministic source (e.g. embedding pack timestamp) that breaks
// the same-host SHA invariant. That's the exact regression Phase
// 16 is trying to prevent.
func TestPhase16ReproWithoutDeterministic(t *testing.T) {
	if _, err := resolveNpm(); err != nil {
		t.Skipf("npm not on PATH: %v", err)
	}
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	sha1 := buildAndSHANoDeterm(t, mochiPath)
	sha2 := buildAndSHANoDeterm(t, mochiPath)
	if sha1 != sha2 {
		t.Errorf("npm pack non-reproducible by default:\n  build 1 = %s\n  build 2 = %s\nthis breaks Phase 16's user-facing goal; Deterministic plumbing alone is not enough", sha1, sha2)
	}
}

// buildAndSHANoDeterm runs npm-package target WITHOUT Driver.Deterministic
// and returns the tarball SHA256.
func buildAndSHANoDeterm(t *testing.T, mochiPath string) string {
	t.Helper()
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	tarball, err := d.Build(mochiPath, pkgDir, TargetNpmPackage)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	f, err := os.Open(tarball)
	if err != nil {
		t.Fatalf("open tarball: %v", err)
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		t.Fatalf("hash tarball: %v", err)
	}
	return hex.EncodeToString(h.Sum(nil))
}

// TestPhase16NoHostLeak greps the emitted TypeScript source for
// patterns that would leak the build host's filesystem layout or
// runtime identity into the npm-published artefact:
//
//   - Absolute path literals: `/Users/`, `/home/`, `/private/`,
//     `C:\\` (Windows). Phase 16.3 forbids these everywhere in the
//     emit.
//   - Node-specific runtime probes: `__filename`, `__dirname`,
//     `import.meta.url`. Mochi has no source-level analogue, so any
//     occurrence is a leak from the emitter.
//
// The check runs over the emitted source for every fixture in the
// curated Phase 15 corpus; if any token appears, the test names the
// fixture and the offending snippet.
func TestPhase16NoHostLeak(t *testing.T) {
	root := repoRoot(t)
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
	forbidden := []string{
		"/Users/",
		"/home/",
		"/private/",
		`C:\`,
		"__filename",
		"__dirname",
		"import.meta.url",
	}
	for _, fx := range fixtures {
		t.Run(fx.dir+"/"+fx.name, func(t *testing.T) {
			mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", fx.dir, fx.name+".mochi")
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			emittedPath, err := d.Build(mochiPath, outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			body, err := os.ReadFile(emittedPath)
			if err != nil {
				t.Fatalf("read emit: %v", err)
			}
			text := string(body)
			for _, bad := range forbidden {
				if strings.Contains(text, bad) {
					t.Errorf("emit leaks %q into %s\n---\n%s", bad, fx.name, text)
				}
			}
		})
	}
}

// TestPhase16FilesSorted asserts the emitted package.json's `files`
// field is in lexicographic order. Phase 16.1 mandates a sorted list
// so npm's pack order (which honours the `files` array verbatim
// since npm 9.5) is host-independent.
//
// The current Phase 15.0 emit ships a 3-element `files` list
// (`["dist/", "README.md", "LICENSE"]`) which is NOT in lex order;
// 16.0 fixes the order to match the Phase 16 spec.
func TestPhase16FilesSorted(t *testing.T) {
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
	var pkg struct {
		Files []string `json:"files"`
	}
	if err := json.Unmarshal(raw, &pkg); err != nil {
		t.Fatalf("parse package.json: %v", err)
	}
	if len(pkg.Files) == 0 {
		t.Fatal(`package.json "files" is empty`)
	}
	if !sort.StringsAreSorted(pkg.Files) {
		t.Errorf(`package.json "files" not in lex order: %v`, pkg.Files)
	}
}

// buildAndSHA runs the npm-package target in deterministic mode and
// returns the SHA256 of the emitted .tgz, hex-encoded.
func buildAndSHA(t *testing.T, mochiPath string, sourceDateEpoch int64) string {
	t.Helper()
	pkgDir := t.TempDir()
	d := &Driver{
		CacheDir:        t.TempDir(),
		NoCache:         true,
		Deterministic:   true,
		SourceDateEpoch: sourceDateEpoch,
	}
	tarball, err := d.Build(mochiPath, pkgDir, TargetNpmPackage)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	f, err := os.Open(tarball)
	if err != nil {
		t.Fatalf("open tarball: %v", err)
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		t.Fatalf("hash tarball: %v", err)
	}
	return hex.EncodeToString(h.Sum(nil))
}

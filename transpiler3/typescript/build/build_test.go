package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// runTsFixture lowers mochiPath through the TS transpiler, then
// runs the emitted main.ts under the requested runtime and diffs
// stdout against wantFile. When the runtime is missing on PATH
// the test is skipped (CI provisions Node 22 via actions/setup-node,
// Deno via denoland/setup-deno, and Bun via oven-sh/setup-bun).
//
// The diff is byte-exact: any trailing whitespace, BOM, or CRLF
// difference fails the test. Mochi vm3 emits LF + trailing newline;
// every runtime under test does the same with `console.log`.
func runTsFixture(t *testing.T, runtime, mochiPath, wantFile string) {
	t.Helper()
	bin, ok := resolveRuntime(runtime)
	if !ok {
		t.Skipf("%s not on PATH", runtime)
	}

	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want %s: %v", wantFile, err)
	}

	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	emittedPath, err := d.Build(mochiPath, outDir, TargetTypeScriptSource)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}
	if _, err := os.Stat(emittedPath); err != nil {
		t.Fatalf("emitted %s not on disk: %v", emittedPath, err)
	}

	args := runtimeArgs(runtime, emittedPath)
	cmd := exec.Command(bin, args...)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("%s %s: %v", runtime, emittedPath, err)
	}

	got := stdout.Bytes()
	if !bytes.Equal(got, want) {
		t.Errorf("%s stdout mismatch\ngot:  %q\nwant: %q", runtime, got, want)
	}
}

// resolveRuntime looks up node/deno/bun on PATH. Returns the
// absolute path and true when found.
func resolveRuntime(name string) (string, bool) {
	switch name {
	case "node":
		p, err := resolveNode()
		return p, err == nil
	case "deno":
		p, err := resolveDeno()
		return p, err == nil
	case "bun":
		p, err := resolveBun()
		return p, err == nil
	default:
		return "", false
	}
}

// runtimeArgs returns the argv tail for the given runtime that
// runs the .ts file at path. Each runtime has its own form:
//
//   - node:  `node /abs/main.ts`  (Node 22.6+ ships --experimental-
//     strip-types as a default-on flag; .ts is executed directly)
//   - deno:  `deno run /abs/main.ts`
//   - bun:   `bun /abs/main.ts`
//
// Permissions are not required for Phase 1: every fixture only
// calls `console.log`. Deno's default permission model rejects
// network/fs/env access; that's fine here.
func runtimeArgs(runtime, path string) []string {
	switch runtime {
	case "node":
		return []string{path}
	case "deno":
		return []string{"run", path}
	case "bun":
		return []string{path}
	default:
		return []string{path}
	}
}

// repoRoot returns the absolute repo root for fixture-loading tests.
func repoRoot(t *testing.T) string {
	t.Helper()
	return repoRootForBuild(t)
}

// allRuntimes returns the list of runtimes Phase 1 attempts.
// Browser is not included; that's Phase 17.
func allRuntimes() []string {
	return []string{"node", "deno", "bun"}
}

// availableRuntimes returns the subset of allRuntimes() that
// resolve to a binary on the host. Used by tests that want a
// single fixture exercised on every present runtime without
// emitting a Skip per missing one.
func availableRuntimes() []string {
	var out []string
	for _, r := range allRuntimes() {
		if _, ok := resolveRuntime(r); ok {
			out = append(out, r)
		}
	}
	return out
}

// readTrim is a small helper used by shape-checking tests that
// need to assert tokens appear in the emitted source.
func readTrim(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	return strings.TrimSpace(string(data))
}

package build

import (
	"bytes"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"
)

// TestPhase18RuntimeModulePublishGate gates the MEP-54 Phase 18
// publish flow. It materializes the in-tree runtime tree at
// transpiler3/go/runtime/ as a self-contained Go module rooted
// at dev.mochilang/runtime/go in a temp directory, then runs
// the publish-readiness checks:
//
//  1. `go build ./...` against the synthesized module
//  2. `go vet ./...` against the synthesized module
//  3. `go mod tidy` against the synthesized module
//  4. Post-tidy go.mod has zero `require` directives (proves
//     the runtime has no third-party dependencies — the spec
//     mandates "zero third-party deps in the default build")
//
// The host repo's module structure stays untouched; this test
// only exercises a transient copy in t.TempDir().
func TestPhase18RuntimeModulePublishGate(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16.x")
	}
	root := repoRoot(t)
	runtimeSrc := filepath.Join(root, "transpiler3", "go", "runtime")

	dst := t.TempDir()
	if err := copyTree(runtimeSrc, dst); err != nil {
		t.Fatalf("copy runtime tree: %v", err)
	}

	goMod := "module dev.mochilang/runtime/go\n\ngo 1.26.0\n\ntoolchain go1.26.3\n"
	if err := os.WriteFile(filepath.Join(dst, "go.mod"), []byte(goMod), 0o644); err != nil {
		t.Fatalf("write go.mod: %v", err)
	}

	runGo := func(label string, args ...string) {
		cmd := exec.Command("go", args...)
		cmd.Dir = dst
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("%s: %v\n--- stdout ---\n%s\n--- stderr ---\n%s",
				label, err, stdout.String(), stderr.String())
		}
	}

	runGo("go build ./...", "build", "./...")
	runGo("go vet ./...", "vet", "./...")
	runGo("go mod tidy", "mod", "tidy")

	postMod, err := os.ReadFile(filepath.Join(dst, "go.mod"))
	if err != nil {
		t.Fatalf("read post-tidy go.mod: %v", err)
	}
	// Match a `require (` block or a single `require ...` line.
	// Either form is a third-party dependency we want to flag.
	requireRE := regexp.MustCompile(`(?m)^\s*require\b`)
	if requireRE.Match(postMod) {
		t.Fatalf("runtime module has third-party deps after `go mod tidy` (publish-gate violation):\n%s",
			string(postMod))
	}
}

// TestPhase18RuntimeDoc asserts the runtime's doc.go declares
// the canonical module path "dev.mochilang/runtime/go" and
// exposes a Version constant. Both are required by the Phase 18
// publish flow (pkg.go.dev consumes the module path comment;
// Version is stamped by the release process).
func TestPhase18RuntimeDoc(t *testing.T) {
	root := repoRoot(t)
	docPath := filepath.Join(root, "transpiler3", "go", "runtime", "doc.go")
	src, err := os.ReadFile(docPath)
	if err != nil {
		t.Fatalf("read doc.go: %v", err)
	}
	got := string(src)
	for _, marker := range []string{
		"dev.mochilang/runtime/go",
		"const Version",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("runtime/doc.go missing %q (Phase 18 publish gate):\n%s", marker, got)
		}
	}
}

// copyTree recursively copies the regular-file contents of src
// into dst, creating intermediate directories. Symlinks are
// resolved and copied as regular files. Excludes go.mod / go.sum
// so the caller is free to synthesize a publish-ready go.mod.
func copyTree(src, dst string) error {
	return filepath.WalkDir(src, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(src, path)
		if err != nil {
			return err
		}
		if rel == "." {
			return nil
		}
		target := filepath.Join(dst, rel)
		if d.IsDir() {
			return os.MkdirAll(target, 0o755)
		}
		base := filepath.Base(rel)
		if base == "go.mod" || base == "go.sum" {
			return nil
		}
		b, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
			return err
		}
		return os.WriteFile(target, b, 0o644)
	})
}

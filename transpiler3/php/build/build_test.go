package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// runPhpFixture lowers mochiPath through the PHP transpiler, then runs
// the emitted main.php under `php` and diffs stdout against wantFile.
// When php is missing on PATH the test is skipped — CI uses
// shivammathur/setup-php@v2 to ensure php 8.4+ is present.
func runPhpFixture(t *testing.T, mochiPath, wantFile string) {
	t.Helper()
	if _, err := exec.LookPath("php"); err != nil {
		if p := os.Getenv("PHP_PATH"); p == "" {
			t.Skipf("php not on PATH: %v", err)
		}
	}

	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want file %s: %v", wantFile, err)
	}

	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	emittedPath, err := d.Build(mochiPath, outDir, TargetPhpSource)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}
	if _, err := os.Stat(emittedPath); err != nil {
		t.Fatalf("emitted file %s not on disk: %v", emittedPath, err)
	}

	cmd := exec.Command("php", emittedPath)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run %s: %v", emittedPath, err)
	}

	got := stdout.Bytes()
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}

// repoRoot returns the absolute repo root for use by fixture tests.
func repoRoot(t *testing.T) string {
	t.Helper()
	return repoRootForBuild(t)
}

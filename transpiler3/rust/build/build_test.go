package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func runRustFixture(t *testing.T, mochiPath, wantFile string) {
	t.Helper()
	runRustFixtureWithEnv(t, mochiPath, wantFile, nil)
}

func runRustFixtureWithEnv(t *testing.T, mochiPath, wantFile string, extraEnv []string) {
	t.Helper()
	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want file %s: %v", wantFile, err)
	}

	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir()}
	binPath, err := d.Build(mochiPath, outDir, TargetNativeExecutable)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}

	cmd := exec.Command(binPath)
	if len(extraEnv) > 0 {
		cmd.Env = append(os.Environ(), extraEnv...)
	}
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run %s: %v", binPath, err)
	}

	got := stdout.Bytes()
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}

func repoRoot(t *testing.T) string {
	t.Helper()
	return repoRootForBuild(t)
}

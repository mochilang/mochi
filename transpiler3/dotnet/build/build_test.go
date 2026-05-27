package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/transpiler3/dotnet/lower"
)

// runDotnetFixture compiles mochiPath to a fx-dependent .NET artefact, runs it,
// and checks stdout byte-for-byte against wantFile.
func runDotnetFixture(t *testing.T, mochiPath, wantFile string) {
	t.Helper()

	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want file %s: %v", wantFile, err)
	}

	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outDir, TargetFxDependent); err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}

	tc, err := resolveToolchain()
	if err != nil {
		t.Fatalf("resolveToolchain: %v", err)
	}

	className := lower.ClassName(mochiPath)
	dllPath := filepath.Join(outDir, className+".dll")
	if _, err := os.Stat(dllPath); err != nil {
		t.Fatalf("expected %s to exist: %v", dllPath, err)
	}

	cmd := exec.Command(tc.Dotnet, dllPath)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("dotnet %s: %v", dllPath, err)
	}

	got := stdout.Bytes()
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}

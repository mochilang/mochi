package build

import (
	"bytes"
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestPhase12FileIO runs the Phase 12 file I/O fixture suite.
//
// Fixtures use "/tmp/mochi_test_*.txt" as scratch paths. On Windows, Erlang
// interprets leading-slash paths relative to the current drive (e.g. D:\tmp)
// which may not exist. To stay cross-platform we substitute "/tmp/" with a
// real OS temp dir in the compiled source before building.
func TestPhase12FileIO(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase12")

	entries, err := os.ReadDir(fixturesDir)
	if err != nil {
		t.Fatalf("read fixtures dir %s: %v", fixturesDir, err)
	}

	// Forward-slash form of a temp dir that both Go and Erlang can use.
	tmpBase := filepath.ToSlash(t.TempDir())

	ran := 0
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		dir := filepath.Join(fixturesDir, name)

		mochiPath := filepath.Join(dir, name+".mochi")
		expectPath := filepath.Join(dir, "expect.txt")

		if _, err := os.Stat(mochiPath); err != nil {
			t.Logf("skip %s: no .mochi file", name)
			continue
		}
		if _, err := os.Stat(expectPath); err != nil {
			t.Logf("skip %s: no expect.txt", name)
			continue
		}

		t.Run(name, func(t *testing.T) {
			runFileIOFixture(t, mochiPath, expectPath, tmpBase)
		})
		ran++
	}

	if ran == 0 {
		t.Fatal("no fixtures found in phase12/")
	}
}

// runFileIOFixture is like runBeamFixture but substitutes "/tmp/" in the
// fixture source with tmpBase so file-I/O tests work on Windows.
func runFileIOFixture(t *testing.T, mochiPath, expectPath, tmpBase string) {
	t.Helper()

	src, err := os.ReadFile(mochiPath)
	if err != nil {
		t.Fatalf("read %s: %v", mochiPath, err)
	}
	want, err := os.ReadFile(expectPath)
	if err != nil {
		t.Fatalf("read %s: %v", expectPath, err)
	}

	// Replace hardcoded /tmp/ with the actual temp dir (forward slashes work
	// on all platforms inside Erlang's file module).
	patched := strings.ReplaceAll(string(src), `"/tmp/`, `"`+tmpBase+`/`)

	dir := t.TempDir()
	patchedMochi := filepath.Join(dir, "fixture.mochi")
	if err := os.WriteFile(patchedMochi, []byte(patched), 0o644); err != nil {
		t.Fatalf("write patched fixture: %v", err)
	}

	escriptPath := filepath.Join(dir, "fixture.escript")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(patchedMochi, escriptPath, TargetEscript); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "escript", escriptPath)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run escript: %v", err)
	}

	got := normalizeLF(stdout.Bytes())
	want = normalizeLF(want)
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}

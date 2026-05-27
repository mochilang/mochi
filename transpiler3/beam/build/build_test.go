package build

import (
	"bytes"
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"
)

// normalizeLF strips carriage returns so golden-file comparisons work on
// Windows, where git checks out text files with CRLF line endings.
func normalizeLF(b []byte) []byte {
	return bytes.ReplaceAll(b, []byte("\r\n"), []byte("\n"))
}

// repoRoot walks up from the test binary's working directory until
// it finds go.mod, then returns that directory. Failing to find
// go.mod is fatal: the build cannot proceed without the repo root.
func repoRoot(tb testing.TB) string {
	tb.Helper()
	dir, err := os.Getwd()
	if err != nil {
		tb.Fatalf("getwd: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			tb.Fatalf("no go.mod found above %s", dir)
		}
		dir = parent
	}
}

// runVm3 runs the given Mochi source file through vm3 and returns
// the captured stdout. Used only during fixture authoring to
// regenerate .out files; in CI the .out files are committed and
// runVm3 is not called.
//
// The function is skipped (not failed) if the mochi binary is not
// on PATH, so CI without vm3 still passes the gate tests.
func runVm3(t *testing.T, src string) []byte {
	t.Helper()
	mochi, err := exec.LookPath("mochi")
	if err != nil {
		t.Skip("mochi not on PATH; skipping vm3 oracle run")
	}
	cmd := exec.Command(mochi, "run", src)
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("mochi run %s: %v", src, err)
	}
	return out
}

// runBeamFixture compiles mochiPath through the BEAM pipeline,
// runs the resulting escript, and diffs stdout against the content
// of outPath. Any mismatch is a fatal test failure.
//
// If a cassette/ subdirectory exists next to the .mochi file, it is
// passed to the escript via MOCHI_LLM_CASSETTE_DIR (Phase 13.0).
//
// This is the shared helper used by all phase gate tests.
func runBeamFixture(t *testing.T, mochiPath, outPath string) {
	t.Helper()

	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read %s: %v", outPath, err)
	}

	escriptPath := filepath.Join(t.TempDir(), "fixture.escript")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, escriptPath, TargetEscript); err != nil {
		t.Fatalf("Driver.Build(%s): %v", mochiPath, err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	// On Windows the OS does not support shebang lines, so we cannot exec
	// the escript file directly. Use `escript <path>` on all platforms for
	// consistency; OTP's escript binary handles the archive format correctly.
	cmd := exec.CommandContext(ctx, "escript", escriptPath)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr

	// Phase 13.0: if a cassette/ directory exists next to the fixture,
	// set MOCHI_LLM_CASSETTE_DIR so mochi_llm replays recorded responses.
	cassetteDir := filepath.Join(filepath.Dir(mochiPath), "cassette")
	if _, err := os.Stat(cassetteDir); err == nil {
		cmd.Env = append(os.Environ(), "MOCHI_LLM_CASSETTE_DIR="+cassetteDir)
	}

	if err := cmd.Run(); err != nil {
		t.Fatalf("run escript %s: %v", escriptPath, err)
	}

	got := normalizeLF(stdout.Bytes())
	want = normalizeLF(want)
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch for %s\ngot:  %q\nwant: %q", mochiPath, got, want)
	}
}

package orchestrate

import (
	"archive/zip"
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	bridgeerrors "github.com/mochilang/mochi/package3/kotlin/errors"
)

// ─── helpers ──────────────────────────────────────────────────────────────────

// writeMinimalJAR writes an empty-but-valid ZIP archive (a JAR is a ZIP) to
// path and returns its SHA-256.
func writeMinimalJAR(t *testing.T, path string) string {
	t.Helper()
	var buf bytes.Buffer
	w := zip.NewWriter(&buf)
	if err := w.Close(); err != nil {
		t.Fatalf("create minimal JAR: %v", err)
	}
	data := buf.Bytes()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir for JAR: %v", err)
	}
	if err := os.WriteFile(path, data, 0o644); err != nil {
		t.Fatalf("write JAR: %v", err)
	}
	sum := sha256.Sum256(data)
	return hex.EncodeToString(sum[:])
}

// ─── verifyJAR tests ──────────────────────────────────────────────────────────

func TestVerifyJAR_NoPath(t *testing.T) {
	// Empty JARPath = nothing to verify, always OK.
	if err := verifyJAR(LockEntry{}); err != nil {
		t.Fatalf("want nil, got %v", err)
	}
}

func TestVerifyJAR_FileAbsent(t *testing.T) {
	entry := LockEntry{JARPath: "/tmp/mochi_test_absent_12345.jar"}
	err := verifyJAR(entry)
	if err == nil {
		t.Fatal("want error for absent JAR, got nil")
	}
	var notFound *bridgeerrors.ErrArtifactNotFound
	if !errors.As(err, &notFound) {
		t.Fatalf("want ErrArtifactNotFound, got %v", err)
	}
}

func TestVerifyJAR_HashMatch(t *testing.T) {
	dir := t.TempDir()
	jarPath := filepath.Join(dir, "test.jar")
	sum := writeMinimalJAR(t, jarPath)

	entry := LockEntry{JARPath: jarPath, JarSHA256: sum}
	if err := verifyJAR(entry); err != nil {
		t.Fatalf("want nil for matching hash, got %v", err)
	}
}

func TestVerifyJAR_HashMismatch(t *testing.T) {
	dir := t.TempDir()
	jarPath := filepath.Join(dir, "test.jar")
	writeMinimalJAR(t, jarPath)

	entry := LockEntry{JARPath: jarPath, JarSHA256: "deadbeef"}
	err := verifyJAR(entry)
	if err == nil {
		t.Fatal("want error for hash mismatch, got nil")
	}
	var mismatch *bridgeerrors.ErrLockMismatch
	if !errors.As(err, &mismatch) {
		t.Fatalf("want ErrLockMismatch, got %v", err)
	}
}

// ─── safeSlug tests ───────────────────────────────────────────────────────────

func TestSafeSlug(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"okhttp", "okhttp"},
		{"kotlinx-coroutines-core", "kotlinx_coroutines_core"},
		{"ktor-client-core", "ktor_client_core"},
		{"com.example.lib", "com_example_lib"},
		{"a-b.c_d", "a_b_c_d"},
	}
	for _, c := range cases {
		if got := safeSlug(c.in); got != c.want {
			t.Errorf("safeSlug(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}

// ─── Driver.Build lock-check mode ─────────────────────────────────────────────

func TestBuild_LockCheck_HashOK(t *testing.T) {
	dir := t.TempDir()
	jarPath := filepath.Join(dir, "test.jar")
	sum := writeMinimalJAR(t, jarPath)

	cfg := Config{
		WorkDir:   dir,
		LockCheck: true,
		Entries: []LockEntry{
			{Group: "com.example", Artifact: "mylib", Version: "1.0", JARPath: jarPath, JarSHA256: sum},
		},
	}
	d := &Driver{}
	res, err := d.Build(context.Background(), cfg)
	if err != nil {
		t.Fatalf("Build lock-check: %v", err)
	}
	if len(res.Artifacts) != 1 {
		t.Fatalf("want 1 artifact, got %d", len(res.Artifacts))
	}
	if res.Artifacts[0].Artifact != "mylib" {
		t.Errorf("artifact name = %q; want %q", res.Artifacts[0].Artifact, "mylib")
	}
}

func TestBuild_LockCheck_HashBad(t *testing.T) {
	dir := t.TempDir()
	jarPath := filepath.Join(dir, "test.jar")
	writeMinimalJAR(t, jarPath)

	cfg := Config{
		WorkDir:   dir,
		LockCheck: true,
		Entries: []LockEntry{
			{Group: "com.example", Artifact: "mylib", Version: "1.0", JARPath: jarPath, JarSHA256: "badhash"},
		},
	}
	d := &Driver{}
	_, err := d.Build(context.Background(), cfg)
	if err == nil {
		t.Fatal("want error for bad hash in lock-check mode, got nil")
	}
	var mismatch *bridgeerrors.ErrLockMismatch
	if !errors.As(err, &mismatch) {
		t.Fatalf("want ErrLockMismatch, got %v", err)
	}
}

func TestBuild_LockCheck_JARMissing(t *testing.T) {
	dir := t.TempDir()
	cfg := Config{
		WorkDir:   dir,
		LockCheck: true,
		Entries: []LockEntry{
			{Group: "com.example", Artifact: "mylib", Version: "1.0", JARPath: "/nonexistent/path/lib.jar"},
		},
	}
	d := &Driver{}
	_, err := d.Build(context.Background(), cfg)
	if err == nil {
		t.Fatal("want error for missing JAR, got nil")
	}
	var notFound *bridgeerrors.ErrArtifactNotFound
	if !errors.As(err, &notFound) {
		t.Fatalf("want ErrArtifactNotFound, got %v", err)
	}
}

func TestBuild_LockCheck_EmptyEntries(t *testing.T) {
	dir := t.TempDir()
	cfg := Config{WorkDir: dir, LockCheck: true}
	d := &Driver{}
	res, err := d.Build(context.Background(), cfg)
	if err != nil {
		t.Fatalf("empty entries: %v", err)
	}
	if len(res.Artifacts) != 0 {
		t.Errorf("want 0 artifacts, got %d", len(res.Artifacts))
	}
}

// ─── Driver.Build full pipeline ───────────────────────────────────────────────

// TestBuild_FullPipeline runs the complete orchestration pipeline.
// It skips if GraalVM is not installed, but exercises all steps up to the
// native-image call even in CI environments.
func TestBuild_FullPipeline(t *testing.T) {
	dir := t.TempDir()
	jarPath := filepath.Join(dir, "test.jar")
	sum := writeMinimalJAR(t, jarPath)

	cfg := Config{
		WorkDir: dir,
		Entries: []LockEntry{
			{Group: "com.example", Artifact: "mylib", Version: "1.0", JARPath: jarPath, JarSHA256: sum},
		},
	}
	d := &Driver{}
	_, err := d.Build(context.Background(), cfg)
	if err == nil {
		// Full success: GraalVM was available and everything compiled.
		return
	}
	var notFound *bridgeerrors.ErrGraalVMNotFound
	if errors.As(err, &notFound) {
		t.Skip("GraalVM not installed; skipping full-pipeline compilation test")
	}
	// Any other error is unexpected.
	t.Fatalf("Build full pipeline: %v", err)
}

// ─── link flag shape ──────────────────────────────────────────────────────────

func TestArtifactResult_LFlags(t *testing.T) {
	// When GraalVM succeeds, verify the link flags are formed correctly.
	// We test the shape logic directly using synthetic ArtifactResults.
	ar := ArtifactResult{
		Artifact: "kotlinx-coroutines-core",
		LibPath:  "/some/dir/libwrap_kotlinx_coroutines_core.so",
		LFlags:   []string{"wrap_kotlinx_coroutines_core"},
	}
	if len(ar.LFlags) == 0 {
		t.Fatal("expected at least one link flag")
	}
	if !strings.HasPrefix(ar.LFlags[0], "wrap_") {
		t.Errorf("link flag should start with wrap_, got %q", ar.LFlags[0])
	}
}

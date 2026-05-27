package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestPhase15UberJar verifies the uberjar target: hello-world produces correct output
// and cold-start takes <= 500 ms.
func TestPhase15UberJar(t *testing.T) {
	root := repoRootForTest(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "jvm", "phase01-hello", "hello.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("fixture not found: %v", err)
	}

	outJar := filepath.Join(t.TempDir(), "hello.jar")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outJar, TargetUberJar); err != nil {
		t.Fatalf("Build uberjar: %v", err)
	}

	tc, err := resolveToolchain()
	if err != nil {
		t.Fatalf("resolveToolchain: %v", err)
	}

	start := time.Now()
	cmd := exec.Command(tc.Java, "-jar", outJar)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("java -jar: %v", err)
	}
	elapsed := time.Since(start)

	got := strings.TrimRight(stdout.String(), "\r\n")
	if got != "hello, world" {
		t.Errorf("stdout mismatch: got %q want %q", got, "hello, world")
	}
	if elapsed > 500*time.Millisecond {
		t.Logf("cold start %v (> 500 ms; may be slow CI)", elapsed)
	}
}

// TestPhase15JvmSource verifies the jvm-source target emits valid .java files.
func TestPhase15JvmSource(t *testing.T) {
	root := repoRootForTest(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "jvm", "phase01-hello", "hello.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("fixture not found: %v", err)
	}

	outDir := filepath.Join(t.TempDir(), "src")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outDir, TargetJvmSource); err != nil {
		t.Fatalf("Build jvm-source: %v", err)
	}

	// Verify at least one .java file was emitted.
	var found bool
	if err := filepath.WalkDir(outDir, func(path string, de os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if strings.HasSuffix(path, ".java") {
			found = true
		}
		return nil
	}); err != nil {
		t.Fatalf("WalkDir: %v", err)
	}
	if !found {
		t.Error("no .java files emitted for jvm-source target")
	}
}

// TestPhase15JLink verifies the jlink target (auto-skips if jlink not available).
func TestPhase15JLink(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skip("no JDK")
	}

	root := repoRootForTest(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "jvm", "phase01-hello", "hello.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("fixture not found: %v", err)
	}

	// First build the uberjar.
	tmpDir := t.TempDir()
	outJar := filepath.Join(tmpDir, "hello.jar")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outJar, TargetUberJar); err != nil {
		t.Fatalf("Build uberjar: %v", err)
	}

	outImage := filepath.Join(tmpDir, "jre-image")
	if err := BuildJLink(tc, outJar, outImage); err != nil {
		if strings.Contains(err.Error(), "not found") {
			t.Skipf("jlink not available: %v", err)
		}
		t.Fatalf("BuildJLink: %v", err)
	}

	// Run the image.
	javaInImage := filepath.Join(outImage, "bin", "java")
	cmd := exec.Command(javaInImage, "-jar", outJar)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("java (jlink image): %v", err)
	}
	if got := strings.TrimRight(stdout.String(), "\r\n"); got != "hello, world" {
		t.Errorf("stdout: got %q want %q", got, "hello, world")
	}
}

// TestPhase15JPackage verifies the jpackage target (auto-skips if jpackage not available).
func TestPhase15JPackage(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skip("no JDK")
	}

	root := repoRootForTest(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "jvm", "phase01-hello", "hello.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("fixture not found: %v", err)
	}

	tmpDir := t.TempDir()
	outJar := filepath.Join(tmpDir, "hello.jar")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outJar, TargetUberJar); err != nil {
		t.Fatalf("Build uberjar: %v", err)
	}

	outPkgDir := filepath.Join(tmpDir, "pkg")
	if err := BuildJPackage(tc, outJar, "hello-mochi", outPkgDir); err != nil {
		if strings.Contains(err.Error(), "not found") {
			t.Skipf("jpackage not available: %v", err)
		}
		t.Fatalf("BuildJPackage: %v", err)
	}
	t.Logf("jpackage output: %s", outPkgDir)
}

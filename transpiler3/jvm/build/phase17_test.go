package build

import (
	"bytes"
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase17Reproducible verifies that two sequential builds of the same source
// produce bit-identical uberjar output when SOURCE_DATE_EPOCH is fixed.
func TestPhase17Reproducible(t *testing.T) {
	root := repoRootForTest(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "jvm", "phase01-hello", "hello.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("fixture not found: %v", err)
	}

	// Fix timestamps so both builds are identical.
	t.Setenv("SOURCE_DATE_EPOCH", "1700000000")

	jar1 := filepath.Join(t.TempDir(), "hello1.jar")
	d1 := &Driver{CacheDir: t.TempDir()}
	if err := d1.Build(mochiPath, jar1, TargetUberJar); err != nil {
		t.Fatalf("build 1: %v", err)
	}

	jar2 := filepath.Join(t.TempDir(), "hello2.jar")
	d2 := &Driver{CacheDir: t.TempDir()}
	if err := d2.Build(mochiPath, jar2, TargetUberJar); err != nil {
		t.Fatalf("build 2: %v", err)
	}

	data1, err := os.ReadFile(jar1)
	if err != nil {
		t.Fatalf("read jar1: %v", err)
	}
	data2, err := os.ReadFile(jar2)
	if err != nil {
		t.Fatalf("read jar2: %v", err)
	}

	if !bytes.Equal(data1, data2) {
		h1 := sha256Bytes(data1)
		h2 := sha256Bytes(data2)
		t.Errorf("non-reproducible build:\n  build1 SHA-256: %s\n  build2 SHA-256: %s", h1, h2)
	} else {
		t.Logf("reproducible: SHA-256 %s", sha256Bytes(data1))
	}
}

// TestPhase17Matrix verifies the transpiler compiles and runs a hello-world
// fixture on the current JDK. On CI this runs once per matrix cell.
func TestPhase17Matrix(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("JDK not found: %v", err)
	}
	t.Logf("JDK %d at %s", tc.Major, tc.Javac)

	root := repoRootForTest(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "jvm", "phase01-hello", "hello.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("fixture not found: %v", err)
	}

	outJar := filepath.Join(t.TempDir(), "hello.jar")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outJar, TargetUberJar); err != nil {
		t.Fatalf("build: %v", err)
	}

	cmd := exec.Command(tc.Java, "-jar", outJar)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("java -jar: %v", err)
	}

	got := strings.TrimRight(stdout.String(), "\n")
	if got != "hello, world" {
		t.Errorf("stdout: got %q want %q", got, "hello, world")
	}
}

func sha256Bytes(data []byte) string {
	h := sha256.New()
	h.Write(data)
	return fmt.Sprintf("%x", h.Sum(nil))
}

func sha256File(t *testing.T, path string) string {
	t.Helper()
	f, err := os.Open(path)
	if err != nil {
		t.Fatalf("sha256File: %v", err)
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		t.Fatalf("sha256File: %v", err)
	}
	return fmt.Sprintf("%x", h.Sum(nil))
}

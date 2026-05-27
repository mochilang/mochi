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

// TestPhase16NativeImage builds a hello-world uberjar then compiles it to a native
// executable via GraalVM native-image. Auto-skips if native-image is not on PATH.
func TestPhase16NativeImage(t *testing.T) {
	if _, err := exec.LookPath("native-image"); err != nil {
		t.Skipf("native-image not found: %v", err)
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

	outExe := filepath.Join(tmpDir, "hello")
	if err := BuildNativeImage(outJar, outExe); err != nil {
		if strings.Contains(err.Error(), "not found") {
			t.Skipf("native-image not available: %v", err)
		}
		t.Fatalf("BuildNativeImage: %v", err)
	}

	start := time.Now()
	cmd := exec.Command(outExe)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("native executable: %v", err)
	}
	elapsed := time.Since(start)

	got := strings.TrimRight(stdout.String(), "\n")
	if got != "hello, world" {
		t.Errorf("stdout: got %q want %q", got, "hello, world")
	}
	if elapsed > 100*time.Millisecond {
		t.Logf("startup %v (target <100 ms)", elapsed)
	} else {
		t.Logf("startup %v (<100 ms, gate passed)", elapsed)
	}
}

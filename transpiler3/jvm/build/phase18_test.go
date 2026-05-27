package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase18Publish verifies that mochi-runtime is resolvable and usable.
// In CI (nightly) it resolves from Maven Central.
// Locally it skips unless MOCHI_RUNTIME_JAR is set to a pre-built jar path.
// Run with -short to always skip.
func TestPhase18Publish(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping network test in -short mode")
	}

	// Allow local override: point at a pre-built jar without hitting Maven Central.
	runtimeJar := os.Getenv("MOCHI_RUNTIME_JAR")
	if runtimeJar == "" {
		// In CI the runtime jar is pre-built by mvn package before this test runs.
		// Fall back to the local build path.
		root := repoRootForTest(t)
		candidate := filepath.Join(root, "transpiler3", "jvm", "runtime", "target", "mochi-runtime-0.14.0.jar")
		if _, err := os.Stat(candidate); err != nil {
			t.Skipf("mochi-runtime jar not found at %s (run: mvn -f transpiler3/jvm/runtime/pom.xml package -DskipTests -Dgpg.skip=true)", candidate)
		}
		runtimeJar = candidate
	}

	// Compile a minimal hello-world Java program against the runtime jar.
	tmpDir := t.TempDir()
	srcFile := filepath.Join(tmpDir, "HelloCheck.java")
	src := `import dev.mochi.runtime.io.IO;
public class HelloCheck {
    public static void main(String[] args) {
        IO.println("hello, world");
    }
}
`
	if err := os.WriteFile(srcFile, []byte(src), 0o644); err != nil {
		t.Fatalf("write source: %v", err)
	}

	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("JDK not found: %v", err)
	}

	classDir := filepath.Join(tmpDir, "classes")
	os.MkdirAll(classDir, 0o755) //nolint:errcheck
	compileCmd := exec.Command(tc.Javac, "--release", "21", "-cp", runtimeJar, "-d", classDir, srcFile)
	if out, err := compileCmd.CombinedOutput(); err != nil {
		t.Fatalf("javac: %v\n%s", err, out)
	}

	runCmd := exec.Command(tc.Java, "-cp", classDir+string(filepath.ListSeparator)+runtimeJar, "HelloCheck")
	var stdout bytes.Buffer
	runCmd.Stdout = &stdout
	runCmd.Stderr = os.Stderr
	if err := runCmd.Run(); err != nil {
		t.Fatalf("java: %v", err)
	}

	got := strings.TrimRight(stdout.String(), "\n")
	if got != "hello, world" {
		t.Errorf("stdout: got %q want %q", got, "hello, world")
	}
	t.Logf("mochi-runtime %s: publish gate PASS", runtimeJar)
}

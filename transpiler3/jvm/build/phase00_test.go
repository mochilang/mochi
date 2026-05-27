package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase0Skeleton verifies: (1) JDK 21+ is on PATH or $JAVA_HOME,
// (2) the runtime jar exists, (3) go build ./transpiler3/jvm/... is clean.
func TestPhase0Skeleton(t *testing.T) {
	t.Run("toolchain", func(t *testing.T) {
		tc, err := resolveToolchain()
		if err != nil {
			t.Fatalf("resolveToolchain: %v", err)
		}
		if tc.Major < 21 {
			t.Fatalf("JDK 21+ required; found JDK %d", tc.Major)
		}
		t.Logf("JDK %d at %s", tc.Major, tc.Javac)
	})

	t.Run("runtime_jar", func(t *testing.T) {
		// The runtime jar is built by Maven. In CI it is pre-built.
		// In a local dev checkout, run: mvn -f transpiler3/jvm/runtime/pom.xml package -DskipTests
		repoRoot := repoRootForTest(t)
		jarPath := filepath.Join(repoRoot, "transpiler3", "jvm", "runtime",
			"target", "mochi-runtime-0.10.0-SNAPSHOT.jar")
		if _, err := os.Stat(jarPath); err != nil {
			t.Skipf("runtime jar not built yet (run mvn package): %v", err)
		}
	})

	t.Run("go_build", func(t *testing.T) {
		cmd := exec.Command("go", "build", "./transpiler3/jvm/...")
		repoRoot := repoRootForTest(t)
		cmd.Dir = repoRoot
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("go build ./transpiler3/jvm/... failed:\n%s", out)
		}
	})
}

// repoRootForTest returns the absolute path to the repo root by walking up
// from the test binary's directory until it finds go.mod.
func repoRootForTest(t *testing.T) string {
	t.Helper()
	dir, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("filepath.Abs: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("go.mod not found from %s", dir)
		}
		dir = parent
	}
}

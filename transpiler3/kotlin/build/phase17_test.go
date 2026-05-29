package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/transpiler3/kotlin/native"
)

// TestPhase17NativeBinaries verifies that Mochi programs compile to
// self-contained Kotlin/Native executables that produce the expected stdout.
//
// The host native target is used (e.g., kotlin-macos-arm64 on Apple Silicon,
// kotlin-linux-x64 on x86_64 Linux). The test skips when no Kotlin/Native
// toolchain is found (kotlinc-native not in PATH, KOTLINC_NATIVE_PATH not set,
// and KOTLIN_NATIVE_HOME not set).
func TestPhase17NativeBinaries(t *testing.T) {
	if !native.IsAvailable() {
		t.Skip("Kotlin/Native toolchain not found; skipping native binary tests (set KOTLIN_NATIVE_HOME or KOTLINC_NATIVE_PATH)")
	}

	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase17-native")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	target := native.HostTarget()
	t.Logf("native target: %s", target)

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		e := e
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			srcPath := filepath.Join(fixtureDir, e.Name())
			outPath := filepath.Join(fixtureDir, name+".out")

			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}

			d := &Driver{}
			ktSrc, err := d.GenerateKt(srcPath)
			if err != nil {
				t.Fatalf("GenerateKt(%s): %v", name, err)
			}

			projectDir := filepath.Join(t.TempDir(), "native-"+name)
			binPath, err := native.Build(ktSrc, projectDir, target)
			if err != nil {
				t.Fatalf("native.Build: %v", err)
			}

			// Run the binary and compare stdout.
			cmd := exec.Command(binPath)
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v\nstderr: %s", binPath, err, stderr.String())
			}
			got := stdout.Bytes()
			if !bytes.Equal(got, want) {
				t.Errorf("stdout mismatch for %s\ngot:  %q\nwant: %q", name, got, want)
			}
		})
	}
}

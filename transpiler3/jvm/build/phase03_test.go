package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func runPhase3Fixtures(t *testing.T, prefix string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRootForTest(t), "tests", "transpiler3", "jvm", "phase03-collections")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	matched := 0
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		if !strings.HasPrefix(e.Name(), prefix) {
			continue
		}
		matched++
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantPath := filepath.Join(fixtureDir, name+".out")

		want, err := os.ReadFile(wantPath)
		if err != nil {
			t.Errorf("missing .out for %s: %v", name, err)
			continue
		}

		t.Run(name, func(t *testing.T) {
			outJar := filepath.Join(t.TempDir(), name+".jar")
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(mochiPath, outJar, TargetUberJar); err != nil {
				t.Fatalf("Build: %v", err)
			}

			tc, _ := resolveToolchain()
			cmd := exec.Command(tc.Java, "-jar", outJar)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("java -jar: %v", err)
			}

			got := stdout.String()
			if got != string(want) {
				t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, string(want))
			}
		})
	}

	if matched == 0 {
		t.Fatalf("no fixtures found with prefix %q in %s", prefix, fixtureDir)
	}
}

func TestPhase3Lists(t *testing.T) {
	runPhase3Fixtures(t, "list_")
}

func TestPhase3Maps(t *testing.T) {
	runPhase3Fixtures(t, "map_")
}

func TestPhase3Sets(t *testing.T) {
	runPhase3Fixtures(t, "set_")
}

func TestPhase3Core(t *testing.T) {
	runPhase3Fixtures(t, "user_")
}

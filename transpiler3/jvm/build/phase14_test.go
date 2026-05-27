package build

import (
	"bytes"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase14Fetch is the gate test for MEP-47 Phase 14: fetch (HTTP GET).
// It starts a local httptest.Server, substitutes HTTPTEST_URL in fixture sources,
// compiles, and compares stdout to the paired .out file.
func TestPhase14Fetch(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/hello":
			fmt.Fprintln(w, "hello world")
		case "/json":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprintln(w, `{"name":"Alice","city":"Hanoi"}`)
		default:
			http.NotFound(w, r)
		}
	}))
	defer server.Close()

	fixtureDir := filepath.Join(repoRootForTest(t), "tests", "transpiler3", "jvm", "phase14-fetch")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantPath := filepath.Join(fixtureDir, name+".out")

		want, err := os.ReadFile(wantPath)
		if err != nil {
			t.Errorf("missing .out for %s: %v", name, err)
			continue
		}

		t.Run(name, func(t *testing.T) {
			// Read and substitute the server URL placeholder.
			src, err := os.ReadFile(mochiPath)
			if err != nil {
				t.Fatalf("read fixture: %v", err)
			}
			src = []byte(strings.ReplaceAll(string(src), "HTTPTEST_URL", server.URL))

			// Write to a temp file for the build driver.
			tmpDir := t.TempDir()
			tmpMochi := filepath.Join(tmpDir, name+".mochi")
			if err := os.WriteFile(tmpMochi, src, 0o644); err != nil {
				t.Fatalf("write temp: %v", err)
			}

			outJar := filepath.Join(tmpDir, name+".jar")
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(tmpMochi, outJar, TargetUberJar); err != nil {
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

			got := strings.TrimRight(stdout.String(), "\n")
			wantStr := strings.TrimRight(string(want), "\n")
			if got != wantStr {
				t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, wantStr)
			}
		})
	}
}

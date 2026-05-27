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

	"mochi/transpiler3/dotnet/lower"
)

// TestPhase14Fetch is the gate test for MEP-48 Phase 14: HTTP fetch + JSON decode.
// It starts a local httptest.Server, substitutes HTTPTEST_URL in each fixture source,
// compiles to a fx-dependent DLL, runs it, and compares stdout to the paired .out file.
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

	fixtureBase := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "dotnet", "fixtures", "phase14-fetch")
	entries, err := os.ReadDir(fixtureBase)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureBase, err)
	}

	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		fixtureDir := filepath.Join(fixtureBase, name)
		mochiPath := filepath.Join(fixtureDir, name+".mochi")
		wantPath := filepath.Join(fixtureDir, name+".out")

		if _, err := os.Stat(mochiPath); err != nil {
			continue
		}
		want, err := os.ReadFile(wantPath)
		if err != nil {
			t.Errorf("missing .out for %s: %v", name, err)
			continue
		}

		t.Run(name, func(t *testing.T) {
			src, err := os.ReadFile(mochiPath)
			if err != nil {
				t.Fatalf("read fixture: %v", err)
			}
			src = []byte(strings.ReplaceAll(string(src), "HTTPTEST_URL", server.URL))

			tmpDir := t.TempDir()
			tmpMochi := filepath.Join(tmpDir, name+".mochi")
			if err := os.WriteFile(tmpMochi, src, 0o644); err != nil {
				t.Fatalf("write temp: %v", err)
			}

			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(tmpMochi, outDir, TargetFxDependent); err != nil {
				t.Fatalf("Build(%s): %v", name, err)
			}

			tc, err := resolveToolchain()
			if err != nil {
				t.Fatalf("resolveToolchain: %v", err)
			}

			className := lower.ClassName(tmpMochi)
			dllPath := filepath.Join(outDir, className+".dll")
			cmd := exec.Command(tc.Dotnet, dllPath)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("dotnet %s: %v", dllPath, err)
			}

			got := strings.TrimRight(stdout.String(), "\r\n")
			wantStr := strings.TrimRight(string(want), "\r\n")
			if got != wantStr {
				t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, wantStr)
			}
		})
	}
}

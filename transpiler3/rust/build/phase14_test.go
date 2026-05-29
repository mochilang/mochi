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

// TestPhase14Fetch starts a local httptest.Server, substitutes HTTPTEST_URL
// in each fixture's source, builds the Rust crate, and compares stdout to
// the paired .out file.
func TestPhase14Fetch(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/hello":
			fmt.Fprint(w, "hello world")
		case "/short":
			fmt.Fprint(w, "ok")
		case "/empty":
			fmt.Fprint(w, "")
		case "/multi":
			fmt.Fprint(w, "alpha")
		case "/multi2":
			fmt.Fprint(w, "beta")
		case "/json":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"name":"Alice","city":"Hanoi"}`)
		case "/json_name":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"name":"Bob"}`)
		case "/json_age":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"age":42}`)
		case "/json_bool":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"ok":true}`)
		case "/json_escapes":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"msg":"hi\nthere"}`)
		default:
			http.NotFound(w, r)
		}
	}))
	defer server.Close()

	fixtureBase := filepath.Join(repoRoot(t), "tests", "transpiler3", "rust", "fixtures", "phase14-fetch")
	entries, err := os.ReadDir(fixtureBase)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureBase, err)
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureBase, e.Name())
		wantPath := filepath.Join(fixtureBase, name+".out")

		t.Run(name, func(t *testing.T) {
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read want %s: %v", wantPath, err)
			}
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
			binPath, err := d.Build(tmpMochi, outDir, TargetNativeExecutable)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			cmd := exec.Command(binPath)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run: %v", err)
			}
			if !bytes.Equal(stdout.Bytes(), want) {
				t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", stdout.Bytes(), want)
			}
		})
	}
}

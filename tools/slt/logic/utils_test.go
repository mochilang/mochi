//go:build slow

package logic

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
)

func TestGenerate(t *testing.T) {
	outDir := t.TempDir()
	err := GenerateFiles([]string{"evidence/slt_lang_update.test"}, outDir, false, 1, 1)
	if err != nil {
		t.Fatalf("generate failed: %v", err)
	}
	outPath := filepath.Join(outDir, "evidence", "slt_lang_update", "case1.mochi")
	if _, err := os.Stat(outPath); err != nil {
		t.Fatalf("generated file not found: %v", err)
	}
}

func TestFindRepoRoot(t *testing.T) {
	root, err := FindRepoRoot()
	if err != nil {
		t.Fatalf("find root: %v", err)
	}
	if _, err := os.Stat(filepath.Join(root, "go.mod")); err != nil {
		t.Fatalf("go.mod not found at %s", root)
	}
}

func TestRunMochi(t *testing.T) {
	out, err := RunMochi("print(1 + 2)")
	if err != nil {
		t.Fatalf("run mochi: %v", err)
	}
	if out != "3" {
		t.Fatalf("unexpected output %q", out)
	}
}

func TestDownloadFile(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, "hello")
	}))
	defer srv.Close()

	path := filepath.Join(t.TempDir(), "f.txt")
	if err := DownloadFile(srv.URL, path); err != nil {
		t.Fatalf("download file: %v", err)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read file: %v", err)
	}
	if string(data) != "hello" {
		t.Fatalf("unexpected contents: %q", string(data))
	}
}

func TestDiscoverTests(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, `{"tree":[{"path":"a/b.test","type":"blob"},{"path":"c/d.sql","type":"blob"}]}`)
	}))
	defer srv.Close()

	files, err := DiscoverTests(srv.URL)
	if err != nil {
		t.Fatalf("discover tests: %v", err)
	}
	if len(files) != 1 || files[0] != "a/b.test" {
		t.Fatalf("unexpected files %v", files)
	}
}

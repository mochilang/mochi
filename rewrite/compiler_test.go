package rewrite

import (
	"archive/zip"
	"bytes"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

func TestNewCompilerRequiresBinary(t *testing.T) {
	_, err := NewCompiler("")
	if err == nil {
		t.Fatal("expected error for empty binary")
	}
}

func TestCompileAndRunSQLite(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping sqlite e2e in short mode")
	}
	if runtime.GOOS != "linux" {
		t.Skip("sqlite e2e currently supported on linux test env")
	}

	repoRoot := projectRoot(t)
	chibicc := filepath.Join(repoRoot, "exp", "compiler", "chibicc", "chibicc")
	buildChibicc(t, filepath.Join(repoRoot, "exp", "compiler", "chibicc"))

	compiler, err := NewCompiler(chibicc)
	if err != nil {
		t.Fatal(err)
	}

	work := t.TempDir()
	extractSQLiteAmalgamation(t, work)

	compiler.WorkDir = work
	if err := compiler.Run("shell.c", "sqlite3.c", "-O0", "-g", "-ldl", "-lpthread", "-lm", "-o", "sqlite3"); err != nil {
		t.Fatalf("compile sqlite shell: %v", err)
	}

	cmd := exec.Command(filepath.Join(work, "sqlite3"), ":memory:", "select 40+2;")
	cmd.Dir = work
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run sqlite3: %v\n%s", err, out)
	}
	if !bytes.Contains(out, []byte("42")) {
		t.Fatalf("expected sqlite output to contain 42, got: %s", out)
	}
}

func projectRoot(t *testing.T) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	root := filepath.Clean(filepath.Join(wd, ".."))
	if _, err := os.Stat(filepath.Join(root, "go.mod")); err != nil {
		t.Fatalf("cannot locate project root from %s: %v", wd, err)
	}
	return root
}

func buildChibicc(t *testing.T, dir string) {
	t.Helper()
	cmd := exec.Command("make", "-j2")
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("build chibicc failed: %v\n%s", err, out)
	}
}

func extractSQLiteAmalgamation(t *testing.T, dest string) {
	t.Helper()
	const url = "https://sqlite.org/2024/sqlite-amalgamation-3460100.zip"

	resp, err := http.Get(url)
	if err != nil {
		t.Fatalf("download sqlite amalgamation: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("unexpected status downloading sqlite: %s", resp.Status)
	}

	zipPath := filepath.Join(dest, "sqlite.zip")
	f, err := os.Create(zipPath)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := io.Copy(f, resp.Body); err != nil {
		f.Close()
		t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}

	r, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("open zip: %v", err)
	}
	defer r.Close()

	for _, file := range r.File {
		name := filepath.Base(file.Name)
		switch name {
		case "sqlite3.c", "shell.c", "sqlite3.h", "sqlite3ext.h":
		default:
			continue
		}

		rc, err := file.Open()
		if err != nil {
			t.Fatal(err)
		}
		outPath := filepath.Join(dest, name)
		out, err := os.Create(outPath)
		if err != nil {
			rc.Close()
			t.Fatal(err)
		}
		if _, err := io.Copy(out, rc); err != nil {
			out.Close()
			rc.Close()
			t.Fatal(err)
		}
		if err := out.Close(); err != nil {
			rc.Close()
			t.Fatal(err)
		}
		if err := rc.Close(); err != nil {
			t.Fatal(err)
		}
	}

	for _, required := range []string{"sqlite3.c", "shell.c", "sqlite3.h", "sqlite3ext.h"} {
		if _, err := os.Stat(filepath.Join(dest, required)); err != nil {
			t.Fatalf("missing %s after extraction: %v", required, err)
		}
	}

	fmt.Println("sqlite amalgamation extracted to", dest)
}

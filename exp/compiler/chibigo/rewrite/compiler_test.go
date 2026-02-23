package chibigo_test

import (
	"archive/zip"
	"bytes"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/exp/compiler/chibigo/rewrite"
)

func TestCompileSQLiteAndRun(t *testing.T) {
	root := repoRoot(t)
	buildChibicc(t, filepath.Join(root, "exp", "compiler", "chibicc"))
	compiler, err := chibigo.New(filepath.Join(root, "exp", "compiler", "chibicc", "chibicc"))
	if err != nil {
		t.Fatal(err)
	}
	work := t.TempDir()
	downloadSQLite(t, work)
	compiler.WorkDir = work
	if err := compiler.Compile("shell.c", "sqlite3.c", "-O0", "-g", "-ldl", "-lpthread", "-lm", "-o", "sqlite3"); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command(filepath.Join(work, "sqlite3"), ":memory:", "select 40+2;").CombinedOutput()
	if err != nil {
		t.Fatalf("run sqlite: %v\n%s", err, out)
	}
	if !bytes.Contains(out, []byte("42")) {
		t.Fatalf("expected 42 in output, got %s", out)
	}
}

func repoRoot(t *testing.T) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	return filepath.Clean(filepath.Join(wd, "..", "..", "..", ".."))
}

func buildChibicc(t *testing.T, dir string) {
	t.Helper()
	cmd := exec.Command("make", "-j2")
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("build chibicc: %v\n%s", err, out)
	}
}

func downloadSQLite(t *testing.T, dest string) {
	t.Helper()
	const url = "https://sqlite.org/2024/sqlite-amalgamation-3460100.zip"
	resp, err := http.Get(url)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status: %s", resp.Status)
	}
	zf := filepath.Join(dest, "sqlite.zip")
	f, err := os.Create(zf)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := io.Copy(f, resp.Body); err != nil {
		_ = f.Close()
		t.Fatal(err)
	}
	_ = f.Close()
	r, err := zip.OpenReader(zf)
	if err != nil {
		t.Fatal(err)
	}
	defer r.Close()
	for _, file := range r.File {
		base := filepath.Base(file.Name)
		switch base {
		case "sqlite3.c", "shell.c", "sqlite3.h", "sqlite3ext.h":
		default:
			continue
		}
		rc, err := file.Open()
		if err != nil {
			t.Fatal(err)
		}
		out, err := os.Create(filepath.Join(dest, base))
		if err != nil {
			_ = rc.Close()
			t.Fatal(err)
		}
		if _, err := io.Copy(out, rc); err != nil {
			_ = out.Close()
			_ = rc.Close()
			t.Fatal(err)
		}
		_ = out.Close()
		_ = rc.Close()
	}
}

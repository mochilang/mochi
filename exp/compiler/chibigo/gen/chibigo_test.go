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
)

var chibigoPath string

func TestMain(m *testing.M) {
	root, err := repoRoot()
	if err != nil {
		panic(err)
	}
	tmpDir, err := os.MkdirTemp("", "chibigo-gen-test-")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(tmpDir)

	chibigoPath = filepath.Join(tmpDir, "chibigo")
	cmd := exec.Command("go", "build", "-o", chibigoPath, "./exp/compiler/chibigo/gen/cmd/chibigo")
	cmd.Dir = root
	if output, err := cmd.CombinedOutput(); err != nil {
		panic(string(output) + err.Error())
	}

	os.Exit(m.Run())
}

func TestCompileAndRun(t *testing.T) {
	dir := t.TempDir()
	mainPath := writeFile(t, dir, "main.c", `int main() { return 42; }`)
	outPath := filepath.Join(dir, "app.bin")

	runCmd(t, nil, nil, chibigoPath, "-o", outPath, mainPath)

	if code := runBinary(t, outPath); code != 42 {
		t.Fatalf("expected exit code 42, got %d", code)
	}
}

func TestCompileSQLiteObject(t *testing.T) {
	if os.Getenv("CHIBIGO_SQLITE_E2E") == "" {
		t.Skip("set CHIBIGO_SQLITE_E2E=1 to run slow sqlite compilation test")
	}
	root, err := repoRoot()
	if err != nil {
		t.Fatal(err)
	}
	work := t.TempDir()
	downloadSQLite(t, work)
	inc := filepath.Join(root, "exp", "compiler", "chibicc", "include")
	obj := filepath.Join(work, "sqlite3.o")

	runCmd(t, append(os.Environ(), "CHIBICC_INCLUDE="+inc), nil, chibigoPath, "-I"+inc, "-c", filepath.Join(work, "sqlite3.c"), "-o", obj)
	if _, err := os.Stat(obj); err != nil {
		t.Fatalf("expected sqlite object output: %v", err)
	}
}

func runCmd(t *testing.T, env []string, stdin []byte, name string, args ...string) {
	t.Helper()
	cmd := exec.Command(name, args...)
	if len(env) > 0 {
		cmd.Env = env
	}
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("command %s %v failed: %v\n%s", name, args, err, output)
	}
}

func runBinary(t *testing.T, path string) int {
	t.Helper()
	cmd := exec.Command(path)
	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ExitCode()
		}
		t.Fatalf("run binary: %v", err)
	}
	return 0
}

func writeFile(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("write file: %v", err)
	}
	return path
}

func repoRoot() (string, error) {
	wd, err := os.Getwd()
	if err != nil {
		return "", err
	}
	return filepath.Clean(filepath.Join(wd, "..", "..", "..", "..")), nil
}

func downloadSQLite(t *testing.T, dest string) {
	t.Helper()
	const url = "https://sqlite.org/2024/sqlite-amalgamation-3460100.zip"
	resp, err := http.Get(url)
	if err != nil {
		t.Fatalf("download sqlite: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("download sqlite unexpected status: %s", resp.Status)
	}
	zipPath := filepath.Join(dest, "sqlite.zip")
	f, err := os.Create(zipPath)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := io.Copy(f, resp.Body); err != nil {
		_ = f.Close()
		t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}

	r, err := zip.OpenReader(zipPath)
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

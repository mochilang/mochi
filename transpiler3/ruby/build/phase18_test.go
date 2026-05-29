package build

import (
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase18IO exercises file I/O, JSON decode, CSV load, and HTTP GET.
func TestPhase18IO(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}
	repoRoot := repoRootForTest(t)
	runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")

	t.Run("read_file", func(t *testing.T) {
		dataDir := t.TempDir()
		dataPath := filepath.Join(dataDir, "data.txt")
		if err := os.WriteFile(dataPath, []byte("hello world"), 0o644); err != nil {
			t.Fatalf("seed data: %v", err)
		}
		src := "let s = readFile(\"" + dataPath + "\")\nprint(s)\n"
		runRubyFixture(t, tc, runtimeLib, "read_file", src, "hello world\n")
	})

	t.Run("write_file", func(t *testing.T) {
		dataDir := t.TempDir()
		dataPath := filepath.Join(dataDir, "out.txt")
		src := "writeFile(\"" + dataPath + "\", \"abc\")\n" +
			"let s = readFile(\"" + dataPath + "\")\n" +
			"print(s)\n"
		runRubyFixture(t, tc, runtimeLib, "write_file", src, "abc\n")
	})

	t.Run("append_file", func(t *testing.T) {
		dataDir := t.TempDir()
		dataPath := filepath.Join(dataDir, "log.txt")
		if err := os.WriteFile(dataPath, []byte("ab"), 0o644); err != nil {
			t.Fatalf("seed: %v", err)
		}
		src := "appendFile(\"" + dataPath + "\", \"cd\")\n" +
			"let s = readFile(\"" + dataPath + "\")\n" +
			"print(s)\n"
		runRubyFixture(t, tc, runtimeLib, "append_file", src, "abcd\n")
	})

	t.Run("lines", func(t *testing.T) {
		dataDir := t.TempDir()
		dataPath := filepath.Join(dataDir, "lines.txt")
		if err := os.WriteFile(dataPath, []byte("first\nsecond\nthird\n"), 0o644); err != nil {
			t.Fatalf("seed: %v", err)
		}
		src := "let ls = lines(\"" + dataPath + "\")\n" +
			"print(ls[0])\n" +
			"print(ls[2])\n" +
			"print(len(ls))\n"
		runRubyFixture(t, tc, runtimeLib, "lines", src, "first\nthird\n3\n")
	})

	t.Run("json_decode", func(t *testing.T) {
		dataDir := t.TempDir()
		dataPath := filepath.Join(dataDir, "data.json")
		if err := os.WriteFile(dataPath, []byte(`{"x": 1, "y": 2}`), 0o644); err != nil {
			t.Fatalf("seed: %v", err)
		}
		src := "let s = readFile(\"" + dataPath + "\")\n" +
			"let m = json_decode(s)\n" +
			"print(m[\"x\"])\n" +
			"print(m[\"y\"])\n"
		runRubyFixture(t, tc, runtimeLib, "json_decode", src, "1\n2\n")
	})

	t.Run("load_csv", func(t *testing.T) {
		dataDir := t.TempDir()
		dataPath := filepath.Join(dataDir, "rows.csv")
		if err := os.WriteFile(dataPath, []byte("a,b,c\n1,2,3\n"), 0o644); err != nil {
			t.Fatalf("seed: %v", err)
		}
		src := "let rows = loadCSV(\"" + dataPath + "\")\n" +
			"print(rows[0][0])\n" +
			"print(rows[1][2])\n"
		runRubyFixture(t, tc, runtimeLib, "load_csv", src, "a\n3\n")
	})

	t.Run("http_get", func(t *testing.T) {
		srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			_, _ = w.Write([]byte("from-server"))
		}))
		defer srv.Close()
		src := "let body = fetch \"" + srv.URL + "\"\nprint(body)\n"
		runRubyFixture(t, tc, runtimeLib, "http_get", src, "from-server\n")
	})
}

func runRubyFixture(t *testing.T, tc *Toolchain, runtimeLib, name, src, want string) {
	t.Helper()
	srcDir := t.TempDir()
	srcPath := filepath.Join(srcDir, name+".mochi")
	if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}
	outDir := t.TempDir()
	d := &Driver{}
	if err := d.Build(srcPath, outDir, TargetRubySource); err != nil {
		t.Fatalf("Build: %v\nsrc:\n%s", err, src)
	}
	rb := filepath.Join(outDir, name+".rb")
	cmd := exec.Command(tc.Ruby, "-I", runtimeLib, rb)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("ruby run failed: %v\noutput: %s\nrb source:\n%s", err, out, readFile(t, rb))
	}
	got := string(out)
	if got != want {
		t.Fatalf("%s output mismatch:\ngot:\n%s\nwant:\n%s\nrb source:\n%s",
			name, got, want, readFile(t, rb))
	}
}

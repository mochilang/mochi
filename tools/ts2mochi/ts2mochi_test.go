//go:build slow

package ts2mochi_test

import (
	"bytes"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/tools/ts2mochi"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/compiler/ts", "*.ts")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".ts")
		t.Run(name, func(t *testing.T) {
			out, err := ts2mochi.ConvertFile(src)
			if err != nil {
				if shouldUpdate() {
					os.WriteFile(filepath.Join(root, "tests/compiler/ts", name+".error"), []byte(err.Error()+"\n"), 0644)
				}
				want, werr := os.ReadFile(filepath.Join(root, "tests/compiler/ts", name+".error"))
				if werr != nil {
					t.Fatalf("missing golden error: %v", werr)
				}
				if strings.TrimSpace(string(want)) != strings.TrimSpace(err.Error()) {
					t.Fatalf("unexpected error\nwant: %s\ngot: %s", want, err.Error())
				}
				return
			}
			if shouldUpdate() {
				os.WriteFile(filepath.Join(root, "tests/compiler/ts", name+".mochi.out"), out, 0644)
			}
			want, werr := os.ReadFile(filepath.Join(root, "tests/compiler/ts", name+".mochi.out"))
			if werr != nil {
				t.Fatalf("missing golden: %v", werr)
			}
			got := bytes.TrimSpace(out)
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}

func TestConvert_Execution(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/compiler/ts", "*.ts")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".ts")
		t.Run(name, func(t *testing.T) {
			code, err := ts2mochi.ConvertFile(src)
			if err != nil {
				t.Skipf("convert error: %v", err)
			}
			prog, err := parser.ParseString(string(code))
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var buf bytes.Buffer
			m := vm.New(p, &buf)
			if err := m.Run(); err != nil {
				t.Fatalf("runtime error: %v", err)
			}
			want, werr := os.ReadFile(filepath.Join(root, "tests/compiler/ts", name+".out"))
			if werr != nil {
				t.Fatalf("missing output golden: %v", werr)
			}
			got := strings.TrimSpace(buf.String())
			if got != strings.TrimSpace(string(want)) {
				t.Fatalf("output mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}

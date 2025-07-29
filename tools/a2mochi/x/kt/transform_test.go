//go:build slow

package kt_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	kt "mochi/tools/a2mochi/x/kt"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
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

func runMochi(src string) ([]byte, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, err
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func runCase(t *testing.T, name, srcPath, outDir, root string) {
	t.Helper()

	data, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	p, err := kt.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	b, err := json.Marshal(p)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var q kt.Program
	if err := json.Unmarshal(b, &q); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	q.Source = string(data)
	node, err := kt.Transform(&q)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	code, err := kt.Print(node)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	astPath := filepath.Join(outDir, name+".ast")
	outPath := filepath.Join(outDir, name+".out")
	if *update {
		os.WriteFile(astPath, []byte(node.String()), 0o644)
		os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(code), 0o644)
		if out, err := runMochi(code); err == nil {
			os.WriteFile(outPath, out, 0o644)
		}
	}
	want, err := os.ReadFile(astPath)
	if err != nil {
		t.Fatalf("missing golden: %v", err)
	}
	if node.String() != string(want) {
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", node.String(), want)
	}
	gotOut, err := runMochi(code)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if *update {
		os.WriteFile(outPath, gotOut, 0o644)
	}
	wantOut, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("missing golden output: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
	}
}

func TestTransform_Golden(t *testing.T) {
	t.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")

	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/kt", "*.kt")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/kt")
	os.MkdirAll(outDir, 0o755)
	if matches, _ := filepath.Glob(filepath.Join(outDir, "*.mochi")); len(matches) == 0 && !*update {
		t.Skip("golden files not present")
	}

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".kt")
		t.Run(name, func(t *testing.T) {
			runCase(t, name, srcPath, outDir, root)
		})
	}
	if *update {
		kt.UpdateReadmeForTests()
	}
}

//go:build slow

package cpp_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	"mochi/tools/a2mochi/x/cpp"
)

var updateGolden = flag.Bool("update", false, "update golden files")

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

func processFile(t *testing.T, root, outDir, srcPath string) {
	data, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	prog, err := cpp.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	node, err := cpp.Transform(prog)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	got := []byte(node.String())
	astPath := filepath.Join(outDir, strings.TrimSuffix(filepath.Base(srcPath), ".cpp")+".ast")
	if *updateGolden {
		os.WriteFile(astPath, got, 0644)
	}
	want, err := os.ReadFile(astPath)
	if err != nil {
		t.Fatalf("missing golden: %v", err)
	}
	if string(got) != string(want) {
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
	}

	code, err := cpp.Print(node)
	if err != nil {
		t.Fatalf("convert source: %v", err)
	}
	name := strings.TrimSuffix(filepath.Base(srcPath), ".cpp")
	mochiPath := filepath.Join(outDir, name+".mochi")
	if *updateGolden {
		os.WriteFile(mochiPath, []byte(code), 0644)
	}
	gotOut, err := runMochi(code)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if *updateGolden {
		os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
	}
	vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
	if err != nil {
		t.Fatalf("missing vm source: %v", err)
	}
	wantOut, err := runMochi(string(vmSrc))
	if err != nil {
		t.Fatalf("run vm: %v", err)
	}
	if !bytes.Equal(gotOut, wantOut) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
	}
}

func TestTransform_Golden(t *testing.T) {
	if _, err := exec.LookPath("clang++"); err != nil {
		t.Skipf("clang++ not installed: %v", err)
	}

	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/cpp", "*.cpp")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/cpp")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".cpp")
		t.Run(name, func(t *testing.T) {
			processFile(t, root, outDir, srcPath)
		})
	}
}

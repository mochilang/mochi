//go:build slow

package swift_test

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

	"mochi/tools/a2mochi/x/swift"
)

var updateGolden = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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

func runMochi(t *testing.T, src string) []byte {
	t.Helper()
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatal(errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		t.Fatal(err)
	}
	return bytes.TrimSpace(out.Bytes())
}

func TestTransformGolden(t *testing.T) {
	if _, err := exec.LookPath("swiftc"); err != nil {
		t.Skip("swiftc not installed")
	}
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "swift", "*.swift")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"basic_compare":       true,
		"count_builtin":       true,
		"for_list_collection": true,
		"for_loop":            true,
		"while_loop":          true,
		"fun_three_args":      true,
		"len_map":             true,
		"list_index":          true,
		"math_ops":            true,
		"min_max_builtin":     true,
		"print_hello":         true,
		"in_operator":         true,
		"map_in_operator":     true,
		"string_concat":       true,
		"str_builtin":         true,
		"unary_neg":           true,
		"var_assignment":      true,
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "swift")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".swift")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			testFile(t, root, outDir, src)
		})
	}
}

func testFile(t *testing.T, root, outDir, src string) {
	t.Helper()
	name := strings.TrimSuffix(filepath.Base(src), ".swift")
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := swift.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	node, err := swift.Transform(prog)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	astPath := filepath.Join(outDir, name+".ast")
	if *updateGolden {
		os.WriteFile(astPath, []byte(node.String()), 0o644)
	}
	want, err := os.ReadFile(astPath)
	if err != nil {
		t.Fatalf("missing golden: %v", err)
	}
	got := node.String()
	if strings.TrimSpace(string(want)) != strings.TrimSpace(got) {
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
	}
	code, err := swift.Print(node)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	mochiPath := filepath.Join(outDir, name+".mochi")
	if *updateGolden {
		os.WriteFile(mochiPath, []byte(code), 0o644)
	}
	gotOut := runMochi(t, code)
	if *updateGolden {
		os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
	}
	vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
	if err != nil {
		t.Fatalf("missing vm source: %v", err)
	}
	wantOut := runMochi(t, string(vmSrc))
	if !bytes.Equal(gotOut, wantOut) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
	}
}

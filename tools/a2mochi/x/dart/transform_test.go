//go:build slow

package dart_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	"mochi/tools/a2mochi/x/dart"
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

func runTransformFile(t *testing.T, root, outDir, srcPath string) {
	name := strings.TrimSuffix(filepath.Base(srcPath), ".dart")
	data, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	prog, err := dart.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	astNode, err := dart.Transform(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	src, err := dart.Print(astNode)
	if err != nil {
		t.Fatalf("print source: %v", err)
	}
	got := []byte(astNode.String())
	astPath := filepath.Join(outDir, name+".ast")
	if *update {
		os.WriteFile(astPath, got, 0o644)
		os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(src), 0o644)
	}
	want, err := os.ReadFile(astPath)
	if err != nil {
		t.Fatalf("missing golden: %v", err)
	}
	if string(got) != string(want) {
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
	}

	var buf bytes.Buffer
	if err := ast.Fprint(&buf, astNode); err != nil {
		t.Fatalf("print: %v", err)
	}
	_ = buf.String()

	mochiPath := filepath.Join(outDir, name+".mochi")
	if *update {
		os.WriteFile(mochiPath, []byte(src), 0o644)
	}
	gotOut, err := runMochi(src)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
	if err != nil {
		t.Fatalf("missing vm source: %v", err)
	}
	wantOut, err := runMochi(string(vmSrc))
	if err != nil {
		t.Fatalf("run vm: %v", err)
	}
	if *update {
		os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
	}
	if !bytes.Equal(gotOut, wantOut) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
	}
}

func TestTransform_Golden(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skipf("go not installed: %v", err)
	}

	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/dart", "*.dart")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"append_builtin":      true,
		"avg_builtin":         true,
		"basic_compare":       true,
		"binary_precedence":   true,
		"bool_chain":          true,
		"break_continue":      true,
		"cast_string_to_int":  true,
		"exists_builtin":      true,
		"for_list_collection": true,
		"for_loop":            true,
		"in_operator":         true,
		"let_and_print":       true,
		"len_builtin":         true,
		"list_index":          true,
		"map_int_key":         true,
		"membership":          true,
		"print_hello":         true,
		"string_concat":       true,
		"string_contains":     true,
		"string_in_operator":  true,
		"string_index":        true,
		"test_block":          true,
		"unary_neg":           true,
		"var_assignment":      true,
		"while_loop":          true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/dart")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".dart")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			runTransformFile(t, root, outDir, srcPath)
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "dart")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "dart")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.dart"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".dart")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Dart Converter\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	tz := time.FixedZone("GMT+7", 7*60*60)
	buf.WriteString(fmt.Sprintf("Date: %s\n\n", time.Now().In(tz).Format("2006-01-02 15:04:05 MST")))
	buf.WriteString("This directory holds golden outputs for the Dart to Mochi converter.\n")
	buf.WriteString("Each `.dart` source in `tests/transpiler/x/dart` has a matching `.mochi` and `.ast` file generated by the tests.\n")
	buf.WriteString("\n## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "dart", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

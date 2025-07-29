//go:build slow

package swift_test

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

func runMochi(t *testing.T, src string) ([]byte, error) {
	t.Helper()
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

func runMochiNoTest(src string) ([]byte, error) {
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
		"fun_call":            true,
		"if_else":             true,
		"binary_precedence":   true,
		"bool_chain":          true,
		"basic_compare":       true,
		"break_continue":      true,
		"cast_string_to_int":  true,
		"cast_struct":         true,
		"len_builtin":         true,
		"len_string":          true,
		"map_index":           true,
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
		"list_assign":         true,
		"map_assign":          true,
		"map_int_key":         true,
		"map_membership":      true,
		"membership":          true,
		"string_contains":     true,
		"string_index":        true,
		"substring_builtin":   true,
		"string_in_operator":  true,
		"append_builtin":      true,
		"avg_builtin":         true,
		"sum_builtin":         true,
		"let_and_print":       true,
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
	if *updateGolden {
		updateReadme()
	}
}

func testFile(t *testing.T, root, outDir, src string) {
	t.Helper()
	name := strings.TrimSuffix(filepath.Base(src), ".swift")
	errPath := filepath.Join(outDir, name+".error")
	data, err := os.ReadFile(src)
	if err != nil {
		if *updateGolden {
			os.WriteFile(errPath, []byte("read: "+err.Error()), 0o644)
		}
		t.Fatalf("read: %v", err)
	}
	prog, err := swift.Parse(string(data))
	if err != nil {
		if *updateGolden {
			os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		}
		t.Fatalf("parse: %v", err)
	}
	node, err := swift.Transform(prog)
	if err != nil {
		if *updateGolden {
			os.WriteFile(errPath, []byte("convert: "+err.Error()), 0o644)
		}
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
		if *updateGolden {
			os.WriteFile(errPath, []byte("print: "+err.Error()), 0o644)
		}
		t.Fatalf("print: %v", err)
	}
	mochiPath := filepath.Join(outDir, name+".mochi")
	if *updateGolden {
		os.WriteFile(mochiPath, []byte(code), 0o644)
	}
	gotOut, err := runMochi(t, code)
	if err != nil {
		if *updateGolden {
			os.WriteFile(errPath, []byte("run: "+err.Error()), 0o644)
		}
		t.Fatalf("run: %v", err)
	}
	if *updateGolden {
		os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
	}
	vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
	if err != nil {
		t.Fatalf("missing vm source: %v", err)
	}
	wantOut, err := runMochi(t, string(vmSrc))
	if err != nil {
		t.Fatalf("vm run: %v", err)
	}
	if !bytes.Equal(gotOut, wantOut) {
		if *updateGolden {
			os.WriteFile(errPath, []byte("output mismatch\nGot: "+string(gotOut)+"\nWant: "+string(wantOut)), 0o644)
		}
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
	}
	_ = os.Remove(errPath)
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "swift")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "swift")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.swift"))
	sort.Strings(files)
	total := len(files)
	passed := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".swift")
		mark := "[ ]"
		outPath := filepath.Join(outDir, name+".out")
		if outData, err := os.ReadFile(outPath); err == nil {
			vmSrc, err2 := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err2 == nil {
				want, err3 := runMochiNoTest(string(vmSrc))
				if err3 == nil && bytes.Equal(bytes.TrimSpace(outData), want) {
					passed++
					mark = "[x]"
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	tz := time.FixedZone("GMT+7", 7*60*60)
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Swift Converter\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", passed, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", time.Now().In(tz).Format("2006-01-02 15:04 MST"))
	buf.WriteString("This directory holds golden outputs for the Swift to Mochi converter. Each `.swift` source in `tests/transpiler/x/swift` has a matching `.mochi` and `.ast` file generated by the tests.\n\n")
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "swift", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

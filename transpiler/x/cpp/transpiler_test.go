//go:build slow

package cpp_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/compiler/x/testutil"
	"mochi/parser"
	cpp "mochi/transpiler/x/cpp"
	"mochi/types"
)

func runExample(t *testing.T, base string) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cpp")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", base+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	ast, err := cpp.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile error: %v", err)
	}
	code := ast.Emit()
	codePath := filepath.Join(outDir, base+".cpp")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	bin := filepath.Join(outDir, base)
	if out, err := exec.Command("g++", codePath, "-std=c++20", "-o", bin).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
		t.Fatalf("compile error: %v", err)
	}
	defer os.Remove(bin)
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, base+".out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestCPPTranspiler_PrintHello(t *testing.T)        { runExample(t, "print_hello") }
func TestCPPTranspiler_StringConcat(t *testing.T)      { runExample(t, "string_concat") }
func TestCPPTranspiler_LenString(t *testing.T)         { runExample(t, "len_string") }
func TestCPPTranspiler_WhileLoop(t *testing.T)         { runExample(t, "while_loop") }
func TestCPPTranspiler_FunCall(t *testing.T)           { runExample(t, "fun_call") }
func TestCPPTranspiler_FunThreeArgs(t *testing.T)      { runExample(t, "fun_three_args") }
func TestCPPTranspiler_FunExprInLet(t *testing.T)      { runExample(t, "fun_expr_in_let") }
func TestCPPTranspiler_BoolChain(t *testing.T)         { runExample(t, "bool_chain") }
func TestCPPTranspiler_StringCompare(t *testing.T)     { runExample(t, "string_compare") }
func TestCPPTranspiler_ShortCircuit(t *testing.T)      { runExample(t, "short_circuit") }
func TestCPPTranspiler_LenBuiltin(t *testing.T)        { runExample(t, "len_builtin") }
func TestCPPTranspiler_BreakContinue(t *testing.T)     { runExample(t, "break_continue") }
func TestCPPTranspiler_ForListCollection(t *testing.T) { runExample(t, "for_list_collection") }
func TestCPPTranspiler_StringContains(t *testing.T)    { runExample(t, "string_contains") }
func TestCPPTranspiler_StringInOperator(t *testing.T)  { runExample(t, "string_in_operator") }
func TestCPPTranspiler_StringIndex(t *testing.T)       { runExample(t, "string_index") }
func TestCPPTranspiler_SubstringBuiltin(t *testing.T)  { runExample(t, "substring_builtin") }
func TestCPPTranspiler_CastStringToInt(t *testing.T)   { runExample(t, "cast_string_to_int") }
func TestCPPTranspiler_SumBuiltin(t *testing.T)        { runExample(t, "sum_builtin") }
func TestCPPTranspiler_ListIndex(t *testing.T)         { runExample(t, "list_index") }

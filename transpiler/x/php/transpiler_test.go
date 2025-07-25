//go:build slow

package php_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	php "mochi/transpiler/x/php"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
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

func TestTranspile_PrintHello(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "php")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := php.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := php.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	code := buf.Bytes()
	phpFile := filepath.Join(outDir, "print_hello.php")
	if err := os.WriteFile(phpFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("php", phpFile)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "print_hello.error"))
	wantPath := filepath.Join(outDir, "print_hello.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_LetAndPrint(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "php")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "let_and_print.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := php.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := php.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	code := buf.Bytes()
	phpFile := filepath.Join(outDir, "let_and_print.php")
	if err := os.WriteFile(phpFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("php", phpFile)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "let_and_print.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "let_and_print.error"))
	wantPath := filepath.Join(outDir, "let_and_print.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_UnaryNeg(t *testing.T) {
	runTranspileTest(t, "unary_neg")
}

func TestTranspile_StringConcat(t *testing.T) {
	runTranspileTest(t, "string_concat")
}

func TestTranspile_SubstringBuiltin(t *testing.T) {
	runTranspileTest(t, "substring_builtin")
}

func TestTranspile_LenBuiltin(t *testing.T) {
	runTranspileTest(t, "len_builtin")
}

func TestTranspile_VarAssignment(t *testing.T) {
	runTranspileTest(t, "var_assignment")
}

func TestTranspile_TypedLet(t *testing.T) {
	runTranspileTest(t, "typed_let")
}

func TestTranspile_BasicCompare(t *testing.T) {
	runTranspileTest(t, "basic_compare")
}

func TestTranspile_TypedVar(t *testing.T) {
	runTranspileTest(t, "typed_var")
}

func TestTranspile_LenString(t *testing.T) {
	runTranspileTest(t, "len_string")
}

func TestTranspile_IfThenElse(t *testing.T) {
	runTranspileTest(t, "if_then_else")
}

func TestTranspile_IfThenElseNested(t *testing.T) {
	runTranspileTest(t, "if_then_else_nested")
}

func TestTranspile_IfElse(t *testing.T) {
	runTranspileTest(t, "if_else")
}

func TestTranspile_BinaryPrecedence(t *testing.T) {
	runTranspileTest(t, "binary_precedence")
}

func TestTranspile_MathOps(t *testing.T) {
	runTranspileTest(t, "math_ops")
}

func TestTranspile_StringCompare(t *testing.T) {
	runTranspileTest(t, "string_compare")
}

func TestTranspile_StrBuiltin(t *testing.T) {
	runTranspileTest(t, "str_builtin")
}

func TestTranspile_SumBuiltin(t *testing.T) {
	runTranspileTest(t, "sum_builtin")
}

func TestTranspile_AvgBuiltin(t *testing.T) {
	runTranspileTest(t, "avg_builtin")
}

func TestTranspile_FunCall(t *testing.T) {
	runTranspileTest(t, "fun_call")
}

func TestTranspile_FunThreeArgs(t *testing.T) {
	runTranspileTest(t, "fun_three_args")
}

func TestTranspile_BoolChain(t *testing.T) {
	runTranspileTest(t, "bool_chain")
}

func TestTranspile_MinMaxBuiltin(t *testing.T) {
	runTranspileTest(t, "min_max_builtin")
}

func TestTranspile_CastStringToInt(t *testing.T) {
	runTranspileTest(t, "cast_string_to_int")
}

func TestTranspile_CountBuiltin(t *testing.T) {
	runTranspileTest(t, "count_builtin")
}

func TestTranspile_AppendBuiltin(t *testing.T) {
	runTranspileTest(t, "append_builtin")
}

func TestTranspile_StringContains(t *testing.T) {
	runTranspileTest(t, "string_contains")
}

func TestTranspile_StringInOperator(t *testing.T) {
	runTranspileTest(t, "string_in_operator")
}

func TestTranspile_StringIndex(t *testing.T) {
	runTranspileTest(t, "string_index")
}

func TestTranspile_StringPrefixSlice(t *testing.T) {
	runTranspileTest(t, "string_prefix_slice")
}

func TestTranspile_UpdateStmt(t *testing.T) {
	runTranspileTest(t, "update_stmt")
}

func TestTranspile_BigIntOps(t *testing.T) {
	runTranspileExtendedTest(t, "bigint_ops")
}

func runTranspileExtendedTest(t *testing.T, name string) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "php")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm_extended", "valid", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := php.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := php.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	code := buf.Bytes()
	phpFile := filepath.Join(outDir, name+".php")
	if err := os.WriteFile(phpFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("php", phpFile)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	wantPath := filepath.Join(outDir, name+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func runTranspileTest(t *testing.T, name string) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "php")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := php.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := php.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	code := buf.Bytes()
	phpFile := filepath.Join(outDir, name+".php")
	if err := os.WriteFile(phpFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("php", phpFile)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	wantPath := filepath.Join(outDir, name+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

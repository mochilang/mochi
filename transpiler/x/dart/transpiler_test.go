//go:build slow

package dartt_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	dartt "mochi/transpiler/x/dart"
	"mochi/types"
)

func findRepoRoot(t *testing.T) string {
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

func runGolden(t *testing.T, name string) {
	t.Helper()
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "dart")
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
	ast, err := dartt.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := dartt.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	code := buf.Bytes()
	dartFile := filepath.Join(outDir, name+".dart")
	if err := os.WriteFile(dartFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("dart", dartFile)
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

func TestTranspile_PrintHello(t *testing.T) {
	runGolden(t, "print_hello")
}

func TestTranspile_LetAndPrint(t *testing.T) {
	runGolden(t, "let_and_print")
}

func TestTranspile_VarAssignment(t *testing.T) {
	runGolden(t, "var_assignment")
}

func TestTranspile_IfElse(t *testing.T) {
	runGolden(t, "if_else")
}

func TestTranspile_BasicCompare(t *testing.T) {
	runGolden(t, "basic_compare")
}

func TestTranspile_TypedLet(t *testing.T) {
	runGolden(t, "typed_let")
}

func TestTranspile_TypedVar(t *testing.T) {
	runGolden(t, "typed_var")
}

func TestTranspile_UnaryNeg(t *testing.T) {
	runGolden(t, "unary_neg")
}

func TestTranspile_WhileLoop(t *testing.T) {
	runGolden(t, "while_loop")
}

func TestTranspile_BinaryPrecedence(t *testing.T) {
	runGolden(t, "binary_precedence")
}

func TestTranspile_LenString(t *testing.T) {
	runGolden(t, "len_string")
}

func TestTranspile_ForLoop(t *testing.T) {
	runGolden(t, "for_loop")
}

func TestTranspile_ForListCollection(t *testing.T) {
	runGolden(t, "for_list_collection")
}

func TestTranspile_FunCall(t *testing.T) {
	runGolden(t, "fun_call")
}

func TestTranspile_FunExprInLet(t *testing.T) {
	runGolden(t, "fun_expr_in_let")
}

func TestTranspile_FunThreeArgs(t *testing.T) {
	runGolden(t, "fun_three_args")
}

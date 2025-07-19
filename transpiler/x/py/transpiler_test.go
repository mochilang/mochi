//go:build slow

package py_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	py "mochi/transpiler/x/py"
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

func runCase(t *testing.T, name string) {
	t.Helper()
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "py")
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
	progAST, err := py.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := py.Emit(&buf, progAST); err != nil {
		t.Fatalf("emit: %v", err)
	}
	code := buf.Bytes()
	pyFile := filepath.Join(outDir, name+".py")
	if err := os.WriteFile(pyFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("python3", pyFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
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
	runCase(t, "print_hello")
}

func TestTranspile_BinaryPrecedence(t *testing.T) {
	runCase(t, "binary_precedence")
}

func TestTranspile_BasicCompare(t *testing.T) {
	runCase(t, "basic_compare")
}

func TestTranspile_LetAndPrint(t *testing.T) {
	runCase(t, "let_and_print")
}

func TestTranspile_ListIndex(t *testing.T) {
	runCase(t, "list_index")
}

func TestTranspile_StringIndex(t *testing.T) {
	runCase(t, "string_index")
}

func TestTranspile_VarAssignment(t *testing.T) {
	runCase(t, "var_assignment")
}

func TestTranspile_MathOps(t *testing.T) {
	runCase(t, "math_ops")
}

func TestTranspile_StringConcat(t *testing.T) {
	runCase(t, "string_concat")
}

func TestTranspile_UnaryNeg(t *testing.T) {
	runCase(t, "unary_neg")
}

func TestTranspile_LenBuiltin(t *testing.T) {
	runCase(t, "len_builtin")
}

func TestTranspile_IfThenElse(t *testing.T) {
	runCase(t, "if_then_else")
}

func TestTranspile_IfThenElseNested(t *testing.T) {
	runCase(t, "if_then_else_nested")
}

func TestTranspile_InOperator(t *testing.T) {
	runCase(t, "in_operator")
}

func TestTranspile_StringCompare(t *testing.T) {
	runCase(t, "string_compare")
}

func TestTranspile_FunCall(t *testing.T) {
	runCase(t, "fun_call")
}

func TestTranspile_FunThreeArgs(t *testing.T) {
	runCase(t, "fun_three_args")
}

func TestTranspile_FunExprInLet(t *testing.T) {
	runCase(t, "fun_expr_in_let")
}

func TestTranspile_Closure(t *testing.T) {
	runCase(t, "closure")
}

func TestTranspile_BoolChain(t *testing.T) {
	runCase(t, "bool_chain")
}

func TestTranspile_StringContains(t *testing.T) {
	runCase(t, "string_contains")
}

func TestTranspile_AppendBuiltin(t *testing.T) {
	runCase(t, "append_builtin")
}

func TestTranspile_AvgBuiltin(t *testing.T) {
	runCase(t, "avg_builtin")
}

func TestTranspile_LenString(t *testing.T) {
	runCase(t, "len_string")
}

func TestTranspile_CountBuiltin(t *testing.T) {
	runCase(t, "count_builtin")
}

func TestTranspile_SumBuiltin(t *testing.T) {
	runCase(t, "sum_builtin")
}

func TestTranspile_ValuesBuiltin(t *testing.T) {
	runCase(t, "values_builtin")
}

func TestTranspile_StrBuiltin(t *testing.T) {
	runCase(t, "str_builtin")
}

func TestTranspile_StringInOperator(t *testing.T) {
	runCase(t, "string_in_operator")
}

func TestTranspile_MapLiteralDynamic(t *testing.T) {
	runCase(t, "map_literal_dynamic")
}

func TestTranspile_MapIntKey(t *testing.T) {
	runCase(t, "map_int_key")
}

func TestTranspile_MapIndex(t *testing.T) {
	runCase(t, "map_index")
}

func TestTranspile_LenMap(t *testing.T) {
	runCase(t, "len_map")
}

func TestTranspile_Slice(t *testing.T) {
	runCase(t, "slice")
}

func TestTranspile_StringPrefixSlice(t *testing.T) {
	runCase(t, "string_prefix_slice")
}

func TestTranspile_SubstringBuiltin(t *testing.T) {
	runCase(t, "substring_builtin")
}

func TestTranspile_WhileLoop(t *testing.T) {
	runCase(t, "while_loop")
}

func TestTranspile_ForLoop(t *testing.T) {
	runCase(t, "for_loop")
}

func TestTranspile_ForListCollection(t *testing.T) {
	runCase(t, "for_list_collection")
}

func TestTranspile_BreakContinue(t *testing.T) {
	runCase(t, "break_continue")
}

func TestTranspile_IfElse(t *testing.T) {
	runCase(t, "if_else")
}

func TestTranspile_ListAssign(t *testing.T) {
	runCase(t, "list_assign")
}

func TestTranspile_MapAssign(t *testing.T) {
	runCase(t, "map_assign")
}

func TestTranspile_MapInOperator(t *testing.T) {
	runCase(t, "map_in_operator")
}

func TestTranspile_TypedLet(t *testing.T) {
	runCase(t, "typed_let")
}

func TestTranspile_TypedVar(t *testing.T) {
	runCase(t, "typed_var")
}

func TestTranspile_ListNestedAssign(t *testing.T) {
	runCase(t, "list_nested_assign")
}

func TestTranspile_ListSetOps(t *testing.T) {
	runCase(t, "list_set_ops")
}

func TestTranspile_MapMembership(t *testing.T) {
	runCase(t, "map_membership")
}

func TestTranspile_MapNestedAssign(t *testing.T) {
	runCase(t, "map_nested_assign")
}

func TestTranspile_ForMapCollection(t *testing.T) {
	runCase(t, "for_map_collection")
}

func TestTranspile_Membership(t *testing.T) {
	runCase(t, "membership")
}

func TestTranspile_MinMaxBuiltin(t *testing.T) {
	runCase(t, "min_max_builtin")
}

func TestTranspile_TailRecursion(t *testing.T) {
	runCase(t, "tail_recursion")
}

func TestTranspile_TwoSum(t *testing.T) {
	runCase(t, "two-sum")
}

func TestTranspile_ShortCircuit(t *testing.T) {
	runCase(t, "short_circuit")
}

func TestTranspile_CastStringToInt(t *testing.T) {
	runCase(t, "cast_string_to_int")
}

func TestTranspile_CastStruct(t *testing.T) {
	runCase(t, "cast_struct")
}

func TestTranspile_NestedFunction(t *testing.T) {
	runCase(t, "nested_function")
}

func TestTranspile_RecordAssign(t *testing.T) {
	runCase(t, "record_assign")
}

func TestTranspile_UserTypeLiteral(t *testing.T) {
	runCase(t, "user_type_literal")
}

func TestTranspile_TestBlock(t *testing.T) {
	runCase(t, "test_block")
}

func TestTranspile_MatchExpr(t *testing.T) {
	runCase(t, "match_expr")
}

func TestTranspile_MatchFull(t *testing.T) {
	runCase(t, "match_full")
}

func TestTranspile_PythonAuto(t *testing.T) {
	runCase(t, "python_auto")
}

func TestTranspile_PythonMath(t *testing.T) {
	runCase(t, "python_math")
}

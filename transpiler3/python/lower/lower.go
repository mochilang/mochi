package lower

import (
	"fmt"
	"path/filepath"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/python/pysrc"
)

// ModuleName derives the Python module name from a Mochi source path.
// "hello.mochi" → "hello"; "my_program.mochi" → "my_program".
func ModuleName(src string) string {
	src = filepath.Base(src)
	src = strings.TrimSuffix(src, ".mochi")
	src = strings.ReplaceAll(src, "-", "_")
	if src == "" {
		return "main"
	}
	return src
}

// PackageName derives the Python distribution package name from a Mochi source path.
// The Phase 1 default is `mochi_user_<module>` which matches the
// `src/<pkg>/` layout used by hatchling.
func PackageName(src string) string {
	return "mochi_user_" + ModuleName(src)
}

// Lower translates an aotir.Program into a pysrc.Module covering the
// Phase 1 surface. ColourMap is accepted as a future-proof parameter
// (Phase 9 will mark agents and message handlers async) but is unused
// in Phase 1: every function is sync.
func Lower(prog *aotir.Program) (*pysrc.Module, error) {
	mod := &pysrc.Module{
		FutureAnnotations: true,
		Imports: []pysrc.ImportStmt{
			{From: "mochi_runtime.io", Names: []string{"Print"}},
		},
	}

	mainFn := prog.Functions[prog.Main]
	bodyStmts, err := lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainDef := &pysrc.FunctionDef{
		Name:       "main",
		ReturnType: pysrc.TypeNone,
		Body:       bodyStmts,
	}
	mod.Stmts = append(mod.Stmts, mainDef)

	guard := &pysrc.IfStmt{
		Cond: &pysrc.BinaryEq{Left: &pysrc.Name{Id: "__name__"}, Right: &pysrc.StrLit{Value: "__main__"}},
		Then: []pysrc.Stmt{
			&pysrc.ExprStmt{X: &pysrc.Call{Func: &pysrc.Name{Id: "main"}}},
		},
	}
	mod.Stmts = append(mod.Stmts, guard)

	return mod, nil
}

func lowerBlock(blk *aotir.Block) ([]pysrc.Stmt, error) {
	out := make([]pysrc.Stmt, 0, len(blk.Statements))
	for _, s := range blk.Statements {
		ps, err := lowerStmt(s)
		if err != nil {
			return nil, err
		}
		out = append(out, ps)
	}
	if len(out) == 0 {
		out = append(out, &pysrc.PassStmt{})
	}
	return out, nil
}

func lowerStmt(s aotir.Stmt) (pysrc.Stmt, error) {
	switch v := s.(type) {
	case *aotir.CallStmt:
		return lowerCallStmt(v)
	case *aotir.LetStmt:
		return lowerLetStmt(v)
	default:
		return nil, fmt.Errorf("python/lower: unsupported statement %T", s)
	}
}

// lowerCallStmt covers the print family (mochi_print_*).
// Every Mochi print lowers to Print.line(arg); the runtime dispatches on
// isinstance to produce vm3-byte-equal output (true/false for bools,
// repr for floats, str for ints/strings).
func lowerCallStmt(s *aotir.CallStmt) (pysrc.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("python/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		call := &pysrc.Call{
			Func: &pysrc.Attribute{Value: &pysrc.Name{Id: "Print"}, Attr: "line"},
			Args: []pysrc.Expr{arg},
		}
		return &pysrc.ExprStmt{X: call}, nil
	default:
		return nil, fmt.Errorf("python/lower: unsupported builtin %q", s.Func)
	}
}

func lowerLetStmt(s *aotir.LetStmt) (pysrc.Stmt, error) {
	val, err := lowerExpr(s.Init)
	if err != nil {
		return nil, err
	}
	return &pysrc.AssignStmt{
		Target: s.Name,
		Type:   pyTypeFor(s.VarType),
		Value:  val,
	}, nil
}

func lowerExpr(e aotir.Expr) (pysrc.Expr, error) {
	switch v := e.(type) {
	case *aotir.StringLit:
		return &pysrc.StrLit{Value: v.Value}, nil
	case *aotir.IntLit:
		return &pysrc.IntLit{Value: v.Value}, nil
	case *aotir.FloatLit:
		return &pysrc.FloatLit{Value: v.Value}, nil
	case *aotir.BoolLit:
		return &pysrc.BoolLit{Value: v.Value}, nil
	case *aotir.VarRef:
		return &pysrc.Name{Id: v.Name}, nil
	default:
		return nil, fmt.Errorf("python/lower: unsupported expression %T", e)
	}
}

// pyTypeFor maps an aotir.Type to the Python annotation used in Phase 1.
// Phase 2 widens this to cover floats and the full scalar lattice;
// Phase 3+ widens it to parameterised list/dict/set/tuple annotations.
func pyTypeFor(t aotir.Type) pysrc.TypeRef {
	switch t {
	case aotir.TypeString:
		return pysrc.TypeStr
	case aotir.TypeInt:
		return pysrc.TypeInt
	case aotir.TypeFloat:
		return pysrc.TypeFloat
	case aotir.TypeBool:
		return pysrc.TypeBool
	default:
		return pysrc.TypeRef{}
	}
}

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
// The Phase 1 default is `mochi_user_<module>` matching the `src/<pkg>/` layout.
func PackageName(src string) string {
	return "mochi_user_" + ModuleName(src)
}

// lowerer carries per-program state.
type lowerer struct {
	needsMath bool // true once any float div emitted; pulls in mochi_runtime.math
	needsFmt  bool // true once print(float) emitted; pulls in mochi_runtime.fmt
}

// Lower translates an aotir.Program into a pysrc.Module covering the
// Phase 2 surface: scalar arithmetic, comparison, bool short-circuit,
// string concat/index/contains/len, if/while/for-range, break/continue,
// user-defined functions (including recursion), int cast.
func Lower(prog *aotir.Program) (*pysrc.Module, error) {
	mod := &pysrc.Module{FutureAnnotations: true}
	l := &lowerer{}

	mainFn := prog.Functions[prog.Main]
	mainBody, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	var defs []pysrc.Stmt
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		def, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		defs = append(defs, def)
	}

	mainDef := &pysrc.FunctionDef{
		Name:       "main",
		ReturnType: pysrc.TypeNone,
		Body:       mainBody,
	}
	mod.Stmts = append(mod.Stmts, defs...)
	mod.Stmts = append(mod.Stmts, mainDef)

	mod.Stmts = append(mod.Stmts, &pysrc.IfStmt{
		Cond: &pysrc.BinaryEq{Left: &pysrc.Name{Id: "__name__"}, Right: &pysrc.StrLit{Value: "__main__"}},
		Then: []pysrc.Stmt{
			&pysrc.ExprStmt{X: &pysrc.Call{Func: &pysrc.Name{Id: "main"}}},
		},
	})

	mod.Imports = []pysrc.ImportStmt{
		{From: "mochi_runtime.io", Names: []string{"Print"}},
	}
	if l.needsMath {
		mod.Imports = append(mod.Imports, pysrc.ImportStmt{From: "mochi_runtime.math", Names: []string{"fdiv"}})
	}
	return mod, nil
}

func (l *lowerer) lowerFunction(fn *aotir.Function) (pysrc.Stmt, error) {
	params := make([]pysrc.Param, 0, len(fn.Params))
	for _, p := range fn.Params {
		params = append(params, pysrc.Param{Name: p.Name, Type: pyTypeFor(p.Type)})
	}
	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, fmt.Errorf("function %s: %w", fn.Name, err)
	}
	ret := pyTypeFor(fn.ReturnType)
	if fn.ReturnType == aotir.TypeUnit {
		ret = pysrc.TypeNone
	}
	return &pysrc.FunctionDef{
		Name:       fn.Name,
		Params:     params,
		ReturnType: ret,
		Body:       body,
	}, nil
}

func (l *lowerer) lowerBlock(blk *aotir.Block) ([]pysrc.Stmt, error) {
	out := make([]pysrc.Stmt, 0, len(blk.Statements))
	for _, s := range blk.Statements {
		ps, err := l.lowerStmt(s)
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

func (l *lowerer) lowerStmt(s aotir.Stmt) (pysrc.Stmt, error) {
	switch v := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(v)
	case *aotir.LetStmt:
		return l.lowerLetStmt(v)
	case *aotir.AssignStmt:
		val, err := l.lowerExpr(v.Value)
		if err != nil {
			return nil, err
		}
		return &pysrc.ReassignStmt{Target: v.Name, Value: val}, nil
	case *aotir.IfStmt:
		return l.lowerIfStmt(v)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(v)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(v)
	case *aotir.BreakStmt:
		return &pysrc.BreakStmt{}, nil
	case *aotir.ContinueStmt:
		return &pysrc.ContinueStmt{}, nil
	case *aotir.ReturnStmt:
		if v.Value == nil {
			return &pysrc.ReturnStmt{}, nil
		}
		val, err := l.lowerExpr(v.Value)
		if err != nil {
			return nil, err
		}
		return &pysrc.ReturnStmt{Value: val}, nil
	default:
		return nil, fmt.Errorf("python/lower: unsupported statement %T", s)
	}
}

func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) (pysrc.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("python/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		return &pysrc.ExprStmt{X: &pysrc.Call{
			Func: &pysrc.Attribute{Value: &pysrc.Name{Id: "Print"}, Attr: "line"},
			Args: []pysrc.Expr{arg},
		}}, nil
	case "mochi_print_f64":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("python/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		l.needsFmt = true
		return &pysrc.ExprStmt{X: &pysrc.Call{
			Func: &pysrc.Attribute{Value: &pysrc.Name{Id: "Print"}, Attr: "line"},
			Args: []pysrc.Expr{arg},
		}}, nil
	default:
		if strings.HasPrefix(s.Func, "mochi_") {
			return nil, fmt.Errorf("python/lower: unsupported builtin %q", s.Func)
		}
		// User function call as a statement.
		args, err := l.lowerExprs(s.Args)
		if err != nil {
			return nil, err
		}
		return &pysrc.ExprStmt{X: &pysrc.Call{Func: &pysrc.Name{Id: s.Func}, Args: args}}, nil
	}
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (pysrc.Stmt, error) {
	val, err := l.lowerExpr(s.Init)
	if err != nil {
		return nil, err
	}
	return &pysrc.AssignStmt{
		Target: s.Name,
		Type:   pyTypeFor(s.VarType),
		Value:  val,
	}, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (pysrc.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	then, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	out := &pysrc.IfStmt{Cond: cond, Then: then}
	if s.Else != nil {
		elseStmts, err := l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
		out.Else = elseStmts
	}
	return out, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (pysrc.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &pysrc.WhileStmt{Cond: cond, Body: body}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (pysrc.Stmt, error) {
	start, err := l.lowerExpr(s.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(s.End)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &pysrc.ForRangeStmt{Var: s.Var, Start: start, End: end, Body: body}, nil
}

func (l *lowerer) lowerExprs(exprs []aotir.Expr) ([]pysrc.Expr, error) {
	out := make([]pysrc.Expr, 0, len(exprs))
	for _, e := range exprs {
		pe, err := l.lowerExpr(e)
		if err != nil {
			return nil, err
		}
		out = append(out, pe)
	}
	return out, nil
}

func (l *lowerer) lowerExpr(e aotir.Expr) (pysrc.Expr, error) {
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
	case *aotir.BinaryExpr:
		return l.lowerBinaryExpr(v)
	case *aotir.UnaryExpr:
		return l.lowerUnaryExpr(v)
	case *aotir.CallExpr:
		args, err := l.lowerExprs(v.Args)
		if err != nil {
			return nil, err
		}
		return &pysrc.Call{Func: &pysrc.Name{Id: v.Func}, Args: args}, nil
	case *aotir.StrLenExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &pysrc.Call{Func: &pysrc.Name{Id: "len"}, Args: []pysrc.Expr{recv}}, nil
	case *aotir.StrIndexExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := l.lowerExpr(v.Index)
		if err != nil {
			return nil, err
		}
		return &pysrc.IndexExpr{Receiver: recv, Index: idx}, nil
	case *aotir.StrContainsExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		sub, err := l.lowerExpr(v.Sub)
		if err != nil {
			return nil, err
		}
		return &pysrc.BinaryExpr{Left: sub, Op: "in", Right: recv}, nil
	case *aotir.NumCastExpr:
		operand, err := l.lowerExpr(v.Operand)
		if err != nil {
			return nil, err
		}
		// vm3 semantics: truncate toward zero. Python int(float) truncates toward zero
		// for both positive and negative values (per PEP 237 / 3141), matching vm3.
		return &pysrc.Call{Func: &pysrc.Name{Id: "int"}, Args: []pysrc.Expr{operand}}, nil
	default:
		return nil, fmt.Errorf("python/lower: unsupported expression %T", e)
	}
}

func (l *lowerer) lowerBinaryExpr(e *aotir.BinaryExpr) (pysrc.Expr, error) {
	left, err := l.lowerExpr(e.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(e.Right)
	if err != nil {
		return nil, err
	}
	// Float division must produce IEEE 754 Inf / NaN on divide-by-zero,
	// not raise ZeroDivisionError. Route through mochi_runtime.math.fdiv.
	if e.Op == aotir.BinDivF64 {
		l.needsMath = true
		return &pysrc.Call{Func: &pysrc.Name{Id: "fdiv"}, Args: []pysrc.Expr{left, right}}, nil
	}
	op, ok := binOpToPython[e.Op]
	if !ok {
		return nil, fmt.Errorf("python/lower: unsupported binop %v", e.Op)
	}
	return &pysrc.BinaryExpr{Left: left, Op: op, Right: right}, nil
}

func (l *lowerer) lowerUnaryExpr(e *aotir.UnaryExpr) (pysrc.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &pysrc.UnaryExpr{Op: "-", Operand: operand}, nil
	case aotir.UnNotBool:
		return &pysrc.UnaryExpr{Op: "not", Operand: operand}, nil
	default:
		return nil, fmt.Errorf("python/lower: unsupported unop %v", e.Op)
	}
}

// binOpToPython maps an aotir.BinOp to a Python operator token. The
// integer division case is special-cased because Python's `/` is true
// (float) division while Mochi's int / int produces an int floor.
var binOpToPython = map[aotir.BinOp]string{
	aotir.BinAddI64:  "+",
	aotir.BinAddF64:  "+",
	aotir.BinStrCat:  "+",
	aotir.BinSubI64:  "-",
	aotir.BinSubF64:  "-",
	aotir.BinMulI64:  "*",
	aotir.BinMulF64:  "*",
	aotir.BinDivI64:  "//",
	aotir.BinModI64:  "%",
	aotir.BinEqI64:   "==",
	aotir.BinEqF64:   "==",
	aotir.BinEqBool:  "==",
	aotir.BinEqStr:   "==",
	aotir.BinNeI64:   "!=",
	aotir.BinNeF64:   "!=",
	aotir.BinNeBool:  "!=",
	aotir.BinNeStr:   "!=",
	aotir.BinLtI64:   "<",
	aotir.BinLtF64:   "<",
	aotir.BinLeI64:   "<=",
	aotir.BinLeF64:   "<=",
	aotir.BinGtI64:   ">",
	aotir.BinGtF64:   ">",
	aotir.BinGeI64:   ">=",
	aotir.BinGeF64:   ">=",
	aotir.BinAndBool: "and",
	aotir.BinOrBool:  "or",
}

// pyTypeFor maps an aotir.Type to a Python annotation.
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

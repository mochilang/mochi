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
	needsMath       bool // true once any float div emitted; pulls in mochi_runtime.math
	needsFmt        bool // true once print(float) emitted; pulls in mochi_runtime.fmt
	needsMapping    bool // true once a sorted map keys/values call is emitted
	needsDataclass  bool // true once any record class is emitted (Phase 3.4)
	needsCallable   bool // true once any Callable[...] annotation is emitted (Phase 6.0)
}

// Lower translates an aotir.Program into a pysrc.Module covering the
// Phase 2 surface (scalars, control flow, user functions) plus the
// Phase 3.1 list surface: literal, index, len, for-each, append,
// slice, filter/map via the lifted-closure plumbing the c lower built.
func Lower(prog *aotir.Program) (*pysrc.Module, error) {
	mod := &pysrc.Module{FutureAnnotations: true}
	l := &lowerer{}

	mainFn := prog.Functions[prog.Main]
	mainBody, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	var defs []pysrc.Stmt
	for _, rec := range prog.Records {
		l.needsDataclass = true
		defs = append(defs, lowerRecordDecl(rec))
	}
	for _, u := range prog.Unions {
		l.needsDataclass = true
		defs = append(defs, lowerUnionDecl(u)...)
	}
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		// Capturing lifted closures reference their environment via
		// C-specific emit names baked into VarRef.Name (e.g. `__e->factor`);
		// the aotir IR layer leaks that into the lifted function body. The
		// Python lowerer cannot rewrite these without an upstream pass that
		// surfaces captures structurally (e.g. a `CaptureRef` node). Until
		// the c lower stops baking C-specific emit names into VarRef.Name,
		// any capturing closure is unrepresentable in Python.
		if len(fn.Captures) > 0 {
			return nil, fmt.Errorf("python/lower: capturing closure %q not supported until upstream surfaces captures as a structural IR node (currently baked as C-specific `__e->X` in VarRef.Name)", fn.Name)
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
	if l.needsMapping {
		mod.Imports = append(mod.Imports, pysrc.ImportStmt{From: "mochi_runtime.mapping", Names: []string{"keys_sorted", "values_sorted"}})
	}
	if l.needsDataclass {
		mod.Imports = append(mod.Imports, pysrc.ImportStmt{From: "dataclasses", Names: []string{"dataclass"}})
	}
	if l.needsCallable {
		mod.Imports = append(mod.Imports, pysrc.ImportStmt{From: "collections.abc", Names: []string{"Callable"}})
	}
	return mod, nil
}

// lowerUnionDecl emits a Mochi sum type as N frozen+slotted dataclass
// variants followed by a PEP 695 `type` alias `Name = V1 | V2 | ...`.
// Variants are emitted in declaration order so the emitted source
// matches Mochi source order; PEP 695 type-alias evaluation is lazy,
// so the order does not affect runtime semantics. Nullary variants
// render as a `pass` body.
func lowerUnionDecl(u *aotir.UnionDecl) []pysrc.Stmt {
	out := make([]pysrc.Stmt, 0, len(u.Variants)+1)
	names := make([]string, 0, len(u.Variants))
	for _, v := range u.Variants {
		fields := make([]pysrc.ClassField, 0, len(v.Fields))
		for _, f := range v.Fields {
			fields = append(fields, pysrc.ClassField{
				Name: f.Name,
				Type: pyTypeForUnion(f.FieldType, aotir.TypeInvalid, f.RecordName, "", f.UnionName, "", aotir.TypeInvalid, aotir.TypeInvalid),
			})
		}
		out = append(out, &pysrc.ClassDef{
			Name:       v.Name,
			Decorators: []string{"dataclass(frozen=True, slots=True)"},
			Fields:     fields,
		})
		names = append(names, v.Name)
	}
	out = append(out, &pysrc.UnionDef{Name: u.Name, Variants: names})
	return out
}

// lowerRecordDecl emits a Mochi record as a frozen + slotted dataclass.
// Frozen gives structural immutability (Phase 4 `with` will use
// dataclasses.replace), slots gives memory locality and protects against
// typo-attribute writes. Fields render with PEP 526 annotations under
// the module's `from __future__ import annotations`.
func lowerRecordDecl(rec *aotir.RecordDecl) pysrc.Stmt {
	fields := make([]pysrc.ClassField, 0, len(rec.Fields))
	for _, f := range rec.Fields {
		fields = append(fields, pysrc.ClassField{
			Name: f.Name,
			Type: pyTypeForRecord(f.Type, aotir.TypeInvalid, f.RecordName, "", aotir.TypeInvalid, aotir.TypeInvalid),
		})
	}
	return &pysrc.ClassDef{
		Name:       rec.Name,
		Decorators: []string{"dataclass(frozen=True, slots=True)"},
		Fields:     fields,
	}
}

func (l *lowerer) lowerFunction(fn *aotir.Function) (pysrc.Stmt, error) {
	params := make([]pysrc.Param, 0, len(fn.Params))
	for _, p := range fn.Params {
		annot := pyTypeForUnion(p.Type, p.ElemType, p.RecordName, p.ElemRecordName, p.UnionName, "", p.KeyType, p.ValueType)
		if p.Type == aotir.TypeFun {
			annot = l.pyTypeForFun(p.FunSig)
		}
		params = append(params, pysrc.Param{Name: p.Name, Type: annot})
	}
	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, fmt.Errorf("function %s: %w", fn.Name, err)
	}
	ret := pyTypeForUnion(fn.ReturnType, fn.ReturnElemType, fn.ReturnRecordName, fn.ReturnElemRecordName, fn.ReturnUnionName, "", fn.ReturnKeyType, fn.ReturnValueType)
	if fn.ReturnType == aotir.TypeFun {
		ret = l.pyTypeForFun(fn.ReturnFunSig)
	}
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
	case *aotir.MatchStmt:
		return l.lowerMatchStmt(v)
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
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(v)
	case *aotir.MapPutStmt:
		key, err := l.lowerExpr(v.Key)
		if err != nil {
			return nil, err
		}
		val, err := l.lowerExpr(v.Value)
		if err != nil {
			return nil, err
		}
		return &pysrc.IndexAssignStmt{
			Target: &pysrc.Name{Id: v.Name},
			Key:    key,
			Value:  val,
		}, nil
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
	annot := pyTypeForUnion(s.VarType, s.ElemType, s.RecordName, s.ElemRecordName, s.UnionName, "", s.KeyType, s.ValueType)
	// Init==nil happens when the c lower introduces a mutable result var
	// for a match expression: the LetStmt declares the binding and the
	// following MatchStmt assigns into it from every arm. Emit a PEP 526
	// annotation-only statement; the match arms bind the name before
	// any read.
	if s.Init == nil {
		return &pysrc.AnnotateStmt{Target: s.Name, Type: annot}, nil
	}
	val, err := l.lowerExpr(s.Init)
	if err != nil {
		return nil, err
	}
	return &pysrc.AssignStmt{
		Target: s.Name,
		Type:   annot,
		Value:  val,
	}, nil
}

// lowerMatchStmt emits a PEP 634 match. For statement-position matches
// (ResultVar==""), each arm body lowers straight through. For expression-
// position matches (ResultVar non-empty), the c lower has already rewritten
// every arm body to end with `ResultVar = <expr>`, and a LetStmt that
// declares ResultVar (Init==nil) sits immediately before us in the parent
// block. Variant patterns use keyword form `Variant(field=binding)` so
// dataclass field reordering cannot silently rebind positional patterns.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) (pysrc.Stmt, error) {
	target, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, err
	}
	cases := make([]pysrc.MatchCase, 0, len(s.Arms)+1)
	for _, arm := range s.Arms {
		body, err := l.lowerBlock(arm.Body)
		if err != nil {
			return nil, err
		}
		bindings := make([]pysrc.FieldBinding, 0, len(arm.Bindings))
		for _, b := range arm.Bindings {
			bindings = append(bindings, pysrc.FieldBinding{FieldName: b.FieldName, BindName: b.VarName})
		}
		var guard pysrc.Expr
		if arm.Guard != nil {
			guard, err = l.lowerExpr(arm.Guard)
			if err != nil {
				return nil, err
			}
		}
		cases = append(cases, pysrc.MatchCase{
			Variant:  arm.VariantName,
			Bindings: bindings,
			Guard:    guard,
			Body:     body,
		})
	}
	if s.Default != nil {
		body, err := l.lowerBlock(s.Default.Body)
		if err != nil {
			return nil, err
		}
		var guard pysrc.Expr
		if s.Default.Guard != nil {
			guard, err = l.lowerExpr(s.Default.Guard)
			if err != nil {
				return nil, err
			}
		}
		cases = append(cases, pysrc.MatchCase{Wildcard: true, Guard: guard, Body: body})
	}
	return &pysrc.MatchStmt{Target: target, Cases: cases}, nil
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

func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) (pysrc.Stmt, error) {
	iter, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &pysrc.ForEachStmt{Var: s.Var, Iter: iter, Body: body}, nil
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
	case *aotir.FunCallExpr:
		callee, err := l.lowerExpr(v.Callee)
		if err != nil {
			return nil, err
		}
		args, err := l.lowerExprs(v.Args)
		if err != nil {
			return nil, err
		}
		return &pysrc.Call{Func: callee, Args: args}, nil
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
	case *aotir.ListLit:
		elems, err := l.lowerExprs(v.Elems)
		if err != nil {
			return nil, err
		}
		return &pysrc.ListLit{Elems: elems}, nil
	case *aotir.IndexExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := l.lowerExpr(v.Index)
		if err != nil {
			return nil, err
		}
		return &pysrc.IndexExpr{Receiver: recv, Index: idx}, nil
	case *aotir.LenExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &pysrc.Call{Func: &pysrc.Name{Id: "len"}, Args: []pysrc.Expr{recv}}, nil
	case *aotir.AppendExpr:
		// Mochi append returns a new list (functional semantics). Python
		// `xs + [v]` produces a fresh list and leaves the input untouched,
		// matching vm3 byte-for-byte (no aliasing into the original).
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		val, err := l.lowerExpr(v.Value)
		if err != nil {
			return nil, err
		}
		return &pysrc.BinaryExpr{
			Left:  recv,
			Op:    "+",
			Right: &pysrc.ListLit{Elems: []pysrc.Expr{val}},
		}, nil
	case *aotir.ListSliceExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		start, err := l.lowerExpr(v.Start)
		if err != nil {
			return nil, err
		}
		end, err := l.lowerExpr(v.End)
		if err != nil {
			return nil, err
		}
		return &pysrc.SliceExpr{Receiver: recv, Start: start, End: end}, nil
	case *aotir.ListFilterExpr:
		list, err := l.lowerExpr(v.List)
		if err != nil {
			return nil, err
		}
		fn, err := l.lowerExpr(v.Fn)
		if err != nil {
			return nil, err
		}
		// list(filter(fn, xs)) — Python filter preserves order, matching vm3.
		return &pysrc.Call{
			Func: &pysrc.Name{Id: "list"},
			Args: []pysrc.Expr{&pysrc.Call{
				Func: &pysrc.Name{Id: "filter"},
				Args: []pysrc.Expr{fn, list},
			}},
		}, nil
	case *aotir.ListMapExpr:
		list, err := l.lowerExpr(v.List)
		if err != nil {
			return nil, err
		}
		fn, err := l.lowerExpr(v.Fn)
		if err != nil {
			return nil, err
		}
		return &pysrc.Call{
			Func: &pysrc.Name{Id: "list"},
			Args: []pysrc.Expr{&pysrc.Call{
				Func: &pysrc.Name{Id: "map"},
				Args: []pysrc.Expr{fn, list},
			}},
		}, nil
	case *aotir.MapLit:
		keys, err := l.lowerExprs(v.Keys)
		if err != nil {
			return nil, err
		}
		vals, err := l.lowerExprs(v.Values)
		if err != nil {
			return nil, err
		}
		return &pysrc.DictLit{Keys: keys, Values: vals}, nil
	case *aotir.MapGetExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		key, err := l.lowerExpr(v.Key)
		if err != nil {
			return nil, err
		}
		return &pysrc.IndexExpr{Receiver: recv, Index: key}, nil
	case *aotir.MapHasExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		key, err := l.lowerExpr(v.Key)
		if err != nil {
			return nil, err
		}
		// `k in m` is the idiomatic, O(1)-average Python form.
		return &pysrc.BinaryExpr{Left: key, Op: "in", Right: recv}, nil
	case *aotir.MapLenExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &pysrc.Call{Func: &pysrc.Name{Id: "len"}, Args: []pysrc.Expr{recv}}, nil
	case *aotir.MapKeysExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		l.needsMapping = true
		return &pysrc.Call{Func: &pysrc.Name{Id: "keys_sorted"}, Args: []pysrc.Expr{recv}}, nil
	case *aotir.MapValuesExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		l.needsMapping = true
		return &pysrc.Call{Func: &pysrc.Name{Id: "values_sorted"}, Args: []pysrc.Expr{recv}}, nil
	case *aotir.SetLiteralExpr:
		elems, err := l.lowerExprs(v.Elems)
		if err != nil {
			return nil, err
		}
		if len(elems) == 0 {
			// `{}` is a dict literal in Python; empty set must use `set()`.
			return &pysrc.Call{Func: &pysrc.Name{Id: "set"}}, nil
		}
		return &pysrc.SetLit{Elems: elems}, nil
	case *aotir.SetAddExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		elem, err := l.lowerExpr(v.Elem)
		if err != nil {
			return nil, err
		}
		// Functional add: union with singleton. `s | {x}` allocates a
		// fresh set, matching the aotir spec where SetAddExpr returns
		// TypeSet rather than mutating the receiver.
		return &pysrc.BinaryExpr{
			Left:  recv,
			Op:    "|",
			Right: &pysrc.SetLit{Elems: []pysrc.Expr{elem}},
		}, nil
	case *aotir.SetHasExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		elem, err := l.lowerExpr(v.Elem)
		if err != nil {
			return nil, err
		}
		return &pysrc.BinaryExpr{Left: elem, Op: "in", Right: recv}, nil
	case *aotir.SetLenExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &pysrc.Call{Func: &pysrc.Name{Id: "len"}, Args: []pysrc.Expr{recv}}, nil
	case *aotir.SetToListExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		// vm3 returns the elements in sorted order for byte-equal stdout.
		// Python set iteration order is undefined; sorted(s) produces a list.
		return &pysrc.Call{Func: &pysrc.Name{Id: "sorted"}, Args: []pysrc.Expr{recv}}, nil
	case *aotir.FunLit:
		// Closure-converted FunLit. The c lower lifts the closure body
		// to a top-level aotir.Function emitted by lowerFunction above,
		// so the Python name resolves to a first-class callable.
		// Capturing closures (non-empty Captures) are deferred to Phase 6;
		// fail loudly so they cannot silently regress as `Name`.
		if len(v.Captures) > 0 {
			return nil, fmt.Errorf("python/lower: capturing closure %q not supported until phase 6", v.FuncName)
		}
		return &pysrc.Name{Id: v.FuncName}, nil
	case *aotir.RecordLit:
		return l.lowerRecordLit(v)
	case *aotir.FieldAccess:
		return l.lowerFieldAccess(v)
	case *aotir.VariantLit:
		return l.lowerVariantLit(v)
	case *aotir.UnionVarRef:
		return &pysrc.Name{Id: v.Name}, nil
	case *aotir.VariantFieldAccess:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &pysrc.Attribute{Value: recv, Attr: v.FieldName}, nil
	default:
		return nil, fmt.Errorf("python/lower: unsupported expression %T", e)
	}
}

// lowerVariantLit emits `VariantName(field=v, ...)`. For nullary variants
// the emit collapses to `VariantName()`, which PEP 634 `case Variant():`
// also matches without binding any fields.
func (l *lowerer) lowerVariantLit(e *aotir.VariantLit) (pysrc.Expr, error) {
	kwargs := make([]pysrc.KeywordArg, 0, len(e.Fields))
	for _, f := range e.Fields {
		val, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		kwargs = append(kwargs, pysrc.KeywordArg{Name: f.Name, Value: val})
	}
	return &pysrc.Call{
		Func:   &pysrc.Name{Id: e.VariantName},
		Kwargs: kwargs,
	}, nil
}

// lowerRecordLit emits `RecordName(field1=v1, field2=v2)`. The c lower
// orders RecordLit.Fields in declared field order; the emitter keeps
// keyword form for readability and to make positional drift impossible
// if the dataclass field order ever shifts.
func (l *lowerer) lowerRecordLit(e *aotir.RecordLit) (pysrc.Expr, error) {
	kwargs := make([]pysrc.KeywordArg, 0, len(e.Fields))
	for _, f := range e.Fields {
		val, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		kwargs = append(kwargs, pysrc.KeywordArg{Name: f.Name, Value: val})
	}
	return &pysrc.Call{
		Func:   &pysrc.Name{Id: e.TypeName},
		Kwargs: kwargs,
	}, nil
}

// lowerFieldAccess emits `recv.field`.
func (l *lowerer) lowerFieldAccess(e *aotir.FieldAccess) (pysrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &pysrc.Attribute{Value: recv, Attr: e.FieldName}, nil
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
	aotir.BinEqRec:   "==",
	aotir.BinNeI64:   "!=",
	aotir.BinNeF64:   "!=",
	aotir.BinNeBool:  "!=",
	aotir.BinNeStr:   "!=",
	aotir.BinNeRec:   "!=",
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

// pyTypeFor maps a scalar aotir.Type to its PEP 585 Python annotation.
// Returns an empty TypeRef (no annotation) for compound types; callers
// holding the compound's element identity should use pyTypeForFull.
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

// pyTypeForCompound resolves a (Type, ElemType, KeyType, ValueType)
// tuple into the matching Python annotation. For scalar types the
// compound fields are ignored. For TypeList it emits `list[<elem>]`,
// for TypeMap it emits `dict[<key>, <value>]` (both PEP 585 built-in
// subscripted generics, lazy under `from __future__ import annotations`).
func pyTypeForCompound(t, elem, k, v aotir.Type) pysrc.TypeRef {
	return pyTypeForRecord(t, elem, "", "", k, v)
}

// pyTypeForRecord is the Phase 3.4 callsite shape, preserved for record
// annotations that do not need union threading. Delegates to pyTypeForUnion.
func pyTypeForRecord(t, elem aotir.Type, recordName, elemRecordName string, k, v aotir.Type) pysrc.TypeRef {
	return pyTypeForUnion(t, elem, recordName, elemRecordName, "", "", k, v)
}

// pyTypeForFun maps an aotir.FunSig to `Callable[[P1, P2, ...], R]` from
// collections.abc (Phase 6.3). Empty param list becomes `Callable[[], R]`;
// a TypeUnit return becomes `Callable[[...], None]`. Sets needsCallable so
// the module emits the right import. Phase 6 limits FunSig to the scalar
// primitives that aotir enforces upstream, so any non-scalar slot is a
// verifier bug and surfaces as an empty TypeRef (no annotation), which lets
// the emitter still produce valid Python.
func (l *lowerer) pyTypeForFun(sig *aotir.FunSig) pysrc.TypeRef {
	if sig == nil {
		return pysrc.TypeRef{}
	}
	l.needsCallable = true
	params := make([]string, 0, len(sig.ParamTypes))
	for _, pt := range sig.ParamTypes {
		pr := pyTypeFor(pt)
		if pr.Name == "" {
			return pysrc.TypeRef{}
		}
		params = append(params, pr.Name)
	}
	var retName string
	if sig.ReturnType == aotir.TypeUnit {
		retName = "None"
	} else {
		rr := pyTypeFor(sig.ReturnType)
		if rr.Name == "" {
			return pysrc.TypeRef{}
		}
		retName = rr.Name
	}
	return pysrc.TypeRef{Name: "Callable[[" + strings.Join(params, ", ") + "], " + retName + "]"}
}

// pyTypeForUnion adds union-name slots so the lowerer can emit a bare
// `UnionName` annotation (Phase 5) alongside the record-name plumbing.
// unionName is the receiver's union name when t==TypeUnion; elemUnionName
// is the list element's union name when t==TypeList && elem==TypeUnion.
func pyTypeForUnion(t, elem aotir.Type, recordName, elemRecordName, unionName, elemUnionName string, k, v aotir.Type) pysrc.TypeRef {
	switch t {
	case aotir.TypeRecord:
		if recordName == "" {
			return pysrc.TypeRef{}
		}
		return pysrc.TypeRef{Name: recordName}
	case aotir.TypeUnion:
		if unionName == "" {
			return pysrc.TypeRef{}
		}
		return pysrc.TypeRef{Name: unionName}
	case aotir.TypeList:
		if elem == aotir.TypeRecord {
			if elemRecordName == "" {
				return pysrc.TypeRef{}
			}
			return pysrc.TypeRef{Name: "list[" + elemRecordName + "]"}
		}
		if elem == aotir.TypeUnion {
			if elemUnionName == "" {
				return pysrc.TypeRef{}
			}
			return pysrc.TypeRef{Name: "list[" + elemUnionName + "]"}
		}
		inner := pyTypeFor(elem)
		if inner.Name == "" {
			return pysrc.TypeRef{}
		}
		return pysrc.TypeRef{Name: "list[" + inner.Name + "]"}
	case aotir.TypeMap:
		kr := pyTypeFor(k)
		vr := pyTypeFor(v)
		if kr.Name == "" || vr.Name == "" {
			return pysrc.TypeRef{}
		}
		return pysrc.TypeRef{Name: "dict[" + kr.Name + ", " + vr.Name + "]"}
	case aotir.TypeSet:
		inner := pyTypeFor(elem)
		if inner.Name == "" {
			return pysrc.TypeRef{}
		}
		return pysrc.TypeRef{Name: "set[" + inner.Name + "]"}
	}
	return pyTypeFor(t)
}

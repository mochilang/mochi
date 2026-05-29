package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/go/gotree"
)

// lowerBlock walks an aotir.Block's statement list into a
// gotree.BlockStmt.
//
// ClosureEnvStmt is dropped at this layer: the Go FunLit emitter
// allocates the env struct inline via an IIFE so the env pointer
// is always materialised at the FunLit's expression site, regardless
// of whether the upstream C lowerer inserted a paired env allocation
// statement. Keeping a separate local would also force Go's
// "declared and not used" diagnostic to fire whenever the FunLit
// flowed through a path the env-allocation statement did not cover.
func (l *lowerer) lowerBlock(b *aotir.Block) (*gotree.BlockStmt, error) {
	out := &gotree.BlockStmt{}
	for _, s := range b.Statements {
		if _, ok := s.(*aotir.ClosureEnvStmt); ok {
			continue
		}
		if qs, ok := s.(*aotir.QueryScopeStmt); ok {
			inner, err := l.lowerBlock(qs.Body)
			if err != nil {
				return nil, err
			}
			out.List = append(out.List, inner.List...)
			continue
		}
		gs, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		out.List = append(out.List, gs)
	}
	return out, nil
}

func (l *lowerer) lowerStmt(s aotir.Stmt) (gotree.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(s)
	case *aotir.LetStmt:
		return l.lowerLetStmt(s)
	case *aotir.AssignStmt:
		return l.lowerAssignStmt(s)
	case *aotir.IfStmt:
		return l.lowerIfStmt(s)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(s)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(s)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(s)
	case *aotir.ListSetStmt:
		return l.lowerListSetStmt(s)
	case *aotir.MapPutStmt:
		return l.lowerMapPutStmt(s)
	case *aotir.MatchStmt:
		return l.lowerMatchStmt(s)
	case *aotir.BreakStmt:
		return &gotree.BranchStmt{Tok: "break"}, nil
	case *aotir.ContinueStmt:
		return &gotree.BranchStmt{Tok: "continue"}, nil
	case *aotir.ReturnStmt:
		return l.lowerReturnStmt(s)
	case *aotir.WriteFileStmt:
		return l.lowerWriteFileStmt(s)
	case *aotir.AppendFileStmt:
		return l.lowerAppendFileStmt(s)
	case *aotir.SaveCSVStmt:
		return l.lowerSaveCSVStmt(s)
	case *aotir.TryCatchStmt:
		return l.lowerTryCatchStmt(s)
	case *aotir.PanicStmt:
		return l.lowerPanicStmt(s)
	case *aotir.ChanSendStmt:
		return l.lowerChanSendStmt(s)
	case *aotir.OMapPutStmt:
		return l.lowerOMapPutStmt(s)
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: does not handle stmt %T", s)
	}
}

// lowerWriteFileStmt emits `os.WriteFile(path, []byte(content), 0644)` and
// discards the returned error to match the Mochi semantics that `writeFile`
// is a fire-and-forget statement (the C runtime also ignores errors here).
func (l *lowerer) lowerWriteFileStmt(s *aotir.WriteFileStmt) (gotree.Stmt, error) {
	path, err := l.lowerExpr(s.Path)
	if err != nil {
		return nil, fmt.Errorf("writeFile path: %w", err)
	}
	content, err := l.lowerExpr(s.Content)
	if err != nil {
		return nil, fmt.Errorf("writeFile content: %w", err)
	}
	l.addImport("os")
	bytesCall := &gotree.CallExpr{
		Fun:  &gotree.ArrayType{Elt: &gotree.Ident{Name: "byte"}},
		Args: []gotree.Expr{content},
	}
	call := &gotree.CallExpr{
		Fun: &gotree.SelectorExpr{X: &gotree.Ident{Name: "os"}, Sel: "WriteFile"},
		Args: []gotree.Expr{
			path,
			bytesCall,
			&gotree.BasicLit{Kind: gotree.IntLit, Value: "0644"},
		},
	}
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: "_"}},
		Tok: "=",
		Rhs: []gotree.Expr{call},
	}, nil
}

// lowerAppendFileStmt emits a tiny helper call mochiAppendFile(path, content)
// because Go's stdlib has no one-liner for "open in append mode, write, close,
// ignore errors". The helper is registered via addHelper.
func (l *lowerer) lowerAppendFileStmt(s *aotir.AppendFileStmt) (gotree.Stmt, error) {
	path, err := l.lowerExpr(s.Path)
	if err != nil {
		return nil, fmt.Errorf("appendFile path: %w", err)
	}
	content, err := l.lowerExpr(s.Content)
	if err != nil {
		return nil, fmt.Errorf("appendFile content: %w", err)
	}
	l.addImport("os")
	l.addHelper("mochiAppendFile")
	return &gotree.ExprStmt{X: &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiAppendFile"},
		Args: []gotree.Expr{path, content},
	}}, nil
}

// lowerSaveCSVStmt emits mochiSaveCSV(path, data) which writes the
// list<list<string>> using encoding/csv (RFC 4180 quoting). The helper
// ignores errors to match the C runtime's fire-and-forget semantics.
func (l *lowerer) lowerSaveCSVStmt(s *aotir.SaveCSVStmt) (gotree.Stmt, error) {
	path, err := l.lowerExpr(s.Path)
	if err != nil {
		return nil, fmt.Errorf("saveCSV path: %w", err)
	}
	data, err := l.lowerExpr(s.Data)
	if err != nil {
		return nil, fmt.Errorf("saveCSV data: %w", err)
	}
	l.addImport("os")
	l.addImport("encoding/csv")
	l.addHelper("mochiSaveCSV")
	return &gotree.ExprStmt{X: &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiSaveCSV"},
		Args: []gotree.Expr{path, data},
	}}, nil
}

// lowerCallStmt handles the print runtime shims emitted by the
// shared aotir lowerer (mochi_print_str, mochi_print_i64,
// mochi_print_f64, mochi_print_bool). Each lowers to a single
// fmt.Println(arg) call; the runtime shim names go away.
func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) (gotree.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("transpiler3/go/lower: %s takes one arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		l.addImport("fmt")
		return &gotree.ExprStmt{X: &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "fmt"}, Sel: "Println"},
			Args: []gotree.Expr{arg},
		}}, nil
	default:
		// User-defined function call as a statement. Func is the
		// already-mangled mochi__<source> name; args lower as usual.
		// Phase 10.2: Go FFI calls arrive with a `mochi_go_` prefix
		// from the shared aotir lowerer (used by the C target for
		// its subprocess-RPC trampoline). The Go target calls the
		// user-supplied Go function directly, so the prefix is
		// stripped here.
		args := make([]gotree.Expr, 0, len(s.Args))
		for i, a := range s.Args {
			v, err := l.lowerExpr(a)
			if err != nil {
				return nil, fmt.Errorf("call %s arg %d: %w", s.Func, i, err)
			}
			args = append(args, v)
		}
		return &gotree.ExprStmt{X: &gotree.CallExpr{
			Fun:  &gotree.Ident{Name: stripFFIPrefix(s.Func)},
			Args: args,
		}}, nil
	}
}

// stripFFIPrefix removes the C-target-specific FFI mangling
// (mochi_go_, mochi_py_, mochi_js_) from a function name so
// the Go target calls the user's Go function directly. User
// Mochi functions carry a `mochi__` prefix; that one stays.
func stripFFIPrefix(name string) string {
	for _, p := range []string{"mochi_go_", "mochi_py_", "mochi_js_"} {
		if len(name) > len(p) && name[:len(p)] == p {
			return name[len(p):]
		}
	}
	return name
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (gotree.Stmt, error) {
	typeText, err := l.letTypeText(s)
	if err != nil {
		return nil, fmt.Errorf("transpiler3/go/lower: let %s: %w", s.Name, err)
	}
	init, err := l.lowerExpr(s.Init)
	if err != nil {
		return nil, fmt.Errorf("transpiler3/go/lower: let %s init: %w", s.Name, err)
	}
	name := mangleIdent(s.Name)
	if s.Mutable {
		// `var name Type = init` keeps the binding mutable so
		// subsequent AssignStmts can update it; gofmt collapses
		// to the appropriate form.
		return &gotree.DeclStmt{Decl: &gotree.GenDecl{
			Tok: "var",
			Specs: []gotree.Spec{&gotree.ValueSpec{
				Names:  []string{name},
				Type:   &gotree.Ident{Name: typeText},
				Values: []gotree.Expr{init},
			}},
		}}, nil
	}
	// Immutable let: use `name := init`. The type annotation is
	// implicit; gotree-level typing is handled by gofmt and the
	// Go compiler.
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: name}},
		Tok: ":=",
		Rhs: []gotree.Expr{init},
	}, nil
}

func (l *lowerer) lowerAssignStmt(s *aotir.AssignStmt) (gotree.Stmt, error) {
	if stmt, ok, err := l.lowerQuerySortInPlace(s); err != nil {
		return nil, err
	} else if ok {
		return stmt, nil
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: mangleIdent(s.Name)}},
		Tok: "=",
		Rhs: []gotree.Expr{val},
	}, nil
}

// lowerQuerySortInPlace recognises the post-query patterns
//   __queryN = ListSortAscExpr(__queryN)
//   __queryN = ListSliceExpr(__queryN, start, end)
// emitted by the shared C lowerer for `order by` / `skip` / `take`,
// and rewrites them to in-place Go forms (`slices.Sort(xs)` and
// `xs = xs[start:end]`). The receiver and LHS always alias the same
// freshly-built list so in-place mutation matches Mochi semantics
// without an extra clone.
func (l *lowerer) lowerQuerySortInPlace(s *aotir.AssignStmt) (gotree.Stmt, bool, error) {
	switch v := s.Value.(type) {
	case *aotir.ListSortAscExpr:
		recv, ok := v.Receiver.(*aotir.VarRef)
		if !ok || recv.Name != s.Name {
			return nil, false, nil
		}
		l.addImport("slices")
		return &gotree.ExprStmt{X: &gotree.CallExpr{
			Fun: &gotree.SelectorExpr{X: &gotree.Ident{Name: "slices"}, Sel: "Sort"},
			Args: []gotree.Expr{&gotree.Ident{Name: mangleIdent(s.Name)}},
		}}, true, nil
	case *aotir.ListSliceExpr:
		recv, ok := v.Receiver.(*aotir.VarRef)
		if !ok || recv.Name != s.Name {
			return nil, false, nil
		}
		start, err := l.lowerExpr(v.Start)
		if err != nil {
			return nil, false, err
		}
		end, err := l.lowerExpr(v.End)
		if err != nil {
			return nil, false, err
		}
		l.addHelper("mochiListSlice")
		return &gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: mangleIdent(s.Name)}},
			Tok: "=",
			Rhs: []gotree.Expr{&gotree.CallExpr{
				Fun:  &gotree.Ident{Name: "mochiListSlice"},
				Args: []gotree.Expr{&gotree.Ident{Name: mangleIdent(s.Name)}, start, end},
			}},
		}, true, nil
	}
	return nil, false, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (gotree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	thenBlk, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	out := &gotree.IfStmt{Cond: cond, Body: thenBlk}
	if s.Else != nil {
		elseBlk, err := l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
		out.Else = elseBlk
	}
	return out, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (gotree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &gotree.ForStmt{Cond: cond, Body: body}, nil
}

// lowerForRangeStmt lowers Mochi `for i in a..b { ... }` to
// the canonical Go `for i := a; i < b; i++ { ... }` form.
func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (gotree.Stmt, error) {
	start, err := l.lowerExpr(s.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(s.End)
	if err != nil {
		return nil, err
	}
	name := mangleIdent(s.Var)
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	init := &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: name}},
		Tok: ":=",
		Rhs: []gotree.Expr{start},
	}
	cond := &gotree.BinaryExpr{X: &gotree.Ident{Name: name}, Op: "<", Y: end}
	post := &gotree.IncDecStmt{X: &gotree.Ident{Name: name}, Tok: "++"}
	return &gotree.ForStmt{Init: init, Cond: cond, Post: post, Body: body}, nil
}

// letTypeText returns the Go type-expression text for a LetStmt's
// binding type, dispatching on VarType so compound types (lists
// and maps today, records and sums later) can carry their element
// / field metadata through the type renderer.
func (l *lowerer) letTypeText(s *aotir.LetStmt) (string, error) {
	switch s.VarType {
	case aotir.TypeList:
		if s.ElemType == aotir.TypeRecord {
			if s.ElemRecordName == "" {
				return "", fmt.Errorf("list<record> missing ElemRecordName")
			}
			return "[]" + s.ElemRecordName, nil
		}
		if s.ElemType == aotir.TypeList {
			inner, err := l.lowerListType(s.InnerElemType)
			if err != nil {
				return "", fmt.Errorf("list<list>: %w", err)
			}
			return "[]" + inner, nil
		}
		return l.lowerListType(s.ElemType)
	case aotir.TypeMap:
		return l.lowerMapTypeWithList(s.KeyType, s.ValueType, s.ListValueElemType)
	case aotir.TypeSet:
		return l.lowerSetType(s.ElemType)
	case aotir.TypeOMap:
		l.addHelper("mochiOMap")
		return l.lowerOMapType(s.KeyType, s.ValueType)
	case aotir.TypeChan:
		return l.lowerChanType(s.ChanElemType)
	case aotir.TypeRecord:
		if s.RecordName == "" {
			return "", fmt.Errorf("record let missing RecordName")
		}
		return s.RecordName, nil
	case aotir.TypeUnion:
		if s.UnionName == "" {
			return "", fmt.Errorf("union let missing UnionName")
		}
		return s.UnionName, nil
	case aotir.TypeFun:
		return l.lowerFunType(s.FunSig)
	}
	return l.lowerType(s.VarType)
}

// lowerForEachStmt lowers Mochi `for x in xs { ... }` to
// `for _, x := range xs { ... }`. The key (index) slot is
// discarded with `_`.
//
// When the iteration variable is never read by the body
// (as in the hash-join inner loop, where the C lowerer's
// desugaring binds y but the select clause only projects x),
// the binding is dropped to `for range xs { ... }` so Go's
// "declared and not used" check does not fire.
func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) (gotree.Stmt, error) {
	xs, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	if !blockReferencesVar(s.Body, s.Var) {
		return &gotree.RangeStmt{
			Tok:  ":=",
			X:    xs,
			Body: body,
		}, nil
	}
	return &gotree.RangeStmt{
		Key:   &gotree.Ident{Name: "_"},
		Value: &gotree.Ident{Name: mangleIdent(s.Var)},
		Tok:   ":=",
		X:     xs,
		Body:  body,
	}, nil
}

// lowerListSetStmt lowers Mochi `xs[i] = v` to `xs[int(i)] = v`.
func (l *lowerer) lowerListSetStmt(s *aotir.ListSetStmt) (gotree.Stmt, error) {
	idx, err := l.lowerExpr(s.Index)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.IndexExpr{
			X:     &gotree.Ident{Name: mangleIdent(s.Name)},
			Index: narrowToInt(idx),
		}},
		Tok: "=",
		Rhs: []gotree.Expr{val},
	}, nil
}

// lowerMapPutStmt lowers Mochi `m[k] = v` to the same Go form.
func (l *lowerer) lowerMapPutStmt(s *aotir.MapPutStmt) (gotree.Stmt, error) {
	key, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.IndexExpr{
			X:     &gotree.Ident{Name: mangleIdent(s.Name)},
			Index: key,
		}},
		Tok: "=",
		Rhs: []gotree.Expr{val},
	}, nil
}

// lowerMatchStmt emits a `switch <tmp>.Tag { case <n>: ... }` block.
// The target is evaluated once into a fresh `__m<NNN>` local so each
// arm can address `<tmp>.<Variant>_<Field>` for its bindings.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) (gotree.Stmt, error) {
	target, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, fmt.Errorf("match target: %w", err)
	}
	tmp := l.freshName("__m")
	tmpAssign := &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: tmp}},
		Tok: ":=",
		Rhs: []gotree.Expr{target},
	}
	cases := make([]*gotree.CaseClause, 0, len(s.Arms)+1)
	for _, arm := range s.Arms {
		body, err := l.lowerMatchArmBody(tmp, arm)
		if err != nil {
			return nil, fmt.Errorf("match arm %s: %w", arm.VariantName, err)
		}
		cases = append(cases, &gotree.CaseClause{
			List: []gotree.Expr{&gotree.BasicLit{Kind: gotree.IntLit, Value: fmt.Sprintf("%d", arm.Tag)}},
			Body: body,
		})
	}
	if s.Default != nil {
		body, err := l.lowerBlock(s.Default.Body)
		if err != nil {
			return nil, fmt.Errorf("match default: %w", err)
		}
		cases = append(cases, &gotree.CaseClause{Body: body.List})
	}
	sw := &gotree.SwitchStmt{
		Init:  tmpAssign,
		Tag:   &gotree.SelectorExpr{X: &gotree.Ident{Name: tmp}, Sel: "Tag"},
		Cases: cases,
	}
	return sw, nil
}

// lowerMatchArmBody emits the per-arm body, with the pattern-variable
// bindings introduced via `<v> := <tmp>.<Variant>_<Field>` assignments
// before the user-written body runs. A trailing `_ = <v>` swallows any
// unused-variable warning the Go compiler would raise if the body never
// touched the binding.
func (l *lowerer) lowerMatchArmBody(tmp string, arm aotir.MatchArm) ([]gotree.Stmt, error) {
	out := make([]gotree.Stmt, 0, 2*len(arm.Bindings)+1)
	for _, b := range arm.Bindings {
		name := mangleIdent(b.VarName)
		out = append(out, &gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: name}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.SelectorExpr{
				X:   &gotree.Ident{Name: tmp},
				Sel: variantFieldName(arm.VariantName, b.FieldName),
			}},
		})
		out = append(out, &gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "_"}},
			Tok: "=",
			Rhs: []gotree.Expr{&gotree.Ident{Name: name}},
		})
	}
	body, err := l.lowerBlock(arm.Body)
	if err != nil {
		return nil, err
	}
	out = append(out, body.List...)
	return out, nil
}

func (l *lowerer) lowerReturnStmt(s *aotir.ReturnStmt) (gotree.Stmt, error) {
	if s.Value == nil {
		return &gotree.ReturnStmt{}, nil
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.ReturnStmt{Results: []gotree.Expr{val}}, nil
}

// lowerTryCatchStmt emits `mochiTry(func() { try }, func(<catchVar> int64) { catch })`.
// The helper installs defer+recover and dispatches mochiPanicValue (user panic)
// to its code field; runtime.Error from a Go runtime panic is translated to a
// matching Mochi error code (4=index, 5=divzero, 9=other). Other recover
// values are re-raised so they cannot be silently swallowed.
func (l *lowerer) lowerTryCatchStmt(s *aotir.TryCatchStmt) (gotree.Stmt, error) {
	tryBody, err := l.lowerBlock(s.TryBody)
	if err != nil {
		return nil, fmt.Errorf("try body: %w", err)
	}
	catchBody, err := l.lowerBlock(s.CatchBody)
	if err != nil {
		return nil, fmt.Errorf("catch body: %w", err)
	}
	l.addHelper("mochiPanicValue")
	l.addHelper("mochiPanic")
	l.addHelper("mochiTry")
	l.addImport("runtime")
	l.addImport("strings")
	tryLit := &gotree.FuncLit{Type: &gotree.FuncType{}, Body: tryBody}
	catchLit := &gotree.FuncLit{
		Type: &gotree.FuncType{Params: []gotree.Field{{
			Names: []string{mangleIdent(s.CatchVar)},
			Type:  &gotree.Ident{Name: "int64"},
		}}},
		Body: catchBody,
	}
	return &gotree.ExprStmt{X: &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiTry"},
		Args: []gotree.Expr{tryLit, catchLit},
	}}, nil
}

// lowerOMapPutStmt emits `mochiOMapSet(m, k, v)`, mutating the receiver
// in place. The C lowerer rebinds via orddict:store, but Go can mutate
// the underlying struct through its pointer so a re-assignment is
// unnecessary.
func (l *lowerer) lowerOMapPutStmt(s *aotir.OMapPutStmt) (gotree.Stmt, error) {
	key, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, fmt.Errorf("omap put key: %w", err)
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, fmt.Errorf("omap put value: %w", err)
	}
	l.addHelper("mochiOMap")
	l.addHelper("mochiOMapSet")
	return &gotree.ExprStmt{X: &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiOMapSet"},
		Args: []gotree.Expr{&gotree.Ident{Name: mangleIdent(s.Name)}, key, val},
	}}, nil
}

// lowerChanSendStmt lowers `send(c, v)` (Phase 9.1) to Go's `c <- v`.
// Like ChanMake / ChanRecv the mapping is direct because Mochi's send
// semantics (block on full, no error) match Go's native channel send.
func (l *lowerer) lowerChanSendStmt(s *aotir.ChanSendStmt) (gotree.Stmt, error) {
	ch, err := l.lowerExpr(s.Chan)
	if err != nil {
		return nil, fmt.Errorf("chan send chan: %w", err)
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, fmt.Errorf("chan send value: %w", err)
	}
	return &gotree.SendStmt{Chan: ch, Value: val}, nil
}

// lowerPanicStmt emits `mochiPanic(code, msg)` so the value propagates as
// mochiPanicValue, recognised by the mochiTry recover handler.
func (l *lowerer) lowerPanicStmt(s *aotir.PanicStmt) (gotree.Stmt, error) {
	code, err := l.lowerExpr(s.Code)
	if err != nil {
		return nil, fmt.Errorf("panic code: %w", err)
	}
	msg, err := l.lowerExpr(s.Msg)
	if err != nil {
		return nil, fmt.Errorf("panic msg: %w", err)
	}
	l.addHelper("mochiPanicValue")
	l.addHelper("mochiPanic")
	return &gotree.ExprStmt{X: &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiPanic"},
		Args: []gotree.Expr{code, msg},
	}}, nil
}

// Phase 3 widens the lowerer to collection types and operations.
// Phase 3.1 lands lists: ListLit, IndexExpr, LenExpr, AppendExpr,
// ListContainsExpr, ListSumExpr / Min / Max, ListSortAscExpr,
// ListSliceExpr, ForEachStmt, ListSetStmt. Maps (3.2), sets (3.3),
// and lists-of-records (3.4) land as later sub-phases.
//
// Numeric representation: list elements share the Phase 2 rule;
// `int` lowers to `number`, `float` to `number`, `bool` to
// `boolean`, `string` to `string`. The TS array literal `[1, 2, 3]`
// is typed `readonly number[]` when the IR proves no mutation site
// (today every Phase 3.1 fixture mutates indirectly only through
// `append`, which is functional; in-place `xs[i] = v` lands in 3.1
// too and forces `number[]`). The lowerer emits a `number[]` type
// today; the readonly narrowing follows once the IR carries a
// Mutability bit.
//
// Index access (`xs[i]`) routes through `mochi_list_at(xs, i)` to
// preserve Mochi's panic-on-out-of-range contract. Bare `xs[i]`
// under TS `--noUncheckedIndexedAccess` would otherwise type as
// `T | undefined` and force every caller to narrow, which would
// drown the emit in `!` non-null assertions.
//
// `len(xs)` lowers to `xs.length` (a number, matching the Phase 2
// `int` representation as `number`). When the bigint sub-phase
// lands, the emitter will widen to `BigInt(xs.length)`.
//
// `append(xs, v)` lowers to `[...xs, v]` because Mochi's append is
// functional (returns a fresh list without mutating the input).
// The TS spread is O(n) per call which matches the C runtime's
// allocation-and-copy semantics; v8 / SpiderMonkey / JavaScriptCore
// all special-case spread of a single iterable to avoid the
// iterator-protocol overhead for plain arrays.
//
// `for x in xs` lowers to `for (const x of xs) { ... }`. The
// induction variable's type is taken from the aotir ForEachStmt's
// ElemType field so `tsc --strict` doesn't have to infer it.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// tsTypeForList returns the TS element type for a list-typed slot.
// The list itself renders as `T[]`; this helper supplies the T.
// Phase 3.1 only handles scalar element types; widening to record
// elements lands with sub-phase 3.4.
func tsTypeForList(elem aotir.Type) (string, error) {
	switch elem {
	case aotir.TypeString:
		return "string", nil
	case aotir.TypeInt, aotir.TypeFloat:
		return "number", nil
	case aotir.TypeBool:
		return "boolean", nil
	default:
		return "", fmt.Errorf("ts lower: unsupported list element type %v (Phase 3.1)", elem)
	}
}

// tsTypeForCompound extends Phase 2's tsTypeFor to handle list
// slots. It shadows tsTypeFor at the call sites that pass a
// LetStmt-style envelope where the list's element type is carried
// on the side. Callers that already know they have a scalar should
// keep using tsTypeFor; this helper exists for compound containers.
func tsTypeForCompound(t aotir.Type, elem aotir.Type) (string, error) {
	if t != aotir.TypeList {
		return tsTypeFor(t)
	}
	es, err := tsTypeForList(elem)
	if err != nil {
		return "", err
	}
	return es + "[]", nil
}

// lowerListLit translates an aotir ListLit (`[a, b, c]`) into a TS
// array literal.
func (l *lowerer) lowerListLit(e *aotir.ListLit) (tstree.Expr, error) {
	elems := make([]tstree.Expr, 0, len(e.Elems))
	for _, el := range e.Elems {
		le, err := l.lowerExpr(el)
		if err != nil {
			return nil, fmt.Errorf("ts lower: list literal elem: %w", err)
		}
		elems = append(elems, le)
	}
	return &tstree.ListLit{Elems: elems}, nil
}

// lowerIndexExpr translates `xs[i]` through the `mochi_list_at`
// runtime helper. Bare `xs[i]` under TS strict mode types as
// `T | undefined` and forces every caller to narrow; the runtime
// helper bounds-checks and raises so the emit stays clean of `!`
// non-null assertions.
func (l *lowerer) lowerIndexExpr(e *aotir.IndexExpr) (tstree.Expr, error) {
	l.runtime.listAt = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: index receiver: %w", err)
	}
	idx, err := l.lowerExpr(e.Index)
	if err != nil {
		return nil, fmt.Errorf("ts lower: index: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_list_at"},
		Args:   []tstree.Expr{recv, idx},
	}, nil
}

// lowerLenExpr translates `len(xs)` into `xs.length`.
func (l *lowerer) lowerLenExpr(e *aotir.LenExpr) (tstree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: len receiver: %w", err)
	}
	return &tstree.MemberAccessExpr{Receiver: recv, Member: "length"}, nil
}

// lowerAppendExpr translates Mochi's functional `append(xs, v)`
// into `[...xs, v]` (allocates, never mutates).
func (l *lowerer) lowerAppendExpr(e *aotir.AppendExpr) (tstree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: append receiver: %w", err)
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, fmt.Errorf("ts lower: append value: %w", err)
	}
	return &tstree.SpreadAppendExpr{List: recv, Tail: val}, nil
}

// lowerListContainsExpr translates `v in xs` into `xs.includes(v)`.
// Array.prototype.includes uses SameValueZero (NaN matches NaN).
func (l *lowerer) lowerListContainsExpr(e *aotir.ListContainsExpr) (tstree.Expr, error) {
	recv, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list-contains list: %w", err)
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list-contains value: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "includes",
		Args:     []tstree.Expr{val},
	}, nil
}

func (l *lowerer) lowerListSumExpr(e *aotir.ListSumExpr) (tstree.Expr, error) {
	l.runtime.listSum = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: sum receiver: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_list_sum"},
		Args:   []tstree.Expr{recv},
	}, nil
}

func (l *lowerer) lowerListMinExpr(e *aotir.ListMinExpr) (tstree.Expr, error) {
	l.runtime.listMin = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: min receiver: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_list_min"},
		Args:   []tstree.Expr{recv},
	}, nil
}

func (l *lowerer) lowerListMaxExpr(e *aotir.ListMaxExpr) (tstree.Expr, error) {
	l.runtime.listMax = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: max receiver: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_list_max"},
		Args:   []tstree.Expr{recv},
	}, nil
}

func (l *lowerer) lowerListSliceExpr(e *aotir.ListSliceExpr) (tstree.Expr, error) {
	l.runtime.listSlice = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: slice receiver: %w", err)
	}
	start, err := l.lowerExpr(e.Start)
	if err != nil {
		return nil, fmt.Errorf("ts lower: slice start: %w", err)
	}
	end, err := l.lowerExpr(e.End)
	if err != nil {
		return nil, fmt.Errorf("ts lower: slice end: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_list_slice"},
		Args:   []tstree.Expr{recv, start, end},
	}, nil
}

func (l *lowerer) lowerListSortAscExpr(e *aotir.ListSortAscExpr) (tstree.Expr, error) {
	l.runtime.listSortAsc = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: sort receiver: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_list_sort_asc"},
		Args:   []tstree.Expr{recv},
	}, nil
}

// lowerForEachStmt translates `for x in xs { ... }` into a TS
// for-of loop.
func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) ([]tstree.Stmt, error) {
	list, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, fmt.Errorf("ts lower: for-each list: %w", err)
	}
	if s.Body == nil {
		return nil, fmt.Errorf("ts lower: for-each has nil Body")
	}
	body, err := l.lowerBlock(s.Body.Statements)
	if err != nil {
		return nil, err
	}
	if _, err := tsTypeForList(s.ElemType); err != nil {
		return nil, fmt.Errorf("ts lower: for-each elem: %w", err)
	}
	return []tstree.Stmt{&tstree.ForEachStmt{
		Var:  s.Var,
		List: list,
		Body: body,
	}}, nil
}

// lowerListSetStmt translates `xs[i] = v` into a bracket-assignment.
func (l *lowerer) lowerListSetStmt(s *aotir.ListSetStmt) ([]tstree.Stmt, error) {
	recv := &tstree.IdentExpr{Name: s.Name}
	idx, err := l.lowerExpr(s.Index)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list-set index: %w", err)
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list-set value: %w", err)
	}
	return []tstree.Stmt{&tstree.IndexAssignStmt{
		Receiver: recv,
		Index:    idx,
		Value:    val,
	}}, nil
}

// runtimeListDecls returns the Phase 3 list runtime helper decls
// keyed off the runtimeFlags. Order is deterministic.
func (l *lowerer) runtimeListDecls() []tstree.Decl {
	var out []tstree.Decl
	if l.runtime.listAt {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return xs[i], panicking on out-of-range index.",
				"`Array.prototype.at(i)` returns undefined on miss;",
				"this helper raises to match Mochi's panic contract.",
			},
			Name:     "mochi_list_at",
			Generics: []string{"T"},
			Params: []tstree.FuncParam{
				{Name: "xs", Type: "readonly T[]"},
				{Name: "i", Type: "number"},
			},
			ReturnType: "T",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "if (i < 0 || i >= xs.length) { throw new RangeError(\"mochi_list_at: index \" + i + \" out of range for list of length \" + xs.length); }"},
				&tstree.RawStmt{Text: "return xs[i] as T;"},
			},
		})
	}
	if l.runtime.listSum {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Sum the elements of a numeric list.",
				"Empty list returns 0 (matches vm3).",
			},
			Name:       "mochi_list_sum",
			Params:     []tstree.FuncParam{{Name: "xs", Type: "readonly number[]"}},
			ReturnType: "number",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "let s = 0;"},
				&tstree.RawStmt{Text: "for (const x of xs) { s += x; }"},
				&tstree.RawStmt{Text: "return s;"},
			},
		})
	}
	if l.runtime.listMin {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return the minimum element of a numeric list.",
				"Empty list raises (matches vm3 panic contract).",
			},
			Name:       "mochi_list_min",
			Params:     []tstree.FuncParam{{Name: "xs", Type: "readonly number[]"}},
			ReturnType: "number",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "if (xs.length === 0) { throw new RangeError(\"mochi_list_min: empty list\"); }"},
				&tstree.RawStmt{Text: "let m = xs[0] as number;"},
				&tstree.RawStmt{Text: "for (let i = 1; i < xs.length; i++) { const v = xs[i] as number; if (v < m) { m = v; } }"},
				&tstree.RawStmt{Text: "return m;"},
			},
		})
	}
	if l.runtime.listMax {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return the maximum element of a numeric list.",
				"Empty list raises (matches vm3 panic contract).",
			},
			Name:       "mochi_list_max",
			Params:     []tstree.FuncParam{{Name: "xs", Type: "readonly number[]"}},
			ReturnType: "number",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "if (xs.length === 0) { throw new RangeError(\"mochi_list_max: empty list\"); }"},
				&tstree.RawStmt{Text: "let m = xs[0] as number;"},
				&tstree.RawStmt{Text: "for (let i = 1; i < xs.length; i++) { const v = xs[i] as number; if (v > m) { m = v; } }"},
				&tstree.RawStmt{Text: "return m;"},
			},
		})
	}
	if l.runtime.listSlice {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Slice a list with Mochi's clamp-to-bounds semantics.",
				"Returns a fresh array (no shared backing).",
			},
			Name:     "mochi_list_slice",
			Generics: []string{"T"},
			Params: []tstree.FuncParam{
				{Name: "xs", Type: "readonly T[]"},
				{Name: "a", Type: "number"},
				{Name: "b", Type: "number"},
			},
			ReturnType: "T[]",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "const n = xs.length;"},
				&tstree.RawStmt{Text: "let lo = a < 0 ? 0 : a > n ? n : a;"},
				&tstree.RawStmt{Text: "let hi = b < 0 ? 0 : b > n ? n : b;"},
				&tstree.RawStmt{Text: "if (hi < lo) { hi = lo; }"},
				&tstree.RawStmt{Text: "return xs.slice(lo, hi);"},
			},
		})
	}
	if l.runtime.listSortAsc {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return a new list sorted ascending. Uses ES2023",
				"Array.prototype.toSorted so the input is preserved.",
			},
			Name:       "mochi_list_sort_asc",
			Generics:   []string{"T"},
			Params:     []tstree.FuncParam{{Name: "xs", Type: "readonly T[]"}},
			ReturnType: "T[]",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "return xs.toSorted((a, b) => (a < b ? -1 : a > b ? 1 : 0));"},
			},
		})
	}
	return out
}

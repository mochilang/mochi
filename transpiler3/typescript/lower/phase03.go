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

// tsTypeForMapSlot returns the TS type for a map slot (`Map<K, V>`).
// Phase 3.2 only handles scalar key/value types (int / float / bool /
// string); record-valued maps land with sub-phase 3.4. Phase 7 widens
// the value side to allow list values (`Map<K, T[]>`) so that the
// hash-join optimisation lowered by the aotir query pass type-checks.
// listValueElem is meaningful only when value == TypeList.
func tsTypeForMapSlot(key aotir.Type, value aotir.Type, listValueElem aotir.Type) (string, error) {
	ks, err := tsTypeForList(key)
	if err != nil {
		return "", fmt.Errorf("ts lower: map key: %w", err)
	}
	var vs string
	if value == aotir.TypeList {
		es, err := tsTypeForList(listValueElem)
		if err != nil {
			return "", fmt.Errorf("ts lower: map list value elem: %w", err)
		}
		vs = es + "[]"
	} else {
		vs, err = tsTypeForList(value)
		if err != nil {
			return "", fmt.Errorf("ts lower: map value: %w", err)
		}
	}
	return "Map<" + ks + ", " + vs + ">", nil
}

// tsTypeForSetSlot returns the TS type for a set slot (`Set<T>`).
// Phase 3.3 only handles scalar element types (int / float / bool /
// string), matching the aotir verifier's restriction in
// verifySetLiteralExpr; record-element sets land with sub-phase 3.4.
func tsTypeForSetSlot(elem aotir.Type) (string, error) {
	es, err := tsTypeForList(elem)
	if err != nil {
		return "", fmt.Errorf("ts lower: set elem: %w", err)
	}
	return "Set<" + es + ">", nil
}

// tsTypeForLetSlot picks the right type renderer for a LetStmt's
// declared shape. Lists use ElemType (+ ElemRecordName when the
// element is a record, Phase 3.4); maps use KeyType + ValueType;
// sets use ElemType (sharing the slot with lists); records use
// RecordName (Phase 3.4); unions use UnionName (Phase 5); every
// other VarType falls through to tsTypeFor.
func tsTypeForLetSlot(t, elem, key, value aotir.Type, recordName, elemRecordName, unionName string) (string, error) {
	return tsTypeForLetSlotV2(t, elem, key, value, aotir.TypeInvalid, recordName, elemRecordName, unionName)
}

// tsTypeForLetSlotV2 extends tsTypeForLetSlot with a listValueElem
// argument used when the slot is a map whose value type is a list of
// scalars (`Map<K, T[]>`). Phase 7 hash-join introduces this shape.
func tsTypeForLetSlotV2(t, elem, key, value, listValueElem aotir.Type, recordName, elemRecordName, unionName string) (string, error) {
	switch t {
	case aotir.TypeList:
		if elem == aotir.TypeRecord {
			es, err := tsTypeForRecord(elemRecordName)
			if err != nil {
				return "", fmt.Errorf("ts lower: list-of-record slot: %w", err)
			}
			return es + "[]", nil
		}
		return tsTypeForCompound(t, elem)
	case aotir.TypeMap:
		return tsTypeForMapSlot(key, value, listValueElem)
	case aotir.TypeSet:
		return tsTypeForSetSlot(elem)
	case aotir.TypeRecord:
		return tsTypeForRecord(recordName)
	case aotir.TypeUnion:
		return tsTypeForUnion(unionName)
	default:
		return tsTypeFor(t)
	}
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
	// Phase 11: mochi_list_at throws MochiPanic(4) on OOB, so opt
	// the class in alongside the helper.
	l.runtime.panicClass = true
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
	if s.ElemType == aotir.TypeRecord {
		if _, err := tsTypeForRecord(s.ElemRecordName); err != nil {
			return nil, fmt.Errorf("ts lower: for-each record elem: %w", err)
		}
	} else if _, err := tsTypeForList(s.ElemType); err != nil {
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

// ---- Phase 3.2 maps ----

// lowerMapLit translates `{k1: v1, k2: v2}` to
// `new Map<K, V>([[k1, v1], [k2, v2]])`. The Mochi semantic guarantees
// every key shares KeyType and every value shares ValueType; the
// emitter therefore renders one parameterised constructor call. An
// empty map literal `{}` lowers to `new Map<K, V>()`.
func (l *lowerer) lowerMapLit(e *aotir.MapLit) (tstree.Expr, error) {
	if len(e.Keys) != len(e.Values) {
		return nil, fmt.Errorf("ts lower: map literal keys/values length mismatch: %d vs %d", len(e.Keys), len(e.Values))
	}
	keyTy, err := tsTypeForList(e.KeyType)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map literal key: %w", err)
	}
	var valTy string
	if e.ValueType == aotir.TypeList {
		es, err := tsTypeForList(e.ListValueElemType)
		if err != nil {
			return nil, fmt.Errorf("ts lower: map literal list-value elem: %w", err)
		}
		valTy = es + "[]"
	} else {
		valTy, err = tsTypeForList(e.ValueType)
		if err != nil {
			return nil, fmt.Errorf("ts lower: map literal value: %w", err)
		}
	}
	keys := make([]tstree.Expr, 0, len(e.Keys))
	vals := make([]tstree.Expr, 0, len(e.Values))
	for i := range e.Keys {
		k, err := l.lowerExpr(e.Keys[i])
		if err != nil {
			return nil, fmt.Errorf("ts lower: map literal key %d: %w", i, err)
		}
		v, err := l.lowerExpr(e.Values[i])
		if err != nil {
			return nil, fmt.Errorf("ts lower: map literal value %d: %w", i, err)
		}
		keys = append(keys, k)
		vals = append(vals, v)
	}
	return &tstree.NewMapExpr{
		KeyType:   keyTy,
		ValueType: valTy,
		Keys:      keys,
		Values:    vals,
	}, nil
}

// lowerMapGetExpr translates `m[k]` through a `mochi_map_get` runtime
// helper. The helper raises on a missing key (matching aotir's
// "panic if absent" contract documented on MapGetExpr); fixtures that
// might miss must guard with `k in m`.
//
// Reason for a helper rather than bare `m.get(k)`: `Map.prototype.get`
// returns `V | undefined`, which under `--noUncheckedIndexedAccess` /
// `--strict` forces every caller to narrow. The helper bounds-checks
// once and returns `V` so the emit stays clean.
func (l *lowerer) lowerMapGetExpr(e *aotir.MapGetExpr) (tstree.Expr, error) {
	l.runtime.mapGet = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-get receiver: %w", err)
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-get key: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_map_get"},
		Args:   []tstree.Expr{recv, key},
	}, nil
}

// lowerMapHasExpr translates `k in m` to `m.has(k)`.
// `Map.prototype.has` uses SameValueZero comparison which matches
// Mochi's scalar key equality (NaN -> NaN, +0 -> -0).
func (l *lowerer) lowerMapHasExpr(e *aotir.MapHasExpr) (tstree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-has receiver: %w", err)
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-has key: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "has",
		Args:     []tstree.Expr{key},
	}, nil
}

// lowerMapLenExpr translates `len(m)` to `m.size`.
// Note: vm3 has a known bug where `len(m)` reads the original
// allocation length and does not update after a mutation; the
// TypeScript emit reads the actual live entry count.
// Fixtures avoid post-mutation len checks to stay byte-equal.
func (l *lowerer) lowerMapLenExpr(e *aotir.MapLenExpr) (tstree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-len receiver: %w", err)
	}
	return &tstree.MemberAccessExpr{Receiver: recv, Member: "size"}, nil
}

// lowerMapKeysExpr translates `keys(m)` (and the synthesised list
// inside `for k in m`) to `mochi_map_keys_sorted(m)`. vm3 iterates a
// map in lexicographic-by-string-of-key order regardless of insertion
// order; the helper sorts by `String(k)` ascending.
func (l *lowerer) lowerMapKeysExpr(e *aotir.MapKeysExpr) (tstree.Expr, error) {
	l.runtime.mapKeysSorted = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-keys receiver: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_map_keys_sorted"},
		Args:   []tstree.Expr{recv},
	}, nil
}

// lowerMapValuesExpr translates `values(m)` to
// `mochi_map_values_sorted(m)` (values in key-sorted order, mirroring
// the keys helper so a parallel iteration prints the same shape).
func (l *lowerer) lowerMapValuesExpr(e *aotir.MapValuesExpr) (tstree.Expr, error) {
	l.runtime.mapValuesSorted = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-values receiver: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_map_values_sorted"},
		Args:   []tstree.Expr{recv},
	}, nil
}

// lowerMapPutStmt translates `m[k] = v` to `m.set(k, v);`.
func (l *lowerer) lowerMapPutStmt(s *aotir.MapPutStmt) ([]tstree.Stmt, error) {
	key, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-put key: %w", err)
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, fmt.Errorf("ts lower: map-put value: %w", err)
	}
	return []tstree.Stmt{&tstree.ExprStmt{
		Expr: &tstree.MemberCallExpr{
			Receiver: &tstree.IdentExpr{Name: s.Name},
			Method:   "set",
			Args:     []tstree.Expr{key, val},
		},
	}}, nil
}

// ---- Phase 3.3 sets ----

// lowerSetLiteralExpr translates `set{e1, e2, ...}` to
// `new Set<T>([e1, e2, ...])`. The element type is rendered
// explicitly so an empty literal still types under `--strict`
// without inference and the printed form stays uniform.
func (l *lowerer) lowerSetLiteralExpr(e *aotir.SetLiteralExpr) (tstree.Expr, error) {
	elemTy, err := tsTypeForList(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("ts lower: set literal elem type: %w", err)
	}
	elems := make([]tstree.Expr, 0, len(e.Elems))
	for i, el := range e.Elems {
		le, err := l.lowerExpr(el)
		if err != nil {
			return nil, fmt.Errorf("ts lower: set literal elem %d: %w", i, err)
		}
		elems = append(elems, le)
	}
	return &tstree.NewSetExpr{ElemType: elemTy, Elems: elems}, nil
}

// lowerSetAddExpr translates `add(s, x)` to `new Set<T>([...s, x])`.
// Mochi's `add` is functional (`Type() returns TypeSet` and the
// aotir verifier enforces no mutation of the receiver), so the TS
// emit constructs a fresh set rather than calling `s.add(x)` which
// would mutate. Using the constructor over a spread reads cleanly
// inside re-assignments (`s = add(s, x)`) and function arguments.
func (l *lowerer) lowerSetAddExpr(e *aotir.SetAddExpr) (tstree.Expr, error) {
	elemTy, err := tsTypeForList(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("ts lower: set-add elem type: %w", err)
	}
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: set-add receiver: %w", err)
	}
	val, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, fmt.Errorf("ts lower: set-add elem: %w", err)
	}
	return &tstree.SetAddNewExpr{ElemType: elemTy, Receiver: recv, Elem: val}, nil
}

// lowerSetHasExpr translates `has(s, x)` (and `x in s`) to
// `s.has(x)`. `Set.prototype.has` uses SameValueZero, which matches
// Mochi's scalar equality contract (NaN -> NaN, +0 -> -0).
func (l *lowerer) lowerSetHasExpr(e *aotir.SetHasExpr) (tstree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: set-has receiver: %w", err)
	}
	val, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, fmt.Errorf("ts lower: set-has elem: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "has",
		Args:     []tstree.Expr{val},
	}, nil
}

// lowerSetLenExpr translates `len(s)` to `s.size`. JS Set's size
// is a getter property (not a method), so the emit is a
// MemberAccessExpr matching the Map.size lowering.
func (l *lowerer) lowerSetLenExpr(e *aotir.SetLenExpr) (tstree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: set-len receiver: %w", err)
	}
	return &tstree.MemberAccessExpr{Receiver: recv, Member: "size"}, nil
}

// lowerSetToListExpr translates `for x in s` (which the C aotir
// lowerer synthesises as SetToListExpr) into a sorted-by-String
// list. JS Sets preserve insertion order, but vm3's set support is
// broken; defining a deterministic iter order on the TS side keeps
// the byte-equal gate stable across Node, Deno, and Bun. The order
// matches mochi_map_keys_sorted for the same reason (parallel
// iteration produces matching shapes).
func (l *lowerer) lowerSetToListExpr(e *aotir.SetToListExpr) (tstree.Expr, error) {
	l.runtime.setToListSorted = true
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: set-to-list receiver: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_set_to_list_sorted"},
		Args:   []tstree.Expr{recv},
	}, nil
}

// runtimeSetDecls emits the Phase 3.3 set runtime helpers in flag-
// declaration order so two builds of the same program produce a
// byte-stable file (Phase 16 reproducibility gate).
func (l *lowerer) runtimeSetDecls() []tstree.Decl {
	var out []tstree.Decl
	if l.runtime.setToListSorted {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return the set's elements sorted ascending by String(x).",
				"JS Set iteration preserves insertion order; Mochi's set",
				"iteration order is unspecified, so the runtime sorts to",
				"keep the printed shape byte-stable across Node, Deno, Bun.",
			},
			Name:     "mochi_set_to_list_sorted",
			Generics: []string{"T"},
			Params: []tstree.FuncParam{
				{Name: "s", Type: "ReadonlySet<T>"},
			},
			ReturnType: "T[]",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "const xs = [...s];"},
				&tstree.RawStmt{Text: "xs.sort((a, b) => { const sa = String(a); const sb = String(b); return sa < sb ? -1 : sa > sb ? 1 : 0; });"},
				&tstree.RawStmt{Text: "return xs;"},
			},
		})
	}
	return out
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
				"Phase 11: out-of-range raises MochiPanic(4) so user",
				"`try / catch e` sees the index error code (4).",
			},
			Name:     "mochi_list_at",
			Generics: []string{"T"},
			Params: []tstree.FuncParam{
				{Name: "xs", Type: "readonly T[]"},
				{Name: "i", Type: "number"},
			},
			ReturnType: "T",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "if (i < 0 || i >= xs.length) { throw new MochiPanic(4, \"mochi_list_at: index \" + i + \" out of range for list of length \" + xs.length); }"},
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

// runtimeMapDecls emits Phase 3.2 map helpers. Order matches the
// flag-declaration order in runtimeFlags so two builds of the same
// program emit the same byte sequence (Phase 16 gate).
func (l *lowerer) runtimeMapDecls() []tstree.Decl {
	var out []tstree.Decl
	if l.runtime.mapGet {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return m[k], panicking on a missing key.",
				"`Map.prototype.get` returns V | undefined; this helper",
				"raises so the emit can stay strict-mode clean without",
				"forcing every caller to narrow.",
			},
			Name:     "mochi_map_get",
			Generics: []string{"K", "V"},
			Params: []tstree.FuncParam{
				{Name: "m", Type: "ReadonlyMap<K, V>"},
				{Name: "k", Type: "K"},
			},
			ReturnType: "V",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "if (!m.has(k)) { throw new RangeError(\"mochi_map_get: missing key \" + String(k)); }"},
				&tstree.RawStmt{Text: "return m.get(k) as V;"},
			},
		})
	}
	if l.runtime.mapKeysSorted {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return the map's keys sorted ascending by String(k).",
				"Matches vm3's iteration order (lexicographic by",
				"stringified key) so `for k in m` output is byte-equal.",
			},
			Name:     "mochi_map_keys_sorted",
			Generics: []string{"K", "V"},
			Params: []tstree.FuncParam{
				{Name: "m", Type: "ReadonlyMap<K, V>"},
			},
			ReturnType: "K[]",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "const ks = [...m.keys()];"},
				&tstree.RawStmt{Text: "ks.sort((a, b) => { const sa = String(a); const sb = String(b); return sa < sb ? -1 : sa > sb ? 1 : 0; });"},
				&tstree.RawStmt{Text: "return ks;"},
			},
		})
	}
	if l.runtime.mapValuesSorted {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return the map's values, ordered by sorted-String(key).",
				"Mirrors mochi_map_keys_sorted so parallel iteration",
				"produces matching (k, v) pairs.",
			},
			Name:     "mochi_map_values_sorted",
			Generics: []string{"K", "V"},
			Params: []tstree.FuncParam{
				{Name: "m", Type: "ReadonlyMap<K, V>"},
			},
			ReturnType: "V[]",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "const ks = [...m.keys()];"},
				&tstree.RawStmt{Text: "ks.sort((a, b) => { const sa = String(a); const sb = String(b); return sa < sb ? -1 : sa > sb ? 1 : 0; });"},
				&tstree.RawStmt{Text: "return ks.map((k) => m.get(k) as V);"},
			},
		})
	}
	return out
}

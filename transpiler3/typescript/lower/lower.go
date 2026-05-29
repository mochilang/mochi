// Package lower translates an aotir.Program into a tstree.SourceFile.
// Entry point: Lower(prog, colours) → *tstree.SourceFile.
//
// Phase 1 wires CallStmt for the four print runtime entries by
// emitting matching `mochi_print_*` TypeScript helpers inline.
// Phase 2 widens the surface to scalars + control flow + user
// functions (let, var, if, while, for, break, continue, return,
// binary and unary expressions, string len / index / contains).
// Later phases switch the inline helpers to `import { mochi_print_str
// } from "@mochi/runtime/io"` once the runtime package ships (Phase 15).
//
// Determinism: source-order traversal of prog.Functions (skipping
// the Main entry, which is emitted last under the canonical
// `mochi_main` name). Phase 16 reproducibility relies on this.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/colour"
	"mochi/transpiler3/typescript/tstree"
)

// runtimeFlags tracks which inline runtime helpers the lowered
// program needs so the emit pass only includes the ones that are
// actually used. The set grows phase by phase.
type runtimeFlags struct {
	// Phase 1 print helpers.
	printStr  bool
	printI64  bool
	printF64  bool
	printBool bool
	// Phase 2 string helpers. `mochi_str_len` returns the code-
	// point count (distinct from `.length` which counts UTF-16
	// units). `mochi_str_at` and `mochi_str_slice` iterate code
	// points to match Mochi indexing semantics. `mochi_str_contains`
	// wraps `String.prototype.includes` (Mochi's empty-needle
	// contract returns true, matching JS spec).
	strLen      bool
	strAt       bool
	strSlice    bool
	strContains bool
	// Phase 3 list helpers. `mochi_list_at` performs the bounds
	// check that Mochi's index semantics require (panic on
	// out-of-range, not silently undefined as bare `xs[i]` would
	// produce under `--noUncheckedIndexedAccess`). `mochi_list_sum`,
	// `_min`, `_max`, `_contains` cover the scalar reductions.
	listAt       bool
	listSum      bool
	listMin      bool
	listMax      bool
	listContains bool
	listSlice    bool
	listSortAsc  bool
	// Phase 3.2 map helpers. `mochi_map_get` raises on a missing
	// key (matches the aotir MapGetExpr "panic if absent" contract
	// and keeps the strict-mode emit clean of `V | undefined`
	// narrowing). `mochi_map_keys_sorted` and
	// `mochi_map_values_sorted` sort by `String(k)` to match vm3's
	// lex-sort iteration order.
	mapGet          bool
	mapKeysSorted   bool
	mapValuesSorted bool
	// Phase 3.3 set helpers. `mochi_set_to_list_sorted` is the
	// only runtime-side helper; SetHas / SetLen / SetAdd lower
	// inline (`.has(x)`, `.size`, `new Set([...s, x])`). The
	// helper sorts by `String(x)` so byte-equal stdout holds
	// across Node, Deno, and Bun even though JS Set iteration
	// preserves insertion order.
	setToListSorted bool
	// Phase 5 exhaustiveness trap. `mochiUnreachable(x: never)`
	// emits exactly when a match site lacks a wildcard arm so the
	// helper opts in on first use and stays absent otherwise.
	unreachable bool
}

type lowerer struct {
	prog    *aotir.Program
	colours colour.ColourMap
	runtime runtimeFlags
	// recordEqFlags is the set of record names that need a
	// `mochi_eq_<R>` helper generated. Populated as the lowerer
	// walks BinEqRec / BinNeRec sites; consumed by recordEqDecls
	// in the prelude assembly. Phase 4.2.
	recordEqFlags map[string]bool
	// matchLocalCounter assigns a fresh suffix to every match
	// target capture local (`__mochi_match_<n>`). Phase 5.
	matchLocalCounter int
	// liftedByName indexes the program's lifted (anonymous
	// closure body / function-ref shim) functions by name so
	// FunLit nodes can look up their body when inlining. Phase 6.
	liftedByName map[string]*aotir.Function
}

// Lower translates an aotir.Program into a single-file
// tstree.SourceFile. The file holds: (1) inline runtime helpers
// the body needs, (2) user-defined functions in source order,
// (3) one mochi_main() function carrying the entry function's
// body, (4) a trailing mochi_main(); call.
//
// Phase 1 ignored colours (every function is Blue per the
// Phase 11 stub). Phase 2 keeps the stub.
func Lower(prog *aotir.Program, colours colour.ColourMap) (*tstree.SourceFile, error) {
	if prog == nil {
		return nil, fmt.Errorf("ts lower: nil program")
	}
	if prog.Main < 0 || prog.Main >= len(prog.Functions) {
		return nil, fmt.Errorf("ts lower: invalid Main index %d", prog.Main)
	}
	l := &lowerer{prog: prog, colours: colours}

	// Phase 6: index lifted (closure-body / shim) functions by name
	// so FunLit nodes can inline them at their use sites. Lifted
	// functions are NOT emitted as top-level TS functions; JS
	// closures get their captures from the enclosing lexical scope
	// automatically.
	l.liftedByName = make(map[string]*aotir.Function)
	for _, fn := range prog.Functions {
		if fn.IsLifted || isLiftedFuncName(fn.Name) {
			l.liftedByName[fn.Name] = fn
		}
	}

	// Lower non-main user functions first so the main body's
	// CallExpr nodes resolve forwards. JS hoists `function`
	// declarations regardless, but emitting in source order keeps
	// the file readable and the byte-stable output property
	// Phase 16 depends on intact. Phase 6: lifted closure bodies
	// (__anon_*) and shim wrappers (__shim_*) are inlined at their
	// FunLit sites, so they do not get their own top-level decl.
	var userDecls []tstree.Decl
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		if fn.IsLifted || isLiftedFuncName(fn.Name) {
			continue
		}
		d, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		userDecls = append(userDecls, d)
	}

	mainFn := prog.Functions[prog.Main]
	if mainFn.Body == nil {
		return nil, fmt.Errorf("ts lower: main function has nil Body")
	}
	mainBody, err := l.lowerBlock(mainFn.Body.Statements)
	if err != nil {
		return nil, err
	}

	// Record classes go first so user functions and main body
	// references resolve forward (TS hoists `function` but not
	// `class`, and a class used before its declaration would be
	// a TDZ ReferenceError under `--strict`). Then runtime
	// helpers, then user functions. Source-map stability and
	// Phase 16 reproducibility rely on this fixed order.
	recordClassDecls, err := l.recordDecls()
	if err != nil {
		return nil, err
	}
	decls := recordClassDecls
	// Phase 5: sum-type alias declarations. Emitted after record
	// classes (so a union variant whose field is a record type can
	// name that class) and before runtime helpers (so a future
	// helper that operates on a union value can name the alias).
	unionAliasDecls, err := l.unionDecls()
	if err != nil {
		return nil, err
	}
	decls = append(decls, unionAliasDecls...)
	decls = append(decls, l.runtimeDecls()...)
	decls = append(decls, l.runtimeListDecls()...)
	decls = append(decls, l.runtimeMapDecls()...)
	decls = append(decls, l.runtimeSetDecls()...)
	// Phase 4.2: per-record equality helpers. Emitted after the
	// record ClassDecls (so the `R` type is in scope) and after
	// the runtime helpers (so the prelude reads top-to-bottom as
	// container helpers, then structural helpers, then user code).
	recordEqHelpers, err := l.recordEqDecls()
	if err != nil {
		return nil, err
	}
	decls = append(decls, recordEqHelpers...)
	// Phase 5: mochiUnreachable trap. Emitted after the structural
	// helpers (it is itself a structural helper) and before user
	// functions so a match-call inside a user function resolves
	// forward. The flag is set by lowerMatchStmt during the user-
	// function lowering pass that already ran above.
	decls = append(decls, l.unreachableDecls()...)
	decls = append(decls, userDecls...)

	mainDecl := &tstree.FuncDecl{
		Doc: []string{
			"Generated Mochi entry point. Do not edit by hand.",
		},
		Name:       "mochi_main",
		ReturnType: "void",
		Body:       mainBody,
	}
	decls = append(decls, mainDecl)

	return &tstree.SourceFile{
		HeaderComment: "Generated by mochi transpiler3/typescript. Do not edit.",
		Decls:         decls,
		TrailingExec: []tstree.Stmt{
			&tstree.ExprStmt{Expr: &tstree.CallExpr{
				Callee: &tstree.IdentExpr{Name: "mochi_main"},
			}},
		},
	}, nil
}

// runtimeDecls emits FuncDecl entries for each inline runtime
// helper the lowered body requested. The signatures match the
// vm3 print contract:
//
//   - mochi_print_str(value: string): void → echo + newline
//   - mochi_print_i64(value: number): void → decimal + newline
//   - mochi_print_f64(value: number): void → 'g' -1 64 with
//     NaN/+Inf/-Inf canonical labels matching vm3
//   - mochi_print_bool(value: boolean): void → "true"/"false"
//   - mochi_str_len(value: string): number → code-point count
//   - mochi_str_at(value: string, index: number): string → i-th code point
//   - mochi_str_slice(value: string, a: number, b: number): string → code-point slice
//   - mochi_str_contains(haystack: string, needle: string): boolean
//
// Each helper is emitted exactly once per file, regardless of
// how many call sites reference it. Order is deterministic
// (Phase 16 reproducibility gate).
func (l *lowerer) runtimeDecls() []tstree.Decl {
	var out []tstree.Decl
	if l.runtime.printStr {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Print a string followed by a newline (vm3 println contract).",
			},
			Name:       "mochi_print_str",
			Params:     []tstree.FuncParam{{Name: "value", Type: "string"}},
			ReturnType: "void",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "console.log(value);"},
			},
		})
	}
	if l.runtime.printI64 {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Print a 64-bit signed integer followed by a newline.",
				"Phase 2 emits the bound argument as a TS `number`.",
				"The bigint specialisation per MEP-52 §6 (int) lands as a",
				"sub-phase once the aotir IR carries a per-occurrence Repr.",
			},
			Name:       "mochi_print_i64",
			Params:     []tstree.FuncParam{{Name: "value", Type: "number"}},
			ReturnType: "void",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "console.log(value.toString());"},
			},
		})
	}
	if l.runtime.printF64 {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Print a 64-bit float followed by a newline.",
				"NaN renders \"NaN\"; +Infinity \"+Inf\"; -Infinity \"-Inf\".",
				"Integer-valued finite floats render without a fractional part.",
			},
			Name:       "mochi_print_f64",
			Params:     []tstree.FuncParam{{Name: "value", Type: "number"}},
			ReturnType: "void",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "if (Number.isNaN(value)) { console.log(\"NaN\"); return; }"},
				&tstree.RawStmt{Text: "if (!Number.isFinite(value)) { console.log(value < 0 ? \"-Inf\" : \"+Inf\"); return; }"},
				&tstree.RawStmt{Text: "console.log(value.toString());"},
			},
		})
	}
	if l.runtime.printBool {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Print a bool as the lowercase literal, followed by a newline.",
			},
			Name:       "mochi_print_bool",
			Params:     []tstree.FuncParam{{Name: "value", Type: "boolean"}},
			ReturnType: "void",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "console.log(value ? \"true\" : \"false\");"},
			},
		})
	}
	if l.runtime.strLen {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return the number of Unicode code points in `value`.",
				"`String.prototype.length` counts UTF-16 code units;",
				"Mochi `len(s)` is code-point semantics, so the runtime",
				"iterates the string (for...of is code-point granular).",
			},
			Name:       "mochi_str_len",
			Params:     []tstree.FuncParam{{Name: "value", Type: "string"}},
			ReturnType: "number",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "let n = 0;"},
				&tstree.RawStmt{Text: "for (const _ of value) { n++; }"},
				&tstree.RawStmt{Text: "return n;"},
			},
		})
	}
	if l.runtime.strAt {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return the `index`-th Unicode code point of `value`",
				"as a length-1-or-2 string. Surrogate pairs (U+10000+)",
				"occupy two UTF-16 units but one code point; iterating",
				"with for...of yields them as one step.",
			},
			Name: "mochi_str_at",
			Params: []tstree.FuncParam{
				{Name: "value", Type: "string"},
				{Name: "index", Type: "number"},
			},
			ReturnType: "string",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "let i = 0;"},
				&tstree.RawStmt{Text: "for (const ch of value) {"},
				&tstree.RawStmt{Text: "  if (i === index) { return ch; }"},
				&tstree.RawStmt{Text: "  i++;"},
				&tstree.RawStmt{Text: "}"},
				&tstree.RawStmt{Text: "throw new RangeError(\"mochi_str_at: index out of range\");"},
			},
		})
	}
	if l.runtime.strSlice {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return the code-point slice `value[a..b)` as a fresh string.",
			},
			Name: "mochi_str_slice",
			Params: []tstree.FuncParam{
				{Name: "value", Type: "string"},
				{Name: "a", Type: "number"},
				{Name: "b", Type: "number"},
			},
			ReturnType: "string",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "let i = 0;"},
				&tstree.RawStmt{Text: "let out = \"\";"},
				&tstree.RawStmt{Text: "for (const ch of value) {"},
				&tstree.RawStmt{Text: "  if (i >= a && i < b) { out += ch; }"},
				&tstree.RawStmt{Text: "  i++;"},
				&tstree.RawStmt{Text: "}"},
				&tstree.RawStmt{Text: "return out;"},
			},
		})
	}
	if l.runtime.strContains {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Return true when `needle` is a substring of `haystack`.",
				"Matches Mochi vm3's `str.contains` empty-needle rule",
				"(empty needle is always contained).",
			},
			Name: "mochi_str_contains",
			Params: []tstree.FuncParam{
				{Name: "haystack", Type: "string"},
				{Name: "needle", Type: "string"},
			},
			ReturnType: "boolean",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "return haystack.includes(needle);"},
			},
		})
	}
	return out
}

// lowerBlock translates a statement list inside one function body.
func (l *lowerer) lowerBlock(stmts []aotir.Stmt) ([]tstree.Stmt, error) {
	out := make([]tstree.Stmt, 0, len(stmts))
	for _, s := range stmts {
		lowered, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		out = append(out, lowered...)
	}
	return out, nil
}

// lowerStmt translates one aotir statement. Phase 1 supported only
// CallStmt for the four print builtins. Phase 2 widens to the full
// scalar + control-flow + user-function surface.
func (l *lowerer) lowerStmt(s aotir.Stmt) ([]tstree.Stmt, error) {
	switch v := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(v)
	case *aotir.LetStmt:
		return l.lowerLetStmt(v)
	case *aotir.AssignStmt:
		return l.lowerAssignStmt(v)
	case *aotir.IfStmt:
		return l.lowerIfStmt(v)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(v)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(v)
	case *aotir.BreakStmt:
		return []tstree.Stmt{&tstree.BreakStmt{}}, nil
	case *aotir.ContinueStmt:
		return []tstree.Stmt{&tstree.ContinueStmt{}}, nil
	case *aotir.ReturnStmt:
		return l.lowerReturnStmt(v)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(v)
	case *aotir.ListSetStmt:
		return l.lowerListSetStmt(v)
	case *aotir.MapPutStmt:
		return l.lowerMapPutStmt(v)
	case *aotir.MatchStmt:
		return l.lowerMatchStmt(v)
	case *aotir.ClosureEnvStmt:
		return l.lowerClosureEnvStmt(v)
	case *aotir.QueryScopeStmt:
		return l.lowerQueryScopeStmt(v)
	default:
		return nil, fmt.Errorf("ts lower: unsupported stmt %T (Phase 7 surface)", s)
	}
}

// lowerCallStmt translates a print or user-function call evaluated
// for side effect. The Phase 1 path covered only the four print
// builtins; Phase 2 widens it to any function in the program's
// function table.
func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) ([]tstree.Stmt, error) {
	switch s.Func {
	case "mochi_print_str":
		l.runtime.printStr = true
	case "mochi_print_i64":
		l.runtime.printI64 = true
	case "mochi_print_f64":
		l.runtime.printF64 = true
	case "mochi_print_bool":
		l.runtime.printBool = true
	default:
		// Phase 2: any other CallStmt targets a user-defined
		// function. We do not check Func against the program's
		// function table here; the C lowerer already did, and a
		// stale name would surface as a TS reference error at
		// runtime (caught by the fixture gate).
	}
	args := make([]tstree.Expr, 0, len(s.Args))
	for _, a := range s.Args {
		la, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, la)
	}
	return []tstree.Stmt{
		&tstree.ExprStmt{Expr: &tstree.CallExpr{
			Callee: &tstree.IdentExpr{Name: s.Func},
			Args:   args,
		}},
	}, nil
}

// lowerLetStmt translates `let x: T = init` (immutable) and
// `var y: T = init` (mutable). Mochi guarantees the binding has
// an Init expression; the C lowerer rejects bare declarations.
//
// Phase 3 widens the type renderer to compound containers: list
// slots emit `T[]` using the LetStmt's ElemType side-channel.
func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) ([]tstree.Stmt, error) {
	var tn string
	var err error
	if s.VarType == aotir.TypeFun {
		tn, err = tsTypeForFunSig(s.FunSig)
	} else {
		tn, err = tsTypeForLetSlotV2(s.VarType, s.ElemType, s.KeyType, s.ValueType, s.ListValueElemType, s.RecordName, s.ElemRecordName, s.UnionName)
	}
	if err != nil {
		return nil, fmt.Errorf("ts lower: let %q: %w", s.Name, err)
	}
	// Phase 5: the C lowerer pre-emits `LetStmt{Init: nil}` for the
	// match-as-expression result temp. The match arms then assign to
	// the binding via AssignStmt. We render this as `let NAME!: T;`
	// (definite assignment assertion); the exhaustiveness trap in
	// the default arm proves the binding is assigned before use.
	// Immutable (const) without an initialiser is a TS syntax error,
	// so we reject that combination defensively.
	if s.Init == nil {
		if !s.Mutable {
			return nil, fmt.Errorf("ts lower: let %q: immutable binding with nil Init has no TS surface (need `const NAME: T = E;`)", s.Name)
		}
		return []tstree.Stmt{&tstree.LetDecl{
			Name:    s.Name,
			Type:    tn,
			Init:    nil,
			Mutable: true,
		}}, nil
	}
	init, err := l.lowerExpr(s.Init)
	if err != nil {
		return nil, fmt.Errorf("ts lower: let %q init: %w", s.Name, err)
	}
	return []tstree.Stmt{&tstree.LetDecl{
		Name:    s.Name,
		Type:    tn,
		Init:    init,
		Mutable: s.Mutable,
	}}, nil
}

func (l *lowerer) lowerAssignStmt(s *aotir.AssignStmt) ([]tstree.Stmt, error) {
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, fmt.Errorf("ts lower: assign %q: %w", s.Name, err)
	}
	return []tstree.Stmt{&tstree.AssignStmt{Name: s.Name, Value: val}}, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) ([]tstree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, fmt.Errorf("ts lower: if cond: %w", err)
	}
	if s.Then == nil {
		return nil, fmt.Errorf("ts lower: if has nil Then block")
	}
	then, err := l.lowerBlock(s.Then.Statements)
	if err != nil {
		return nil, err
	}
	var elseBody []tstree.Stmt
	if s.Else != nil {
		elseBody, err = l.lowerBlock(s.Else.Statements)
		if err != nil {
			return nil, err
		}
	}
	return []tstree.Stmt{&tstree.IfStmt{
		Cond: cond,
		Then: then,
		Else: elseBody,
	}}, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) ([]tstree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, fmt.Errorf("ts lower: while cond: %w", err)
	}
	if s.Body == nil {
		return nil, fmt.Errorf("ts lower: while has nil Body")
	}
	body, err := l.lowerBlock(s.Body.Statements)
	if err != nil {
		return nil, err
	}
	return []tstree.Stmt{&tstree.WhileStmt{Cond: cond, Body: body}}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) ([]tstree.Stmt, error) {
	start, err := l.lowerExpr(s.Start)
	if err != nil {
		return nil, fmt.Errorf("ts lower: for-range start: %w", err)
	}
	end, err := l.lowerExpr(s.End)
	if err != nil {
		return nil, fmt.Errorf("ts lower: for-range end: %w", err)
	}
	if s.Body == nil {
		return nil, fmt.Errorf("ts lower: for-range has nil Body")
	}
	body, err := l.lowerBlock(s.Body.Statements)
	if err != nil {
		return nil, err
	}
	return []tstree.Stmt{&tstree.ForRangeStmt{
		Var:   s.Var,
		Start: start,
		End:   end,
		Body:  body,
	}}, nil
}

func (l *lowerer) lowerReturnStmt(s *aotir.ReturnStmt) ([]tstree.Stmt, error) {
	if s.Value == nil {
		return []tstree.Stmt{&tstree.ReturnStmt{Value: nil}}, nil
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, fmt.Errorf("ts lower: return: %w", err)
	}
	return []tstree.Stmt{&tstree.ReturnStmt{Value: val}}, nil
}

// lowerExpr translates one aotir expression. Phase 1 supported the
// four scalar literals; Phase 2 widens to variable references,
// binary / unary expressions, value-producing user function calls,
// and the string-intrinsic family (len, index, contains, substring).
func (l *lowerer) lowerExpr(e aotir.Expr) (tstree.Expr, error) {
	switch v := e.(type) {
	case *aotir.StringLit:
		return &tstree.StringLit{Value: v.Value}, nil
	case *aotir.IntLit:
		return &tstree.IntLit{Value: v.Value}, nil
	case *aotir.FloatLit:
		return &tstree.FloatLit{Value: v.Value}, nil
	case *aotir.BoolLit:
		return &tstree.BoolLit{Value: v.Value}, nil
	case *aotir.VarRef:
		// Phase 6: the C lowerer stamps captured-variable refs
		// inside a closure body as `__e->X` so its emitter can
		// route them through the env struct. JS doesn't have an
		// env struct, so we strip the prefix and let lexical
		// scope resolve the identifier.
		return &tstree.IdentExpr{Name: stripEnvPrefix(v.Name)}, nil
	case *aotir.FunLit:
		return l.lowerFunLit(v)
	case *aotir.FunCallExpr:
		return l.lowerFunCallExpr(v)
	case *aotir.BinaryExpr:
		return l.lowerBinary(v)
	case *aotir.UnaryExpr:
		return l.lowerUnary(v)
	case *aotir.CallExpr:
		return l.lowerCallExpr(v)
	case *aotir.StrLenExpr:
		l.runtime.strLen = true
		arg, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &tstree.CallExpr{
			Callee: &tstree.IdentExpr{Name: "mochi_str_len"},
			Args:   []tstree.Expr{arg},
		}, nil
	case *aotir.StrIndexExpr:
		l.runtime.strAt = true
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := l.lowerExpr(v.Index)
		if err != nil {
			return nil, err
		}
		return &tstree.CallExpr{
			Callee: &tstree.IdentExpr{Name: "mochi_str_at"},
			Args:   []tstree.Expr{recv, idx},
		}, nil
	case *aotir.StrSubstringExpr:
		l.runtime.strSlice = true
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
		return &tstree.CallExpr{
			Callee: &tstree.IdentExpr{Name: "mochi_str_slice"},
			Args:   []tstree.Expr{recv, start, end},
		}, nil
	case *aotir.StrContainsExpr:
		l.runtime.strContains = true
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		needle, err := l.lowerExpr(v.Sub)
		if err != nil {
			return nil, err
		}
		return &tstree.CallExpr{
			Callee: &tstree.IdentExpr{Name: "mochi_str_contains"},
			Args:   []tstree.Expr{recv, needle},
		}, nil
	case *aotir.ListLit:
		return l.lowerListLit(v)
	case *aotir.IndexExpr:
		return l.lowerIndexExpr(v)
	case *aotir.LenExpr:
		return l.lowerLenExpr(v)
	case *aotir.AppendExpr:
		return l.lowerAppendExpr(v)
	case *aotir.ListContainsExpr:
		return l.lowerListContainsExpr(v)
	case *aotir.ListSumExpr:
		return l.lowerListSumExpr(v)
	case *aotir.ListMinExpr:
		return l.lowerListMinExpr(v)
	case *aotir.ListMaxExpr:
		return l.lowerListMaxExpr(v)
	case *aotir.ListSliceExpr:
		return l.lowerListSliceExpr(v)
	case *aotir.ListSortAscExpr:
		return l.lowerListSortAscExpr(v)
	case *aotir.MapLit:
		return l.lowerMapLit(v)
	case *aotir.MapGetExpr:
		return l.lowerMapGetExpr(v)
	case *aotir.MapHasExpr:
		return l.lowerMapHasExpr(v)
	case *aotir.MapLenExpr:
		return l.lowerMapLenExpr(v)
	case *aotir.MapKeysExpr:
		return l.lowerMapKeysExpr(v)
	case *aotir.MapValuesExpr:
		return l.lowerMapValuesExpr(v)
	case *aotir.SetLiteralExpr:
		return l.lowerSetLiteralExpr(v)
	case *aotir.SetAddExpr:
		return l.lowerSetAddExpr(v)
	case *aotir.SetHasExpr:
		return l.lowerSetHasExpr(v)
	case *aotir.SetLenExpr:
		return l.lowerSetLenExpr(v)
	case *aotir.SetToListExpr:
		return l.lowerSetToListExpr(v)
	case *aotir.RecordLit:
		return l.lowerRecordLit(v)
	case *aotir.FieldAccess:
		return l.lowerFieldAccess(v)
	case *aotir.VariantLit:
		return l.lowerVariantLit(v)
	case *aotir.UnionVarRef:
		return l.lowerUnionVarRef(v)
	default:
		return nil, fmt.Errorf("ts lower: unsupported expr %T (Phase 5 surface)", e)
	}
}

func (l *lowerer) lowerBinary(e *aotir.BinaryExpr) (tstree.Expr, error) {
	// Phase 4.2 record equality: BinEqRec / BinNeRec lower to a
	// generated `mochi_eq_<R>` helper call, not a `===` operator
	// (TS `===` on class instances is reference equality, not
	// the structural equality Mochi `==` contracts).
	if e.Op == aotir.BinEqRec || e.Op == aotir.BinNeRec {
		return l.lowerRecordEq(e)
	}
	op, err := tsBinOp(e.Op)
	if err != nil {
		return nil, err
	}
	lhs, err := l.lowerExpr(e.Left)
	if err != nil {
		return nil, err
	}
	rhs, err := l.lowerExpr(e.Right)
	if err != nil {
		return nil, err
	}
	return &tstree.BinaryExpr{Op: op, Left: lhs, Right: rhs}, nil
}

func (l *lowerer) lowerUnary(e *aotir.UnaryExpr) (tstree.Expr, error) {
	op, err := tsUnOp(e.Op)
	if err != nil {
		return nil, err
	}
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	return &tstree.UnaryExpr{Op: op, Operand: operand}, nil
}

func (l *lowerer) lowerCallExpr(e *aotir.CallExpr) (tstree.Expr, error) {
	args := make([]tstree.Expr, 0, len(e.Args))
	for _, a := range e.Args {
		la, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, la)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: e.Func},
		Args:   args,
	}, nil
}

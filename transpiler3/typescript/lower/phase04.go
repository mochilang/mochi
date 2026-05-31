// Phase 4 ships the rest of the record surface that Phase 3.4 did
// not need. The work splits into:
//
//   - 4.0 class shape (private constructor + readonly fields + static
//     `of(opts)` factory). Already landed with Phase 3.4 because
//     lists-of-records required the same emit.
//   - 4.1 record methods. Deferred until vm3 lands a fix for
//     method bodies that return composite arithmetic expressions
//     (today vm3 returns the first product of `(x*x) + (y*y)`
//     instead of the sum, which would mask any TS-side bug).
//   - 4.2 record equality. Whole-record `a == b` / `a != b` lowers
//     to a per-record `mochi_eq_<R>(a, b): boolean` helper that
//     ANDs each field comparison. The Mochi frontend rejects
//     equality between values of different record types at parse
//     time, so the lowerer can assume `Left` and `Right` carry
//     the same record name. This file ships 4.2.
//   - 4.3 multi-file emit. Deferred; the single-file `src/index.ts`
//     emit Phase 1 introduced already serves the Phase 15 npm
//     pack gate.
//   - 4.4 identifier mangling for reserved-word collisions.
//     Deferred until a fixture surfaces a collision.
//
// Why a generated helper instead of inline field comparisons.
// Mochi `==` between two records is a single operator, but TS
// `===` on two class instances does reference equality (which is
// false for two `Pt.of({x:1,y:2})` calls that produce two distinct
// instances). The contract Mochi wants is structural-by-field, so
// the emit has to spell out the field-wise comparison somewhere.
// Putting it inline at every call site would bloat both source
// and bundle for repeated comparisons; threading it through a
// per-record helper keeps each `==` site to a single call and
// lets V8 / Deno / Bun inline the helper at JIT time.
//
// String field equality uses TS `===`, which is byte-wise on JS
// strings (the spec defines `===` on strings as `==` on the
// SameValueNonNumeric algorithm, which is character-by-character
// comparison). No need for an inline strcmp helper on the TS side
// (unlike the C transpiler, where strings are `const char *` and
// pointer-equality is the wrong default).
//
// Nested record fields aren't permitted by aotir's Phase 3.0
// verifier (record fields are scalar-only today), so the helper
// only handles the four scalar primitives. When Phase 5 lifts the
// nested-record gate, fieldEqExpr will grow a `mochi_eq_<Inner>`
// recursive case.
package lower

import (
	"fmt"
	"sort"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// tsExprRecordName mirrors the C-side exprRecordName: it returns
// the record-name identity of a record-typed aotir expression so
// the lowerer can route BinEqRec / BinNeRec to the right per-record
// helper. The cases mirror exprRecordName in transpiler3/c/lower.
//
// Why a TS-side copy and not a shared aotir export. aotir is the
// IR contract, not a utility package; pulling lower-side helpers
// in would couple aotir to every transpiler's lowering strategy.
// The two copies cost ~15 lines and stay in sync with verifier
// expectations (Phase 3.0 added FieldAccess.ResultRecordName,
// Phase 3.4 added IndexExpr.ElemRecordName, etc.).
func tsExprRecordName(e aotir.Expr) string {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.RecordName
	case *aotir.RecordLit:
		return v.TypeName
	case *aotir.FieldAccess:
		return v.ResultRecordName
	case *aotir.CallExpr:
		return v.ResultRecordName
	case *aotir.IndexExpr:
		return v.ElemRecordName
	}
	return ""
}

// lowerRecordEq translates BinEqRec / BinNeRec.
//
// The aotir BinaryExpr does not stamp the operand record name onto
// the node itself (Op + Left + Right + Result are the carrier
// fields). We recover the record name from Left via tsExprRecordName;
// the Mochi frontend's typechecker has already proven Left and
// Right carry the same record name before stamping BinEqRec, so
// reading from Left is sufficient.
//
// BinEqRec emits `mochi_eq_<R>(a, b)`. BinNeRec wraps in `!(...)`
// rather than emitting a parallel `mochi_ne_<R>` helper; the
// frontend has eliminated short-circuit semantics from `!=` (it is
// a pure boolean negation), so the wrap is exact, and the helper
// surface stays half the size.
func (l *lowerer) lowerRecordEq(e *aotir.BinaryExpr) (tstree.Expr, error) {
	recName := tsExprRecordName(e.Left)
	if recName == "" {
		recName = tsExprRecordName(e.Right)
	}
	if recName == "" {
		return nil, fmt.Errorf("ts lower: record equality missing record name on both operands")
	}
	if l.recordEqFlags == nil {
		l.recordEqFlags = make(map[string]bool)
	}
	l.recordEqFlags[recName] = true

	lhs, err := l.lowerExpr(e.Left)
	if err != nil {
		return nil, fmt.Errorf("ts lower: record eq lhs: %w", err)
	}
	rhs, err := l.lowerExpr(e.Right)
	if err != nil {
		return nil, fmt.Errorf("ts lower: record eq rhs: %w", err)
	}
	call := &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_eq_" + recName},
		Args:   []tstree.Expr{lhs, rhs},
	}
	if e.Op == aotir.BinNeRec {
		return &tstree.UnaryExpr{Op: "!", Operand: call}, nil
	}
	return call, nil
}

// recordEqDecls emits one `mochi_eq_<R>` helper per record name in
// the flag set. The emit walks prog.Records in source order and
// filters to flagged names so the prelude order stays byte-stable
// across runs (Phase 16 reproducibility) regardless of the order
// in which BinEqRec sites were lowered.
//
// Each helper body reads:
//
//	function mochi_eq_<R>(a: <R>, b: <R>): boolean {
//	  return (a.f1 === b.f1) && (a.f2 === b.f2) && ...;
//	}
//
// Empty-field record: returns `true` unconditionally (any two
// instances of an empty record are structurally equal). aotir's
// verifier permits empty records; the helper has to handle them.
func (l *lowerer) recordEqDecls() ([]tstree.Decl, error) {
	if len(l.recordEqFlags) == 0 {
		return nil, nil
	}
	// Walk prog.Records in source order, emitting only flagged
	// names. Records the lowerer never asked equality for don't
	// pay for an unused helper.
	out := make([]tstree.Decl, 0, len(l.recordEqFlags))
	for _, rd := range l.prog.Records {
		if rd == nil || !l.recordEqFlags[rd.Name] {
			continue
		}
		decl, err := l.buildRecordEqDecl(rd)
		if err != nil {
			return nil, err
		}
		out = append(out, decl)
	}
	// Sanity check: every flagged name should have appeared in
	// prog.Records. If a name is flagged but unmatched, the IR
	// is inconsistent (a BinEqRec referenced a record the program
	// never declared); we fail loudly rather than silently dropping
	// the helper. The sort is deterministic for the error message.
	if len(out) != len(l.recordEqFlags) {
		var missing []string
		seen := make(map[string]bool, len(out))
		for _, rd := range l.prog.Records {
			if rd != nil && l.recordEqFlags[rd.Name] {
				seen[rd.Name] = true
			}
		}
		for name := range l.recordEqFlags {
			if !seen[name] {
				missing = append(missing, name)
			}
		}
		sort.Strings(missing)
		return nil, fmt.Errorf("ts lower: record equality referenced undeclared record(s): %v", missing)
	}
	return out, nil
}

// buildRecordEqDecl renders one mochi_eq_<R> helper. Field types
// are validated against the Phase 4.2 supported set (the four
// scalar primitives); a record with a non-scalar field surfaces
// here as a hard error rather than silently producing a wrong
// helper. Nested-record / list-of-record fields land in Phase 5+.
func (l *lowerer) buildRecordEqDecl(rd *aotir.RecordDecl) (tstree.Decl, error) {
	for _, f := range rd.Fields {
		switch f.Type {
		case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
			// ok
		default:
			return nil, fmt.Errorf("ts lower: record equality on %q: field %q has unsupported type %v (Phase 4.2 only supports scalar fields)", rd.Name, f.Name, f.Type)
		}
	}

	body := []tstree.Stmt{}
	if len(rd.Fields) == 0 {
		body = append(body, &tstree.RawStmt{Text: "return true;"})
	} else {
		// Build `(a.f1 === b.f1) && (a.f2 === b.f2) && ...` as a
		// single return expression. Parenthesising each comparison
		// keeps emit unambiguous against TS precedence (`&&` binds
		// looser than `===`, so the parens are technically
		// redundant, but explicit parens are the Phase 3.4
		// precedence-pass convention and stay readable).
		var expr string
		for i, f := range rd.Fields {
			if i > 0 {
				expr += " && "
			}
			expr += fmt.Sprintf("(a.%s === b.%s)", f.Name, f.Name)
		}
		body = append(body, &tstree.RawStmt{Text: "return " + expr + ";"})
	}

	return &tstree.FuncDecl{
		Doc: []string{
			"Structural equality for record `" + rd.Name + "`.",
			"Generated by the Phase 4.2 record-equality lowerer.",
			"Two instances are equal iff every scalar field matches.",
		},
		Name: "mochi_eq_" + rd.Name,
		Params: []tstree.FuncParam{
			{Name: "a", Type: rd.Name},
			{Name: "b", Type: rd.Name},
		},
		ReturnType: "boolean",
		Body:       body,
	}, nil
}

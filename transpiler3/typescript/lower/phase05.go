// Phase 5 ships the lowering for Mochi sum types and `match`
// expressions. The decisions land here:
//
// Sum declaration to discriminated union. A Mochi declaration
//
//	type Shape =
//	  Circle(r: int)
//	  | Square(side: int)
//
// lowers to the canonical TypeScript discriminated-union form:
//
//	type Shape =
//	  | { readonly kind: "Circle"; readonly r: number }
//	  | { readonly kind: "Square"; readonly side: number };
//
// Why a discriminated union over a literal `kind` tag rather than a
// class hierarchy (`abstract class Shape; class Circle extends
// Shape; ...`). tsc strict narrows `s` to the matching variant
// inside `case "Circle":` automatically when the discriminator is a
// literal tag, so the emit pass can rely on `s.r` being typed as
// `number` (not `number | undefined` or `any`) without writing an
// explicit type assertion. The class-hierarchy form would force
// `instanceof Circle` chains and lose narrowing under `switch`, so
// the literal-tag form is the only form that satisfies the
// "tsc-strict-clean emit" gate. The TypeScript handbook recommends
// this pattern for tagged unions since 2.0 and it composes cleanly
// with the Phase 5.2 exhaustiveness check (`mochiUnreachable(x:
// never)`).
//
// The `readonly` modifier on every field (including `kind`) is
// load-bearing for narrowing soundness: tsc's flow analysis assumes
// the tag cannot change between the discriminator read and the body
// of the case arm, so making the tag mutable would re-introduce the
// narrowing-defeat surface that motivated the class-hierarchy form
// in earlier TS versions.
//
// Variant construction. Mochi `Circle(5)` lowers to a plain TS
// object literal `{ kind: "Circle", r: 5 }`, not a constructor
// function call. The object-literal form lets tsc do contextual
// typing against the union-typed slot (LetStmt with VarType=TypeUnion,
// or a Function param of union type), inferring the variant member
// from the literal `kind`. A constructor function would also work,
// but adds N functions per union and a name-mangling step for free
// variables that collide with the variant names; the literal form
// has none of those costs. Field order in the emitted object literal
// matches the variant's source order so two builds of the same
// program produce byte-equal output (Phase 16 reproducibility).
//
// Match to switch. Mochi
//
//	match s {
//	  Circle(r) => r * r
//	  Square(side) => side * side
//	}
//
// lowers to:
//
//	switch (s.kind) {
//	  case "Circle": {
//	    const r = s.r;
//	    ... arm body ...
//	    break;
//	  }
//	  case "Square": {
//	    const side = s.side;
//	    ... arm body ...
//	    break;
//	  }
//	  default: {
//	    mochiUnreachable(s);
//	    break;
//	  }
//	}
//
// The match target is captured in a local `const __mochi_match_<id>`
// so the discriminator read and every field access in the arm bodies
// see the same value (the target expression may have side effects;
// the C transpiler does the same one-shot capture). Pattern variables
// lower to `const <varName> = <target>.<fieldName>;` reads at the
// top of each arm's block, scoped to the arm via the block braces.
// Wildcard arms (`_ => ...`) lower to the `default:` case. When the
// Mochi match has no wildcard, the emit pass adds a synthetic default
// that calls `mochiUnreachable(target)`, whose parameter type is
// `never` — tsc rejects the call (and the build) if the discriminator
// is not a tag the switch has handled.
//
// When the match is used as an expression (aotir.MatchStmt.ResultVar
// non-empty), the C lowerer has already pre-emitted a LetStmt for the
// result variable above the MatchStmt; arm bodies end with an
// AssignStmt to that variable. The TS emit treats those statements
// like any other and the result variable is in scope below the
// switch. The arm-body block scoping does NOT capture the result
// variable because it was declared outside the switch.
//
// What we don't ship in Phase 5. Pattern guards (`when` clauses) and
// nested patterns (`Some(Some(x))`) are absent from the C transpiler's
// emitMatchStmt and from the current fixture corpus. aotir reserves
// the IR slots (MatchArm.Guard, MatchBinding nesting via FieldType=
// TypeUnion), but until those land on the C side the TS lowerer
// rejects them with a Phase 5.3 error so a regression surfaces here
// instead of producing wrong-but-byte-equal stdout.
package lower

import (
	"fmt"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// tsTypeForUnion returns the TS type identifier for a union-typed
// slot. The aotir UnionDecl.Name is already a valid JS identifier
// (the Mochi frontend rejects names with non-ident characters), so
// the helper exists primarily to surface the missing-name error
// uniformly with tsTypeForRecord.
func tsTypeForUnion(name string) (string, error) {
	if name == "" {
		return "", fmt.Errorf("ts lower: union slot missing union name")
	}
	return name, nil
}

// tsTypeForVariantField renders one variant-field TS type. Variant
// fields cover the four scalar primitives plus nested record and
// nested union types (recursive sums such as `Tree = Leaf |
// Node(left: Tree, ...)`). List, map, and set fields are deferred
// to Phase 5.4 — the aotir verifier accepts them but the current
// Phase 5 fixture corpus does not exercise them, and rendering
// them safely needs the elem-type side-channels the VariantField
// struct does not carry.
func tsTypeForVariantField(f aotir.VariantField) (string, error) {
	switch f.FieldType {
	case aotir.TypeInt, aotir.TypeFloat:
		return "number", nil
	case aotir.TypeBool:
		return "boolean", nil
	case aotir.TypeString:
		return "string", nil
	case aotir.TypeRecord:
		return tsTypeForRecord(f.RecordName)
	case aotir.TypeUnion:
		return tsTypeForUnion(f.UnionName)
	default:
		return "", fmt.Errorf("ts lower: variant field %q has unsupported type %v (Phase 5 covers scalars, records, and nested unions)", f.Name, f.FieldType)
	}
}

// unionDecls walks prog.Unions in source order and emits one
// TypeAliasDecl per declared sum type. The emit order is fixed by
// aotir's source-order traversal so two builds of the same program
// produce a byte-stable prelude (Phase 16 reproducibility gate).
//
// Each declared sum type with N variants emits as:
//
//	type Foo =
//	  | { readonly kind: "V1"; readonly f1: T1 }
//	  | { readonly kind: "V2" };
//
// Unit variants (Mochi `Color = Red | Green | Blue`) render as
// `{ readonly kind: "<Name>" }` (no field beyond the tag). Empty-
// union edge case (no variants) renders as `never` since a union
// over zero alternatives is vacuously uninhabited; aotir's parser
// rejects this earlier, but the emit pass handles it defensively.
func (l *lowerer) unionDecls() ([]tstree.Decl, error) {
	if len(l.prog.Unions) == 0 {
		return nil, nil
	}
	out := make([]tstree.Decl, 0, len(l.prog.Unions))
	for _, ud := range l.prog.Unions {
		if ud == nil {
			continue
		}
		decl, err := l.buildUnionDecl(ud)
		if err != nil {
			return nil, fmt.Errorf("ts lower: union %q: %w", ud.Name, err)
		}
		out = append(out, decl)
	}
	return out, nil
}

// buildUnionDecl renders one TypeAliasDecl for a UnionDecl. The
// Body is constructed as one variant per line, prefixed with `  | `
// (two-space hanging indent + pipe + space), matching tstree's
// two-space indentation rule. Each variant is rendered as an
// inline object-type with readonly fields in declaration source
// order.
func (l *lowerer) buildUnionDecl(ud *aotir.UnionDecl) (tstree.Decl, error) {
	if len(ud.Variants) == 0 {
		return &tstree.TypeAliasDecl{
			Doc: []string{
				"Generated sum type for Mochi `type " + ud.Name + "`.",
				"Empty union (no variants) → `never`.",
			},
			Name: ud.Name,
			Body: "never",
		}, nil
	}
	var lines []string
	for _, v := range ud.Variants {
		line, err := buildVariantTypeLine(&v)
		if err != nil {
			return nil, fmt.Errorf("variant %q: %w", v.Name, err)
		}
		lines = append(lines, "  | "+line)
	}
	body := strings.Join(lines, "\n")
	return &tstree.TypeAliasDecl{
		Doc: []string{
			"Generated sum type for Mochi `type " + ud.Name + "`.",
			"Discriminated union over the literal `kind` tag; each",
			"variant carries the variant's source-order fields. tsc",
			"narrows on `switch (x.kind)` so per-arm field reads are",
			"sound under --strict.",
		},
		Name: ud.Name,
		Body: body,
	}, nil
}

// buildVariantTypeLine renders one variant as an object-type
// alternative: `{ readonly kind: "Name"; readonly f1: T1; ... }`.
// The kind property always leads so the discriminator appears at
// the same position across variants (cosmetic, but it matches the
// TS handbook examples and keeps the prelude scannable).
func buildVariantTypeLine(v *aotir.VariantDecl) (string, error) {
	var b strings.Builder
	b.WriteString("{ readonly kind: ")
	b.WriteByte('"')
	b.WriteString(v.Name)
	b.WriteByte('"')
	for _, f := range v.Fields {
		ty, err := tsTypeForVariantField(f)
		if err != nil {
			return "", fmt.Errorf("field %q: %w", f.Name, err)
		}
		b.WriteString("; readonly ")
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(ty)
	}
	b.WriteString(" }")
	return b.String(), nil
}

// lowerVariantLit translates a Mochi variant construction
// (`Circle(5)`, `Leaf`, `Node { left: ..., value: ..., right: ... }`)
// into a TS object literal `{ kind: "Circle", r: 5 }`. Fields are
// emitted in the aotir VariantLit's source order (which mirrors
// the variant's declaration order; the C lowerer reorders user
// args into declaration order before stamping the IR).
//
// Why an object literal and not a constructor function call. tsc's
// contextual typing infers the union-typed slot's variant member
// from the literal `kind` value, so an object literal is sufficient
// for type narrowing. A constructor function would add N functions
// per union (and name-mangling for collisions) without buying any
// type-narrowing benefit. The bundle cost is the same in either
// direction (a JIT inlines both), so the simpler form wins.
func (l *lowerer) lowerVariantLit(e *aotir.VariantLit) (tstree.Expr, error) {
	if e.VariantName == "" {
		return nil, fmt.Errorf("ts lower: variant literal missing VariantName")
	}
	if e.UnionName == "" {
		return nil, fmt.Errorf("ts lower: variant literal %q missing UnionName", e.VariantName)
	}
	fields := make([]tstree.ObjectLitField, 0, 1+len(e.Fields))
	fields = append(fields, tstree.ObjectLitField{
		Name:  "kind",
		Value: &tstree.StringLit{Value: e.VariantName},
	})
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("ts lower: variant %q field %d (%q): %w", e.VariantName, i, f.Name, err)
		}
		fields = append(fields, tstree.ObjectLitField{Name: f.Name, Value: v})
	}
	// Wrap in `<lit> as <UnionName>` so tsc widens the literal back
	// to the discriminated union. Without the cast, a `const s:
	// Shape = Circle(5)` narrows `s` to the Circle variant and any
	// later `switch (s.kind)` reports the other cases as unreachable.
	return &tstree.TypeAssertExpr{
		Inner:    &tstree.ObjectLit{Fields: fields},
		TypeName: e.UnionName,
	}, nil
}

// lowerUnionVarRef translates a read of a union-typed variable. The
// TS emit is identical to a plain VarRef: the variable's name. The
// union identity is enforced by the surrounding tsTypeForLetSlot /
// paramType emit; this helper does not re-emit a type annotation.
func (l *lowerer) lowerUnionVarRef(v *aotir.UnionVarRef) (tstree.Expr, error) {
	if v.Name == "" {
		return nil, fmt.Errorf("ts lower: union var ref missing Name")
	}
	return &tstree.IdentExpr{Name: stripEnvPrefix(v.Name)}, nil
}

// lowerMatchStmt translates a Mochi `match` to a TS `switch` on the
// variant `kind` tag. The emit shape is:
//
//	{
//	  const __mochi_match_<id> = <target>;
//	  switch (__mochi_match_<id>.kind) {
//	    case "<Variant1>": {
//	      const <var1> = __mochi_match_<id>.<field1>;
//	      ... arm body ...
//	      break;
//	    }
//	    ...
//	    default: {
//	      mochiUnreachable(__mochi_match_<id>);
//	      break;
//	    }
//	  }
//	}
//
// The outer block scopes the match-target local so concurrent
// matches in the same enclosing block don't collide. The
// __mochi_match_<id> id is fresh per match site (counter on the
// lowerer); this keeps two matches on the same union in the same
// function from colliding on a single name.
//
// When the Mochi match has an explicit wildcard arm (_), the
// default case carries the wildcard's body. When the Mochi match
// is exhaustive (no wildcard), the default case calls
// `mochiUnreachable(target)` so tsc rejects the build if any
// variant goes unhandled (the parameter type is `never`, which
// narrows to a missing-variant union if a case is dropped).
//
// When aotir.MatchStmt.ResultVar is non-empty, the C-side lowerer
// has already pre-emitted a `let <ResultVar>: <ResultType>;` LetStmt
// above the MatchStmt. The arm bodies contain AssignStmts to
// ResultVar. The TS lowerer doesn't need to re-emit either; the
// LetStmt and AssignStmts come through the normal lowerStmt path.
//
// Pattern bindings (e.g. `Circle(r)` binding `r` to the variant's
// `r` field) lower to a `const <varName> = <target>.<fieldName>;`
// at the top of each arm's body block.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) ([]tstree.Stmt, error) {
	if s.Target == nil {
		return nil, fmt.Errorf("ts lower: match has nil Target")
	}
	if s.UnionName == "" {
		return nil, fmt.Errorf("ts lower: match has no UnionName")
	}
	// Reject guards / nested patterns; the C transpiler doesn't
	// emit them either, so a regression here surfaces immediately
	// rather than producing silently-wrong output. Phase 5.3 lifts
	// this restriction once the C emit catches up.
	for i, arm := range s.Arms {
		if arm.Guard != nil {
			return nil, fmt.Errorf("ts lower: match arm %d (%q): pattern guards are Phase 5.3 (deferred)", i, arm.VariantName)
		}
	}
	if s.Default != nil && s.Default.Guard != nil {
		return nil, fmt.Errorf("ts lower: match default arm: pattern guards are Phase 5.3 (deferred)")
	}

	targetExpr, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, fmt.Errorf("ts lower: match target: %w", err)
	}

	// Capture target in a fresh local so the discriminator read and
	// every arm-body field access see the same value. The local
	// name is unique per match site; the counter is bumped per
	// call so two matches on the same union don't collide.
	localName := l.freshMatchLocal()
	targetCapture := &tstree.LetDecl{
		Name:    localName,
		Type:    s.UnionName,
		Init:    targetExpr,
		Mutable: false,
	}
	discriminator := &tstree.MemberAccessExpr{
		Receiver: &tstree.IdentExpr{Name: localName},
		Member:   "kind",
	}

	cases := make([]tstree.SwitchCase, 0, len(s.Arms)+1)
	for i := range s.Arms {
		arm := &s.Arms[i]
		body, err := l.lowerMatchArmBody(localName, arm)
		if err != nil {
			return nil, fmt.Errorf("ts lower: match arm %d (%q): %w", i, arm.VariantName, err)
		}
		cases = append(cases, tstree.SwitchCase{
			Label: arm.VariantName,
			Body:  body,
		})
	}
	// Default: explicit wildcard arm if present, otherwise the
	// exhaustiveness-trap call to mochiUnreachable.
	if s.Default != nil {
		body, err := l.lowerMatchArmBody(localName, s.Default)
		if err != nil {
			return nil, fmt.Errorf("ts lower: match default arm: %w", err)
		}
		cases = append(cases, tstree.SwitchCase{
			IsDefault: true,
			Body:      body,
		})
	} else {
		l.runtime.unreachable = true
		cases = append(cases, tstree.SwitchCase{
			IsDefault: true,
			Body: []tstree.Stmt{
				&tstree.ExprStmt{Expr: &tstree.CallExpr{
					Callee: &tstree.IdentExpr{Name: "mochiUnreachable"},
					Args:   []tstree.Expr{&tstree.IdentExpr{Name: localName}},
				}},
			},
		})
	}

	// Wrap target-capture + switch in an outer block so the
	// __mochi_match_<id> local doesn't leak.
	wrapped := &tstree.BlockStmt{
		Body: []tstree.Stmt{
			targetCapture,
			&tstree.SwitchStmt{Discriminator: discriminator, Cases: cases},
		},
	}
	return []tstree.Stmt{wrapped}, nil
}

// lowerMatchArmBody renders the per-arm body: pattern-variable
// `const` reads first (in MatchArm.Bindings order — the C lowerer
// preserves source order), then the arm's lowered statement list.
// SwitchStmt.TsString appends `break;` after this list for every
// arm, so the body itself does not include a terminator.
func (l *lowerer) lowerMatchArmBody(targetLocal string, arm *aotir.MatchArm) ([]tstree.Stmt, error) {
	out := make([]tstree.Stmt, 0, len(arm.Bindings)+1)
	for _, b := range arm.Bindings {
		if b.VarName == "" || b.FieldName == "" {
			return nil, fmt.Errorf("binding missing VarName/FieldName: %+v", b)
		}
		bindType, err := tsTypeForBinding(b)
		if err != nil {
			return nil, fmt.Errorf("binding %q: %w", b.VarName, err)
		}
		out = append(out, &tstree.LetDecl{
			Name: b.VarName,
			Type: bindType,
			Init: &tstree.MemberAccessExpr{
				Receiver: &tstree.IdentExpr{Name: targetLocal},
				Member:   b.FieldName,
			},
			Mutable: false,
		})
	}
	if arm.Body != nil {
		armStmts, err := l.lowerBlock(arm.Body.Statements)
		if err != nil {
			return nil, fmt.Errorf("body: %w", err)
		}
		out = append(out, armStmts...)
	}
	return out, nil
}

// tsTypeForBinding renders the TS type of a pattern variable. The
// MatchBinding carries the variant field's aotir type and (for
// record / union fields) the nested type's identity.
func tsTypeForBinding(b aotir.MatchBinding) (string, error) {
	switch b.FieldType {
	case aotir.TypeInt, aotir.TypeFloat:
		return "number", nil
	case aotir.TypeBool:
		return "boolean", nil
	case aotir.TypeString:
		return "string", nil
	case aotir.TypeRecord:
		return tsTypeForRecord(b.RecordName)
	case aotir.TypeUnion:
		// MatchBinding does not carry a UnionName side-channel
		// (the C-side IR uses VariantField.UnionName but
		// MatchBinding only stores FieldType + RecordName). Until
		// aotir grows the slot, recursive-sum bindings can only
		// type as the source variant's union name read from the
		// MatchStmt context — which the binding does not have at
		// this layer. Phase 5 ships scalar / record bindings only
		// and surfaces the missing-name case as an error so a
		// regression doesn't silently emit `any`.
		return "", fmt.Errorf("union-typed pattern variable %q: aotir MatchBinding lacks UnionName side-channel (Phase 5.3 deferred)", b.VarName)
	default:
		return "", fmt.Errorf("pattern variable %q has unsupported field type %v", b.VarName, b.FieldType)
	}
}

// freshMatchLocal returns a unique local name for the match-target
// capture. The counter lives on the lowerer; each match site bumps
// it by one. The id form (`__mochi_match_<n>`) is chosen so it
// can't collide with any user identifier (Mochi rejects names
// starting with `__`) and is grep-able in the emit.
func (l *lowerer) freshMatchLocal() string {
	l.matchLocalCounter++
	return fmt.Sprintf("__mochi_match_%d", l.matchLocalCounter)
}

// unreachableDecls emits the `mochiUnreachable(x: never): never`
// runtime helper when the lowerer has seen any match without an
// explicit wildcard. The helper's body throws an Error whose
// message includes the offending value, formatted via JSON.stringify
// for diagnosability. The function returns `never` so tsc accepts
// it in arm-body positions where a return value is required (e.g.
// inside the default case of an expression-form match).
//
// Why a runtime helper instead of inlining the throw. Two reasons:
//
//  1. tsc's exhaustiveness check requires the default arm to call a
//     function whose parameter type is `never`. Inlining
//     `throw new Error(...)` does not satisfy this — tsc still
//     accepts a non-exhaustive switch as long as the default
//     terminates. The named helper with `x: never` is the canonical
//     form the TS handbook prescribes and the only form that turns
//     a missing-variant case into a compile-time error.
//
//  2. The helper is a single declaration regardless of how many
//     match sites the program contains (the runtimeFlags machinery
//     guarantees opt-in single emit). Inlining would scatter the
//     same throw across N sites, bloating the bundle by O(matches).
func (l *lowerer) unreachableDecls() []tstree.Decl {
	if !l.runtime.unreachable {
		return nil
	}
	return []tstree.Decl{
		&tstree.FuncDecl{
			Doc: []string{
				"Exhaustiveness trap for sum-type match expressions.",
				"Mochi's emit pass calls this from the default arm of",
				"every `match`. The parameter type is `never`, so tsc",
				"rejects any call site where the discriminator union",
				"has an unhandled variant (the call argument's type",
				"narrows to that missing variant rather than `never`).",
			},
			Name:       "mochiUnreachable",
			Params:     []tstree.FuncParam{{Name: "x", Type: "never"}},
			ReturnType: "never",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "throw new Error(\"mochi: unreachable variant: \" + JSON.stringify(x as unknown));"},
			},
		},
	}
}

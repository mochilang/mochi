// Phase 5 widens tstree with the two surface forms needed to lower
// Mochi sum types and `match` expressions:
//
//   - TypeAliasDecl renders `type X = T;` (or a multi-line
//     discriminated union when T spans several variants). Sum-type
//     declarations lower to this form per MEP-52 §Phase 5: a
//     literal-tag discriminated union over `kind` rather than a class
//     hierarchy, since `switch (x.kind)` narrows variant types under
//     tsc strict without `instanceof` chains.
//
//   - SwitchStmt renders `switch (E) { case "k1": { ... break; } ...
//     default: { ... } }`. Mochi `match` lowers here; the discriminator
//     is always the variant `kind` tag string, and each case carries a
//     block-scoped body so the per-arm pattern-variable `const`s do
//     not leak across arms.
//
// Both nodes obey tstree's printer contract: two-space indentation,
// no trailing whitespace, LF line endings, deterministic emit order.
package tstree

import (
	"strings"
)

// TypeAliasDecl is a top-level `type NAME = BODY;` declaration. The
// emit pass writes BODY verbatim so the union-of-object-types syntax
// the discriminated-union lowering wants ("| { kind: \"A\"; ... } |
// { kind: \"B\"; ... }") stays expressible without a dedicated AST
// node per variant member. Determinism is the lowerer's problem: it
// must build Body in source order.
//
// Why a single Body string rather than a structured []Variant. The
// Mochi-side IR (aotir.UnionDecl) already carries variants in source
// order with fully-typed fields, and the BODY shape this node renders
// is the only legal TS form for a tagged-union type alias. A
// structured representation would add four AST nodes (Variant,
// VariantField, ...) for one renderer that already lives in the
// lowering pass. The trade-off is that Body must be a syntactically
// valid TS type expression; the lowerer owns that invariant.
type TypeAliasDecl struct {
	// Doc is the leading `//` comment block (one line per entry).
	Doc []string
	// Modifiers lists `export`, etc. in source order. Empty for
	// Phase 5 (helpers stay module-private; Phase 15 may re-export).
	Modifiers []string
	// Name is the emitted type identifier (union name).
	Name string
	// Body is the right-hand side of the `=`. The emit pass writes
	// it verbatim followed by `;` and a trailing newline. The
	// lowerer is responsible for any necessary indentation inside
	// Body (e.g., one variant per line, prefixed with `| ` and a
	// two-space hanging indent matching tstree's indentation rule).
	Body string
}

func (d *TypeAliasDecl) declNode() {}
func (d *TypeAliasDecl) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	for _, line := range d.Doc {
		b.WriteString(pad)
		b.WriteString("// ")
		b.WriteString(line)
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	for _, m := range d.Modifiers {
		b.WriteString(m)
		b.WriteByte(' ')
	}
	b.WriteString("type ")
	b.WriteString(d.Name)
	b.WriteString(" =")
	if strings.Contains(d.Body, "\n") {
		b.WriteByte('\n')
		b.WriteString(d.Body)
	} else {
		b.WriteByte(' ')
		b.WriteString(d.Body)
	}
	b.WriteByte(';')
	return b.String()
}

// SwitchCase is one arm of a SwitchStmt. The Label is rendered as
// `case "<Label>":` (always a double-quoted string literal, since
// Phase 5 only switches on a discriminated-union `kind` tag). When
// IsDefault is true the Label is ignored and the arm renders as
// `default:`. Body is wrapped in a block (`{ ... }`) so that each
// arm's pattern-variable `const`s stay scoped to that arm; the block
// also guarantees `break;` lands inside it (not at the outer switch
// level) which keeps the tstree printer's indentation stable.
type SwitchCase struct {
	Label     string // quoted-form variant tag; ignored when IsDefault
	IsDefault bool
	// Body is the statement list inside the arm's block. The lowerer
	// emits pattern-variable `const`s first, then the arm body, then
	// no terminator. SwitchStmt.TsString adds a `break;` after Body
	// for every arm (default included) so fallthrough is impossible.
	Body []Stmt
}

// SwitchStmt is `switch (Discriminator) { case ...; default: ... }`.
// The Discriminator expression is conventionally a member access on
// the bound match target (`target.kind`); the lowerer captures the
// target in a `const` local immediately before the switch so the
// member access reads from a stable, side-effect-free name. Cases
// are emitted in source order; the lowerer is responsible for any
// ordering invariants (Phase 5 inherits prog.Records-style source
// order via aotir.MatchStmt.Arms).
type SwitchStmt struct {
	Discriminator Expr
	Cases         []SwitchCase
}

func (s *SwitchStmt) stmtNode() {}
func (s *SwitchStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	armPad := pad + "  "
	bodyPad := armPad + "  "
	var b strings.Builder
	b.WriteString(pad)
	b.WriteString("switch (")
	b.WriteString(s.Discriminator.TsString(0))
	b.WriteString(") {\n")
	for _, c := range s.Cases {
		b.WriteString(armPad)
		if c.IsDefault {
			b.WriteString("default: {\n")
		} else {
			b.WriteString("case ")
			b.WriteString(quoteCaseLabel(c.Label))
			b.WriteString(": {\n")
		}
		for _, st := range c.Body {
			b.WriteString(st.TsString(indent + 2))
			b.WriteByte('\n')
		}
		b.WriteString(bodyPad)
		b.WriteString("break;\n")
		b.WriteString(armPad)
		b.WriteString("}\n")
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// BlockStmt is `{ STMTS }` rendered as a standalone scope. Phase 5
// uses it to wrap a match's target-capture LetDecl and the switch
// itself so the `__mochi_match_<n>` local does not leak past the
// match site. Two matches in the same enclosing block then keep
// their captures separate without a counter-driven name clash.
//
// Why a dedicated node rather than reusing a statement list. A bare
// statement list at the lower layer is just a `[]Stmt`; the printer
// needs an explicit node to emit the surrounding `{` / `}` and to
// indent the body one level. The grammar form is identical to a
// function body, but FuncDecl already prints `function NAME ... {`
// before the body, so a free-standing block needs its own node.
type BlockStmt struct {
	Body []Stmt
}

func (s *BlockStmt) stmtNode() {}
func (s *BlockStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	b.WriteString(pad)
	b.WriteByte('{')
	if len(s.Body) == 0 {
		b.WriteByte('}')
		return b.String()
	}
	b.WriteByte('\n')
	for _, t := range s.Body {
		b.WriteString(t.TsString(indent + 1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// TypeAssertExpr is `INNER as TYPE`. Phase 5 emits it around every
// variant literal so the resulting object reads as the discriminated
// union, not the single-variant literal type. Without the cast, tsc's
// control-flow analysis narrows the const binding to the literal
// variant at its declaration site (`const s: Shape = { kind: "Circle",
// r: 5 }` narrows `s` to `{kind:"Circle";r:5}`), and a later `switch
// (s.kind)` then reports the unreachable `"Square"` case as a type
// error. The `as Shape` cast widens the initialiser's static type
// back to the declared union, keeping every variant case reachable
// under tsc strict.
//
// The cast is syntactic only at the emit layer; the generated JS is
// identical to the bare literal. The shape (`<expr> as <type>`) is
// the modern TS form (not the legacy `<type>expr` form) because the
// JSX-friendly form is the one bundlers and TS-stripping runtimes
// (Node 22 `--experimental-strip-types`, Bun, Deno) all accept.
type TypeAssertExpr struct {
	Inner    Expr
	TypeName string
}

func (e *TypeAssertExpr) exprNode() {}
func (e *TypeAssertExpr) TsString(_ int) string {
	return e.Inner.TsString(0) + " as " + e.TypeName
}

// quoteCaseLabel returns the case label wrapped in TS double quotes.
// Variant names from aotir are valid JS identifiers, but we always
// emit them as quoted strings to match the discriminated-union tag
// type (`"Circle" | "Square" | ...`) declared in the corresponding
// TypeAliasDecl. The escape goes through Go's strconv.Quote indirectly
// — for a known-safe identifier, plain wrapping with `"` is sufficient.
func quoteCaseLabel(s string) string {
	return `"` + s + `"`
}

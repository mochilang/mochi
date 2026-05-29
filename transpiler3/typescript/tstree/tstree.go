// Package tstree models a Mochi-side TypeScript 5.6 syntax tree.
//
// Phase 1 ships the minimum shape sufficient for hello-world emit:
// top-level FuncDecl, ExprStmt, CallExpr, IdentExpr, and the four
// scalar literal nodes (StringLit, IntLit, FloatLit, BoolLit).
//
// Phase 2 widens to scalars + control flow + user functions:
// LetDecl (let / const), AssignStmt, IfStmt, WhileStmt, ForRangeStmt,
// BreakStmt, ContinueStmt, BinaryExpr, UnaryExpr, ParenExpr, and
// MemberCallExpr (for `s.includes(t)`-style runtime helper dispatch).
//
// Each node implements TsString(indent int) string for tree-printer
// emission. The printer guarantees: no trailing whitespace, LF line
// endings, two-space indentation, semicolon-terminated statements,
// and idempotent re-emission (TsString(TsString(x)) == TsString(x))
// up to whitespace normalisation by prettier.
//
// Determinism: source order matters. Callers must keep Decls in
// the order they want them emitted (Phase 16 reproducibility gate
// requires byte-identical output across two CI hosts). The model
// does not sort; the lower pass is responsible for assembling
// Decls deterministically.
//
// Why a Mochi-side tree rather than the TypeScript Compiler API:
// pulling tsc into the Go build chain would couple Mochi's
// reproducibility to npm's resolver. The TS-side tooling
// (tsc --noEmit --strict, eslint, prettier) runs against the
// emitted .ts text in a separate subprocess; the Go tree is the
// canonical source of truth at lower time.
package tstree

import (
	"fmt"
	"strconv"
	"strings"
)

// SourceFile is a single emitted .ts file.
//
// Phase 1 keeps Imports empty and emits all helpers inline in
// Decls; Phase 4 wires the `@mochi/runtime` import map; Phase 15
// switches inline helpers to `import { mochi_print_str } from
// "@mochi/runtime/io"` once the runtime package ships.
type SourceFile struct {
	// HeaderComment is the leading `//` comment block emitted at
	// the top of the file. Conventionally one line stating the
	// file is generated; the emit pass adds it unconditionally.
	HeaderComment string

	// Imports lists ESM import statements emitted after the
	// header comment. Empty in Phase 1.
	Imports []*ImportDecl

	// Decls lists top-level declarations in source order. Phase
	// 1 holds FuncDecl entries only.
	Decls []Decl

	// TrailingExec is a list of statements emitted at module
	// top level after every declaration. For Phase 1 this holds
	// a single `mochi_main();` ExprStmt.
	TrailingExec []Stmt
}

// TsString emits the full file as TypeScript source text. The
// returned string ends with a single trailing newline.
func (f *SourceFile) TsString() string {
	var b strings.Builder
	if f.HeaderComment != "" {
		for _, line := range strings.Split(strings.TrimRight(f.HeaderComment, "\n"), "\n") {
			b.WriteString("// ")
			b.WriteString(line)
			b.WriteByte('\n')
		}
		b.WriteByte('\n')
	}
	for _, imp := range f.Imports {
		b.WriteString(imp.TsString(0))
		b.WriteByte('\n')
	}
	if len(f.Imports) > 0 {
		b.WriteByte('\n')
	}
	for i, d := range f.Decls {
		b.WriteString(d.TsString(0))
		b.WriteByte('\n')
		if i < len(f.Decls)-1 {
			b.WriteByte('\n')
		}
	}
	if len(f.TrailingExec) > 0 {
		b.WriteByte('\n')
		for _, s := range f.TrailingExec {
			b.WriteString(s.TsString(0))
			b.WriteByte('\n')
		}
	}
	return b.String()
}

// Decl is a top-level declaration node.
type Decl interface {
	TsString(indent int) string
	declNode()
}

// Stmt is a statement node (function body, top-level exec).
type Stmt interface {
	TsString(indent int) string
	stmtNode()
}

// Expr is an expression node.
type Expr interface {
	TsString(indent int) string
	exprNode()
}

// ImportDecl is an ESM `import { ... } from "..."` line.
type ImportDecl struct {
	// Names lists named imports in source order. When IsDefault
	// is true the first entry is the default import binding.
	Names []string
	// IsDefault, when true, emits `import X from "module"`
	// rather than `import { X } from "module"`. Mixed default +
	// named is not modelled in Phase 1.
	IsDefault bool
	// Module is the module specifier string. Always emitted in
	// double quotes.
	Module string
}

func (d *ImportDecl) TsString(_ int) string {
	if d.IsDefault {
		if len(d.Names) != 1 {
			panic("tstree: default ImportDecl needs exactly one name")
		}
		return fmt.Sprintf("import %s from %q;", d.Names[0], d.Module)
	}
	return fmt.Sprintf("import { %s } from %q;", strings.Join(d.Names, ", "), d.Module)
}

// FuncDecl is a top-level `function NAME(params): RET { body }`.
type FuncDecl struct {
	// Doc is the leading JSDoc-style comment, one line per entry.
	// Empty in Phase 1 hello-world.
	Doc []string
	// Modifiers lists `export`, `async`, etc. in source order.
	// Phase 1 emits no modifiers (helpers stay module-private).
	Modifiers []string
	// Name is the emitted identifier.
	Name string
	// Generics lists type parameters (e.g. ["T"], ["K", "V"]).
	// Phase 3 introduces this for the polymorphic list helpers.
	Generics []string
	// Params lists formal parameters.
	Params []FuncParam
	// ReturnType is the declared return type (e.g. "void",
	// "string"). Required; Phase 1 never elides.
	ReturnType string
	// Body is the statement list inside the braces. May be empty.
	Body []Stmt
}

// FuncParam is one formal parameter with its declared type.
type FuncParam struct {
	Name string
	Type string
}

func (d *FuncDecl) declNode() {}
func (d *FuncDecl) TsString(indent int) string {
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
	b.WriteString("function ")
	b.WriteString(d.Name)
	if len(d.Generics) > 0 {
		b.WriteByte('<')
		for i, g := range d.Generics {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(g)
		}
		b.WriteByte('>')
	}
	b.WriteByte('(')
	for i, p := range d.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(p.Name)
		b.WriteString(": ")
		b.WriteString(p.Type)
	}
	b.WriteString("): ")
	b.WriteString(d.ReturnType)
	b.WriteString(" {")
	if len(d.Body) == 0 {
		b.WriteByte('}')
		return b.String()
	}
	b.WriteByte('\n')
	for _, s := range d.Body {
		b.WriteString(s.TsString(indent + 1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// RawStmt is an escape hatch for emit-stable verbatim text. The
// text must not include a trailing newline; it must end with `;`
// when used as a statement. Phase 1 does not use this; later
// phases use it for printf-style runtime helpers.
type RawStmt struct {
	Text string
}

func (s *RawStmt) stmtNode() {}
func (s *RawStmt) TsString(indent int) string {
	return strings.Repeat("  ", indent) + s.Text
}

// ExprStmt wraps an Expr as a statement (the most common form for
// `f(x);` calls).
type ExprStmt struct {
	Expr Expr
}

func (s *ExprStmt) stmtNode() {}
func (s *ExprStmt) TsString(indent int) string {
	return strings.Repeat("  ", indent) + s.Expr.TsString(0) + ";"
}

// ReturnStmt is `return;` or `return E;`. Phase 1 emits the void
// form implicitly (entry function has no return); future phases
// add the explicit form.
type ReturnStmt struct {
	Value Expr // nil for `return;`
}

func (s *ReturnStmt) stmtNode() {}
func (s *ReturnStmt) TsString(indent int) string {
	if s.Value == nil {
		return strings.Repeat("  ", indent) + "return;"
	}
	return strings.Repeat("  ", indent) + "return " + s.Value.TsString(0) + ";"
}

// CallExpr is `callee(arg, arg, ...)`.
type CallExpr struct {
	Callee Expr
	Args   []Expr
}

func (e *CallExpr) exprNode() {}
func (e *CallExpr) TsString(_ int) string {
	var b strings.Builder
	b.WriteString(e.Callee.TsString(0))
	b.WriteByte('(')
	for i, a := range e.Args {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(a.TsString(0))
	}
	b.WriteByte(')')
	return b.String()
}

// IdentExpr is a bare identifier (function name, variable name).
type IdentExpr struct {
	Name string
}

func (e *IdentExpr) exprNode() {}
func (e *IdentExpr) TsString(_ int) string { return e.Name }

// StringLit is a string literal. The emitter quotes via Go's
// strconv.Quote, which produces a valid TS double-quoted string
// (TS accepts the JSON-superset escape sequences strconv emits).
type StringLit struct {
	Value string
}

func (e *StringLit) exprNode() {}
func (e *StringLit) TsString(_ int) string {
	return strconv.Quote(e.Value)
}

// IntLit is an integer literal emitted as a TS `number`. Phase 1
// emits only values that fit safely in a double; Phase 2 introduces
// the bigint vs number monomorphisation per MEP-52 §6 (int).
type IntLit struct {
	Value int64
}

func (e *IntLit) exprNode() {}
func (e *IntLit) TsString(_ int) string {
	return strconv.FormatInt(e.Value, 10)
}

// FloatLit is a IEEE 754 binary64 literal. Emitted via Go's
// strconv.FormatFloat 'g' -1 64 so round-trip matches vm3.
//
// NaN and +/-Inf are emitted as identifier expressions
// (`Number.NaN`, `Number.POSITIVE_INFINITY`, `Number.NEGATIVE_INFINITY`)
// since TS source has no NaN/Infinity literal form; consumers that
// want a literal write the math expression directly.
type FloatLit struct {
	Value float64
}

func (e *FloatLit) exprNode() {}
func (e *FloatLit) TsString(_ int) string {
	switch {
	case isNaN(e.Value):
		return "Number.NaN"
	case isPosInf(e.Value):
		return "Number.POSITIVE_INFINITY"
	case isNegInf(e.Value):
		return "Number.NEGATIVE_INFINITY"
	}
	s := strconv.FormatFloat(e.Value, 'g', -1, 64)
	// Go's 'g' formatter renders integer-valued floats without a
	// fractional part (e.g. "3" not "3.0"). TS will read "3" as
	// a number literal of value 3 either way, so no fix-up needed.
	return s
}

// BoolLit is `true` or `false`.
type BoolLit struct {
	Value bool
}

func (e *BoolLit) exprNode() {}
func (e *BoolLit) TsString(_ int) string {
	if e.Value {
		return "true"
	}
	return "false"
}

// isNaN/isPosInf/isNegInf avoid pulling in math just for three
// trivial predicates and keep tstree's import surface tiny.
func isNaN(f float64) bool    { return f != f }
func isPosInf(f float64) bool { return f > 1e308 && f == f*2 }
func isNegInf(f float64) bool { return f < -1e308 && f == f*2 }

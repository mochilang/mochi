package pysrc

import (
	"fmt"
	"strconv"
	"strings"
)

// Module is the top-level Python source file.
type Module struct {
	// FutureAnnotations emits `from __future__ import annotations` first.
	FutureAnnotations bool
	// Imports is the sorted list of import statements (ruff `I` rule sorts these).
	Imports []ImportStmt
	// Stmts is the body of the module after imports.
	Stmts []Stmt
}

// PySource returns the rendered .py source text terminated with a trailing newline.
func (m *Module) PySource() string {
	var sb strings.Builder
	if m.FutureAnnotations {
		sb.WriteString("from __future__ import annotations\n")
	}
	if m.FutureAnnotations && len(m.Imports) > 0 {
		sb.WriteByte('\n')
	}
	for _, imp := range m.Imports {
		sb.WriteString(imp.PyString(0))
		sb.WriteByte('\n')
	}
	// PEP 8: two blank lines between top-level imports and the first
	// top-level definition, and between successive top-level definitions.
	for i, s := range m.Stmts {
		if i == 0 {
			sb.WriteString("\n\n")
		} else {
			sb.WriteString("\n\n")
		}
		sb.WriteString(s.PyString(0))
		sb.WriteByte('\n')
	}
	return sb.String()
}

// ImportStmt is `from <module> import <names>` or `import <module>`.
type ImportStmt struct {
	// From is empty for plain `import x`; set for `from x import y, z`.
	From string
	// Names lists imported names in source order.
	Names []string
}

// PyString renders the import statement.
func (i ImportStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	if i.From == "" {
		return pad + "import " + strings.Join(i.Names, ", ")
	}
	return pad + "from " + i.From + " import " + strings.Join(i.Names, ", ")
}

// Stmt is one statement.
type Stmt interface {
	isStmt()
	PyString(indent int) string
}

// FunctionDef is `def name(...) -> ret:`.
type FunctionDef struct {
	Name       string
	Params     []Param
	ReturnType TypeRef
	Body       []Stmt
	// Decorators lists @decorator lines above the def.
	Decorators []string
	// Async emits `async def` instead of `def` (Phase 9+).
	Async bool
}

func (*FunctionDef) isStmt() {}

// Param is one formal parameter `name: Type` or `name: Type = default`.
type Param struct {
	Name    string
	Type    TypeRef
	Default Expr
}

// PyString renders the function definition.
func (f *FunctionDef) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	for _, dec := range f.Decorators {
		sb.WriteString(pad)
		sb.WriteByte('@')
		sb.WriteString(dec)
		sb.WriteByte('\n')
	}
	sb.WriteString(pad)
	if f.Async {
		sb.WriteString("async ")
	}
	sb.WriteString("def ")
	sb.WriteString(f.Name)
	sb.WriteByte('(')
	for i, p := range f.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Name)
		if p.Type.Name != "" {
			sb.WriteString(": ")
			sb.WriteString(p.Type.PyString())
		}
		if p.Default != nil {
			sb.WriteString(" = ")
			sb.WriteString(p.Default.PyString())
		}
	}
	sb.WriteByte(')')
	if f.ReturnType.Name != "" {
		sb.WriteString(" -> ")
		sb.WriteString(f.ReturnType.PyString())
	}
	sb.WriteString(":\n")
	if len(f.Body) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	} else {
		for i, s := range f.Body {
			if i > 0 {
				sb.WriteByte('\n')
			}
			sb.WriteString(s.PyString(indent + 1))
		}
	}
	return sb.String()
}

// ClassDef is a `class Name:` declaration with optional decorators and
// PEP 526 annotated attributes. Phase 3.4 uses this for record types
// emitted as `@dataclasses.dataclass(frozen=True, slots=True)`.
type ClassDef struct {
	Name       string
	Decorators []string
	Fields     []ClassField
	// Init is an optional explicit __init__ body. When non-empty the
	// emitter renders `def __init__(self, ...):` after the field
	// annotations. Used by Phase 9 agents whose mutable state cannot live
	// in a frozen dataclass.
	Init *FunctionDef
	// Methods are instance methods rendered after the field block (and
	// after Init when present). Used by Phase 9 agent intent methods.
	Methods []*FunctionDef
}

// ClassField is `name: Type` inside a class body.
type ClassField struct {
	Name string
	Type TypeRef
}

func (*ClassDef) isStmt() {}

// PyString renders the class declaration.
func (c *ClassDef) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	for _, dec := range c.Decorators {
		sb.WriteString(pad)
		sb.WriteByte('@')
		sb.WriteString(dec)
		sb.WriteByte('\n')
	}
	sb.WriteString(pad)
	sb.WriteString("class ")
	sb.WriteString(c.Name)
	sb.WriteString(":\n")
	if len(c.Fields) == 0 && c.Init == nil && len(c.Methods) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
		return sb.String()
	}
	wrote := false
	for i, f := range c.Fields {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(pad)
		sb.WriteString("    ")
		sb.WriteString(f.Name)
		sb.WriteString(": ")
		sb.WriteString(f.Type.PyString())
		wrote = true
	}
	if c.Init != nil {
		if wrote {
			sb.WriteString("\n\n")
		}
		sb.WriteString(c.Init.PyString(indent + 1))
		wrote = true
	}
	for _, m := range c.Methods {
		if wrote {
			sb.WriteString("\n\n")
		}
		sb.WriteString(m.PyString(indent + 1))
		wrote = true
	}
	return sb.String()
}

// KeywordArg is `name=value` inside a Call.
type KeywordArg struct {
	Name  string
	Value Expr
}

// UnionDef is a PEP 695 type alias declaration: `type Name = V1 | V2`.
// Phase 5 uses this for Mochi sum types, after the per-variant
// `@dataclass(frozen=True, slots=True)` classes are emitted.
type UnionDef struct {
	Name     string
	Variants []string
}

func (*UnionDef) isStmt() {}

// PyString renders the PEP 695 type alias on a single line.
func (u *UnionDef) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	return pad + "type " + u.Name + " = " + strings.Join(u.Variants, " | ")
}

// MatchStmt is a PEP 634 structural pattern match.
type MatchStmt struct {
	Target Expr
	Cases  []MatchCase
}

// MatchCase is one `case Pattern:` arm. Wildcard arms set Wildcard=true;
// otherwise Variant is the class name. Bindings render as keyword
// patterns `Variant(field=bind)`. An empty Bindings slice on a non-
// wildcard arm renders as `Variant()`, matching PEP 634 for nullary
// dataclass variants.
type MatchCase struct {
	Wildcard bool
	Variant  string
	Bindings []FieldBinding
	Guard    Expr
	Body     []Stmt
}

// FieldBinding is one keyword pattern field=binding inside a class pattern.
type FieldBinding struct {
	FieldName string
	BindName  string
}

func (*MatchStmt) isStmt() {}

// PyString renders the match statement.
func (m *MatchStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("match ")
	sb.WriteString(m.Target.PyString())
	sb.WriteString(":\n")
	for i, c := range m.Cases {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(pad)
		sb.WriteString("    case ")
		if c.Wildcard {
			sb.WriteByte('_')
		} else {
			sb.WriteString(c.Variant)
			sb.WriteByte('(')
			for j, b := range c.Bindings {
				if j > 0 {
					sb.WriteString(", ")
				}
				sb.WriteString(b.FieldName)
				sb.WriteByte('=')
				sb.WriteString(b.BindName)
			}
			sb.WriteByte(')')
		}
		if c.Guard != nil {
			sb.WriteString(" if ")
			sb.WriteString(c.Guard.PyString())
		}
		sb.WriteString(":\n")
		if len(c.Body) == 0 {
			sb.WriteString(pad)
			sb.WriteString("        pass")
			continue
		}
		for j, s := range c.Body {
			if j > 0 {
				sb.WriteByte('\n')
			}
			sb.WriteString(s.PyString(indent + 2))
		}
	}
	return sb.String()
}

// AnnotateStmt is a PEP 526 declaration-only annotation `name: Type`
// with no value. Used when a mutable binding is introduced before any
// assignment (Phase 5 match-expression lowering: the result var is
// declared, then every match arm assigns to it).
type AnnotateStmt struct {
	Target string
	Type   TypeRef
}

func (*AnnotateStmt) isStmt() {}

// PyString renders the annotation-only declaration.
func (s *AnnotateStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	return pad + s.Target + ": " + s.Type.PyString()
}

// IfStmt is `if cond:` and optional `else:` block.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (*IfStmt) isStmt() {}

// PyString renders the if statement.
func (s *IfStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("if ")
	sb.WriteString(s.Cond.PyString())
	sb.WriteString(":\n")
	for i, st := range s.Then {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(st.PyString(indent + 1))
	}
	if len(s.Else) > 0 {
		sb.WriteByte('\n')
		sb.WriteString(pad)
		sb.WriteString("else:\n")
		for i, st := range s.Else {
			if i > 0 {
				sb.WriteByte('\n')
			}
			sb.WriteString(st.PyString(indent + 1))
		}
	}
	return sb.String()
}

// ExprStmt is an expression evaluated for its side effect.
type ExprStmt struct {
	X Expr
}

func (*ExprStmt) isStmt() {}

// PyString renders the expression statement.
func (s *ExprStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + s.X.PyString()
}

// AssignStmt is `name: Type = value` (PEP 526 annotated) or `name = value`.
type AssignStmt struct {
	Target string
	Type   TypeRef
	Value  Expr
}

func (*AssignStmt) isStmt() {}

// PyString renders the assignment statement.
func (s *AssignStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	if s.Type.Name != "" {
		return fmt.Sprintf("%s%s: %s = %s", pad, s.Target, s.Type.PyString(), s.Value.PyString())
	}
	return fmt.Sprintf("%s%s = %s", pad, s.Target, s.Value.PyString())
}

// AttrAssignStmt is `obj.attr = value`. Used by Phase 9 to mutate agent
// state inside intent methods (`self.count = self.count + 1`).
type AttrAssignStmt struct {
	Target Expr
	Attr   string
	Value  Expr
}

func (*AttrAssignStmt) isStmt() {}

// PyString renders the attribute assignment.
func (s *AttrAssignStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	return fmt.Sprintf("%s%s.%s = %s", pad, s.Target.PyString(), s.Attr, s.Value.PyString())
}

// ReturnStmt is `return value` or bare `return`.
type ReturnStmt struct {
	Value Expr
}

func (*ReturnStmt) isStmt() {}

// PyString renders the return statement.
func (s *ReturnStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	if s.Value == nil {
		return pad + "return"
	}
	return pad + "return " + s.Value.PyString()
}

// RaiseStmt is `raise Exc(args, kw=v)`. Phase 11.0 uses this to lower
// Mochi `panic(code, msg)` to `raise MochiPanic(code, msg)`.
type RaiseStmt struct {
	Exc Expr
}

func (*RaiseStmt) isStmt() {}

// PyString renders the raise statement.
func (s *RaiseStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	if s.Exc == nil {
		return pad + "raise"
	}
	return pad + "raise " + s.Exc.PyString()
}

// TryExceptStmt is `try: ... except (E1, E2) as Bind: <prologue> ...`.
// Phase 11.0 lowers Mochi try/catch to a single except arm matching the
// MochiPanic family. CatchVar is the user-visible Mochi catch binding
// (an int code). The lowerer prepends a `CatchVar = _panic_code(__mp)`
// statement to the catch body so the rest of the body sees the canonical
// integer surface.
type TryExceptStmt struct {
	Body     []Stmt
	ExcTypes []string // identifiers for the except clause tuple, e.g. ["MochiPanic", "ZeroDivisionError", "IndexError"]
	BindName string   // `as <name>` binding (internal scratch, e.g. "__mp")
	Handler  []Stmt
}

func (*TryExceptStmt) isStmt() {}

// PyString renders the try/except statement.
func (s *TryExceptStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("try:\n")
	if len(s.Body) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	} else {
		for i, st := range s.Body {
			if i > 0 {
				sb.WriteByte('\n')
			}
			sb.WriteString(st.PyString(indent + 1))
		}
	}
	sb.WriteByte('\n')
	sb.WriteString(pad)
	sb.WriteString("except ")
	if len(s.ExcTypes) == 1 {
		sb.WriteString(s.ExcTypes[0])
	} else {
		sb.WriteByte('(')
		for i, t := range s.ExcTypes {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(t)
		}
		sb.WriteByte(')')
	}
	if s.BindName != "" {
		sb.WriteString(" as ")
		sb.WriteString(s.BindName)
	}
	sb.WriteString(":\n")
	if len(s.Handler) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	} else {
		for i, st := range s.Handler {
			if i > 0 {
				sb.WriteByte('\n')
			}
			sb.WriteString(st.PyString(indent + 1))
		}
	}
	return sb.String()
}

// PassStmt is the no-op `pass`.
type PassStmt struct{}

func (*PassStmt) isStmt() {}

// PyString renders the pass statement.
func (s *PassStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + "pass"
}

// Expr is one expression.
type Expr interface {
	isExpr()
	PyString() string
}

// Call is `f(args, kw=v)`.
type Call struct {
	Func   Expr
	Args   []Expr
	Kwargs []KeywordArg
}

func (*Call) isExpr() {}

// PyString renders the call expression.
func (c *Call) PyString() string {
	var sb strings.Builder
	sb.WriteString(c.Func.PyString())
	sb.WriteByte('(')
	first := true
	for _, a := range c.Args {
		if !first {
			sb.WriteString(", ")
		}
		first = false
		sb.WriteString(a.PyString())
	}
	for _, kw := range c.Kwargs {
		if !first {
			sb.WriteString(", ")
		}
		first = false
		sb.WriteString(kw.Name)
		sb.WriteByte('=')
		sb.WriteString(kw.Value.PyString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// BinaryEq is `left == right`. Kept as an alias for clarity at the
// __name__ == "__main__" guard site; equivalent to a BinaryExpr with Op="==".
type BinaryEq struct {
	Left  Expr
	Right Expr
}

func (*BinaryEq) isExpr() {}

// PyString renders the equality comparison.
func (b *BinaryEq) PyString() string {
	return b.Left.PyString() + " == " + b.Right.PyString()
}

// BinaryExpr is `left op right` for arithmetic, comparison, and boolean
// operators. Phase 2 only emits the operator forms; the operator string
// must already be the Python token (`+`, `-`, `*`, `/`, `//`, `%`,
// `==`, `!=`, `<`, `<=`, `>`, `>=`, `and`, `or`).
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (*BinaryExpr) isExpr() {}

// PyString renders the binary expression, parenthesising children so
// nested arithmetic and boolean expressions print unambiguously.
// Phase 2 keeps a conservative bracket policy (always parenthesise),
// matching what `ruff format` produces for these structures.
func (b *BinaryExpr) PyString() string {
	switch b.Op {
	case "and", "or":
		return "(" + b.Left.PyString() + " " + b.Op + " " + b.Right.PyString() + ")"
	}
	return "(" + b.Left.PyString() + " " + b.Op + " " + b.Right.PyString() + ")"
}

// UnaryExpr is `op operand`. Phase 2 ships `-` (negation) and `not`.
type UnaryExpr struct {
	Op      string
	Operand Expr
}

func (*UnaryExpr) isExpr() {}

// PyString renders the unary expression.
func (u *UnaryExpr) PyString() string {
	if u.Op == "not" {
		return "(not " + u.Operand.PyString() + ")"
	}
	return "(" + u.Op + u.Operand.PyString() + ")"
}

// ListLit is `[e1, e2, ...]`. The element type is implicit in the
// surrounding annotation (PEP 526 form `xs: list[int] = [...]`).
type ListLit struct {
	Elems []Expr
}

func (*ListLit) isExpr() {}

// PyString renders the list literal.
func (l *ListLit) PyString() string {
	var sb strings.Builder
	sb.WriteByte('[')
	for i, e := range l.Elems {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(e.PyString())
	}
	sb.WriteByte(']')
	return sb.String()
}

// SetLit is `{e1, e2, ...}` (Python set display). Empty sets cannot
// use `{}` (that is a dict literal); the lowerer emits `set()` via
// a `Call(Name "set", nil)` instead, so SetLit is only used with at
// least one element.
type SetLit struct {
	Elems []Expr
}

func (*SetLit) isExpr() {}

// PyString renders the set literal.
func (s *SetLit) PyString() string {
	var sb strings.Builder
	sb.WriteByte('{')
	for i, e := range s.Elems {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(e.PyString())
	}
	sb.WriteByte('}')
	return sb.String()
}

// DictLit is `{k1: v1, k2: v2, ...}` (PEP 448 dict display).
type DictLit struct {
	Keys   []Expr
	Values []Expr
}

func (*DictLit) isExpr() {}

// PyString renders the dict literal.
func (d *DictLit) PyString() string {
	var sb strings.Builder
	sb.WriteByte('{')
	for i := range d.Keys {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(d.Keys[i].PyString())
		sb.WriteString(": ")
		sb.WriteString(d.Values[i].PyString())
	}
	sb.WriteByte('}')
	return sb.String()
}

// IndexAssignStmt is `target[key] = value` (Python subscript assignment).
type IndexAssignStmt struct {
	Target Expr
	Key    Expr
	Value  Expr
}

func (*IndexAssignStmt) isStmt() {}

// PyString renders the index-assign statement.
func (s *IndexAssignStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	return pad + s.Target.PyString() + "[" + s.Key.PyString() + "] = " + s.Value.PyString()
}

// SliceExpr is `receiver[start:end]`. Either bound may be nil for
// open-ended slices (Python `xs[:n]` / `xs[n:]`).
type SliceExpr struct {
	Receiver Expr
	Start    Expr
	End      Expr
}

func (*SliceExpr) isExpr() {}

// PyString renders the slice expression.
func (s *SliceExpr) PyString() string {
	var sb strings.Builder
	sb.WriteString(s.Receiver.PyString())
	sb.WriteByte('[')
	if s.Start != nil {
		sb.WriteString(s.Start.PyString())
	}
	sb.WriteByte(':')
	if s.End != nil {
		sb.WriteString(s.End.PyString())
	}
	sb.WriteByte(']')
	return sb.String()
}

// IndexExpr is `receiver[index]`.
type IndexExpr struct {
	Receiver Expr
	Index    Expr
}

func (*IndexExpr) isExpr() {}

// PyString renders the index expression.
func (i *IndexExpr) PyString() string {
	return i.Receiver.PyString() + "[" + i.Index.PyString() + "]"
}

// WhileStmt is `while cond:` followed by a body.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (*WhileStmt) isStmt() {}

// PyString renders the while statement.
func (s *WhileStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("while ")
	sb.WriteString(s.Cond.PyString())
	sb.WriteString(":\n")
	for i, st := range s.Body {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(st.PyString(indent + 1))
	}
	if len(s.Body) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	}
	return sb.String()
}

// ForEachStmt is `for var in iter:` over an arbitrary iterable expression
// (list, string, range, etc.). Phase 3.1 uses this for `for x in xs`.
type ForEachStmt struct {
	Var  string
	Iter Expr
	Body []Stmt
}

func (*ForEachStmt) isStmt() {}

// PyString renders the for-each statement.
func (s *ForEachStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("for ")
	sb.WriteString(s.Var)
	sb.WriteString(" in ")
	sb.WriteString(s.Iter.PyString())
	sb.WriteString(":\n")
	for i, st := range s.Body {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(st.PyString(indent + 1))
	}
	if len(s.Body) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	}
	return sb.String()
}

// ForRangeStmt is `for var in range(start, end):`.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (*ForRangeStmt) isStmt() {}

// PyString renders the for-range statement.
func (s *ForRangeStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("for ")
	sb.WriteString(s.Var)
	sb.WriteString(" in range(")
	sb.WriteString(s.Start.PyString())
	sb.WriteString(", ")
	sb.WriteString(s.End.PyString())
	sb.WriteString("):\n")
	for i, st := range s.Body {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(st.PyString(indent + 1))
	}
	if len(s.Body) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	}
	return sb.String()
}

// BreakStmt is `break`.
type BreakStmt struct{}

func (*BreakStmt) isStmt() {}

// PyString renders `break`.
func (s *BreakStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + "break"
}

// ContinueStmt is `continue`.
type ContinueStmt struct{}

func (*ContinueStmt) isStmt() {}

// PyString renders `continue`.
func (s *ContinueStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + "continue"
}

// ReassignStmt is plain `target = value` without a PEP 526 annotation,
// used when reassigning an already-declared mutable variable.
type ReassignStmt struct {
	Target string
	Value  Expr
}

func (*ReassignStmt) isStmt() {}

// PyString renders the bare assignment.
func (s *ReassignStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + s.Target + " = " + s.Value.PyString()
}

// Name is a bare identifier.
type Name struct {
	Id string
}

func (*Name) isExpr() {}

// PyString returns the identifier.
func (n *Name) PyString() string { return n.Id }

// Attribute is `value.attr`.
type Attribute struct {
	Value Expr
	Attr  string
}

func (*Attribute) isExpr() {}

// PyString renders the attribute access.
func (a *Attribute) PyString() string {
	return a.Value.PyString() + "." + a.Attr
}

// StrLit is a Python string literal. The renderer uses strconv.Quote
// which produces a Go-style double-quoted string; this happens to be
// valid Python because both languages use the same escape table for
// the characters Phase 1 exercises (\n, \t, \\, \", \xNN). When Phase
// 5+ introduces interpolation, this widens to a JoinedStr node.
type StrLit struct {
	Value string
}

func (*StrLit) isExpr() {}

// PyString returns a double-quoted Python string literal.
func (s *StrLit) PyString() string {
	return strconv.Quote(s.Value)
}

// IntLit is a Python integer literal. Mochi int (int64) is Python int (arbitrary precision).
type IntLit struct {
	Value int64
}

func (*IntLit) isExpr() {}

// PyString returns the decimal int literal.
func (i *IntLit) PyString() string {
	return strconv.FormatInt(i.Value, 10)
}

// FloatLit is a Python float literal. Mochi float (binary64) is Python float (binary64).
type FloatLit struct {
	Value float64
}

func (*FloatLit) isExpr() {}

// PyString returns the float literal. NaN/Inf canonicalisation is deferred to Phase 2.1.
func (f *FloatLit) PyString() string {
	return strconv.FormatFloat(f.Value, 'g', -1, 64)
}

// BoolLit is `True` or `False`.
type BoolLit struct {
	Value bool
}

func (*BoolLit) isExpr() {}

// PyString returns "True" or "False".
func (b *BoolLit) PyString() string {
	if b.Value {
		return "True"
	}
	return "False"
}

// TypeRef is a Python annotation reference (e.g. "int", "str", "None", "list[int]").
type TypeRef struct {
	Name string
}

// PyString returns the type name verbatim.
func (t TypeRef) PyString() string { return t.Name }

// Predefined type refs.
var (
	TypeNone  = TypeRef{Name: "None"}
	TypeInt   = TypeRef{Name: "int"}
	TypeFloat = TypeRef{Name: "float"}
	TypeBool  = TypeRef{Name: "bool"}
	TypeStr   = TypeRef{Name: "str"}
)

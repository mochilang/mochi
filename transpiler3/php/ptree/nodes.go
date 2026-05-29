package ptree

import (
	"fmt"
	"strconv"
	"strings"
)

// indent returns a string of n*4 spaces, matching PER-CS2.0.
func indent(n int) string {
	return strings.Repeat("    ", n)
}

// ---- Top-level ----

// PhpFile is one .php source file. The emitter writes one file
// per Mochi module plus one per record / sum-type declaration.
type PhpFile struct {
	// Namespace is the PSR-4 namespace (e.g. "Mochi\\User"). Empty
	// for files that intentionally live in the global namespace.
	Namespace string
	// Uses is the list of `use Foo\Bar;` import statements. The
	// emitter sorts these alphabetically at render time so the
	// emitted file is reproducible.
	Uses []string
	// Decls is the ordered list of top-level declarations
	// (classes, functions, interfaces, enums). The emitter does
	// not reorder these: phase 16 reproducibility relies on the
	// lowerer producing a deterministic order.
	Decls []Decl
	// TrailingExec is an optional list of expression statements
	// the emitter appends after all Decls (used by the file that
	// owns `mochi_main();` at the bottom of main.php).
	TrailingExec []Stmt
}

// PhpSource returns the full PHP source text for this file.
func (f *PhpFile) PhpSource() string {
	var sb strings.Builder
	sb.WriteString("<?php\n")
	sb.WriteString("\n")
	sb.WriteString("declare(strict_types=1);\n")
	sb.WriteString("\n")
	if f.Namespace != "" {
		fmt.Fprintf(&sb, "namespace %s;\n\n", f.Namespace)
	}
	uses := append([]string(nil), f.Uses...)
	// Sort uses alphabetically for reproducibility. The lowerer
	// may add duplicates from independent passes; we de-dup here.
	sortStringsUnique(&uses)
	for _, u := range uses {
		fmt.Fprintf(&sb, "use %s;\n", u)
	}
	if len(uses) > 0 {
		sb.WriteString("\n")
	}
	for i, d := range f.Decls {
		if i > 0 {
			sb.WriteString("\n")
		}
		sb.WriteString(d.PhpString(0))
		sb.WriteString("\n")
	}
	for _, s := range f.TrailingExec {
		sb.WriteString("\n")
		sb.WriteString(s.PhpString(0))
		sb.WriteString("\n")
	}
	return sb.String()
}

func sortStringsUnique(xs *[]string) {
	seen := make(map[string]struct{}, len(*xs))
	out := (*xs)[:0]
	for _, s := range *xs {
		if _, ok := seen[s]; ok {
			continue
		}
		seen[s] = struct{}{}
		out = append(out, s)
	}
	*xs = out
	// Insertion sort: deterministic and small N (a few imports).
	for i := 1; i < len(*xs); i++ {
		j := i
		for j > 0 && (*xs)[j-1] > (*xs)[j] {
			(*xs)[j-1], (*xs)[j] = (*xs)[j], (*xs)[j-1]
			j--
		}
	}
}

// ---- Decl interface ----

// Decl is a top-level declaration.
type Decl interface {
	phpDecl()
	PhpString(ind int) string
}

// RawDecl is verbatim PHP source spliced into the output. Used for
// inline runtime classes whose shape is fixed (e.g. MochiStream in
// Phase 10) so the lowerer doesn't pay a ClassDecl roundtrip.
type RawDecl struct {
	Text string
}

func (*RawDecl) phpDecl() {}

func (d *RawDecl) PhpString(_ int) string { return strings.TrimRight(d.Text, "\n") }

// FuncDecl is a top-level function declaration.
type FuncDecl struct {
	// PhpDoc is the optional docblock written immediately above
	// the `function` keyword. Each entry is one line including
	// the leading `* `. The emitter wraps it in `/** ... */`.
	PhpDoc []string
	// Name is the function name. The lowerer mangles Mochi names
	// to PHP-safe identifiers (see lower.MangleName).
	Name       string
	Params     []FuncParam
	ReturnType string // empty means no return annotation; "void" emits explicit ": void"
	Body       []Stmt
}

// FuncParam is one parameter of a FuncDecl or MethodDecl.
type FuncParam struct {
	// TypeName is the PHP type expression, e.g. "int", "string", "?Foo".
	TypeName string
	Name     string
	// Default is the optional default-value expression (already PHP-rendered).
	Default Expr
	// ByRef passes the parameter by reference (`&$name`). Off by default.
	ByRef bool
}

func (*FuncDecl) phpDecl() {}

func (f *FuncDecl) PhpString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	if len(f.PhpDoc) > 0 {
		sb.WriteString(pad + "/**\n")
		for _, line := range f.PhpDoc {
			sb.WriteString(pad + " * " + line + "\n")
		}
		sb.WriteString(pad + " */\n")
	}
	sb.WriteString(pad)
	sb.WriteString("function ")
	sb.WriteString(f.Name)
	sb.WriteByte('(')
	for i, p := range f.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		writeParam(&sb, p)
	}
	sb.WriteByte(')')
	if f.ReturnType != "" {
		sb.WriteString(": ")
		sb.WriteString(f.ReturnType)
	}
	sb.WriteString("\n")
	sb.WriteString(pad + "{\n")
	for _, st := range f.Body {
		sb.WriteString(st.PhpString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

func writeParam(sb *strings.Builder, p FuncParam) {
	if p.TypeName != "" {
		sb.WriteString(p.TypeName)
		sb.WriteByte(' ')
	}
	if p.ByRef {
		sb.WriteByte('&')
	}
	sb.WriteByte('$')
	sb.WriteString(p.Name)
	if p.Default != nil {
		sb.WriteString(" = ")
		sb.WriteString(p.Default.PhpString())
	}
}

// ---- Stmt interface ----

// Stmt is a statement.
type Stmt interface {
	phpStmt()
	PhpString(ind int) string
}

// ExprStmt is `<expr>;`.
type ExprStmt struct {
	Expr Expr
}

func (*ExprStmt) phpStmt() {}

func (s *ExprStmt) PhpString(ind int) string {
	return indent(ind) + s.Expr.PhpString() + ";"
}

// ReturnStmt is `return [<expr>];`.
type ReturnStmt struct {
	Value Expr // nil for bare `return;`
}

func (*ReturnStmt) phpStmt() {}

func (s *ReturnStmt) PhpString(ind int) string {
	if s.Value == nil {
		return indent(ind) + "return;"
	}
	return indent(ind) + "return " + s.Value.PhpString() + ";"
}

// AssignStmt is `$name = <value>;`. Used for both `let` (first
// binding) and reassignment to a `var` binding. PHP has no
// declaration form, so both lower the same way.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (*AssignStmt) phpStmt() {}

func (s *AssignStmt) PhpString(ind int) string {
	return indent(ind) + "$" + s.Name + " = " + s.Value.PhpString() + ";"
}

// IfStmt is `if (<cond>) { <then> } else { <else> }`. The else
// branch may be empty (nil) to emit just the if.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt // nil means no else arm
}

func (*IfStmt) phpStmt() {}

func (s *IfStmt) PhpString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("if (")
	sb.WriteString(s.Cond.PhpString())
	sb.WriteString(") {\n")
	for _, st := range s.Then {
		sb.WriteString(st.PhpString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	if s.Else != nil {
		sb.WriteString(" else {\n")
		for _, st := range s.Else {
			sb.WriteString(st.PhpString(ind + 1))
			sb.WriteString("\n")
		}
		sb.WriteString(pad)
		sb.WriteString("}")
	}
	return sb.String()
}

// WhileStmt is `while (<cond>) { <body> }`.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (*WhileStmt) phpStmt() {}

func (s *WhileStmt) PhpString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("while (")
	sb.WriteString(s.Cond.PhpString())
	sb.WriteString(") {\n")
	for _, st := range s.Body {
		sb.WriteString(st.PhpString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	return sb.String()
}

// ForRangeStmt is `for ($var = start; $var < end; $var++) { body }`,
// emitted for Mochi `for x in start..end`. The induction variable is
// treated as immutable in the source but PHP doesn't enforce that;
// the lowerer rejects assignment to it before reaching emit.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (*ForRangeStmt) phpStmt() {}

func (s *ForRangeStmt) PhpString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad)
	fmt.Fprintf(&sb, "for ($%s = ", s.Var)
	sb.WriteString(s.Start.PhpString())
	fmt.Fprintf(&sb, "; $%s < ", s.Var)
	sb.WriteString(s.End.PhpString())
	fmt.Fprintf(&sb, "; $%s++) {\n", s.Var)
	for _, st := range s.Body {
		sb.WriteString(st.PhpString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	return sb.String()
}

// BreakStmt is `break;`.
type BreakStmt struct{}

func (*BreakStmt) phpStmt() {}

func (s *BreakStmt) PhpString(ind int) string { return indent(ind) + "break;" }

// ContinueStmt is `continue;`.
type ContinueStmt struct{}

func (*ContinueStmt) phpStmt() {}

func (s *ContinueStmt) PhpString(ind int) string { return indent(ind) + "continue;" }

// RawStmt is a verbatim PHP statement, used by the lowerer for
// edge cases that do not yet have first-class node types. The
// text must include the trailing semicolon (or block braces) the
// caller wants emitted.
type RawStmt struct {
	Text string
}

func (*RawStmt) phpStmt() {}

func (s *RawStmt) PhpString(ind int) string {
	// Re-indent each line of Text by the requested level.
	lines := strings.Split(s.Text, "\n")
	pad := indent(ind)
	for i, ln := range lines {
		if ln == "" {
			continue
		}
		lines[i] = pad + ln
	}
	return strings.Join(lines, "\n")
}

// ---- Expr interface ----

// Expr is an expression.
type Expr interface {
	phpExpr()
	PhpString() string
}

// CallExpr is `<callee>(<args...>)`.
type CallExpr struct {
	Callee Expr
	Args   []Expr
}

func (*CallExpr) phpExpr() {}

func (e *CallExpr) PhpString() string {
	var sb strings.Builder
	sb.WriteString(e.Callee.PhpString())
	sb.WriteByte('(')
	for i, a := range e.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.PhpString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// StaticCallExpr is `<class>::<method>(<args...>)`.
type StaticCallExpr struct {
	Class  string
	Method string
	Args   []Expr
}

func (*StaticCallExpr) phpExpr() {}

func (e *StaticCallExpr) PhpString() string {
	var sb strings.Builder
	sb.WriteString(e.Class)
	sb.WriteString("::")
	sb.WriteString(e.Method)
	sb.WriteByte('(')
	for i, a := range e.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.PhpString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// IdentExpr is a bare identifier (function name, class name, or
// constant). For PHP variables use VarExpr.
type IdentExpr struct {
	Name string
}

func (*IdentExpr) phpExpr() {}

func (e *IdentExpr) PhpString() string {
	return e.Name
}

// VarExpr is `$name`.
type VarExpr struct {
	Name string
}

func (*VarExpr) phpExpr() {}

func (e *VarExpr) PhpString() string {
	return "$" + e.Name
}

// StringLit is a double-quoted PHP string literal. The
// constructor escapes the value so we always emit a safe
// double-quoted form.
type StringLit struct {
	Value string
}

func (*StringLit) phpExpr() {}

func (e *StringLit) PhpString() string {
	// Use strconv.Quote to get Go-style escaping, then fix the
	// two differences between Go and PHP double-quoted strings:
	//   1. PHP interpolates $variables and {$expr} in
	//      double-quoted strings. We escape $ to suppress that.
	//   2. PHP does not recognise \xNN inside double-quoted
	//      strings the same way Go does for non-ASCII bytes;
	//      strconv.Quote only emits \xNN for non-printable ASCII
	//      and uses \uNNNN / \UNNNNNNNN for non-ASCII runes.
	//      Both forms parse identically in PHP 8.4 because we
	//      preserve the unicode escape with the leading backslash.
	q := strconv.Quote(e.Value)
	// strconv.Quote returns a Go-style string with escapes that PHP
	// also recognises in double-quoted strings (\n, \r, \t, \", \\),
	// plus \u{HHHH}-style unicode escapes that we need to massage.
	// For Phase 0 we keep it simple: replace any "\u" sequence Go
	// produced for non-ASCII with the literal UTF-8 byte sequence
	// PHP interprets natively.
	q = strings.ReplaceAll(q, "$", "\\$")
	return q
}

// IntLit is a PHP integer literal.
type IntLit struct {
	Value int64
}

func (*IntLit) phpExpr() {}

func (e *IntLit) PhpString() string {
	return strconv.FormatInt(e.Value, 10)
}

// FloatLit is a PHP float literal. The emitter formats with Go's
// 'g' format so values round-trip the same way vm3 prints them.
type FloatLit struct {
	Value float64
}

func (*FloatLit) phpExpr() {}

func (e *FloatLit) PhpString() string {
	return strconv.FormatFloat(e.Value, 'g', -1, 64)
}

// BoolLit is `true` or `false`.
type BoolLit struct {
	Value bool
}

func (*BoolLit) phpExpr() {}

func (e *BoolLit) PhpString() string {
	if e.Value {
		return "true"
	}
	return "false"
}

// NullLit is `null`.
type NullLit struct{}

func (*NullLit) phpExpr() {}

func (e *NullLit) PhpString() string {
	return "null"
}

// RawExpr is a verbatim PHP expression fragment.
type RawExpr struct {
	Text string
}

func (*RawExpr) phpExpr() {}

func (e *RawExpr) PhpString() string {
	return e.Text
}

// BinaryExpr is `(<left> <op> <right>)`. The parentheses are
// always emitted so the operator precedence of the source program
// is preserved without the lowerer having to track PHP's
// precedence table.
type BinaryExpr struct {
	Op    string // PHP operator string (e.g. "+", "===", "&&", "intdiv")
	Left  Expr
	Right Expr
	// IsCall is true when Op is a function name like "intdiv".
	// The emitter renders `intdiv(left, right)` instead of an infix.
	IsCall bool
}

func (*BinaryExpr) phpExpr() {}

func (e *BinaryExpr) PhpString() string {
	if e.IsCall {
		return e.Op + "(" + e.Left.PhpString() + ", " + e.Right.PhpString() + ")"
	}
	return "(" + e.Left.PhpString() + " " + e.Op + " " + e.Right.PhpString() + ")"
}

// UnaryExpr is `(<op><operand>)`. Like BinaryExpr the outer parens
// are always emitted so source precedence survives lowering.
type UnaryExpr struct {
	Op      string // "-" or "!"
	Operand Expr
}

func (*UnaryExpr) phpExpr() {}

func (e *UnaryExpr) PhpString() string {
	return "(" + e.Op + e.Operand.PhpString() + ")"
}

// CastExpr is `(<targetType>) <operand>`. PHP has cast operators
// like (int), (float), (string), (bool). Phase 2 uses this to lower
// Mochi's `int(x)` truncating float-to-int cast.
type CastExpr struct {
	TargetType string // "int", "float", "string", "bool"
	Operand    Expr
}

func (*CastExpr) phpExpr() {}

func (e *CastExpr) PhpString() string {
	return "(" + e.TargetType + ") " + e.Operand.PhpString()
}

// ArrayLit is a PHP array literal. When Keys is empty it renders as a
// list literal `[a, b, c]`. When Keys is non-empty it renders as an
// associative literal `[k1 => v1, k2 => v2]` (PHP preserves insertion
// order, which matches Mochi map ordering).
type ArrayLit struct {
	Elems  []Expr // list form
	Keys   []Expr // associative form (paired with Values)
	Values []Expr
}

func (*ArrayLit) phpExpr() {}

func (e *ArrayLit) PhpString() string {
	var sb strings.Builder
	sb.WriteByte('[')
	if len(e.Keys) > 0 {
		for i := range e.Keys {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(e.Keys[i].PhpString())
			sb.WriteString(" => ")
			sb.WriteString(e.Values[i].PhpString())
		}
	} else {
		for i, x := range e.Elems {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(x.PhpString())
		}
	}
	sb.WriteByte(']')
	return sb.String()
}

// ArrayAppendExpr is `[...<inner>, <tail>]`. Lowers Mochi's
// `append(xs, v)` (functional, non-mutating).
type ArrayAppendExpr struct {
	Inner Expr
	Tail  Expr
}

func (*ArrayAppendExpr) phpExpr() {}

func (e *ArrayAppendExpr) PhpString() string {
	return "[..." + e.Inner.PhpString() + ", " + e.Tail.PhpString() + "]"
}

// IndexExpr is `<recv>[<idx>]`.
type IndexExpr struct {
	Receiver Expr
	Index    Expr
}

func (*IndexExpr) phpExpr() {}

func (e *IndexExpr) PhpString() string {
	return e.Receiver.PhpString() + "[" + e.Index.PhpString() + "]"
}

// ForEachStmt is `foreach (<source> as $<var>) { <body> }`.
type ForEachStmt struct {
	Var    string
	Source Expr
	Body   []Stmt
}

func (*ForEachStmt) phpStmt() {}

func (s *ForEachStmt) PhpString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("foreach (")
	sb.WriteString(s.Source.PhpString())
	sb.WriteString(" as $")
	sb.WriteString(s.Var)
	sb.WriteString(") {\n")
	for _, st := range s.Body {
		sb.WriteString(st.PhpString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	return sb.String()
}

// IndexAssignStmt is `$<name>[<key>] = <value>;`. Used for
// MapPutStmt and (later) list element assignment.
type IndexAssignStmt struct {
	Name  string
	Key   Expr
	Value Expr
}

func (*IndexAssignStmt) phpStmt() {}

func (s *IndexAssignStmt) PhpString(ind int) string {
	return indent(ind) + "$" + s.Name + "[" + s.Key.PhpString() + "] = " + s.Value.PhpString() + ";"
}

// ClassField is one field of a ClassDecl. Phase 4 records emit each
// field as a promoted constructor parameter (public readonly int $x).
type ClassField struct {
	TypeName string
	Name     string
	// Default is the optional default-value expression rendered into
	// the constructor signature (`public int $count = 0`). Used by
	// agent classes so `let c = Counter { ... }` matches Mochi's
	// agent-literal semantics with positional defaults.
	Default Expr
}

// MethodDecl is one method on a ClassDecl. Used for agent intents
// (Phase 9): each `intent Name(...) { ... }` becomes one public
// instance method whose body reads and writes the agent's mutable
// fields through `$this->FIELD`.
type MethodDecl struct {
	Name       string
	Params     []FuncParam
	ReturnType string
	Body       []Stmt
}

// ClassDecl is a PHP class declaration. By default emits as a
// `final readonly class Name`. Set Abstract=true for the base of a
// sealed sum-type hierarchy (omits readonly + final, adds abstract).
// Set Mutable=true for agent classes (omits readonly, fields are
// publicly assignable, body can carry methods). Set Extends to
// chain to a parent class.
type ClassDecl struct {
	Name     string
	Fields   []ClassField
	Methods  []MethodDecl
	Abstract bool   // emits as `abstract class Name`
	Mutable  bool   // agent classes: omit readonly so intents can mutate fields
	Extends  string // optional parent class
	// PhpDoc is the optional docblock written above the class keyword.
	PhpDoc []string
}

func (*ClassDecl) phpDecl() {}

func (d *ClassDecl) PhpString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	if len(d.PhpDoc) > 0 {
		sb.WriteString(pad + "/**\n")
		for _, line := range d.PhpDoc {
			sb.WriteString(pad + " * " + line + "\n")
		}
		sb.WriteString(pad + " */\n")
	}
	sb.WriteString(pad)
	switch {
	case d.Abstract:
		// PHP 8.4: a readonly subclass can only extend a readonly base,
		// so sum-type bases must be `abstract readonly` to admit
		// `final readonly` variants.
		sb.WriteString("abstract readonly class ")
	case d.Mutable:
		sb.WriteString("final class ")
	default:
		sb.WriteString("final readonly class ")
	}
	sb.WriteString(d.Name)
	if d.Extends != "" {
		sb.WriteString(" extends ")
		sb.WriteString(d.Extends)
	}
	sb.WriteString("\n")
	sb.WriteString(pad + "{\n")
	if d.Abstract {
		// Abstract sum-type base: body is intentionally empty.
	} else if len(d.Fields) == 0 {
		sb.WriteString(indent(ind+1) + "public function __construct() {}\n")
	} else {
		sb.WriteString(indent(ind+1) + "public function __construct(\n")
		for _, f := range d.Fields {
			sb.WriteString(indent(ind+2) + "public " + f.TypeName + " $" + f.Name)
			if f.Default != nil {
				sb.WriteString(" = ")
				sb.WriteString(f.Default.PhpString())
			}
			sb.WriteString(",\n")
		}
		sb.WriteString(indent(ind+1) + ") {}\n")
	}
	for _, m := range d.Methods {
		sb.WriteString("\n")
		sb.WriteString(indent(ind+1) + "public function ")
		sb.WriteString(m.Name)
		sb.WriteByte('(')
		for i, p := range m.Params {
			if i > 0 {
				sb.WriteString(", ")
			}
			writeParam(&sb, p)
		}
		sb.WriteByte(')')
		if m.ReturnType != "" {
			sb.WriteString(": ")
			sb.WriteString(m.ReturnType)
		}
		sb.WriteString("\n")
		sb.WriteString(indent(ind+1) + "{\n")
		for _, st := range m.Body {
			sb.WriteString(st.PhpString(ind + 2))
			sb.WriteString("\n")
		}
		sb.WriteString(indent(ind+1) + "}\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// InstanceOfExpr is `(<receiver> instanceof <class>)`.
type InstanceOfExpr struct {
	Receiver  Expr
	ClassName string
}

func (*InstanceOfExpr) phpExpr() {}

func (e *InstanceOfExpr) PhpString() string {
	return "(" + e.Receiver.PhpString() + " instanceof " + e.ClassName + ")"
}

// IfBranch is one conditional arm of a ChainedIfStmt.
type IfBranch struct {
	Cond Expr
	Body []Stmt
}

// ChainedIfStmt is `if (c1) {} elseif (c2) {} else {}`. Used to
// lower Mochi's `match` against a sealed sum type into a discriminator
// chain that PHP's JIT can fold into a tagged jump.
type ChainedIfStmt struct {
	Branches []IfBranch
	Default  []Stmt // nil means no else arm
}

func (*ChainedIfStmt) phpStmt() {}

func (s *ChainedIfStmt) PhpString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	for i, br := range s.Branches {
		if i == 0 {
			sb.WriteString(pad)
			sb.WriteString("if (")
		} else {
			sb.WriteString(" elseif (")
		}
		sb.WriteString(br.Cond.PhpString())
		sb.WriteString(") {\n")
		for _, st := range br.Body {
			sb.WriteString(st.PhpString(ind + 1))
			sb.WriteString("\n")
		}
		sb.WriteString(pad)
		sb.WriteString("}")
	}
	if s.Default != nil {
		sb.WriteString(" else {\n")
		for _, st := range s.Default {
			sb.WriteString(st.PhpString(ind + 1))
			sb.WriteString("\n")
		}
		sb.WriteString(pad)
		sb.WriteString("}")
	}
	return sb.String()
}

// NamedArg is one named argument to a NewExpr (`field: value`).
type NamedArg struct {
	Name  string
	Value Expr
}

// NewExpr is `new ClassName(field1: v1, field2: v2)`. PHP 8.0+ named
// arguments let the lowerer pass record fields in any order, which
// keeps the emitter source-faithful to the Mochi literal.
type NewExpr struct {
	Class string
	Args  []NamedArg
}

func (*NewExpr) phpExpr() {}

func (e *NewExpr) PhpString() string {
	var sb strings.Builder
	sb.WriteString("new ")
	sb.WriteString(e.Class)
	sb.WriteByte('(')
	for i, a := range e.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.Name)
		sb.WriteString(": ")
		sb.WriteString(a.Value.PhpString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// ClosureExpr is a PHP arrow function: `fn(<params>): <ret> => <body>`.
// Arrow functions auto-capture variables from the enclosing scope by
// value, which matches Mochi's by-value capture semantics. The body
// is a single expression because Phase 6's FunLit always lowers to
// a single call into the lifted function (which carries the real
// closure body).
type ClosureExpr struct {
	Params     []FuncParam
	ReturnType string
	Body       Expr
}

func (*ClosureExpr) phpExpr() {}

func (e *ClosureExpr) PhpString() string {
	var sb strings.Builder
	sb.WriteString("fn(")
	for i, p := range e.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		writeParam(&sb, p)
	}
	sb.WriteByte(')')
	if e.ReturnType != "" {
		sb.WriteString(": ")
		sb.WriteString(e.ReturnType)
	}
	sb.WriteString(" => ")
	sb.WriteString(e.Body.PhpString())
	return sb.String()
}

// PropAccessExpr is `<recv>-><field>`. Used to read record fields.
type PropAccessExpr struct {
	Receiver Expr
	Field    string
}

func (*PropAccessExpr) phpExpr() {}

func (e *PropAccessExpr) PhpString() string {
	return e.Receiver.PhpString() + "->" + e.Field
}

// MethodCallExpr is `<recv>-><method>(<args>)`. Used by Phase 9 to
// dispatch agent intent calls onto an agent instance.
type MethodCallExpr struct {
	Receiver Expr
	Method   string
	Args     []Expr
}

func (*MethodCallExpr) phpExpr() {}

func (e *MethodCallExpr) PhpString() string {
	var sb strings.Builder
	sb.WriteString(e.Receiver.PhpString())
	sb.WriteString("->")
	sb.WriteString(e.Method)
	sb.WriteByte('(')
	for i, a := range e.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.PhpString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// PropAssignStmt is `<recv>-><field> = <value>;`. Used by Phase 9
// agent intent bodies to mutate `$this->FIELD`.
type PropAssignStmt struct {
	Receiver Expr
	Field    string
	Value    Expr
}

func (*PropAssignStmt) phpStmt() {}

func (s *PropAssignStmt) PhpString(ind int) string {
	return indent(ind) + s.Receiver.PhpString() + "->" + s.Field + " = " + s.Value.PhpString() + ";"
}

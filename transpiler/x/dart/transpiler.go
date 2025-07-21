//go:build slow

package dartt

import (
	"fmt"
	"io"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// --- Struct tracking for generated classes ---
type StructField struct {
	Name string
	Type string
}

var (
	structSeq        int
	structSig        map[string]string
	structFields     map[string][]StructField
	mapLitStructName map[*MapLit]string
	structOrder      []string
	compVarTypes     map[string]string
	localVarTypes    map[string]string
	structNameCount  map[string]int
	nextStructHint   string
)

// GetStructOrder returns the generated struct names (for testing).
func GetStructOrder() []string { return structOrder }

// GetStructFields exposes struct field definitions (for testing).
func GetStructFields() map[string][]StructField { return structFields }

func capitalize(name string) string {
	parts := strings.Split(name, "_")
	for i, p := range parts {
		if len(p) == 0 {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + p[1:]
	}
	return strings.Join(parts, "")
}

// --- Simple Dart AST ---

// Program represents a sequence of statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) error }

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (w *WhileStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "while ("); err != nil {
		return err
	}
	if err := w.Cond.emit(out); err != nil {
		return err
	}
	if _, err := io.WriteString(out, ") {\n"); err != nil {
		return err
	}
	for _, st := range w.Body {
		if _, err := io.WriteString(out, "    "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		if _, ok := st.(*IfStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*WhileStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*ForRangeStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*ForInStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(out, "  }"); err != nil {
		return err
	}
	return nil
}

// ForRangeStmt represents a numeric for-loop like `for i in 0..10 {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "for (var "+f.Name+" = "); err != nil {
		return err
	}
	if f.Start != nil {
		if err := f.Start.emit(out); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(out, "; "+f.Name+" < "); err != nil {
		return err
	}
	if f.End != nil {
		if err := f.End.emit(out); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(out, "; "+f.Name+"++) {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(out, "    "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt:
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(out, "  }"); err != nil {
		return err
	}
	return nil
}

// ForInStmt represents iteration over an iterable expression.
type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (f *ForInStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "for (var "+f.Name+" in "); err != nil {
		return err
	}
	if f.Iterable != nil {
		if err := f.Iterable.emit(out); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(out, ") {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(out, "    "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt:
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(out, "  }"); err != nil {
		return err
	}
	return nil
}

// IfStmt represents a conditional statement with an optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "if ("); err != nil {
		return err
	}
	if err := s.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	for _, st := range s.Then {
		if _, err := io.WriteString(w, "    "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ";\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  }"); err != nil {
		return err
	}
	if len(s.Else) > 0 {
		if _, err := io.WriteString(w, " else {\n"); err != nil {
			return err
		}
		for _, st := range s.Else {
			if _, err := io.WriteString(w, "    "); err != nil {
				return err
			}
			if err := st.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "  }"); err != nil {
			return err
		}
	}
	return nil
}

type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) error {
	nextStructHint = s.Name
	typ := inferType(s.Value)
	nextStructHint = ""
	localVarTypes[s.Name] = typ
	if typ == "var" {
		if _, err := io.WriteString(w, "var "+s.Name); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, typ+" "+s.Name); err != nil {
			return err
		}
	}
	if s.Value != nil {
		if _, err := io.WriteString(w, " = "); err != nil {
			return err
		}
		return s.Value.emit(w)
	}
	return nil
}

type AssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *AssignStmt) emit(w io.Writer) error {
	if se, ok := s.Target.(*SelectorExpr); ok {
		if n, ok := se.Receiver.(*Name); ok && currentEnv != nil {
			if t, err := currentEnv.GetVar(n.Name); err == nil {
				if _, ok := t.(types.StructType); ok {
					fmt.Fprintf(w, "%s = {...%s, %q: ", n.Name, n.Name, se.Field)
					if err := s.Value.emit(w); err != nil {
						return err
					}
					_, err := io.WriteString(w, "}")
					return err
				}
			}
		}
	}
	if err := s.Target.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " = "); err != nil {
		return err
	}
	return s.Value.emit(w)
}

type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) error {
	nextStructHint = s.Name
	typ := inferType(s.Value)
	nextStructHint = ""
	localVarTypes[s.Name] = typ
	if typ == "var" {
		if _, err := io.WriteString(w, "final "+s.Name+" = "); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "final "+typ+" "+s.Name+" = "); err != nil {
			return err
		}
	}
	return s.Value.emit(w)
}

// ReturnStmt represents a `return` statement.
type ReturnStmt struct {
	Value Expr
}

func (s *ReturnStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "return"); err != nil {
		return err
	}
	if s.Value != nil {
		if _, err := io.WriteString(w, " "); err != nil {
			return err
		}
		if err := s.Value.emit(w); err != nil {
			return err
		}
	}
	return nil
}

// BreakStmt represents a `break` statement.
type BreakStmt struct{}

func (s *BreakStmt) emit(w io.Writer) error {
	_, err := io.WriteString(w, "break")
	return err
}

// ContinueStmt represents a `continue` statement.
type ContinueStmt struct{}

func (s *ContinueStmt) emit(w io.Writer) error {
	_, err := io.WriteString(w, "continue")
	return err
}

// FuncDecl represents a function definition.
type FuncDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FuncDecl) emit(w io.Writer) error {
	retType := inferReturnType(f.Body)
	if _, err := io.WriteString(w, retType+" "+f.Name+"("); err != nil {
		return err
	}
	for i, p := range f.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt, *FuncDecl:
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(w, "}")
	return err
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) error { return s.Expr.emit(w) }

type Expr interface{ emit(io.Writer) error }

type UnaryExpr struct {
	Op string
	X  Expr
}

func (u *UnaryExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, u.Op); err != nil {
		return err
	}
	return u.X.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) error {
	if b.Op == "in" {
		if err := b.Right.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".contains("); err != nil {
			return err
		}
		if err := b.Left.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	lt := inferType(b.Left)
	rt := inferType(b.Right)
	if (b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") && lt == "String" && rt == "String" {
		if err := b.Left.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".compareTo("); err != nil {
			return err
		}
		if err := b.Right.emit(w); err != nil {
			return err
		}
		var cmp string
		switch b.Op {
		case "<":
			cmp = " < 0"
		case "<=":
			cmp = " <= 0"
		case ">":
			cmp = " > 0"
		case ">=":
			cmp = " >= 0"
		}
		_, err := io.WriteString(w, ")"+cmp)
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := b.Left.emit(w); err != nil {
		return err
	}
	op := b.Op
	if b.Op == "/" && lt == "int" && rt == "int" {
		op = "~/"
	}
	if _, err := io.WriteString(w, " "+op+" "); err != nil {
		return err
	}
	if err := b.Right.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// CondExpr represents a conditional expression like `cond ? a : b`.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) error {
	if err := c.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " ? "); err != nil {
		return err
	}
	if err := c.Then.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " : "); err != nil {
		return err
	}
	return c.Else.emit(w)
}

type CallExpr struct {
	Func Expr
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) error {
	if err := c.Func.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, a := range c.Args {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := a.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) error { _, err := io.WriteString(w, n.Name); return err }

// SelectorExpr represents receiver.field access.
type SelectorExpr struct {
	Receiver Expr
	Field    string
}

func (s *SelectorExpr) emit(w io.Writer) error {
	t := inferType(s.Receiver)
	if strings.HasPrefix(t, "Map<") {
		if err := s.Receiver.emit(w); err != nil {
			return err
		}
		_, err := fmt.Fprintf(w, "[%q]", s.Field)
		return err
	}
	if err := s.Receiver.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "."+s.Field)
	return err
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) error {
	val := strings.ReplaceAll(s.Value, "\\", "\\\\")
	val = strings.ReplaceAll(val, "\"", "\\\"")
	val = strings.ReplaceAll(val, "$", "\\$")
	_, err := io.WriteString(w, "\""+val+"\"")
	return err
}

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) error { _, err := fmt.Fprintf(w, "%d", i.Value); return err }

// BoolLit represents a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) error {
	if b.Value {
		_, err := io.WriteString(w, "true")
		return err
	}
	_, err := io.WriteString(w, "false")
	return err
}

// ListLit represents a list literal.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	for i, e := range l.Elems {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := e.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

// MapLit represents a simple map literal.
type MapLit struct{ Entries []MapEntry }

// MapEntry is a key/value pair inside a map literal.
type MapEntry struct {
	Key   Expr
	Value Expr
}

// NotNilExpr appends `!` to an expression to assert it is not null.
type NotNilExpr struct{ X Expr }

func (n *NotNilExpr) emit(w io.Writer) error {
	if err := n.X.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "!")
	return err
}

func (m *MapLit) emit(w io.Writer) error {
	if name, ok := mapLitStructName[m]; ok {
		if _, err := io.WriteString(w, name+"("); err != nil {
			return err
		}
		for i, e := range m.Entries {
			if i > 0 {
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
			}
			var fname string
			switch k := e.Key.(type) {
			case *Name:
				fname = k.Name
			case *StringLit:
				fname = k.Value
			default:
				fname = ""
			}
			if fname != "" {
				if _, err := io.WriteString(w, fname+": "); err != nil {
					return err
				}
			}
			if err := e.Value.emit(w); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if _, err := io.WriteString(w, "{"); err != nil {
		return err
	}
	for i, e := range m.Entries {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if n, ok := e.Key.(*Name); ok {
			if currentEnv != nil {
				if _, err := currentEnv.GetVar(n.Name); err == nil {
					if err := n.emit(w); err != nil {
						return err
					}
				} else {
					if _, err := fmt.Fprintf(w, "\"%s\"", n.Name); err != nil {
						return err
					}
				}
			} else {
				if _, err := fmt.Fprintf(w, "\"%s\"", n.Name); err != nil {
					return err
				}
			}
		} else {
			if err := e.Key.emit(w); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, ": "); err != nil {
			return err
		}
		if err := e.Value.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "}")
	return err
}

// IndexExpr represents target[index].
type IndexExpr struct {
	Target Expr
	Index  Expr
}

// SliceExpr represents target[start:end].
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

func (s *SliceExpr) emit(w io.Writer) error {
	if inferType(s.Target) == "String" {
		if err := s.Target.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".substring("); err != nil {
			return err
		}
		if s.Start != nil {
			if err := s.Start.emit(w); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, "0"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, ", "); err != nil {
			return err
		}
		if s.End != nil {
			if err := s.End.emit(w); err != nil {
				return err
			}
		} else {
			if err := s.Target.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ".length"); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if err := s.Target.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".sublist("); err != nil {
		return err
	}
	if s.Start != nil {
		if err := s.Start.emit(w); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "0"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if s.End != nil {
		if err := s.End.emit(w); err != nil {
			return err
		}
	} else {
		if err := s.Target.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".length"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

func (i *IndexExpr) emit(w io.Writer) error {
	if err := i.Target.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if i.Index != nil {
		if err := i.Index.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

// SubstringExpr represents substring(s, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) error {
	if err := s.Str.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".substring("); err != nil {
		return err
	}
	if s.Start != nil {
		if err := s.Start.emit(w); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if s.End != nil {
		if err := s.End.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

// ContainsExpr represents right.contains(left).
type ContainsExpr struct {
	Target Expr
	Elem   Expr
}

func (c *ContainsExpr) emit(w io.Writer) error {
	if err := c.Target.emit(w); err != nil {
		return err
	}
	method := ".contains("
	if strings.HasPrefix(inferType(c.Target), "Map<") {
		method = ".containsKey("
	}
	if _, err := io.WriteString(w, method); err != nil {
		return err
	}
	if err := c.Elem.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// AppendExpr represents append(list, value).
type AppendExpr struct {
	List  Expr
	Value Expr
}

func (a *AppendExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "..."); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if err := a.Value.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]")
	return err
}

// AvgExpr represents avg(list).
type AvgExpr struct{ List Expr }

func (a *AvgExpr) emit(w io.Writer) error {
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".isEmpty ? 0 : ("); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".reduce((a, b) => a + b) / "); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length)")
	return err
}

// SumExpr represents sum(list).
type SumExpr struct{ List Expr }

func (s *SumExpr) emit(w io.Writer) error {
	if err := s.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".isEmpty ? 0 : ("); err != nil {
		return err
	}
	if err := s.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".reduce((a, b) => a + b))"); err != nil {
		return err
	}
	return nil
}

// MinExpr represents min(list).
type MinExpr struct{ List Expr }

func (m *MinExpr) emit(w io.Writer) error {
	if err := m.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".reduce((a, b) => a < b ? a : b)"); err != nil {
		return err
	}
	return nil
}

// MaxExpr represents max(list).
type MaxExpr struct{ List Expr }

func (m *MaxExpr) emit(w io.Writer) error {
	if err := m.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".reduce((a, b) => a > b ? a : b)"); err != nil {
		return err
	}
	return nil
}

// ValuesExpr represents values(map).
type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) error {
	t := inferType(v.Map)
	if fields, ok := structFields[t]; ok {
		if _, err := io.WriteString(w, "["); err != nil {
			return err
		}
		for i, f := range fields {
			if i > 0 {
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
			}
			if err := (&SelectorExpr{Receiver: v.Map, Field: f.Name}).emit(w); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, "]")
		return err
	}
	if err := v.Map.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".values.toList()")
	return err
}

// StrExpr represents str(x).
type StrExpr struct{ Value Expr }

func (s *StrExpr) emit(w io.Writer) error {
	if err := s.Value.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".toString()")
	return err
}

// FormatList renders a list like "[a b]".
type FormatList struct{ List Expr }

func (f *FormatList) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "\"[\" + "); err != nil {
		return err
	}
	if err := f.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".join(', ') + \"]\""); err != nil {
		return err
	}
	return nil
}

// CountExpr represents count(list) or count(group).
type CountExpr struct{ X Expr }

func (c *CountExpr) emit(w io.Writer) error {
	if err := c.X.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length")
	return err
}

// MultiListComp represents a list comprehension with multiple input iterators.
type MultiListComp struct {
	Vars  []string
	Iters []Expr
	Expr  Expr
	Cond  Expr
}

// GroupQueryExpr represents a query with grouping support.
type GroupQueryExpr struct {
	Vars     []string
	Iters    []Expr
	Cond     Expr
	Key      Expr
	Row      Expr
	GroupVar string
	Select   Expr
	Having   Expr
	Sort     *LambdaExpr
}

// LeftJoinExpr represents a simple left join query.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (l *LeftJoinExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  var _res = [];\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  for (var "+l.LeftVar+" in "); err != nil {
		return err
	}
	if err := l.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var matched = false;\n    for (var "+l.RightVar+" in "); err != nil {
		return err
	}
	if err := l.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if (!("); err != nil {
		return err
	}
	if err := l.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n      matched = true;\n      _res.add("); err != nil {
		return err
	}
	if err := l.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n    if (!matched) {\n      var "+l.RightVar+" = null;\n      _res.add("); err != nil {
		return err
	}
	if err := l.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n  }\n  return _res;\n})()"); err != nil {
		return err
	}
	return nil
}

// RightJoinExpr represents a simple right join query.
type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (r *RightJoinExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  var _res = [];\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  for (var "+r.RightVar+" in "); err != nil {
		return err
	}
	if err := r.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var matched = false;\n    for (var "+r.LeftVar+" in "); err != nil {
		return err
	}
	if err := r.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if (!("); err != nil {
		return err
	}
	if err := r.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n      matched = true;\n      _res.add("); err != nil {
		return err
	}
	if err := r.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n    if (!matched) {\n      var "+r.LeftVar+" = null;\n      _res.add("); err != nil {
		return err
	}
	if err := r.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n  }\n  return _res;\n})()"); err != nil {
		return err
	}
	return nil
}

// OuterJoinExpr represents a simple full outer join query.
type OuterJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (o *OuterJoinExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  var _res = [];\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  for (var "+o.LeftVar+" in "); err != nil {
		return err
	}
	if err := o.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var matched = false;\n    for (var "+o.RightVar+" in "); err != nil {
		return err
	}
	if err := o.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if (!("); err != nil {
		return err
	}
	if err := o.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n      matched = true;\n      _res.add("); err != nil {
		return err
	}
	if err := o.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n    if (!matched) {\n      var "+o.RightVar+" = null;\n      _res.add("); err != nil {
		return err
	}
	if err := o.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n  }\n  for (var "+o.RightVar+" in "); err != nil {
		return err
	}
	if err := o.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var exists = false;\n    for (var "+o.LeftVar+" in "); err != nil {
		return err
	}
	if err := o.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if ("); err != nil {
		return err
	}
	if err := o.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") { exists = true; break; }\n    }\n    if (!exists) {\n      var "+o.LeftVar+" = null;\n      _res.add("); err != nil {
		return err
	}
	if err := o.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n  }\n  return _res;\n})()"); err != nil {
		return err
	}
	return nil
}

func (gq *GroupQueryExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  var groups = <String, Map<String, dynamic>>{};\n"); err != nil {
		return err
	}
	for i, v := range gq.Vars {
		if _, err := io.WriteString(w, "  for (var "+v+" in "); err != nil {
			return err
		}
		if err := gq.Iters[i].emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ") {\n"); err != nil {
			return err
		}
	}
	if gq.Cond != nil {
		if _, err := io.WriteString(w, strings.Repeat("  ", len(gq.Vars)+1)+"if (!("); err != nil {
			return err
		}
		if err := gq.Cond.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")) continue;\n"); err != nil {
			return err
		}
	}
	indent := strings.Repeat("  ", len(gq.Vars)+1)
	if _, err := io.WriteString(w, indent+"var key = "); err != nil {
		return err
	}
	if err := gq.Key.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ";\n"+indent+"var ks = key.toString();\n"+indent+"var g = groups[ks];\n"+indent+"if (g == null) {\n"); err != nil {
		return err
	}
	nextStructHint = "group"
	if _, err := io.WriteString(w, indent+"  g = {'key': key, 'items': []};\n"+indent+"  groups[ks] = g;\n"+indent+"}\n"+indent+"(g['items'] as List).add("); err != nil {
		return err
	}
	nextStructHint = ""
	if err := gq.Row.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n"); err != nil {
		return err
	}
	for i := len(gq.Vars); i > 0; i-- {
		if _, err := io.WriteString(w, strings.Repeat("  ", i)+"}\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  var _list = groups.values.toList();\n"); err != nil {
		return err
	}
	if gq.Sort != nil {
		if _, err := io.WriteString(w, "  _list.sort("); err != nil {
			return err
		}
		if err := gq.Sort.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  var res = <dynamic>[];\n  for (var g in _list) {\n"); err != nil {
		return err
	}
	if gq.Having != nil {
		if _, err := io.WriteString(w, "    if ("); err != nil {
			return err
		}
		if err := gq.Having.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ") {\n      res.add("); err != nil {
			return err
		}
		if err := gq.Select.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n    }\n"); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "    res.add("); err != nil {
			return err
		}
		if err := gq.Select.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  }\n  return res;\n})();"); err != nil {
		return err
	}
	return nil
}

func (lc *MultiListComp) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "[for (var "); err != nil {
		return err
	}
	if len(lc.Vars) > 0 {
		if _, err := io.WriteString(w, lc.Vars[0]+" in "); err != nil {
			return err
		}
		if err := lc.Iters[0].emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	}
	for i := 1; i < len(lc.Vars); i++ {
		if _, err := io.WriteString(w, " for (var "+lc.Vars[i]+" in "); err != nil {
			return err
		}
		if err := lc.Iters[i].emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	}
	if lc.Cond != nil {
		if _, err := io.WriteString(w, " if ("); err != nil {
			return err
		}
		if err := lc.Cond.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, " "); err != nil {
		return err
	}
	if err := lc.Expr.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]")
	return err
}

// SortExpr represents sorting of a list with a comparison lambda.
type SortExpr struct {
	List    Expr
	Compare *LambdaExpr
}

func (s *SortExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "List.of("); err != nil {
		return err
	}
	if err := s.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")..sort("); err != nil {
		return err
	}
	if err := s.Compare.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")"); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// LambdaExpr represents an inline function expression.
type LambdaExpr struct {
	Params []string
	Body   Expr
}

func (l *LambdaExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, p := range l.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") => "); err != nil {
		return err
	}
	if err := l.Body.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// LenExpr represents the `len` builtin.
type LenExpr struct{ X Expr }

func (l *LenExpr) emit(w io.Writer) error {
	if err := l.X.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length")
	return err
}

// inferType attempts to guess the Dart type for the given expression.
var currentEnv *types.Env

func dartType(t types.Type) string {
	switch v := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType, types.BigRatType:
		return "num"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "String"
	case types.ListType:
		return "List<" + dartType(v.Elem) + ">"
	case types.MapType:
		return "Map<" + dartType(v.Key) + ", " + dartType(v.Value) + ">"
	case types.GroupType:
		return "Map<String, dynamic>"
	case types.StructType:
		return "Map<String, dynamic>"
	default:
		return "dynamic"
	}
}

func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "int"
	case *BoolLit:
		return "bool"
	case *StringLit:
		return "String"
	case *Name:
		if t, ok := localVarTypes[ex.Name]; ok {
			return t
		}
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				return dartType(t)
			}
		}
		if t, ok := compVarTypes[ex.Name]; ok {
			return t
		}
		return "var"
	case *ListLit:
		if len(ex.Elems) == 0 {
			return "List<dynamic>"
		}
		typ := inferType(ex.Elems[0])
		for _, el := range ex.Elems[1:] {
			if t := inferType(el); t != typ {
				typ = "dynamic"
				break
			}
		}
		return "List<" + typ + ">"
	case *MapLit:
		if len(ex.Entries) == 0 {
			return "Map<dynamic, dynamic>"
		}
		sigParts := make([]string, len(ex.Entries))
		valid := true
		for i, it := range ex.Entries {
			var field string
			switch k := it.Key.(type) {
			case *Name:
				field = k.Name
			case *StringLit:
				field = k.Value
			default:
				valid = false
			}
			if !valid {
				break
			}
			t := inferType(it.Value)
			sigParts[i] = field + ":" + t
		}
		if valid {
			sig := strings.Join(sigParts, ";")
			name, ok := structSig[sig]
			if !ok {
				if nextStructHint != "" {
					base := capitalize(nextStructHint)
					if strings.HasSuffix(base, "s") && len(base) > 1 {
						base = base[:len(base)-1]
					}
					cnt := structNameCount[base]
					if cnt > 0 {
						name = fmt.Sprintf("%s%d", base, cnt+1)
					} else {
						name = base
					}
					structNameCount[base] = cnt + 1
					nextStructHint = ""
				} else {
					structSeq++
					name = fmt.Sprintf("S%d", structSeq)
				}
				structSig[sig] = name
				var fields []StructField
				for _, part := range ex.Entries {
					var fn string
					switch k := part.Key.(type) {
					case *Name:
						fn = k.Name
					case *StringLit:
						fn = k.Value
					}
					fields = append(fields, StructField{Name: fn, Type: inferType(part.Value)})
				}
				structFields[name] = fields
				structOrder = append(structOrder, name)
			}
			mapLitStructName[ex] = name
			return name
		}

		kt := inferType(ex.Entries[0].Key)
		if kt == "var" {
			if _, ok := ex.Entries[0].Key.(*Name); ok {
				kt = "String"
			}
		}
		vt := inferType(ex.Entries[0].Value)
		for _, it := range ex.Entries[1:] {
			t := inferType(it.Key)
			if t == "var" {
				if _, ok := it.Key.(*Name); ok {
					t = "String"
				}
			}
			if t != kt {
				kt = "dynamic"
			}
			if t := inferType(it.Value); t != vt {
				vt = "dynamic"
			}
		}
		if kt == "var" {
			kt = "dynamic"
		}
		if vt == "var" {
			vt = "dynamic"
		}
		return "Map<" + kt + ", " + vt + ">"
	case *MultiListComp:
		saved := map[string]string{}
		for i, v := range ex.Vars {
			t := inferType(ex.Iters[i])
			elem := "dynamic"
			if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
				elem = strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
			}
			saved[v] = compVarTypes[v]
			compVarTypes[v] = elem
		}
		et := inferType(ex.Expr)
		if et == "var" {
			et = "dynamic"
		}
		for _, v := range ex.Vars {
			if old, ok := saved[v]; ok && old != "" {
				compVarTypes[v] = old
			} else {
				delete(compVarTypes, v)
			}
		}
		return "List<" + et + ">"
	case *GroupQueryExpr:
		saved := map[string]string{}
		for i, v := range ex.Vars {
			t := inferType(ex.Iters[i])
			elem := "dynamic"
			if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
				elem = strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
			}
			saved[v] = compVarTypes[v]
			compVarTypes[v] = elem
		}
		if ex.GroupVar != "" {
			saved[ex.GroupVar] = compVarTypes[ex.GroupVar]
			compVarTypes[ex.GroupVar] = "Map<String, dynamic>"
		}
		et := inferType(ex.Select)
		for _, v := range ex.Vars {
			if old, ok := saved[v]; ok && old != "" {
				compVarTypes[v] = old
			} else {
				delete(compVarTypes, v)
			}
		}
		if ex.GroupVar != "" {
			if old, ok := saved[ex.GroupVar]; ok && old != "" {
				compVarTypes[ex.GroupVar] = old
			} else {
				delete(compVarTypes, ex.GroupVar)
			}
		}
		return "List<" + et + ">"
	case *LeftJoinExpr:
		ltype := inferType(ex.LeftSrc)
		rtype := inferType(ex.RightSrc)
		lelem := "dynamic"
		relem := "dynamic"
		if strings.HasPrefix(ltype, "List<") && strings.HasSuffix(ltype, ">") {
			lelem = strings.TrimSuffix(strings.TrimPrefix(ltype, "List<"), ">")
		}
		if strings.HasPrefix(rtype, "List<") && strings.HasSuffix(rtype, ">") {
			relem = strings.TrimSuffix(strings.TrimPrefix(rtype, "List<"), ">")
		}
		savedL, savedR := compVarTypes[ex.LeftVar], compVarTypes[ex.RightVar]
		compVarTypes[ex.LeftVar] = lelem
		compVarTypes[ex.RightVar] = relem
		et := inferType(ex.Select)
		if savedL != "" {
			compVarTypes[ex.LeftVar] = savedL
		} else {
			delete(compVarTypes, ex.LeftVar)
		}
		if savedR != "" {
			compVarTypes[ex.RightVar] = savedR
		} else {
			delete(compVarTypes, ex.RightVar)
		}
		return "List<" + et + ">"
	case *RightJoinExpr:
		ltype := inferType(ex.LeftSrc)
		rtype := inferType(ex.RightSrc)
		lelem := "dynamic"
		relem := "dynamic"
		if strings.HasPrefix(ltype, "List<") && strings.HasSuffix(ltype, ">") {
			lelem = strings.TrimSuffix(strings.TrimPrefix(ltype, "List<"), ">")
		}
		if strings.HasPrefix(rtype, "List<") && strings.HasSuffix(rtype, ">") {
			relem = strings.TrimSuffix(strings.TrimPrefix(rtype, "List<"), ">")
		}
		savedL, savedR := compVarTypes[ex.LeftVar], compVarTypes[ex.RightVar]
		compVarTypes[ex.LeftVar] = lelem
		compVarTypes[ex.RightVar] = relem
		et := inferType(ex.Select)
		if savedL != "" {
			compVarTypes[ex.LeftVar] = savedL
		} else {
			delete(compVarTypes, ex.LeftVar)
		}
		if savedR != "" {
			compVarTypes[ex.RightVar] = savedR
		} else {
			delete(compVarTypes, ex.RightVar)
		}
		return "List<" + et + ">"
	case *OuterJoinExpr:
		ltype := inferType(ex.LeftSrc)
		rtype := inferType(ex.RightSrc)
		lelem := "dynamic"
		relem := "dynamic"
		if strings.HasPrefix(ltype, "List<") && strings.HasSuffix(ltype, ">") {
			lelem = strings.TrimSuffix(strings.TrimPrefix(ltype, "List<"), ">")
		}
		if strings.HasPrefix(rtype, "List<") && strings.HasSuffix(rtype, ">") {
			relem = strings.TrimSuffix(strings.TrimPrefix(rtype, "List<"), ">")
		}
		savedL, savedR := compVarTypes[ex.LeftVar], compVarTypes[ex.RightVar]
		compVarTypes[ex.LeftVar] = lelem
		compVarTypes[ex.RightVar] = relem
		et := inferType(ex.Select)
		if savedL != "" {
			compVarTypes[ex.LeftVar] = savedL
		} else {
			delete(compVarTypes, ex.LeftVar)
		}
		if savedR != "" {
			compVarTypes[ex.RightVar] = savedR
		} else {
			delete(compVarTypes, ex.RightVar)
		}
		return "List<" + et + ">"
	case *SortExpr:
		return inferType(ex.List)
	case *BinaryExpr:
		switch ex.Op {
		case "+":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "String" || rt == "String" {
				return "String"
			}
			return "num"
		case "-", "*", "/", "%":
			return "num"
		case "<", "<=", ">", ">=", "==", "!=", "&&", "||":
			return "bool"
		default:
			return "dynamic"
		}
	case *UnaryExpr:
		if ex.Op == "-" {
			t := inferType(ex.X)
			if t == "int" || t == "num" {
				return t
			}
			return "num"
		}
		return inferType(ex.X)
	case *CondExpr:
		t1 := inferType(ex.Then)
		t2 := inferType(ex.Else)
		if t1 == t2 {
			return t1
		}
		return "dynamic"
	case *CallExpr:
		if n, ok := ex.Func.(*Name); ok {
			switch n.Name {
			case "len":
				return "int"
			case "avg":
				return "num"
			case "sum":
				return "num"
			case "min", "max":
				return "num"
			case "values":
				return "List<dynamic>"
			case "append":
				if len(ex.Args) > 0 {
					return inferType(ex.Args[0])
				}
			}
		}
		return "dynamic"
	case *IndexExpr:
		t := inferType(ex.Target)
		if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
			return strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
		}
		if strings.HasPrefix(t, "Map<") && strings.HasSuffix(t, ">") {
			parts := strings.TrimSuffix(strings.TrimPrefix(t, "Map<"), ">")
			if idx := strings.Index(parts, ","); idx >= 0 {
				return strings.TrimSpace(parts[idx+1:])
			}
		}
		return "dynamic"
	case *SelectorExpr:
		rt := inferType(ex.Receiver)
		if fields, ok := structFields[rt]; ok {
			for _, f := range fields {
				if f.Name == ex.Field {
					return f.Type
				}
			}
		}
		return "dynamic"
	case *ContainsExpr:
		return "bool"
	case *LenExpr:
		return "int"
	case *CountExpr:
		return "int"
	case *SubstringExpr:
		return "String"
	case *SliceExpr:
		t := inferType(ex.Target)
		if t == "String" {
			return "String"
		}
		return t
	case *AppendExpr:
		return inferType(ex.List)
	case *AvgExpr, *SumExpr, *MinExpr, *MaxExpr:
		return "num"
	case *ValuesExpr:
		return "List<dynamic>"
	case *StrExpr, *FormatList:
		return "String"
	default:
		if e == nil {
			return "var"
		}
		return "var"
	}
}

func inferReturnType(body []Stmt) string {
	if len(body) == 0 {
		return "void"
	}
	if ret, ok := body[len(body)-1].(*ReturnStmt); ok {
		if ret.Value == nil {
			return "void"
		}
		t := inferType(ret.Value)
		if t == "var" {
			return "dynamic"
		}
		return t
	}
	return "void"
}

func emitExpr(w io.Writer, e Expr) error { return e.emit(w) }

func isBlockStmt(s Stmt) bool {
	switch s.(type) {
	case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt, *FuncDecl:
		return true
	default:
		return false
	}
}

func gatherTypes(p *Program) {
	for _, st := range p.Stmts {
		walkTypes(st)
	}
}

func walkTypes(s Stmt) {
	switch st := s.(type) {
	case *LetStmt:
		nextStructHint = st.Name
		typ := inferType(st.Value)
		nextStructHint = ""
		localVarTypes[st.Name] = typ
	case *VarStmt:
		if st.Value != nil {
			nextStructHint = st.Name
			typ := inferType(st.Value)
			nextStructHint = ""
			localVarTypes[st.Name] = typ
		} else {
			localVarTypes[st.Name] = "var"
		}
	case *AssignStmt:
		inferType(st.Value)
		inferType(st.Target)
	case *ExprStmt:
		inferType(st.Expr)
	case *IfStmt:
		inferType(st.Cond)
		for _, t := range st.Then {
			walkTypes(t)
		}
		for _, e := range st.Else {
			walkTypes(e)
		}
	case *WhileStmt:
		inferType(st.Cond)
		for _, b := range st.Body {
			walkTypes(b)
		}
	case *ForRangeStmt:
		if st.Start != nil {
			inferType(st.Start)
		}
		if st.End != nil {
			inferType(st.End)
		}
		for _, b := range st.Body {
			walkTypes(b)
		}
	case *ForInStmt:
		t := inferType(st.Iterable)
		elem := "dynamic"
		if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
			elem = strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
		}
		saved := compVarTypes[st.Name]
		compVarTypes[st.Name] = elem
		for _, b := range st.Body {
			walkTypes(b)
		}
		if saved != "" {
			compVarTypes[st.Name] = saved
		} else {
			delete(compVarTypes, st.Name)
		}
	case *FuncDecl:
		for _, b := range st.Body {
			walkTypes(b)
		}
	}
}

// Emit writes Dart source for p to w.
func Emit(w io.Writer, p *Program) error {
	gatherTypes(p)
	if _, err := io.WriteString(w, "// Generated by Mochi transpiler\n"); err != nil {
		return err
	}

	for _, name := range structOrder {
		fields := structFields[name]
		if _, err := fmt.Fprintf(w, "class %s {\n", name); err != nil {
			return err
		}
		for _, f := range fields {
			if _, err := fmt.Fprintf(w, "  final %s %s;\n", f.Type, f.Name); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  const %s({", name); err != nil {
			return err
		}
		for i, f := range fields {
			if i > 0 {
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprintf(w, "required this.%s", f.Name); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "});\n}\n\n"); err != nil {
			return err
		}
	}

	for _, st := range p.Stmts {
		if fd, ok := st.(*FuncDecl); ok {
			if err := fd.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(w, "void main() {\n"); err != nil {
		return err
	}
	for _, st := range p.Stmts {
		if _, ok := st.(*FuncDecl); ok {
			continue
		}
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if isBlockStmt(st) {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(w, "}\n"); err != nil {
		return err
	}
	return nil
}

// Transpile converts a Mochi program into a simple Dart AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	currentEnv = env
	structSeq = 0
	structSig = map[string]string{}
	structFields = map[string][]StructField{}
	mapLitStructName = map[*MapLit]string{}
	structOrder = nil
	compVarTypes = map[string]string{}
	localVarTypes = map[string]string{}
	structNameCount = map[string]int{}
	nextStructHint = ""
	p := &Program{}
	for _, st := range prog.Statements {
		s, err := convertStmtInternal(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			p.Stmts = append(p.Stmts, s)
		}
	}
	return p, nil
}

func convertIfStmt(i *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmtList(i.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := convertIfStmt(i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		elseStmts, err = convertStmtList(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(wst *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(wst.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(wst.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(fst *parser.ForStmt) (Stmt, error) {
	if fst.RangeEnd != nil {
		start, err := convertExpr(fst.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(fst.RangeEnd)
		if err != nil {
			return nil, err
		}
		body, err := convertStmtList(fst.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fst.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fst.Source)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(fst.Body)
	if err != nil {
		return nil, err
	}
	return &ForInStmt{Name: fst.Name, Iterable: iter, Body: body}, nil
}

func convertIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &IntLit{Value: 0}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		s, err := convertStmtInternal(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			out = append(out, s)
		}
	}
	return out, nil
}

func convertStmtInternal(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Test != nil:
		if _, err := convertStmtList(st.Test.Body); err != nil {
			return nil, err
		}
		return nil, nil
	case st.Expect != nil:
		return nil, nil
	case st.Type != nil:
		// ignore type declarations
		return nil, nil
	case st.ExternType != nil:
		return nil, nil
	case st.Expr != nil:
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil && *st.Let.Type.Simple == "int" {
			e = &IntLit{Value: 0}
		} else {
			return nil, fmt.Errorf("let missing value not supported")
		}
		return &LetStmt{Name: st.Let.Name, Value: e}, nil
	case st.Var != nil:
		var e Expr
		if st.Var.Value != nil {
			var err error
			e, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil && *st.Var.Type.Simple == "int" {
			e = &IntLit{Value: 0}
		}
		return &VarStmt{Name: st.Var.Name, Value: e}, nil
	case st.Assign != nil:
		target, err := convertAssignTarget(st.Assign)
		if err != nil {
			return nil, err
		}
		val, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Target: target, Value: val}, nil
	case st.Return != nil:
		var e Expr
		if st.Return.Value != nil {
			var err error
			e, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: e}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Fun != nil:
		body, err := convertStmtList(st.Fun.Body)
		if err != nil {
			return nil, err
		}
		var params []string
		for _, p := range st.Fun.Params {
			params = append(params, p.Name)
		}
		return &FuncDecl{Name: st.Fun.Name, Params: params, Body: body}, nil
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	case st.If != nil:
		return convertIfStmt(st.If)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertAssignTarget(as *parser.AssignStmt) (Expr, error) {
	expr := Expr(&Name{Name: as.Name})
	for i, idx := range as.Index {
		if idx.Start == nil || idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
			return nil, fmt.Errorf("complex assignment not supported")
		}
		val, err := convertExpr(idx.Start)
		if err != nil {
			return nil, err
		}
		iexpr := &IndexExpr{Target: expr, Index: val}
		if i < len(as.Index)-1 || len(as.Field) > 0 {
			expr = &NotNilExpr{X: iexpr}
		} else {
			expr = iexpr
		}
	}
	for i, f := range as.Field {
		nexpr := &SelectorExpr{Receiver: expr, Field: f.Name}
		if i < len(as.Field)-1 {
			expr = &NotNilExpr{X: nexpr}
		} else {
			expr = nexpr
		}
	}
	return expr, nil
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := make([]string, len(b.Right))
	for i, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
		ops[i] = op.Op
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				left := operands[i]
				right := operands[i+1]
				var expr Expr
				if ops[i] == "in" {
					expr = &ContainsExpr{Target: right, Elem: left}
				} else {
					expr = &BinaryExpr{Left: left, Op: ops[i], Right: right}
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	return operands[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	ex, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			ex = &UnaryExpr{Op: op, X: ex}
		default:
			return nil, fmt.Errorf("unary op %s not supported", op)
		}
	}
	return ex, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
				if op.Index.Step != nil || op.Index.Colon2 != nil {
					return nil, fmt.Errorf("slice step not supported")
				}
				var startExpr, endExpr Expr
				if op.Index.Start != nil {
					startExpr, err = convertExpr(op.Index.Start)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.End != nil {
					endExpr, err = convertExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				expr = &SliceExpr{Target: expr, Start: startExpr, End: endExpr}
			} else {
				if op.Index.Start == nil {
					return nil, fmt.Errorf("nil index")
				}
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				iex := &IndexExpr{Target: expr, Index: idx}
				if i < len(pf.Ops)-1 {
					expr = &NotNilExpr{X: iex}
				} else {
					expr = iex
				}
			}
		case op.Field != nil:
			// method call if next op is call
			if i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
				call := pf.Ops[i+1]
				var args []Expr
				for _, a := range call.Call.Args {
					ex, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args = append(args, ex)
				}
				expr = &CallExpr{Func: &SelectorExpr{Receiver: expr, Field: op.Field.Name}, Args: args}
				i++
			} else {
				expr = &SelectorExpr{Receiver: expr, Field: op.Field.Name}
			}
		case op.Call != nil:
			var args []Expr
			for _, a := range op.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ex)
			}
			expr = &CallExpr{Func: expr, Args: args}
		case op.Cast != nil:
			// ignore casts
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &LenExpr{X: arg}, nil
		}
		if p.Call.Func == "append" && len(p.Call.Args) == 2 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			val, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &AppendExpr{List: list, Value: val}, nil
		}
		if p.Call.Func == "avg" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &AvgExpr{List: list}, nil
		}
		if p.Call.Func == "sum" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &SumExpr{List: list}, nil
		}
		if p.Call.Func == "min" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MinExpr{List: list}, nil
		}
		if p.Call.Func == "max" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MaxExpr{List: list}, nil
		}
		if p.Call.Func == "values" && len(p.Call.Args) == 1 {
			mp, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &ValuesExpr{Map: mp}, nil
		}
		if p.Call.Func == "str" && len(p.Call.Args) == 1 {
			v, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &StrExpr{Value: v}, nil
		}
		if p.Call.Func == "count" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			if t := types.ExprType(p.Call.Args[0], currentEnv); t != nil {
				if _, ok := t.(types.GroupType); ok {
					arg = &SelectorExpr{Receiver: arg, Field: "items"}
				}
			}
			return &CountExpr{X: arg}, nil
		}
		if p.Call.Func == "exists" && len(p.Call.Args) == 1 {
			if q := extractQuery(p.Call.Args[0]); q != nil {
				return convertExistsQuery(q)
			}
		}
		if p.Call.Func == "substring" && len(p.Call.Args) == 3 {
			s0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			s1, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			s2, err := convertExpr(p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Str: s0, Start: s1, End: s2}, nil
		}
		if p.Call.Func == "print" {
			var args []Expr
			for _, a := range p.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				if v, ok := ex.(*ValuesExpr); ok {
					ex = &CallExpr{Func: &SelectorExpr{Receiver: v, Field: "join"}, Args: []Expr{&StringLit{Value: " "}}}
				} else if t := types.ExprType(a, currentEnv); t != nil {
					if _, ok := t.(types.ListType); ok {
						ex = &FormatList{List: ex}
					}
				}
				if len(p.Call.Args) == 1 && isBoolExpr(a) {
					ex = &CondExpr{Cond: ex, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
				}
				args = append(args, ex)
			}
			if len(args) == 1 {
				return &CallExpr{Func: &Name{"print"}, Args: args}, nil
			}
			join := &CallExpr{
				Func: &SelectorExpr{Receiver: &ListLit{Elems: args}, Field: "join"},
				Args: []Expr{&StringLit{Value: " "}},
			}
			return &CallExpr{Func: &Name{"print"}, Args: []Expr{join}}, nil
		}

		ce := &CallExpr{Func: &Name{p.Call.Func}}
		for _, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			ce.Args = append(ce.Args, ex)
		}
		return ce, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.List != nil:
		var elems []Expr
		for _, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		var entries []MapEntry
		for _, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			entries = append(entries, MapEntry{Key: k, Value: v})
		}
		return &MapLit{Entries: entries}, nil
	case p.Struct != nil:
		return convertStructLiteral(p.Struct)
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Query != nil:
		if ex, err := convertLeftJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertRightJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertOuterJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		if p.Query.Group != nil {
			if ex, err := convertGroupQuery(p.Query); err == nil {
				return ex, nil
			}
		}
		return convertQueryExpr(p.Query)
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Body: body}, nil
	case p.Selector != nil:
		expr := Expr(&Name{Name: p.Selector.Root})
		for _, f := range p.Selector.Tail {
			expr = &SelectorExpr{Receiver: expr, Field: f}
		}
		return expr, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported expression")
}

func isBoolExpr(e *parser.Expr) bool { return isBoolBinary(e.Binary) }

func isBoolBinary(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if len(b.Right) == 0 {
		return isBoolUnary(b.Left)
	}
	for _, op := range b.Right {
		switch op.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return true
		}
		if isBoolPostfix(op.Right) {
			return true
		}
	}
	return isBoolUnary(b.Left)
}

func isBoolUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	for _, op := range u.Ops {
		if op == "!" {
			return true
		}
	}
	return isBoolPostfix(u.Value)
}

func isBoolPostfix(pf *parser.PostfixExpr) bool {
	if pf == nil || len(pf.Ops) > 0 {
		return false
	}
	return isBoolPrimary(pf.Target)
}

func isBoolPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Bool != nil:
		return true
	case p.Group != nil:
		return isBoolExpr(p.Group)
	default:
		return false
	}
}

func extractQuery(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	if e.Binary.Left.Value.Target != nil {
		return e.Binary.Left.Value.Target.Query
	}
	return nil
}

func convertExistsQuery(q *parser.QueryExpr) (Expr, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if q.Where != nil {
		cond, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		lam := &LambdaExpr{Params: []string{q.Var}, Body: cond}
		return &CallExpr{Func: &SelectorExpr{Receiver: src, Field: "any"}, Args: []Expr{lam}}, nil
	}
	return &SelectorExpr{Receiver: src, Field: "isNotEmpty"}, nil
}

func convertStructLiteral(sl *parser.StructLiteral) (Expr, error) {
	var entries []MapEntry
	for _, f := range sl.Fields {
		v, err := convertExpr(f.Value)
		if err != nil {
			return nil, err
		}
		entries = append(entries, MapEntry{Key: &StringLit{Value: f.Name}, Value: v})
	}
	return &MapLit{Entries: entries}, nil
}

func convertMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr = &Name{Name: "null"}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		pat, err := convertExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		if n, ok := pat.(*Name); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		expr = &CondExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func cloneReplace(e Expr, old, new string) Expr {
	switch ex := e.(type) {
	case *Name:
		if ex.Name == old {
			return &Name{Name: new}
		}
		return &Name{Name: ex.Name}
	case *SelectorExpr:
		return &SelectorExpr{Receiver: cloneReplace(ex.Receiver, old, new), Field: ex.Field}
	case *UnaryExpr:
		return &UnaryExpr{Op: ex.Op, X: cloneReplace(ex.X, old, new)}
	case *BinaryExpr:
		return &BinaryExpr{Left: cloneReplace(ex.Left, old, new), Op: ex.Op, Right: cloneReplace(ex.Right, old, new)}
	case *CallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = cloneReplace(a, old, new)
		}
		return &CallExpr{Func: cloneReplace(ex.Func, old, new), Args: args}
	case *IndexExpr:
		return &IndexExpr{Target: cloneReplace(ex.Target, old, new), Index: cloneReplace(ex.Index, old, new)}
	case *ListLit:
		elems := make([]Expr, len(ex.Elems))
		for i, a := range ex.Elems {
			elems[i] = cloneReplace(a, old, new)
		}
		return &ListLit{Elems: elems}
	case *MapLit:
		ents := make([]MapEntry, len(ex.Entries))
		for i, m := range ex.Entries {
			ents[i] = MapEntry{Key: cloneReplace(m.Key, old, new), Value: cloneReplace(m.Value, old, new)}
		}
		return &MapLit{Entries: ents}
	case *AvgExpr:
		return &AvgExpr{List: cloneReplace(ex.List, old, new)}
	case *SumExpr:
		return &SumExpr{List: cloneReplace(ex.List, old, new)}
	case *MinExpr:
		return &MinExpr{List: cloneReplace(ex.List, old, new)}
	case *MaxExpr:
		return &MaxExpr{List: cloneReplace(ex.List, old, new)}
	case *MultiListComp:
		iters := make([]Expr, len(ex.Iters))
		for i, it := range ex.Iters {
			iters[i] = cloneReplace(it, old, new)
		}
		var cond Expr
		if ex.Cond != nil {
			cond = cloneReplace(ex.Cond, old, new)
		}
		return &MultiListComp{Vars: append([]string(nil), ex.Vars...), Iters: iters, Expr: cloneReplace(ex.Expr, old, new), Cond: cond}
	case *GroupQueryExpr:
		iters := make([]Expr, len(ex.Iters))
		for i, it := range ex.Iters {
			iters[i] = cloneReplace(it, old, new)
		}
		var cond Expr
		if ex.Cond != nil {
			cond = cloneReplace(ex.Cond, old, new)
		}
		var having Expr
		if ex.Having != nil {
			having = cloneReplace(ex.Having, old, new)
		}
		var sort *LambdaExpr
		if ex.Sort != nil {
			sort = &LambdaExpr{Params: append([]string(nil), ex.Sort.Params...), Body: cloneReplace(ex.Sort.Body, old, new)}
		}
		return &GroupQueryExpr{Vars: append([]string(nil), ex.Vars...), Iters: iters, Cond: cond, Key: cloneReplace(ex.Key, old, new), Row: cloneReplace(ex.Row, old, new), GroupVar: ex.GroupVar, Select: cloneReplace(ex.Select, old, new), Having: having, Sort: sort}
	default:
		return ex
	}
}

func convertLeftJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertRightJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "right" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &RightJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertOuterJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "outer" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &OuterJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertGroupQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || q.Group == nil || len(q.Group.Exprs) != 1 || len(q.Froms) > 0 || len(q.Joins) > 0 || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	vars := []string{q.Var}
	iters := []Expr{src}
	var cond Expr
	if q.Where != nil {
		c, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		cond = c
	}
	key, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		return nil, err
	}
	row := Expr(&Name{Name: q.Var})
	saved := currentEnv
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	genv := types.NewEnv(child)
	genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: types.AnyType{}}, true)
	currentEnv = genv
	sel, err := convertExpr(q.Select)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	var having Expr
	var sort *LambdaExpr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
	}
	if q.Sort != nil {
		s, err := convertExpr(q.Sort)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
		desc := false
		if ue, ok := s.(*UnaryExpr); ok && ue.Op == "-" {
			desc = true
			s = ue.X
		}
		a := cloneReplace(s, q.Group.Name, "a")
		b := cloneReplace(s, q.Group.Name, "b")
		if desc {
			a, b = b, a
		}
		cmp := &CallExpr{Func: &SelectorExpr{Receiver: a, Field: "compareTo"}, Args: []Expr{b}}
		sort = &LambdaExpr{Params: []string{"a", "b"}, Body: cmp}
	}
	currentEnv = saved
	return &GroupQueryExpr{Vars: vars, Iters: iters, Cond: cond, Key: key, Row: row, GroupVar: q.Group.Name, Select: sel, Having: having, Sort: sort}, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}

	vars := []string{q.Var}
	iters := []Expr{}

	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	iters = append(iters, src)

	for _, f := range q.Froms {
		e, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, f.Var)
		iters = append(iters, e)
	}

	var cond Expr

	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
		e, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, j.Var)
		iters = append(iters, e)
		jc, err := convertExpr(j.On)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: jc}
		}
	}

	if q.Where != nil {
		wcond, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = wcond
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: wcond}
		}
	}

	body := Expr(&Name{Name: q.Var})
	if q.Select != nil {
		body, err = convertExpr(q.Select)
		if err != nil {
			return nil, err
		}
	}

	expr := Expr(&MultiListComp{Vars: vars, Iters: iters, Expr: body, Cond: cond})

	if q.Sort != nil {
		sortExpr, err := convertExpr(q.Sort)
		if err != nil {
			return nil, err
		}
		desc := false
		if ue, ok := sortExpr.(*UnaryExpr); ok && ue.Op == "-" {
			desc = true
			sortExpr = ue.X
		}
		a := cloneReplace(sortExpr, q.Var, "a")
		b := cloneReplace(sortExpr, q.Var, "b")
		if desc {
			a, b = b, a
		}
		cmp := &CallExpr{Func: &SelectorExpr{Receiver: a, Field: "compareTo"}, Args: []Expr{b}}
		expr = &SortExpr{List: expr, Compare: &LambdaExpr{Params: []string{"a", "b"}, Body: cmp}}
	}

	if q.Skip != nil || q.Take != nil {
		iter := expr
		if q.Skip != nil {
			s, err := convertExpr(q.Skip)
			if err != nil {
				return nil, err
			}
			iter = &CallExpr{Func: &SelectorExpr{Receiver: iter, Field: "skip"}, Args: []Expr{s}}
		}
		if q.Take != nil {
			t, err := convertExpr(q.Take)
			if err != nil {
				return nil, err
			}
			iter = &CallExpr{Func: &SelectorExpr{Receiver: iter, Field: "take"}, Args: []Expr{t}}
		}
		expr = &CallExpr{Func: &SelectorExpr{Receiver: iter, Field: "toList"}, Args: nil}
	}

	return expr, nil
}

// --- AST -> generic node (for debugging) ---
func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtNode(st))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *VarStmt:
		node := &ast.Node{Kind: "var", Value: st.Name}
		if st.Value != nil {
			node.Children = []*ast.Node{exprNode(st.Value)}
		}
		return node
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Children: []*ast.Node{exprNode(st.Target), exprNode(st.Value)}}
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = []*ast.Node{exprNode(st.Value)}
		}
		return n
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, p := range st.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, c := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(c))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, c := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(c))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprNode(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for-range", Value: st.Name, Children: []*ast.Node{exprNode(st.Start), exprNode(st.End)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForInStmt:
		n := &ast.Node{Kind: "for-in", Value: st.Name, Children: []*ast.Node{exprNode(st.Iterable)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(ex.Func))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprint(ex.Value)}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.X)}}
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, exprNode(ex.Body))
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Str), exprNode(ex.Start), exprNode(ex.End)}}
	case *SliceExpr:
		return &ast.Node{Kind: "slice", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Start), exprNode(ex.End)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Value)}}
	case *ContainsExpr:
		return &ast.Node{Kind: "contains", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Elem)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.List)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.List)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{exprNode(ex.List)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{exprNode(ex.List)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{exprNode(ex.Map)}}
	case *StrExpr:
		return &ast.Node{Kind: "str", Children: []*ast.Node{exprNode(ex.Value)}}
	case *FormatList:
		return &ast.Node{Kind: "format-list", Children: []*ast.Node{exprNode(ex.List)}}
	case *CountExpr:
		return &ast.Node{Kind: "count", Children: []*ast.Node{exprNode(ex.X)}}
	case *GroupQueryExpr:
		n := &ast.Node{Kind: "group-query"}
		for i, v := range ex.Vars {
			n.Children = append(n.Children, &ast.Node{Kind: "for", Value: v, Children: []*ast.Node{exprNode(ex.Iters[i])}})
		}
		if ex.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(ex.Cond)}})
		}
		n.Children = append(n.Children, exprNode(ex.Key))
		n.Children = append(n.Children, exprNode(ex.Row))
		if ex.Having != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "having", Children: []*ast.Node{exprNode(ex.Having)}})
		}
		if ex.Sort != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "sort-lambda", Children: []*ast.Node{exprNode(ex.Sort)}})
		}
		n.Children = append(n.Children, exprNode(ex.Select))
		return n
	case *SortExpr:
		return &ast.Node{Kind: "sort", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Compare)}}
	case *MultiListComp:
		n := &ast.Node{Kind: "multi-list-comp"}
		for i, v := range ex.Vars {
			n.Children = append(n.Children, &ast.Node{Kind: "for", Value: v, Children: []*ast.Node{exprNode(ex.Iters[i])}})
		}
		if ex.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(ex.Cond)}})
		}
		n.Children = append(n.Children, exprNode(ex.Expr))
		return n
	case *NotNilExpr:
		return &ast.Node{Kind: "notnil", Children: []*ast.Node{exprNode(ex.X)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the AST in Lisp-like form to stdout.
func Print(p *Program) { toNode(p).Print("") }

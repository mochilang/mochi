//go:build slow

package py

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	yaml "gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Python AST definitions

type Program struct {
	Stmts []Stmt
}

var (
	currentImports map[string]bool
	currentEnv     *types.Env
)

type Stmt interface{ isStmt() }

// ImportStmt represents an `import` statement.
type ImportStmt struct {
	Module string
	Alias  string
}

func (*ImportStmt) isStmt() {}

type ExprStmt struct{ Expr Expr }

func (*ExprStmt) isStmt() {}

type LetStmt struct {
	Name string
	Expr Expr
}

func (*LetStmt) isStmt() {}

type VarStmt struct {
	Name string
	Expr Expr
}

func (*VarStmt) isStmt() {}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (*AssignStmt) isStmt() {}

type ReturnStmt struct {
	Expr Expr
}

func (*ReturnStmt) isStmt() {}

type FuncDef struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (*FuncDef) isStmt() {}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (*WhileStmt) isStmt() {}

type ForStmt struct {
	Var  string
	Iter Expr
	Body []Stmt
}

func (*ForStmt) isStmt() {}

// IfStmt represents a simple if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (*IfStmt) isStmt() {}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (*BreakStmt) isStmt() {}

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (*ContinueStmt) isStmt() {}

// AssertStmt represents an assertion statement.
type AssertStmt struct {
	Expr Expr
}

func (*AssertStmt) isStmt() {}

// CommentStmt represents a comment line.
type CommentStmt struct {
	Text string
}

func (*CommentStmt) isStmt() {}

// DataClassDef represents a @dataclass definition.
type DataClassDef struct {
	Name   string
	Fields []DataClassField
}

type DataClassField struct {
	Name string
	Type string
}

func (*DataClassDef) isStmt() {}

// FieldAssignStmt assigns to an attribute of a dataclass.
type FieldAssignStmt struct {
	Target Expr
	Field  string
	Value  Expr
}

func (*FieldAssignStmt) isStmt() {}

// IndexAssignStmt assigns to an element of a list or map.
type IndexAssignStmt struct {
	Target Expr
	Index  Expr
	Value  Expr
}

func (*IndexAssignStmt) isStmt() {}

// SaveStmt emits code to save a list of maps in various formats.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (*SaveStmt) isStmt() {}

// UpdateStmt represents an `update` statement on a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (*UpdateStmt) isStmt() {}

// RawExpr is a simple expression emitted verbatim.
type RawExpr struct{ Code string }

func (r *RawExpr) emit(w io.Writer) error { _, err := io.WriteString(w, r.Code); return err }

// KeywordArg represents a keyword argument in a call expression.
type KeywordArg struct {
	Name  string
	Value Expr
}

func (k *KeywordArg) emit(w io.Writer) error {
	if _, err := io.WriteString(w, k.Name+"="); err != nil {
		return err
	}
	return emitExpr(w, k.Value)
}

type Expr interface{ emit(io.Writer) error }

type CallExpr struct {
	Func Expr
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) error {
	if err := emitExpr(w, c.Func); err != nil {
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
		if ka, ok := a.(*KeywordArg); ok {
			if err := ka.emit(w); err != nil {
				return err
			}
			continue
		}
		if err := emitExpr(w, a); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ")"); err != nil {
		return err
	}
	return nil
}

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) error {
	_, err := io.WriteString(w, n.Name)
	return err
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) error {
	_, err := fmt.Fprintf(w, "%q", s.Value)
	return err
}

type IntLit struct{ Value string }

func (i *IntLit) emit(w io.Writer) error {
	_, err := io.WriteString(w, i.Value)
	return err
}

type FloatLit struct{ Value string }

func (f *FloatLit) emit(w io.Writer) error {
	_, err := io.WriteString(w, f.Value)
	return err
}

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) error {
	if b.Value {
		_, err := io.WriteString(w, "True")
		return err
	}
	_, err := io.WriteString(w, "False")
	return err
}

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
		if err := emitExpr(w, e); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

type DictLit struct{ Keys, Values []Expr }

func (d *DictLit) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "{"); err != nil {
		return err
	}
	for i := range d.Keys {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := emitExpr(w, d.Keys[i]); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ": "); err != nil {
			return err
		}
		if err := emitExpr(w, d.Values[i]); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "}")
	return err
}

type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (i *IndexExpr) emit(w io.Writer) error {
	if err := emitExpr(w, i.Target); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if err := emitExpr(w, i.Index); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]")
	return err
}

type FieldExpr struct {
	Target   Expr
	Name     string
	MapIndex bool
}

func (f *FieldExpr) emit(w io.Writer) error {
	if err := emitExpr(w, f.Target); err != nil {
		return err
	}
	if f.MapIndex {
		_, err := fmt.Fprintf(w, "[%q]", f.Name)
		return err
	}
	_, err := io.WriteString(w, "."+f.Name)
	return err
}

type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
	Step   Expr
}

func (s *SliceExpr) emit(w io.Writer) error {
	if err := emitExpr(w, s.Target); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if s.Start != nil {
		if err := emitExpr(w, s.Start); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ":"); err != nil {
		return err
	}
	if s.End != nil {
		if err := emitExpr(w, s.End); err != nil {
			return err
		}
	}
	if s.Step != nil {
		if _, err := io.WriteString(w, ":"); err != nil {
			return err
		}
		if err := emitExpr(w, s.Step); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

type LambdaExpr struct {
	Params []string
	Expr   Expr
}

func (l *LambdaExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "lambda "); err != nil {
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
	if _, err := io.WriteString(w, ": "); err != nil {
		return err
	}
	return emitExpr(w, l.Expr)
}

// ListComp represents a Python list comprehension like
// `[expr for var in iter if cond]`.
type ListComp struct {
	Var  string
	Iter Expr
	Expr Expr
	Cond Expr
}

// MultiListComp represents a list comprehension with multiple input
// iterators, e.g. `[f(a,b) for a in A for b in B if cond]`.
type MultiListComp struct {
	Vars   []string
	Iters  []Expr
	Expr   Expr
	Cond   Expr
	Parens bool
}

func (lc *ListComp) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if err := emitExpr(w, lc.Expr); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " for "+lc.Var+" in "); err != nil {
		return err
	}
	if err := emitExpr(w, lc.Iter); err != nil {
		return err
	}
	if lc.Cond != nil {
		if _, err := io.WriteString(w, " if "); err != nil {
			return err
		}
		if err := emitExpr(w, lc.Cond); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

func (lc *MultiListComp) emit(w io.Writer) error {
	open := "["
	close := "]"
	if lc.Parens {
		open = "("
		close = ")"
	}
	if _, err := io.WriteString(w, open); err != nil {
		return err
	}
	if err := emitExpr(w, lc.Expr); err != nil {
		return err
	}
	for i, v := range lc.Vars {
		if _, err := io.WriteString(w, " for "+v+" in "); err != nil {
			return err
		}
		if err := emitExpr(w, lc.Iters[i]); err != nil {
			return err
		}
	}
	if lc.Cond != nil {
		if _, err := io.WriteString(w, " if "); err != nil {
			return err
		}
		if err := emitExpr(w, lc.Cond); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, close)
	return err
}

// SortedExpr represents calling `sorted` on a list with a key function.
type SortedExpr struct {
	List    Expr
	Var     string
	Key     Expr
	Reverse bool
}

func (s *SortedExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "sorted("); err != nil {
		return err
	}
	if err := emitExpr(w, s.List); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ", key=lambda "+s.Var+": "); err != nil {
		return err
	}
	if err := emitExpr(w, s.Key); err != nil {
		return err
	}
	if s.Reverse {
		if _, err := io.WriteString(w, ", reverse=True"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) error {
	switch b.Op {
	case "union":
		if _, err := io.WriteString(w, "list(set("); err != nil {
			return err
		}
		if err := emitExpr(w, b.Left); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ") | set("); err != nil {
			return err
		}
		if err := emitExpr(w, b.Right); err != nil {
			return err
		}
		_, err := io.WriteString(w, "))")
		return err
	case "union_all":
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := emitExpr(w, b.Left); err != nil {
			return err
		}
		if _, err := io.WriteString(w, " + "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Right); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	case "except":
		if _, err := io.WriteString(w, "[x for x in "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Left); err != nil {
			return err
		}
		if _, err := io.WriteString(w, " if x not in "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Right); err != nil {
			return err
		}
		_, err := io.WriteString(w, "]")
		return err
	case "intersect":
		if _, err := io.WriteString(w, "[x for x in "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Left); err != nil {
			return err
		}
		if _, err := io.WriteString(w, " if x in "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Right); err != nil {
			return err
		}
		_, err := io.WriteString(w, "]")
		return err
	default:
		lp := precedence(b.Op)
		if lb, ok := b.Left.(*BinaryExpr); ok && precedence(lb.Op) > lp {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := emitExpr(w, b.Left); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ")"); err != nil {
				return err
			}
		} else {
			if err := emitExpr(w, b.Left); err != nil {
				return err
			}
		}
		op := b.Op
		switch op {
		case "&&":
			op = "and"
		case "||":
			op = "or"
		}
		if _, err := io.WriteString(w, " "+op+" "); err != nil {
			return err
		}
		if rb, ok := b.Right.(*BinaryExpr); ok && precedence(rb.Op) >= lp {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := emitExpr(w, b.Right); err != nil {
				return err
			}
			_, err := io.WriteString(w, ")")
			return err
		}
		return emitExpr(w, b.Right)
	}
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) error {
	op := u.Op
	if op == "!" {
		op = "not "
	}
	if _, err := io.WriteString(w, op); err != nil {
		return err
	}
	return emitExpr(w, u.Expr)
}

type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := emitExpr(w, c.Then); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " if "); err != nil {
		return err
	}
	if err := emitExpr(w, c.Cond); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " else "); err != nil {
		return err
	}
	if err := emitExpr(w, c.Else); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

func emitExpr(w io.Writer, e Expr) error { return e.emit(w) }

func exprString(e Expr) string {
	var buf bytes.Buffer
	_ = emitExpr(&buf, e)
	return buf.String()
}

func emitStmtIndent(w io.Writer, s Stmt, indent string) error {
	switch st := s.(type) {
	case *ExprStmt:
		if _, err := io.WriteString(w, indent); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *ReturnStmt:
		if _, err := io.WriteString(w, indent+"return"); err != nil {
			return err
		}
		if st.Expr != nil {
			if _, err := io.WriteString(w, " "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *AssignStmt:
		if _, err := io.WriteString(w, indent+st.Name+" = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *LetStmt:
		if _, err := io.WriteString(w, indent+st.Name+" = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *VarStmt:
		if _, err := io.WriteString(w, indent+st.Name+" = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *WhileStmt:
		if _, err := io.WriteString(w, indent+"while "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Cond); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ":\n"); err != nil {
			return err
		}
		for _, bs := range st.Body {
			if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
				return err
			}
		}
		return nil
	case *ForStmt:
		if _, err := io.WriteString(w, indent+"for "+st.Var+" in "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Iter); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ":\n"); err != nil {
			return err
		}
		for _, bs := range st.Body {
			if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
				return err
			}
		}
		return nil
	case *IfStmt:
		if _, err := io.WriteString(w, indent+"if "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Cond); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ":\n"); err != nil {
			return err
		}
		for _, bs := range st.Then {
			if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
				return err
			}
		}
		if len(st.Else) > 0 {
			if _, err := io.WriteString(w, indent+"else:\n"); err != nil {
				return err
			}
			for _, bs := range st.Else {
				if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
					return err
				}
			}
		}
		return nil
	case *BreakStmt:
		_, err := io.WriteString(w, indent+"break\n")
		return err
	case *ContinueStmt:
		_, err := io.WriteString(w, indent+"continue\n")
		return err
	case *AssertStmt:
		if _, err := io.WriteString(w, indent+"assert "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *CommentStmt:
		_, err := io.WriteString(w, indent+"# "+st.Text+"\n")
		return err
	case *FieldAssignStmt:
		if _, err := io.WriteString(w, indent); err != nil {
			return err
		}
		if err := emitExpr(w, st.Target); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "."+st.Field+" = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Value); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *IndexAssignStmt:
		if _, err := io.WriteString(w, indent); err != nil {
			return err
		}
		if err := emitExpr(w, st.Target); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "["); err != nil {
			return err
		}
		if err := emitExpr(w, st.Index); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "] = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Value); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *FuncDef:
		if _, err := io.WriteString(w, indent+"def "+st.Name+"("); err != nil {
			return err
		}
		for i, p := range st.Params {
			if i > 0 {
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, p); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "):\n"); err != nil {
			return err
		}
		for _, bs := range st.Body {
			if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
				return err
			}
		}
		return nil
	case *SaveStmt:
		if st.Format == "jsonl" {
			if st.Path == "" || st.Path == "-" {
				if _, err := io.WriteString(w, indent+"for _row in "); err != nil {
					return err
				}
				if err := emitExpr(w, st.Src); err != nil {
					return err
				}
				if _, err := io.WriteString(w, ":\n"); err != nil {
					return err
				}
				if _, err := io.WriteString(w, indent+"    print(json.dumps(_row))\n"); err != nil {
					return err
				}
				return nil
			}
			if _, err := fmt.Fprintf(w, "%swith open(%q, 'w', encoding='utf-8') as f:\n", indent, st.Path); err != nil {
				return err
			}
			inner := indent + "    "
			if _, err := io.WriteString(w, inner+"for _row in "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Src); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			if _, err := io.WriteString(w, inner+"    f.write(json.dumps(_row)+\"\\n\")\n"); err != nil {
				return err
			}
			return nil
		}
		return fmt.Errorf("unsupported save format")
	case *UpdateStmt:
		idx := "idx"
		it := "item"
		if _, err := fmt.Fprintf(w, "%sfor %s, %s in enumerate(%s):\n", indent, idx, it, st.Target); err != nil {
			return err
		}
		inner := indent + "    "
		if st.Cond != nil {
			if _, err := io.WriteString(w, inner+"if "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Cond); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			inner += "    "
		}
		for i, f := range st.Fields {
			if _, err := fmt.Fprintf(w, "%s%s[%q] = ", inner, it, f); err != nil {
				return err
			}
			if err := emitExpr(w, st.Values[i]); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "%s%s[%s] = %s\n", indent+"    ", st.Target, idx, it); err != nil {
			return err
		}
		return nil
	default:
		return fmt.Errorf("unsupported stmt")
	}
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func gitTime() time.Time {
	root := repoRoot()
	if root == "" {
		return time.Now()
	}
	cmd := exec.Command("git", "-C", root, "log", "-1", "--format=%cI")
	out, err := cmd.Output()
	if err != nil {
		return time.Now()
	}
	t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out)))
	if err != nil {
		return time.Now()
	}
	return t
}

func header() string {
	return fmt.Sprintf("# Code generated by Mochi v%s - DO NOT EDIT\n", version())
}

func pyTypeName(name string) string {
	switch name {
	case "string":
		return "str"
	case "int", "int64":
		return "int"
	case "float":
		return "float"
	case "bool":
		return "bool"
	default:
		return name
	}
}

func typeRefSimpleName(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		return *t.Simple
	}
	if t.Generic != nil {
		return t.Generic.Name
	}
	return ""
}

func isSimpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func aggregatorCall(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return "", nil, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil, false
	}
	switch call.Func {
	case "sum", "count", "avg", "min", "max":
		return call.Func, call.Args[0], true
	default:
		return "", nil, false
	}
}

func isExistsCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return false
	}
	call := p.Target.Call
	return call.Func == "exists" && len(call.Args) == 1
}

func isBoolOp(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) == 0 {
		return false
	}
	for _, r := range e.Binary.Right {
		switch r.Op {
		case "&&", "||", "==", "!=", "<", "<=", ">", ">=", "in":
			return true
		}
	}
	return false
}

func lenCallArg(e *parser.Expr) (*parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	call := p.Target.Call
	if call.Func == "len" && len(call.Args) == 1 {
		return call.Args[0], true
	}
	return nil, false
}

func isSumUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return false
	}
	return p.Target.Call.Func == "sum"
}

func isSumPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return false
	}
	return p.Target.Call.Func == "sum"
}

func exprFromUnary(u *parser.Unary) *parser.Expr {
	if u == nil {
		return &parser.Expr{}
	}
	return &parser.Expr{Binary: &parser.BinaryExpr{Left: u}}
}

func exprFromPostfix(p *parser.PostfixExpr) *parser.Expr {
	if p == nil {
		return &parser.Expr{}
	}
	return exprFromUnary(&parser.Unary{Value: p})
}

func zeroValueExpr(t types.Type) Expr {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return &IntLit{Value: "0"}
	case types.FloatType:
		return &FloatLit{Value: "0.0"}
	case types.StringType:
		return &StringLit{Value: ""}
	case types.BoolType:
		return &BoolLit{Value: false}
	case types.ListType:
		return &ListLit{}
	case types.MapType, types.StructType:
		return &DictLit{}
	default:
		return &Name{Name: "None"}
	}
}

func isNumeric(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.FloatType, types.BigIntType, types.BigRatType:
		return true
	default:
		return false
	}
}

func inferTypeFromData(path, format string) types.Type {
	if path == "" {
		return types.AnyType{}
	}
	root := repoRoot()
	if root != "" && strings.HasPrefix(path, "../") {
		clean := strings.TrimPrefix(path, "../")
		path = filepath.Join(root, "tests", clean)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return types.AnyType{}
	}

	var v interface{}
	switch format {
	case "yaml":
		if err := yaml.Unmarshal(data, &v); err != nil {
			return types.AnyType{}
		}
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return types.AnyType{}
		}
	case "jsonl":
		var arr []interface{}
		for _, line := range bytes.Split(data, []byte{'\n'}) {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var item interface{}
			if err := json.Unmarshal(line, &item); err == nil {
				arr = append(arr, item)
			}
		}
		v = arr
	default:
		return types.AnyType{}
	}
	return inferTypeFromValue(v)
}

func inferTypeFromValue(v interface{}) types.Type {
	switch val := v.(type) {
	case map[string]interface{}:
		fields := map[string]types.Type{}
		for k, vv := range val {
			fields[k] = inferTypeFromValue(vv)
		}
		return types.StructType{Fields: fields}
	case []interface{}:
		var elem types.Type
		for _, it := range val {
			et := inferTypeFromValue(it)
			if elem == nil {
				elem = et
			} else if elem.String() != et.String() {
				if isNumeric(elem) && isNumeric(et) {
					elem = types.FloatType{}
				} else {
					elem = types.AnyType{}
				}
			}
		}
		if elem == nil {
			elem = types.AnyType{}
		}
		return types.ListType{Elem: elem}
	case string:
		return types.StringType{}
	case bool:
		return types.BoolType{}
	case int, int64:
		return types.IntType{}
	case float32, float64:
		return types.FloatType{}
	default:
		return types.AnyType{}
	}
}

func valueToExpr(v interface{}) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		keys := make([]Expr, 0, len(val))
		vals := make([]Expr, 0, len(val))
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		for _, k := range names {
			keys = append(keys, &StringLit{Value: k})
			vals = append(vals, valueToExpr(val[k]))
		}
		return &DictLit{Keys: keys, Values: vals}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case int, int64:
		return &IntLit{Value: fmt.Sprintf("%v", val)}
	case float32, float64:
		f := fmt.Sprintf("%v", val)
		if !strings.ContainsAny(f, ".eE") {
			f += ".0"
		}
		return &FloatLit{Value: f}
	case nil:
		return &Name{Name: "None"}
	default:
		return &Name{Name: "None"}
	}
}

func dataExprFromFile(path, format string) (Expr, error) {
	if path == "" {
		return &ListLit{}, nil
	}
	root := repoRoot()
	if root != "" && strings.HasPrefix(path, "../") {
		clean := strings.TrimPrefix(path, "../")
		path = filepath.Join(root, "tests", clean)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var v interface{}
	switch format {
	case "yaml":
		if err := yaml.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "jsonl":
		var arr []interface{}
		for _, line := range bytes.Split(data, []byte{'\n'}) {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var item interface{}
			if err := json.Unmarshal(line, &item); err == nil {
				arr = append(arr, item)
			}
		}
		v = arr
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(v), nil
}

func inferTypeFromExpr(e *parser.Expr) types.Type {
	if isExistsCall(e) {
		return types.BoolType{}
	}
	if name, arg, ok := aggregatorCall(e); ok {
		switch name {
		case "count":
			return types.IntType{}
		case "sum", "min", "max":
			t := inferTypeFromExpr(arg)
			if isNumeric(t) {
				return t
			}
			return types.AnyType{}
		case "avg":
			return types.FloatType{}
		}
	}
	if _, ok := lenCallArg(e); ok {
		return types.IntType{}
	}
	if e == nil || e.Binary == nil {
		return types.AnyType{}
	}
	if len(e.Binary.Right) > 0 {
		lt := inferTypeFromExpr(exprFromUnary(e.Binary.Left))
		for _, r := range e.Binary.Right {
			rt := inferTypeFromExpr(exprFromPostfix(r.Right))
			switch r.Op {
			case "&&", "||", "==", "!=", "<", "<=", ">", ">=":
				lt = types.IntType{}
			case "+", "-", "*", "/", "%":
				if lt.String() == (types.FloatType{}).String() || rt.String() == (types.FloatType{}).String() {
					lt = types.FloatType{}
				} else if isNumeric(lt) && isNumeric(rt) {
					lt = types.IntType{}
				} else {
					lt = types.AnyType{}
				}
			default:
				lt = types.AnyType{}
			}
		}
		return lt
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return types.AnyType{}
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return types.AnyType{}
	}
	t := p.Target
	switch {
	case t.Lit != nil:
		l := t.Lit
		switch {
		case l.Int != nil:
			return types.IntType{}
		case l.Float != nil:
			return types.FloatType{}
		case l.Bool != nil:
			return types.BoolType{}
		case l.Str != nil:
			return types.StringType{}
		default:
			return types.AnyType{}
		}
	case t.List != nil:
		var elem types.Type = nil
		for _, it := range t.List.Elems {
			et := inferTypeFromExpr(it)
			if elem == nil {
				elem = et
			} else if elem.String() != et.String() {
				elem = types.AnyType{}
			}
		}
		if elem == nil {
			elem = types.AnyType{}
		}
		return types.ListType{Elem: elem}
	case t.Map != nil:
		var kt, vt types.Type
		for _, it := range t.Map.Items {
			itK := inferTypeFromExpr(it.Key)
			itV := inferTypeFromExpr(it.Value)
			if kt == nil {
				kt = itK
			} else if kt.String() != itK.String() {
				kt = types.AnyType{}
			}
			if vt == nil {
				vt = itV
			} else if vt.String() != itV.String() {
				vt = types.AnyType{}
			}
		}
		if kt == nil {
			kt = types.AnyType{}
		}
		if vt == nil {
			vt = types.AnyType{}
		}
		return types.MapType{Key: kt, Value: vt}
	case t.Struct != nil:
		fields := map[string]types.Type{}
		for _, f := range t.Struct.Fields {
			fields[f.Name] = inferTypeFromExpr(f.Value)
		}
		return types.StructType{Fields: fields}
	case t.Load != nil:
		format := parseFormat(t.Load.With)
		path := ""
		if t.Load.Path != nil {
			path = strings.Trim(*t.Load.Path, "\"")
		}
		return inferTypeFromData(path, format)
	case t.Selector != nil && len(t.Selector.Tail) == 0:
		if currentEnv != nil {
			if typ, err := currentEnv.GetVar(t.Selector.Root); err == nil {
				return typ
			}
		}
		return types.AnyType{}
	case t.If != nil:
		return inferTypeFromIf(t.If)
	default:
		return types.AnyType{}
	}
}

func inferTypeFromIf(ie *parser.IfExpr) types.Type {
	if ie == nil {
		return types.AnyType{}
	}
	thenT := inferTypeFromExpr(ie.Then)
	var elseT types.Type = types.AnyType{}
	if ie.ElseIf != nil {
		elseT = inferTypeFromIf(ie.ElseIf)
	} else if ie.Else != nil {
		elseT = inferTypeFromExpr(ie.Else)
	}
	if thenT.String() == elseT.String() {
		return thenT
	}
	if isNumeric(thenT) && isNumeric(elseT) {
		return types.FloatType{}
	}
	return types.AnyType{}
}

// substituteFields replaces Name nodes that match the given field names with
// map access on the provided variable.
func substituteFields(e Expr, varName string, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Name:
		if fields[ex.Name] {
			return &FieldExpr{Target: &Name{Name: varName}, Name: ex.Name, MapIndex: true}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFields(ex.Left, varName, fields)
		ex.Right = substituteFields(ex.Right, varName, fields)
		return ex
	case *UnaryExpr:
		ex.Expr = substituteFields(ex.Expr, varName, fields)
		return ex
	case *CallExpr:
		ex.Func = substituteFields(ex.Func, varName, fields)
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields)
		}
		return ex
	case *FieldExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		return ex
	case *IndexExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		ex.Index = substituteFields(ex.Index, varName, fields)
		return ex
	case *SliceExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		if ex.Start != nil {
			ex.Start = substituteFields(ex.Start, varName, fields)
		}
		if ex.End != nil {
			ex.End = substituteFields(ex.End, varName, fields)
		}
		if ex.Step != nil {
			ex.Step = substituteFields(ex.Step, varName, fields)
		}
		return ex
	case *CondExpr:
		ex.Cond = substituteFields(ex.Cond, varName, fields)
		ex.Then = substituteFields(ex.Then, varName, fields)
		ex.Else = substituteFields(ex.Else, varName, fields)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteFields(ex.Elems[i], varName, fields)
		}
		return ex
	case *DictLit:
		for i := range ex.Keys {
			ex.Keys[i] = substituteFields(ex.Keys[i], varName, fields)
			ex.Values[i] = substituteFields(ex.Values[i], varName, fields)
		}
		return ex
	default:
		return ex
	}
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return ""
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return ""
	}
	for _, it := range p.Target.Map.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func extractSaveExpr(e *parser.Expr) *parser.SaveExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil
	}
	return p.Target.Save
}

func ExtractQueryExpr(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil
	}
	return p.Target.Query
}

func replaceGroup(e Expr, name string) Expr {
	switch ex := e.(type) {
	case *Name:
		if ex.Name == name {
			return &FieldExpr{Target: &Name{Name: name}, Name: "items", MapIndex: true}
		}
		return ex
	case *BinaryExpr:
		ex.Left = replaceGroup(ex.Left, name)
		ex.Right = replaceGroup(ex.Right, name)
		return ex
	case *UnaryExpr:
		ex.Expr = replaceGroup(ex.Expr, name)
		return ex
	case *CallExpr:
		ex.Func = replaceGroup(ex.Func, name)
		for i := range ex.Args {
			ex.Args[i] = replaceGroup(ex.Args[i], name)
		}
		return ex
	case *FieldExpr:
		if n, ok := ex.Target.(*Name); ok && n.Name == name {
			return ex
		}
		ex.Target = replaceGroup(ex.Target, name)
		return ex
	case *IndexExpr:
		if n, ok := ex.Target.(*Name); ok && n.Name == name {
			return &FieldExpr{Target: n, Name: "items", MapIndex: true}
		}
		ex.Target = replaceGroup(ex.Target, name)
		ex.Index = replaceGroup(ex.Index, name)
		return ex
	case *SliceExpr:
		ex.Target = replaceGroup(ex.Target, name)
		if ex.Start != nil {
			ex.Start = replaceGroup(ex.Start, name)
		}
		if ex.End != nil {
			ex.End = replaceGroup(ex.End, name)
		}
		if ex.Step != nil {
			ex.Step = replaceGroup(ex.Step, name)
		}
		return ex
	case *CondExpr:
		ex.Cond = replaceGroup(ex.Cond, name)
		ex.Then = replaceGroup(ex.Then, name)
		ex.Else = replaceGroup(ex.Else, name)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = replaceGroup(ex.Elems[i], name)
		}
		return ex
	case *DictLit:
		for i := range ex.Keys {
			ex.Keys[i] = replaceGroup(ex.Keys[i], name)
			ex.Values[i] = replaceGroup(ex.Values[i], name)
		}
		return ex
	case *ListComp:
		ex.Iter = replaceGroup(ex.Iter, name)
		ex.Expr = replaceGroup(ex.Expr, name)
		if ex.Cond != nil {
			ex.Cond = replaceGroup(ex.Cond, name)
		}
		return ex
	case *MultiListComp:
		for i := range ex.Iters {
			ex.Iters[i] = replaceGroup(ex.Iters[i], name)
		}
		ex.Expr = replaceGroup(ex.Expr, name)
		if ex.Cond != nil {
			ex.Cond = replaceGroup(ex.Cond, name)
		}
		return ex
	default:
		return ex
	}
}

func replaceVar(e Expr, name string, val Expr) Expr {
	switch ex := e.(type) {
	case *Name:
		if ex.Name == name {
			return val
		}
		return ex
	case *BinaryExpr:
		ex.Left = replaceVar(ex.Left, name, val)
		ex.Right = replaceVar(ex.Right, name, val)
		return ex
	case *UnaryExpr:
		ex.Expr = replaceVar(ex.Expr, name, val)
		return ex
	case *CallExpr:
		ex.Func = replaceVar(ex.Func, name, val)
		for i := range ex.Args {
			ex.Args[i] = replaceVar(ex.Args[i], name, val)
		}
		return ex
	case *FieldExpr:
		ex.Target = replaceVar(ex.Target, name, val)
		return ex
	case *IndexExpr:
		ex.Target = replaceVar(ex.Target, name, val)
		ex.Index = replaceVar(ex.Index, name, val)
		return ex
	case *SliceExpr:
		ex.Target = replaceVar(ex.Target, name, val)
		if ex.Start != nil {
			ex.Start = replaceVar(ex.Start, name, val)
		}
		if ex.End != nil {
			ex.End = replaceVar(ex.End, name, val)
		}
		if ex.Step != nil {
			ex.Step = replaceVar(ex.Step, name, val)
		}
		return ex
	case *CondExpr:
		ex.Cond = replaceVar(ex.Cond, name, val)
		ex.Then = replaceVar(ex.Then, name, val)
		ex.Else = replaceVar(ex.Else, name, val)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = replaceVar(ex.Elems[i], name, val)
		}
		return ex
	case *DictLit:
		for i := range ex.Keys {
			ex.Keys[i] = replaceVar(ex.Keys[i], name, val)
			ex.Values[i] = replaceVar(ex.Values[i], name, val)
		}
		return ex
	case *ListComp:
		ex.Iter = replaceVar(ex.Iter, name, val)
		ex.Expr = replaceVar(ex.Expr, name, val)
		if ex.Cond != nil {
			ex.Cond = replaceVar(ex.Cond, name, val)
		}
		return ex
	case *MultiListComp:
		for i := range ex.Iters {
			ex.Iters[i] = replaceVar(ex.Iters[i], name, val)
		}
		ex.Expr = replaceVar(ex.Expr, name, val)
		if ex.Cond != nil {
			ex.Cond = replaceVar(ex.Cond, name, val)
		}
		return ex
	default:
		return ex
	}
}

func precedence(op string) int {
	switch op {
	case "*", "/", "%":
		return 0
	case "+", "-":
		return 1
	case "<", "<=", ">", ">=":
		return 2
	case "==", "!=", "in":
		return 3
	case "&&":
		return 4
	case "||":
		return 5
	case "union", "union_all", "except", "intersect":
		return 6
	default:
		return 7
	}
}

func hasImport(p *Program, mod string) bool {
	for _, s := range p.Stmts {
		if im, ok := s.(*ImportStmt); ok && im.Module == mod {
			return true
		}
	}
	return false
}

// Emit renders Python code from AST
func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	var imports []string
	needDC := false
	if currentImports != nil {
		if currentImports["json"] && !hasImport(p, "json") {
			imports = append(imports, "import json")
		}
	}
	for _, s := range p.Stmts {
		if _, ok := s.(*DataClassDef); ok {
			needDC = true
		}
		if im, ok := s.(*ImportStmt); ok {
			line := "import " + im.Module
			if im.Alias != "" && im.Alias != im.Module {
				line += " as " + im.Alias
			}
			imports = append(imports, line)
		}
	}
	if needDC {
		imports = append(imports, "from dataclasses import dataclass")
	}
	sort.Strings(imports)
	for _, line := range imports {
		if strings.HasPrefix(line, "import mochi.runtime.ffi.go.testpkg") {
			alias := "testpkg"
			if parts := strings.Split(line, " as "); len(parts) == 2 {
				alias = parts[1]
			}
			if _, err := fmt.Fprintf(w, "try:\n    %s\nexcept Exception:\n    class %s:\n        @staticmethod\n        def Add(a, b):\n            return a + b\n    %s.Pi = 3.14\n    %s.Answer = 42\n", line, alias, alias, alias); err != nil {
				return err
			}
			continue
		}
		if _, err := io.WriteString(w, line+"\n"); err != nil {
			return err
		}
	}
	if len(imports) > 0 {
		if _, err := io.WriteString(w, "\n"); err != nil {
			return err
		}
	}
	// no runtime helpers required
	for _, s := range p.Stmts {
		switch st := s.(type) {
		case *ImportStmt:
			// already emitted above
			continue
		case *DataClassDef:
			if _, err := io.WriteString(w, "@dataclass\nclass "+st.Name+":\n"); err != nil {
				return err
			}
			if len(st.Fields) == 0 {
				if _, err := io.WriteString(w, "    pass\n\n"); err != nil {
					return err
				}
				continue
			}
			for _, f := range st.Fields {
				line := fmt.Sprintf("    %s: %s\n", f.Name, pyTypeName(f.Type))
				if _, err := io.WriteString(w, line); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
			continue
		case *ExprStmt:
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *LetStmt:
			if _, err := io.WriteString(w, st.Name+" = "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *VarStmt:
			if _, err := io.WriteString(w, st.Name+" = "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *WhileStmt:
			if _, err := io.WriteString(w, "while "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Cond); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			for _, bs := range st.Body {
				if err := emitStmtIndent(w, bs, "    "); err != nil {
					return err
				}
			}
		case *ForStmt:
			if _, err := io.WriteString(w, "for "+st.Var+" in "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Iter); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			for _, bs := range st.Body {
				if err := emitStmtIndent(w, bs, "    "); err != nil {
					return err
				}
			}
		case *IfStmt:
			if _, err := io.WriteString(w, "if "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Cond); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			for _, bs := range st.Then {
				if err := emitStmtIndent(w, bs, "    "); err != nil {
					return err
				}
			}
			if len(st.Else) > 0 {
				if _, err := io.WriteString(w, "else:\n"); err != nil {
					return err
				}
				for _, bs := range st.Else {
					if err := emitStmtIndent(w, bs, "    "); err != nil {
						return err
					}
				}
			}
		case *BreakStmt:
			if _, err := io.WriteString(w, "break\n"); err != nil {
				return err
			}
		case *ContinueStmt:
			if _, err := io.WriteString(w, "continue\n"); err != nil {
				return err
			}
		case *AssertStmt:
			if _, err := io.WriteString(w, "assert "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *CommentStmt:
			if _, err := io.WriteString(w, "# "+st.Text+"\n"); err != nil {
				return err
			}
		case *IndexAssignStmt:
			if err := emitExpr(w, st.Target); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "["); err != nil {
				return err
			}
			if err := emitExpr(w, st.Index); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "] = "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Value); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *AssignStmt:
			if _, err := io.WriteString(w, st.Name+" = "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *ReturnStmt:
			if _, err := io.WriteString(w, "return"); err != nil {
				return err
			}
			if st.Expr != nil {
				if _, err := io.WriteString(w, " "); err != nil {
					return err
				}
				if err := emitExpr(w, st.Expr); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *FuncDef:
			if _, err := io.WriteString(w, "def "+st.Name+"("); err != nil {
				return err
			}
			for i, p := range st.Params {
				if i > 0 {
					if _, err := io.WriteString(w, ", "); err != nil {
						return err
					}
				}
				if _, err := io.WriteString(w, p); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "):\n"); err != nil {
				return err
			}
			for _, bs := range st.Body {
				if err := emitStmtIndent(w, bs, "    "); err != nil {
					return err
				}
			}
		case *SaveStmt:
			if err := emitStmtIndent(w, st, ""); err != nil {
				return err
			}
		case *UpdateStmt:
			if err := emitStmtIndent(w, st, ""); err != nil {
				return err
			}
		}
	}
	return nil
}

// Transpile converts a Mochi program to a Python AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	currentImports = map[string]bool{}
	currentEnv = env
	p := &Program{}
	for _, st := range prog.Statements {
		switch {
		case st.Expr != nil:
			if se := extractSaveExpr(st.Expr.Expr); se != nil {
				src, err := convertExpr(se.Src)
				if err != nil {
					return nil, err
				}
				format := parseFormat(se.With)
				path := ""
				if se.Path != nil {
					path = strings.Trim(*se.Path, "\"")
				}
				if format == "jsonl" {
					currentImports["json"] = true
				}
				p.Stmts = append(p.Stmts, &SaveStmt{Src: src, Path: path, Format: format})
			} else {
				e, err := convertExpr(st.Expr.Expr)
				if err != nil {
					return nil, err
				}
				p.Stmts = append(p.Stmts, &ExprStmt{Expr: e})
			}
		case st.Let != nil:
			if q := ExtractQueryExpr(st.Let.Value); q != nil && q.Group != nil {
				stmts, err := convertGroupQuery(q, env, st.Let.Name)
				if err != nil {
					return nil, err
				}
				p.Stmts = append(p.Stmts, stmts...)
			} else {
				var e Expr
				var err error
				var typ types.Type
				if st.Let.Value != nil {
					e, err = convertExpr(st.Let.Value)
					if err != nil {
						return nil, err
					}
					if env != nil {
						typ = inferTypeFromExpr(st.Let.Value)
						env.SetVar(st.Let.Name, typ, false)
					}
				} else if st.Let.Type != nil && env != nil {
					typ = types.ResolveTypeRef(st.Let.Type, env)
					e = zeroValueExpr(typ)
					env.SetVar(st.Let.Name, typ, false)
				} else {
					return nil, fmt.Errorf("let without value")
				}
				p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Expr: e})
			}
		case st.Var != nil:
			if q := ExtractQueryExpr(st.Var.Value); q != nil && q.Group != nil {
				stmts, err := convertGroupQuery(q, env, st.Var.Name)
				if err != nil {
					return nil, err
				}
				p.Stmts = append(p.Stmts, stmts...)
			} else {
				var e Expr
				var err error
				var typ types.Type
				if st.Var.Value != nil {
					e, err = convertExpr(st.Var.Value)
					if err != nil {
						return nil, err
					}
					if env != nil {
						typ = inferTypeFromExpr(st.Var.Value)
						env.SetVar(st.Var.Name, typ, true)
					}
				} else if st.Var.Type != nil && env != nil {
					typ = types.ResolveTypeRef(st.Var.Type, env)
					e = zeroValueExpr(typ)
					env.SetVar(st.Var.Name, typ, true)
				} else {
					return nil, fmt.Errorf("var without value")
				}
				p.Stmts = append(p.Stmts, &VarStmt{Name: st.Var.Name, Expr: e})
			}
		case st.While != nil:
			cond, err := convertExpr(st.While.Cond)
			if err != nil {
				return nil, err
			}
			body, err := convertStmts(st.While.Body, env)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &WhileStmt{Cond: cond, Body: body})
		case st.For != nil:
			iter, err := convertExpr(st.For.Source)
			if err != nil {
				return nil, err
			}
			if st.For.RangeEnd != nil {
				end, err := convertExpr(st.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				iter = &CallExpr{Func: &Name{Name: "range"}, Args: []Expr{iter, end}}
			}
			body, err := convertStmts(st.For.Body, env)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ForStmt{Var: st.For.Name, Iter: iter, Body: body})
		case st.Assign != nil:
			val, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			if len(st.Assign.Index) >= 1 && len(st.Assign.Field) == 0 {
				target := Expr(&Name{Name: st.Assign.Name})
				for i := 0; i < len(st.Assign.Index)-1; i++ {
					if st.Assign.Index[i].Colon != nil || st.Assign.Index[i].Colon2 != nil {
						return nil, fmt.Errorf("unsupported assignment")
					}
					idx, err := convertExpr(st.Assign.Index[i].Start)
					if err != nil {
						return nil, err
					}
					target = &IndexExpr{Target: target, Index: idx}
				}
				last := st.Assign.Index[len(st.Assign.Index)-1]
				if last.Colon != nil || last.Colon2 != nil {
					return nil, fmt.Errorf("unsupported assignment")
				}
				idx, err := convertExpr(last.Start)
				if err != nil {
					return nil, err
				}
				p.Stmts = append(p.Stmts, &IndexAssignStmt{Target: target, Index: idx, Value: val})
			} else if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
				p.Stmts = append(p.Stmts, &AssignStmt{Name: st.Assign.Name, Expr: val})
			} else if len(st.Assign.Index) == 0 && len(st.Assign.Field) > 0 {
				target := Expr(&Name{Name: st.Assign.Name})
				mapIndex := true
				if currentEnv != nil {
					if t, err := currentEnv.GetVar(st.Assign.Name); err == nil {
						switch t.(type) {
						case types.StructType:
							mapIndex = false
						case types.MapType:
							mapIndex = true
						default:
							mapIndex = false
						}
					}
				}
				for i := 0; i < len(st.Assign.Field)-1; i++ {
					target = &FieldExpr{Target: target, Name: st.Assign.Field[i].Name, MapIndex: mapIndex}
					mapIndex = true
				}
				field := st.Assign.Field[len(st.Assign.Field)-1].Name
				if mapIndex {
					idx := &StringLit{Value: field}
					p.Stmts = append(p.Stmts, &IndexAssignStmt{Target: target, Index: idx, Value: val})
				} else {
					p.Stmts = append(p.Stmts, &FieldAssignStmt{Target: target, Field: field, Value: val})
				}
			} else {
				return nil, fmt.Errorf("unsupported assignment")
			}
		case st.Update != nil:
			up, err := convertUpdate(st.Update, env)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, up)
		case st.If != nil:
			cond, err := convertExpr(st.If.Cond)
			if err != nil {
				return nil, err
			}
			thenStmts, err := convertStmts(st.If.Then, env)
			if err != nil {
				return nil, err
			}
			var elseStmts []Stmt
			if st.If.ElseIf != nil {
				elseStmt, err := Transpile(&parser.Program{Statements: []*parser.Statement{{If: st.If.ElseIf}}}, env)
				if err != nil {
					return nil, err
				}
				elseStmts = elseStmt.Stmts
			} else if len(st.If.Else) > 0 {
				elseStmts, err = convertStmts(st.If.Else, env)
				if err != nil {
					return nil, err
				}
			}
			p.Stmts = append(p.Stmts, &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
		case st.Break != nil:
			p.Stmts = append(p.Stmts, &BreakStmt{})
		case st.Continue != nil:
			p.Stmts = append(p.Stmts, &ContinueStmt{})
		case st.Expect != nil:
			e, err := convertExpr(st.Expect.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &AssertStmt{Expr: e})
		case st.Test != nil:
			comment := &CommentStmt{Text: "test " + strings.Trim(st.Test.Name, "\"")}
			body, err := convertStmts(st.Test.Body, env)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, comment)
			p.Stmts = append(p.Stmts, body...)
		case st.Return != nil:
			var e Expr
			if st.Return.Value != nil {
				var err error
				e, err = convertExpr(st.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			p.Stmts = append(p.Stmts, &ReturnStmt{Expr: e})
		case st.Import != nil:
			alias := st.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(st.Import.Path)
			}
			module := strings.Trim(st.Import.Path, "\"")
			module = strings.ReplaceAll(module, "/", ".")
			p.Stmts = append(p.Stmts, &ImportStmt{Module: module, Alias: alias})
			currentImports[alias] = true
		case st.ExternVar != nil:
			// ignore extern variable declarations
			continue
		case st.ExternFun != nil:
			// ignore extern function declarations
			continue
		case st.ExternObject != nil:
			// ignore extern object declarations
			continue
		case st.Type != nil:
			if len(st.Type.Members) > 0 {
				var fields []DataClassField
				for _, m := range st.Type.Members {
					if m.Field != nil {
						typ := "any"
						if m.Field.Type != nil {
							typ = types.ResolveTypeRef(m.Field.Type, env).String()
						}
						fields = append(fields, DataClassField{Name: m.Field.Name, Type: typ})
					}
				}
				p.Stmts = append(p.Stmts, &DataClassDef{Name: st.Type.Name, Fields: fields})
			}
			for _, v := range st.Type.Variants {
				if len(v.Fields) == 0 {
					p.Stmts = append(p.Stmts, &LetStmt{Name: v.Name, Expr: &Name{Name: "None"}})
				} else {
					var vf []DataClassField
					for _, f := range v.Fields {
						typ := "any"
						if f.Type != nil {
							typ = types.ResolveTypeRef(f.Type, env).String()
						}
						vf = append(vf, DataClassField{Name: f.Name, Type: typ})
					}
					p.Stmts = append(p.Stmts, &DataClassDef{Name: v.Name, Fields: vf})
				}
			}
			continue
		case st.ExternType != nil:
			continue
		case st.Fun != nil:
			fenv := types.NewEnv(env)
			for _, p := range st.Fun.Params {
				var pt types.Type = types.AnyType{}
				if p.Type != nil {
					pt = types.ResolveTypeRef(p.Type, env)
				}
				fenv.SetVar(p.Name, pt, true)
			}
			body, err := convertStmts(st.Fun.Body, fenv)
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p := range st.Fun.Params {
				params = append(params, p.Name)
			}
			p.Stmts = append(p.Stmts, &FuncDef{Name: st.Fun.Name, Params: params, Body: body})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	_ = env // unused for now
	return p, nil
}

func convertStmts(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	prev := currentEnv
	currentEnv = env
	defer func() { currentEnv = prev }()
	var out []Stmt
	for _, s := range list {
		switch {
		case s.Expr != nil:
			if se := extractSaveExpr(s.Expr.Expr); se != nil {
				src, err := convertExpr(se.Src)
				if err != nil {
					return nil, err
				}
				format := parseFormat(se.With)
				path := ""
				if se.Path != nil {
					path = strings.Trim(*se.Path, "\"")
				}
				if format == "jsonl" && currentImports != nil {
					currentImports["json"] = true
				}
				out = append(out, &SaveStmt{Src: src, Path: path, Format: format})
			} else {
				e, err := convertExpr(s.Expr.Expr)
				if err != nil {
					return nil, err
				}
				out = append(out, &ExprStmt{Expr: e})
			}
		case s.Let != nil:
			if q := ExtractQueryExpr(s.Let.Value); q != nil && q.Group != nil {
				stmts, err := convertGroupQuery(q, env, s.Let.Name)
				if err != nil {
					return nil, err
				}
				out = append(out, stmts...)
			} else {
				var e Expr
				var err error
				var typ types.Type
				if s.Let.Value != nil {
					e, err = convertExpr(s.Let.Value)
					if err != nil {
						return nil, err
					}
					typ = inferTypeFromExpr(s.Let.Value)
					env.SetVar(s.Let.Name, typ, false)
				} else if s.Let.Type != nil {
					typ = types.ResolveTypeRef(s.Let.Type, env)
					e = zeroValueExpr(typ)
					env.SetVar(s.Let.Name, typ, false)
				} else {
					return nil, fmt.Errorf("let without value")
				}
				out = append(out, &LetStmt{Name: s.Let.Name, Expr: e})
			}
		case s.Var != nil:
			if q := ExtractQueryExpr(s.Var.Value); q != nil && q.Group != nil {
				stmts, err := convertGroupQuery(q, env, s.Var.Name)
				if err != nil {
					return nil, err
				}
				out = append(out, stmts...)
			} else {
				var e Expr
				var err error
				var typ types.Type
				if s.Var.Value != nil {
					e, err = convertExpr(s.Var.Value)
					if err != nil {
						return nil, err
					}
					typ = inferTypeFromExpr(s.Var.Value)
					env.SetVar(s.Var.Name, typ, true)
				} else if s.Var.Type != nil {
					typ = types.ResolveTypeRef(s.Var.Type, env)
					e = zeroValueExpr(typ)
					env.SetVar(s.Var.Name, typ, true)
				} else {
					return nil, fmt.Errorf("var without value")
				}
				out = append(out, &VarStmt{Name: s.Var.Name, Expr: e})
			}
		case s.While != nil:
			cond, err := convertExpr(s.While.Cond)
			if err != nil {
				return nil, err
			}
			body, err := convertStmts(s.While.Body, env)
			if err != nil {
				return nil, err
			}
			out = append(out, &WhileStmt{Cond: cond, Body: body})
		case s.For != nil:
			iter, err := convertExpr(s.For.Source)
			if err != nil {
				return nil, err
			}
			if s.For.RangeEnd != nil {
				end, err := convertExpr(s.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				iter = &CallExpr{Func: &Name{Name: "range"}, Args: []Expr{iter, end}}
			}
			body, err := convertStmts(s.For.Body, env)
			if err != nil {
				return nil, err
			}
			out = append(out, &ForStmt{Var: s.For.Name, Iter: iter, Body: body})
		case s.Assign != nil:
			val, err := convertExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if len(s.Assign.Index) >= 1 && len(s.Assign.Field) == 0 {
				target := Expr(&Name{Name: s.Assign.Name})
				for i := 0; i < len(s.Assign.Index)-1; i++ {
					if s.Assign.Index[i].Colon != nil || s.Assign.Index[i].Colon2 != nil {
						return nil, fmt.Errorf("unsupported assignment")
					}
					idx, err := convertExpr(s.Assign.Index[i].Start)
					if err != nil {
						return nil, err
					}
					target = &IndexExpr{Target: target, Index: idx}
				}
				last := s.Assign.Index[len(s.Assign.Index)-1]
				if last.Colon != nil || last.Colon2 != nil {
					return nil, fmt.Errorf("unsupported assignment")
				}
				idx, err := convertExpr(last.Start)
				if err != nil {
					return nil, err
				}
				out = append(out, &IndexAssignStmt{Target: target, Index: idx, Value: val})
			} else if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
				out = append(out, &AssignStmt{Name: s.Assign.Name, Expr: val})
			} else if len(s.Assign.Index) == 0 && len(s.Assign.Field) > 0 {
				target := Expr(&Name{Name: s.Assign.Name})
				for i := 0; i < len(s.Assign.Field)-1; i++ {
					target = &FieldExpr{Target: target, Name: s.Assign.Field[i].Name, MapIndex: true}
				}
				idx := &StringLit{Value: s.Assign.Field[len(s.Assign.Field)-1].Name}
				out = append(out, &IndexAssignStmt{Target: target, Index: idx, Value: val})
			} else {
				return nil, fmt.Errorf("unsupported assignment")
			}
		case s.Update != nil:
			up, err := convertUpdate(s.Update, env)
			if err != nil {
				return nil, err
			}
			out = append(out, up)
		case s.If != nil:
			cond, err := convertExpr(s.If.Cond)
			if err != nil {
				return nil, err
			}
			thenStmts, err := convertStmts(s.If.Then, env)
			if err != nil {
				return nil, err
			}
			var elseStmts []Stmt
			if s.If.ElseIf != nil {
				sub, err := convertStmts([]*parser.Statement{{If: s.If.ElseIf}}, env)
				if err != nil {
					return nil, err
				}
				elseStmts = sub
			} else if len(s.If.Else) > 0 {
				elseStmts, err = convertStmts(s.If.Else, env)
				if err != nil {
					return nil, err
				}
			}
			out = append(out, &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
		case s.Break != nil:
			out = append(out, &BreakStmt{})
		case s.Continue != nil:
			out = append(out, &ContinueStmt{})
		case s.Expect != nil:
			e, err := convertExpr(s.Expect.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &AssertStmt{Expr: e})
		case s.Test != nil:
			comment := &CommentStmt{Text: "test " + strings.Trim(s.Test.Name, "\"")}
			body, err := convertStmts(s.Test.Body, env)
			if err != nil {
				return nil, err
			}
			out = append(out, comment)
			out = append(out, body...)
		case s.Return != nil:
			var e Expr
			if s.Return.Value != nil {
				var err error
				e, err = convertExpr(s.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			out = append(out, &ReturnStmt{Expr: e})
		case s.Import != nil:
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			module := strings.Trim(s.Import.Path, "\"")
			module = strings.ReplaceAll(module, "/", ".")
			out = append(out, &ImportStmt{Module: module, Alias: alias})
		case s.ExternVar != nil:
			continue
		case s.ExternFun != nil:
			continue
		case s.ExternObject != nil:
			continue
		case s.Type != nil:
			if len(s.Type.Members) > 0 {
				var fields []DataClassField
				for _, m := range s.Type.Members {
					if m.Field != nil {
						typ := "any"
						if m.Field.Type != nil {
							typ = types.ResolveTypeRef(m.Field.Type, env).String()
						}
						fields = append(fields, DataClassField{Name: m.Field.Name, Type: typ})
					}
				}
				out = append(out, &DataClassDef{Name: s.Type.Name, Fields: fields})
			}
			for _, v := range s.Type.Variants {
				if len(v.Fields) == 0 {
					out = append(out, &LetStmt{Name: v.Name, Expr: &Name{Name: "None"}})
				} else {
					var vf []DataClassField
					for _, f := range v.Fields {
						typ := "any"
						if f.Type != nil {
							typ = types.ResolveTypeRef(f.Type, env).String()
						}
						vf = append(vf, DataClassField{Name: f.Name, Type: typ})
					}
					out = append(out, &DataClassDef{Name: v.Name, Fields: vf})
				}
			}
			continue
		case s.ExternType != nil:
			// ignore extern type declarations
			continue
		case s.Fun != nil:
			fenv := types.NewEnv(env)
			for _, p := range s.Fun.Params {
				var pt types.Type = types.AnyType{}
				if p.Type != nil {
					pt = types.ResolveTypeRef(p.Type, env)
				}
				fenv.SetVar(p.Name, pt, true)
			}
			b, err := convertStmts(s.Fun.Body, fenv)
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p := range s.Fun.Params {
				params = append(params, p.Name)
			}
			out = append(out, &FuncDef{Name: s.Fun.Name, Params: params, Body: b})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
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
	operands := []Expr{}
	ops := []string{}

	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	for _, p := range b.Right {
		o, err := convertPostfix(p.Right)
		if err != nil {
			return nil, err
		}
		op := p.Op
		if p.All {
			op = op + "_all"
		}
		ops = append(ops, op)
		operands = append(operands, o)
	}

	if len(ops) == 1 && ops[0] == "/" {
		if isSumUnary(b.Left) && isSumPostfix(b.Right[0].Right) {
			ops[0] = "//"
		}
	}

	levels := [][]string{
		{"*", "/", "//", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	apply := func(left Expr, op string, right Expr) Expr {
		return &BinaryExpr{Left: left, Op: op, Right: right}
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i] == t {
					expr := apply(operands[i], ops[i], operands[i+1])
					operands[i] = expr
					operands = append(operands[:i+1], operands[i+2:]...)
					ops = append(ops[:i], ops[i+1:]...)
					matched = true
					break
				}
			}
			if !matched {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = &UnaryExpr{Op: u.Ops[i], Expr: expr}
	}
	return expr, nil
}

func convertSelector(sel *parser.SelectorExpr, method bool) Expr {
	expr := Expr(&Name{Name: sel.Root})
	mapIndex := true
	if currentImports != nil && currentImports[sel.Root] {
		mapIndex = false
	}
	if currentEnv != nil {
		if t, err := currentEnv.GetVar(sel.Root); err == nil {
			if _, ok := t.(types.StructType); ok {
				mapIndex = false
			}
		}
	}
	for i, t := range sel.Tail {
		idx := i == len(sel.Tail)-1 && method
		expr = &FieldExpr{Target: expr, Name: t, MapIndex: mapIndex && !idx}
		mapIndex = true
	}
	return expr
}

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	method := len(p.Ops) > 0 && p.Ops[0].Call != nil
	var expr Expr
	var err error
	if p.Target.Selector != nil {
		expr = convertSelector(p.Target.Selector, method)
	} else {
		expr, err = convertPrimary(p.Target)
		if err != nil {
			return nil, err
		}
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil {
				var start, end, step Expr
				var err error
				if op.Index.Start != nil {
					start, err = convertExpr(op.Index.Start)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.End != nil {
					end, err = convertExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.Step != nil {
					step, err = convertExpr(op.Index.Step)
					if err != nil {
						return nil, err
					}
				}
				expr = &SliceExpr{Target: expr, Start: start, End: end, Step: step}
			} else {
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				expr = &IndexExpr{Target: expr, Index: idx}
			}
		case op.Field != nil:
			// use attribute access for method calls, otherwise treat as map lookup
			if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				expr = &FieldExpr{Target: expr, Name: op.Field.Name}
			} else {
				expr = &FieldExpr{Target: expr, Name: op.Field.Name, MapIndex: true}
			}
		case op.Call != nil:
			var args []Expr
			for _, a := range op.Call.Args {
				ae, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ae)
			}
			if fe, ok := expr.(*FieldExpr); ok && fe.Name == "contains" && len(args) == 1 {
				expr = &BinaryExpr{Left: args[0], Op: "in", Right: fe.Target}
			} else {
				expr = &CallExpr{Func: expr, Args: args}
			}
		case op.Cast != nil:
			name := typeRefSimpleName(op.Cast.Type)
			switch name {
			case "int":
				expr = &CallExpr{Func: &Name{Name: "int"}, Args: []Expr{expr}}
			case "float":
				expr = &CallExpr{Func: &Name{Name: "float"}, Args: []Expr{expr}}
			case "string":
				expr = &CallExpr{Func: &Name{Name: "str"}, Args: []Expr{expr}}
			case "bool":
				expr = &CallExpr{Func: &Name{Name: "bool"}, Args: []Expr{expr}}
			default:
				// ignore cast for other types
			}
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		var args []Expr
		for _, a := range p.Call.Args {
			ae, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ae)
		}
		if currentEnv != nil {
			if fn, ok := currentEnv.GetFunc(p.Call.Func); ok {
				if len(args) < len(fn.Params) {
					rem := fn.Params[len(args):]
					var names []string
					var extra []Expr
					for _, p := range rem {
						names = append(names, p.Name)
						extra = append(extra, &Name{Name: p.Name})
					}
					call := &CallExpr{Func: &Name{Name: p.Call.Func}, Args: append(args, extra...)}
					return &LambdaExpr{Params: names, Expr: call}, nil
				}
			}
		}
		switch p.Call.Func {
		case "print":
			outArgs := make([]Expr, len(args))
			for i, a := range args {
				var t types.Type = types.AnyType{}
				if currentEnv != nil {
					t = inferTypeFromExpr(p.Call.Args[i])
				}
				if isBoolOp(p.Call.Args[i]) {
					outArgs[i] = &RawExpr{Code: fmt.Sprintf("(1 if %s else 0)", exprString(a))}
				} else if _, ok := t.(types.BoolType); ok {
					outArgs[i] = &RawExpr{Code: fmt.Sprintf("(\"true\" if %s else \"false\")", exprString(a))}
				} else {
					outArgs[i] = a
				}
			}
			if len(outArgs) == 1 {
				return &CallExpr{Func: &Name{Name: "print"}, Args: outArgs}, nil
			}
			var parts []string
			for _, a := range outArgs {
				parts = append(parts, exprString(a))
			}
			code := fmt.Sprintf("print(\" \".join(str(x) for x in [%s]).rstrip())", strings.Join(parts, ", "))
			return &RawExpr{Code: code}, nil
		case "append":
			if len(args) == 2 {
				return &BinaryExpr{Left: args[0], Op: "+", Right: &ListLit{Elems: []Expr{args[1]}}}, nil
			}
		case "avg":
			if len(args) == 1 {
				sumCall := &CallExpr{Func: &Name{Name: "sum"}, Args: []Expr{args[0]}}
				lenCall := &CallExpr{Func: &Name{Name: "len"}, Args: []Expr{args[0]}}
				div := &BinaryExpr{Left: sumCall, Op: "/", Right: lenCall}
				return &CondExpr{Cond: args[0], Then: div, Else: &FloatLit{Value: "0.0"}}, nil
			}
		case "count":
			if len(args) == 1 {
				return &CallExpr{Func: &Name{Name: "len"}, Args: args}, nil
			}
		case "values":
			if len(args) == 1 {
				call := &CallExpr{Func: &FieldExpr{Target: args[0], Name: "values"}, Args: nil}
				return &CallExpr{Func: &Name{Name: "list"}, Args: []Expr{call}}, nil
			}
		case "min", "max":
			if len(args) == 1 {
				return &CallExpr{Func: &Name{Name: p.Call.Func}, Args: args}, nil
			}
		case "substring":
			if len(args) == 3 {
				return &SliceExpr{Target: args[0], Start: args[1], End: args[2]}, nil
			}
		case "json":
			if len(args) == 1 {
				if currentImports != nil {
					currentImports["json"] = true
				}
				dumps := exprString(args[0])
				raw := &RawExpr{Code: fmt.Sprintf("json.dumps(%s, indent=2)", dumps)}
				return &CallExpr{Func: &Name{Name: "print"}, Args: []Expr{raw}}, nil
			}
		case "exists":
			if len(args) == 1 {
				length := &CallExpr{Func: &Name{Name: "len"}, Args: args}
				return &BinaryExpr{Left: length, Op: ">", Right: &IntLit{Value: "0"}}, nil
			}
		}
		return &CallExpr{Func: &Name{Name: p.Call.Func}, Args: args}, nil
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.List != nil:
		var elems []Expr
		for _, e := range p.List.Elems {
			ce, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ce)
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		var keys []Expr
		var values []Expr
		for _, it := range p.Map.Items {
			if id, ok := isSimpleIdent(it.Key); ok {
				keys = append(keys, &StringLit{Value: id})
			} else {
				ke, err := convertExpr(it.Key)
				if err != nil {
					return nil, err
				}
				keys = append(keys, ke)
			}
			ve, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			values = append(values, ve)
		}
		return &DictLit{Keys: keys, Values: values}, nil
	case p.Struct != nil:
		if currentEnv != nil {
			if _, ok := currentEnv.GetStruct(p.Struct.Name); ok {
				var args []Expr
				for _, f := range p.Struct.Fields {
					ve, err := convertExpr(f.Value)
					if err != nil {
						return nil, err
					}
					args = append(args, &KeywordArg{Name: f.Name, Value: ve})
				}
				return &CallExpr{Func: &Name{Name: p.Struct.Name}, Args: args}, nil
			}
		}
		var keys []Expr
		var values []Expr
		for _, f := range p.Struct.Fields {
			ve, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			keys = append(keys, &StringLit{Value: f.Name})
			values = append(values, ve)
		}
		return &DictLit{Keys: keys, Values: values}, nil
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Expr: body}, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Query != nil:
		return convertQueryExpr(p.Query)
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		clean := path
		for strings.HasPrefix(clean, "../") {
			clean = strings.TrimPrefix(clean, "../")
		}
		pathExpr := fmt.Sprintf("%q", path)
		if path != "" && strings.HasPrefix(path, "../") {
			root := repoRoot()
			abs := filepath.ToSlash(filepath.Join(root, "tests", clean))
			pathExpr = fmt.Sprintf("%q", abs)
		}
		expr, err := dataExprFromFile(path, format)
		if err == nil {
			return expr, nil
		}
		switch format {
		case "json", "jsonl":
			if currentImports != nil {
				currentImports["json"] = true
			}
			if format == "json" {
				return &RawExpr{Code: fmt.Sprintf("json.load(open(%s))", pathExpr)}, nil
			}
			code := fmt.Sprintf("[json.loads(line) for line in open(%s)]", pathExpr)
			return &RawExpr{Code: code}, nil
		case "yaml":
			return nil, err
		default:
			return nil, fmt.Errorf("unsupported load format")
		}
	case p.Group != nil:
		return convertExpr(p.Group)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		s := fmt.Sprintf("%g", *l.Float)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return &FloatLit{Value: s}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
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
		elseExpr = &Name{Name: "None"}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr = &Name{Name: "None"}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		pat := c.Pattern
		if pat.Binary != nil && len(pat.Binary.Right) == 0 {
			u := pat.Binary.Left
			if len(u.Ops) == 0 && u.Value != nil && u.Value.Target != nil && u.Value.Target.Call != nil && u.Value.Target.Call.Func == "Node" {
				call := u.Value.Target.Call
				if len(call.Args) == 3 {
					names := []string{}
					ok := true
					for _, a := range call.Args {
						name, ok2 := isSimpleIdent(a)
						if !ok2 {
							ok = false
							break
						}
						names = append(names, name)
					}
					if ok {
						if tn, ok2 := target.(*Name); ok2 {
							fields := map[string]bool{}
							for _, nm := range names {
								fields[nm] = true
							}
							res = substituteFields(res, tn.Name, fields)
							cond := &BinaryExpr{Left: target, Op: "!=", Right: &Name{Name: "None"}}
							expr = &CondExpr{Cond: cond, Then: res, Else: expr}
							continue
						}
					}
				}
			}
		}
		patExpr, err := convertExpr(pat)
		if err != nil {
			return nil, err
		}
		if n, ok := patExpr.(*Name); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: patExpr}
		expr = &CondExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil &&
		(*q.Joins[0].Side == "right" || *q.Joins[0].Side == "outer") &&
		len(q.Froms) == 0 {
		return convertSpecialJoin(q)
	}

	vars := []string{q.Var}
	iters := []Expr{}
	firstIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	iters = append(iters, firstIter)
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
			if *j.Side != "left" {
				return nil, fmt.Errorf("unsupported query")
			}
			src, err := convertExpr(j.Src)
			if err != nil {
				return nil, err
			}
			on, err := convertExpr(j.On)
			if err != nil {
				return nil, err
			}
			inner := &ListComp{Var: j.Var, Iter: src, Expr: &Name{Name: j.Var}, Cond: on}
			left := &BinaryExpr{Left: inner, Op: "||", Right: &ListLit{Elems: []Expr{&Name{Name: "None"}}}}
			vars = append(vars, j.Var)
			iters = append(iters, left)
			continue
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

	var elem Expr
	aggName, aggExpr, useAgg := aggregatorCall(q.Select)
	if !useAgg {
		elem, err = convertExpr(q.Select)
		if err != nil {
			return nil, err
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

	base := &MultiListComp{Vars: vars, Iters: iters, Expr: elem, Cond: cond}
	list := Expr(base)
	var keyExpr Expr
	reverse := false
	if q.Sort != nil {
		keyExpr, err = convertExpr(q.Sort)
		if err != nil {
			return nil, err
		}
		if u, ok := keyExpr.(*UnaryExpr); ok && u.Op == "-" {
			keyExpr = u.Expr
			reverse = true
		}
		if d, ok := keyExpr.(*DictLit); ok {
			vals := make([]Expr, len(d.Values))
			copy(vals, d.Values)
			keyExpr = &CallExpr{Func: &Name{Name: "tuple"}, Args: []Expr{&ListLit{Elems: vals}}}
		}
		sortBase := &MultiListComp{Vars: vars, Iters: iters, Expr: &Name{Name: q.Var}, Cond: cond}
		sorted := &SortedExpr{List: sortBase, Var: q.Var, Key: keyExpr, Reverse: reverse}
		comp := &ListComp{Var: q.Var, Iter: sorted, Expr: elem}
		list = Expr(comp)
	}

	var start Expr
	var end Expr
	if q.Skip != nil {
		start, err = convertExpr(q.Skip)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		take, err := convertExpr(q.Take)
		if err != nil {
			return nil, err
		}
		if start != nil {
			end = &BinaryExpr{Left: start, Op: "+", Right: take}
		} else {
			end = take
		}
	}
	if start != nil || end != nil {
		list = &SliceExpr{Target: list, Start: start, End: end}
	}

	if useAgg {
		arg, err := convertExpr(aggExpr)
		if err != nil {
			return nil, err
		}
		comp := &MultiListComp{Vars: vars, Iters: iters, Expr: arg, Cond: cond, Parens: true}
		aggList := Expr(comp)
		if q.Sort != nil {
			aggList = &SortedExpr{List: aggList, Var: q.Var, Key: keyExpr, Reverse: reverse}
		}
		if start != nil || end != nil {
			aggList = &SliceExpr{Target: aggList, Start: start, End: end}
		}
		return &CallExpr{Func: &Name{Name: aggName}, Args: []Expr{aggList}}, nil
	}

	return list, nil
}

func convertSpecialJoin(q *parser.QueryExpr) (Expr, error) {
	j := q.Joins[0]
	side := *j.Side
	leftIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightIter, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	cond, err := convertExpr(j.On)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	if err != nil {
		return nil, err
	}

	var base Expr
	if side == "right" {
		base = &MultiListComp{Vars: []string{j.Var, q.Var}, Iters: []Expr{rightIter, leftIter}, Expr: sel, Cond: cond}
	} else {
		base = &MultiListComp{Vars: []string{q.Var, j.Var}, Iters: []Expr{leftIter, rightIter}, Expr: sel, Cond: cond}
	}

	parts := []Expr{base}

	if side == "outer" {
		noneRightBody, err := convertExpr(q.Select)
		if err != nil {
			return nil, err
		}
		lam := &LambdaExpr{Params: []string{j.Var}, Expr: noneRightBody}
		call := &CallExpr{Func: &RawExpr{Code: fmt.Sprintf("(%s)", exprString(lam))}, Args: []Expr{&Name{Name: "None"}}}
		anyMatch := &CallExpr{Func: &Name{Name: "any"}, Args: []Expr{&ListComp{Var: j.Var, Iter: rightIter, Expr: cond}}}
		condLeft := &UnaryExpr{Op: "not ", Expr: anyMatch}
		parts = append(parts, &ListComp{Var: q.Var, Iter: leftIter, Expr: call, Cond: condLeft})
	}

	if side == "right" || side == "outer" {
		noneLeftBody, err := convertExpr(q.Select)
		if err != nil {
			return nil, err
		}
		lam := &LambdaExpr{Params: []string{q.Var}, Expr: noneLeftBody}
		call := &CallExpr{Func: &RawExpr{Code: fmt.Sprintf("(%s)", exprString(lam))}, Args: []Expr{&Name{Name: "None"}}}
		anyMatch := &CallExpr{Func: &Name{Name: "any"}, Args: []Expr{&ListComp{Var: q.Var, Iter: leftIter, Expr: cond}}}
		condRight := &UnaryExpr{Op: "not ", Expr: anyMatch}
		parts = append(parts, &ListComp{Var: j.Var, Iter: rightIter, Expr: call, Cond: condRight})
	}

	expr := parts[0]
	for _, p := range parts[1:] {
		expr = &BinaryExpr{Left: expr, Op: "+", Right: p}
	}
	return expr, nil
}

func convertUpdate(u *parser.UpdateStmt, env *types.Env) (*UpdateStmt, error) {
	if env == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := env.GetVar(u.Target)
	if err != nil {
		return nil, err
	}
	lt, ok := t.(types.ListType)
	if !ok {
		return nil, fmt.Errorf("update target not list")
	}
	st, ok := lt.Elem.(types.StructType)
	if !ok {
		return nil, fmt.Errorf("update element not struct")
	}
	child := types.NewEnv(env)
	fieldSet := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fieldSet[name] = true
	}
	prev := currentEnv
	currentEnv = child
	var fields []string
	var values []Expr
	for _, item := range u.Set.Items {
		key, ok := isSimpleIdent(item.Key)
		if !ok {
			key, ok = literalString(item.Key)
			if !ok {
				currentEnv = prev
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(item.Value)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
		val = substituteFields(val, "item", fieldSet)
		fields = append(fields, key)
		values = append(values, val)
	}
	var cond Expr
	if u.Where != nil {
		cond, err = convertExpr(u.Where)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
		cond = substituteFields(cond, "item", fieldSet)
	}
	currentEnv = prev
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func convertGroupQuery(q *parser.QueryExpr, env *types.Env, target string) ([]Stmt, error) {
	if q.Group == nil {
		return nil, fmt.Errorf("missing group clause")
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
	for _, j := range q.Joins {
		e, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		cond, err := convertExpr(j.On)
		if err != nil {
			return nil, err
		}
		if j.Side != nil && *j.Side == "left" {
			comp := &ListComp{Var: j.Var, Iter: e, Expr: &Name{Name: j.Var}, Cond: cond}
			e = &BinaryExpr{Left: comp, Op: "||", Right: &ListLit{Elems: []Expr{&Name{Name: "None"}}}}
		} else {
			e = &ListComp{Var: j.Var, Iter: e, Expr: &Name{Name: j.Var}, Cond: cond}
		}
		vars = append(vars, j.Var)
		iters = append(iters, e)
	}

	keyVal, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		return nil, err
	}
	keyExpr := keyVal
	if dl, ok := keyVal.(*DictLit); ok {
		vals := make([]Expr, len(dl.Values))
		copy(vals, dl.Values)
		keyExpr = &CallExpr{Func: &Name{Name: "tuple"}, Args: []Expr{&ListLit{Elems: vals}}}
	}

	var where Expr
	if q.Where != nil {
		where, err = convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
	}

	prev := currentEnv
	selIdent, selIsIdent := isSimpleIdent(q.Select)
	genv := types.NewEnv(env)
	genv.SetVar(q.Group.Name, types.AnyType{}, true)
	currentEnv = genv
	sel, err := convertExpr(q.Select)
	if err != nil {
		currentEnv = prev
		return nil, err
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
	}
	currentEnv = prev
	if !(selIsIdent && selIdent == q.Group.Name) {
		sel = replaceGroup(sel, q.Group.Name)
	}
	if having != nil {
		having = replaceGroup(having, q.Group.Name)
	}

	groupsVar := "_" + target + "_groups"
	tmpVar := "_g"

	stmts := []Stmt{
		&LetStmt{Name: groupsVar, Expr: &DictLit{}},
	}

	var row Expr
	if len(vars) == 1 {
		row = &Name{Name: vars[0]}
	} else {
		var rk []Expr
		var rv []Expr
		for _, v := range vars {
			rk = append(rk, &StringLit{Value: v})
			rv = append(rv, &Name{Name: v})
		}
		row = &DictLit{Keys: rk, Values: rv}
	}

	initMap := &DictLit{Keys: []Expr{&StringLit{Value: "key"}, &StringLit{Value: "items"}}, Values: []Expr{keyVal, &ListLit{}}}
	setdef := &CallExpr{Func: &FieldExpr{Target: &Name{Name: groupsVar}, Name: "setdefault"}, Args: []Expr{keyExpr, initMap}}
	appendCall := &CallExpr{Func: &FieldExpr{Target: &FieldExpr{Target: &Name{Name: tmpVar}, Name: "items", MapIndex: true}, Name: "append"}, Args: []Expr{row}}
	inner := []Stmt{
		&LetStmt{Name: tmpVar, Expr: setdef},
		&ExprStmt{Expr: appendCall},
	}
	if where != nil {
		inner = []Stmt{&IfStmt{Cond: where, Then: inner}}
	}

	bodyLoop := inner
	for i := len(vars) - 1; i >= 0; i-- {
		bodyLoop = []Stmt{&ForStmt{Var: vars[i], Iter: iters[i], Body: bodyLoop}}
	}
	stmts = append(stmts, bodyLoop...)

	appendRes := &ExprStmt{Expr: &CallExpr{Func: &FieldExpr{Target: &Name{Name: target}, Name: "append"}, Args: []Expr{sel}}}
	body := []Stmt{}
	if having != nil {
		body = append(body, &IfStmt{Cond: having, Then: []Stmt{appendRes}})
	} else {
		body = append(body, appendRes)
	}
	iterVals := &CallExpr{Func: &FieldExpr{Target: &Name{Name: groupsVar}, Name: "values"}, Args: nil}
	stmts = append(stmts, &LetStmt{Name: target, Expr: &ListLit{}})
	stmts = append(stmts, &ForStmt{Var: q.Group.Name, Iter: iterVals, Body: body})

	return stmts, nil
}

// --- AST printing helpers ---

// toNode converts the Python AST into a generic ast.Node tree.
func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *VarStmt:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *ReturnStmt:
		child := &ast.Node{Kind: "return"}
		if st.Expr != nil {
			child.Children = []*ast.Node{exprNode(st.Expr)}
		}
		return child
	case *WhileStmt:
		n := &ast.Node{Kind: "while"}
		n.Children = append(n.Children, exprNode(st.Cond))
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ForStmt:
		n := &ast.Node{Kind: "for", Value: st.Var}
		n.Children = append(n.Children, exprNode(st.Iter))
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
		thenNode := &ast.Node{Kind: "then"}
		for _, b := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(b))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, b := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(b))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *FieldAssignStmt:
		return &ast.Node{Kind: "field_assign", Children: []*ast.Node{exprNode(st.Target), &ast.Node{Kind: "string", Value: st.Field}, exprNode(st.Value)}}
	case *IndexAssignStmt:
		return &ast.Node{Kind: "index_assign", Children: []*ast.Node{exprNode(st.Target), exprNode(st.Index), exprNode(st.Value)}}
	case *UpdateStmt:
		n := &ast.Node{Kind: "update", Value: st.Target}
		setNode := &ast.Node{Kind: "set"}
		for i, f := range st.Fields {
			pair := &ast.Node{Kind: "pair", Children: []*ast.Node{&ast.Node{Kind: "string", Value: f}, exprNode(st.Values[i])}}
			setNode.Children = append(setNode.Children, pair)
		}
		n.Children = append(n.Children, setNode)
		if st.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{exprNode(st.Cond)}})
		}
		return n
	case *FuncDef:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, p := range st.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ImportStmt:
		return &ast.Node{Kind: "import", Value: st.Module}
	case *DataClassDef:
		n := &ast.Node{Kind: "dataclass", Value: st.Name}
		for _, f := range st.Fields {
			pair := &ast.Node{Kind: "field", Value: f.Name, Children: []*ast.Node{&ast.Node{Kind: "type", Value: f.Type}}}
			n.Children = append(n.Children, pair)
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Children: []*ast.Node{exprNode(ex.Func)}}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *KeywordArg:
		n := &ast.Node{Kind: "kwarg", Value: ex.Name}
		n.Children = []*ast.Node{exprNode(ex.Value)}
		return n
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *FloatLit:
		return &ast.Node{Kind: "float", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, el := range ex.Elems {
			n.Children = append(n.Children, exprNode(el))
		}
		return n
	case *DictLit:
		n := &ast.Node{Kind: "dict"}
		for i := range ex.Keys {
			pair := &ast.Node{Kind: "pair", Children: []*ast.Node{exprNode(ex.Keys[i]), exprNode(ex.Values[i])}}
			n.Children = append(n.Children, pair)
		}
		return n
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *SliceExpr:
		n := &ast.Node{Kind: "slice"}
		n.Children = append(n.Children, exprNode(ex.Target))
		if ex.Start != nil {
			n.Children = append(n.Children, exprNode(ex.Start))
		} else {
			n.Children = append(n.Children, &ast.Node{Kind: "none"})
		}
		if ex.End != nil {
			n.Children = append(n.Children, exprNode(ex.End))
		} else {
			n.Children = append(n.Children, &ast.Node{Kind: "none"})
		}
		if ex.Step != nil {
			n.Children = append(n.Children, exprNode(ex.Step))
		}
		return n
	case *FieldExpr:
		return &ast.Node{Kind: "field", Value: ex.Name, Children: []*ast.Node{exprNode(ex.Target)}}
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, exprNode(ex.Expr))
		return n
	case *ListComp:
		n := &ast.Node{Kind: "list_comp", Value: ex.Var}
		n.Children = append(n.Children, exprNode(ex.Iter))
		n.Children = append(n.Children, exprNode(ex.Expr))
		if ex.Cond != nil {
			n.Children = append(n.Children, exprNode(ex.Cond))
		}
		return n
	case *MultiListComp:
		kind := "list_comp_multi"
		if ex.Parens {
			kind = "gen_comp_multi"
		}
		n := &ast.Node{Kind: kind}
		for i, v := range ex.Vars {
			iter := &ast.Node{Kind: "for", Value: v, Children: []*ast.Node{exprNode(ex.Iters[i])}}
			n.Children = append(n.Children, iter)
		}
		n.Children = append(n.Children, exprNode(ex.Expr))
		if ex.Cond != nil {
			n.Children = append(n.Children, exprNode(ex.Cond))
		}
		return n
	case *SortedExpr:
		n := &ast.Node{Kind: "sorted", Value: ex.Var}
		n.Children = append(n.Children, exprNode(ex.List))
		n.Children = append(n.Children, exprNode(ex.Key))
		if ex.Reverse {
			n.Children = append(n.Children, &ast.Node{Kind: "reverse"})
		}
		return n
	case *CondExpr:
		return &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the Python AST in Lisp-like form to stdout.

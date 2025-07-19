//go:build slow

package cobol

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

// --- Simple COBOL AST ---

// Program contains variable declarations and a sequence of statements.
type Program struct {
	Vars    []VarDecl
	Stmts   []Stmt
	NeedTmp bool
	NeedStr bool
}

func (p *Program) addStmt(s Stmt) {
	p.Stmts = append(p.Stmts, s)
	if stmtNeedsTmp(s) {
		p.NeedTmp = true
	}
	if needsStr(s) {
		p.NeedStr = true
	}
}

func stmtNeedsTmp(s Stmt) bool {
	switch st := s.(type) {
	case *DisplayStmt:
		return st.Temp
	case *IfStmt:
		for _, t := range st.Then {
			if stmtNeedsTmp(t) {
				return true
			}
		}
		for _, e := range st.Else {
			if stmtNeedsTmp(e) {
				return true
			}
		}
	}
	return false
}

func needsStr(s Stmt) bool {
	switch st := s.(type) {
	case *DisplayStmt:
		return !st.IsString
	case *IfStmt:
		for _, t := range st.Then {
			if needsStr(t) {
				return true
			}
		}
		for _, e := range st.Else {
			if needsStr(e) {
				return true
			}
		}
	}
	return false
}

type VarDecl struct {
	Name string
	Pic  string
	Val  Expr // optional literal value
}

// Stmt represents a COBOL statement.
type Stmt interface{ emit(io.Writer) }

// Expr represents a COBOL expression.
type Expr interface{ emitExpr(io.Writer) }

// DisplayStmt prints the value of an expression.
type DisplayStmt struct {
	Expr     Expr
	Temp     bool
	IsString bool
}

// AssignStmt assigns an expression to a variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

// IfStmt represents a simple IF/ELSE statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

// --- Expressions ---

type VarRef struct{ Name string }

type IntLit struct{ Value int }

type StringLit struct{ Value string }

type UnaryExpr struct {
	Op   string
	Expr Expr
}

type BinaryExpr struct {
	Op          string
	Left, Right Expr
}

func (v *VarRef) emitExpr(w io.Writer) {
	io.WriteString(w, strings.ToUpper(v.Name))
}

func (i *IntLit) emitExpr(w io.Writer) {
	fmt.Fprintf(w, "%d", i.Value)
}

func (s *StringLit) emitExpr(w io.Writer) {
	esc := strings.ReplaceAll(s.Value, "\"", "\"\"")
	fmt.Fprintf(w, "\"%s\"", esc)
}

func (u *UnaryExpr) emitExpr(w io.Writer) {
	io.WriteString(w, u.Op)
	switch u.Expr.(type) {
	case *BinaryExpr:
		io.WriteString(w, "(")
		u.Expr.emitExpr(w)
		io.WriteString(w, ")")
	default:
		u.Expr.emitExpr(w)
	}
}

func (b *BinaryExpr) emitExpr(w io.Writer) {
	if b.Op == "+" && (isStringLit(b.Left) || isStringLit(b.Right)) {
		io.WriteString(w, "FUNCTION CONCATENATE(")
		b.Left.emitExpr(w)
		io.WriteString(w, ", ")
		b.Right.emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	b.Left.emitExpr(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emitExpr(w)
}

func isStringLit(e Expr) bool {
	switch e.(type) {
	case *StringLit:
		return true
	default:
		return false
	}
}

func isSimpleExpr(e Expr) bool {
	switch v := e.(type) {
	case *IntLit, *StringLit, *VarRef:
		return true
	case *UnaryExpr:
		if v.Op == "-" {
			if _, ok := v.Expr.(*IntLit); ok {
				return true
			}
		}
	case *BinaryExpr:
		if v.Op == "+" && (isStringLit(v.Left) || isStringLit(v.Right)) {
			return true
		}
	default:
		return false
	}
	return false
}

func isDirectNumber(e Expr) bool {
	switch v := e.(type) {
	case *IntLit, *VarRef:
		return true
	case *UnaryExpr:
		if v.Op == "-" {
			if _, ok := v.Expr.(*IntLit); ok {
				return true
			}
		}
	}
	return false
}

// --- Statement emitters ---

func (d *DisplayStmt) emit(w io.Writer) {
	if d.IsString {
		if d.Temp {
			io.WriteString(w, "COMPUTE TMP = ")
			d.Expr.emitExpr(w)
			io.WriteString(w, "\n    DISPLAY TMP")
		} else {
			io.WriteString(w, "DISPLAY ")
			d.Expr.emitExpr(w)
		}
		return
	}
	if !d.Temp && isDirectNumber(d.Expr) {
		io.WriteString(w, "DISPLAY ")
		d.Expr.emitExpr(w)
		return
	}
	if d.Temp {
		io.WriteString(w, "COMPUTE TMP = ")
		d.Expr.emitExpr(w)
		io.WriteString(w, "\n    MOVE TMP TO TMP-STR")
	} else {
		io.WriteString(w, "MOVE ")
		d.Expr.emitExpr(w)
		io.WriteString(w, " TO TMP-STR")
	}
	io.WriteString(w, "\n    DISPLAY FUNCTION TRIM(TMP-STR)")
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, "COMPUTE ")
	io.WriteString(w, strings.ToUpper(a.Name))
	io.WriteString(w, " = ")
	a.Expr.emitExpr(w)
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "IF ")
	i.Cond.emitExpr(w)
	io.WriteString(w, "\n")
	for _, s := range i.Then {
		io.WriteString(w, "    ")
		s.emit(w)
		io.WriteString(w, "\n")
	}
	if len(i.Else) > 0 {
		io.WriteString(w, "ELSE\n")
		for _, s := range i.Else {
			io.WriteString(w, "    ")
			s.emit(w)
			io.WriteString(w, "\n")
		}
	}
	io.WriteString(w, "END-IF")
}

// --- Program emitter ---

func repoRoot() string {
	dir, _ := os.Getwd()
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
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(b))
}

// Emit renders COBOL source code with a deterministic header.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	loc := time.FixedZone("GMT+7", 7*3600)
	now := time.Now().In(loc)
	fmt.Fprintf(&buf, "*> Generated by Mochi transpiler v%s on %s\n", version(), now.Format("2006-01-02 15:04:05 MST"))
	buf.WriteString(">>SOURCE FORMAT FREE\n")
	buf.WriteString("IDENTIFICATION DIVISION.\n")
	buf.WriteString("PROGRAM-ID. MAIN.\n")
	if len(p.Vars) > 0 || p.NeedTmp {
		buf.WriteString("\nDATA DIVISION.\n")
		buf.WriteString("WORKING-STORAGE SECTION.\n")
		for _, v := range p.Vars {
			fmt.Fprintf(&buf, "01 %s %s", strings.ToUpper(v.Name), v.Pic)
			if v.Val != nil {
				buf.WriteString(" VALUE ")
				v.Val.emitExpr(&buf)
			} else if v.Pic == "PIC 9(9)" {
				buf.WriteString(" VALUE 0")
			}
			buf.WriteString(".\n")
		}
		if p.NeedTmp {
			buf.WriteString("01 TMP PIC S9(9) VALUE 0.\n")
		}
		if p.NeedStr {
			buf.WriteString("01 TMP-STR PIC Z(18).\n")
		}
	}
	buf.WriteString("\nPROCEDURE DIVISION.\n")
	for _, s := range p.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("    STOP RUN.\n")
	return buf.Bytes()
}

// --- Transpiler ---

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	pr := &Program{}
	for _, st := range prog.Statements {
		switch {
		case st.Let != nil:
			vd, initStmt, err := convertVar(st.Let.Name, st.Let.Type, st.Let.Value, env)
			if err != nil {
				return nil, err
			}
			pr.Vars = append(pr.Vars, vd)
			if initStmt != nil {
				pr.addStmt(initStmt)
			}
		case st.Var != nil:
			vd, initStmt, err := convertVar(st.Var.Name, st.Var.Type, st.Var.Value, env)
			if err != nil {
				return nil, err
			}
			pr.Vars = append(pr.Vars, vd)
			if initStmt != nil {
				pr.addStmt(initStmt)
			}
		case st.Assign != nil:
			ex, err := convertExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			pr.addStmt(&AssignStmt{Name: st.Assign.Name, Expr: ex})
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(call.Args) == 1 && len(st.Expr.Expr.Binary.Right) == 0 {
				ex, err := convertExpr(call.Args[0], env)
				if err != nil {
					return nil, err
				}
				temp := !isSimpleExpr(ex)
				t := types.TypeOfExpr(call.Args[0], env)
				isStr := false
				if _, ok := t.(types.StringType); ok {
					isStr = true
				}
				if temp {
					pr.NeedTmp = true
				}
				if !isStr {
					pr.NeedStr = true
				}
				pr.addStmt(&DisplayStmt{Expr: ex, Temp: temp, IsString: isStr})
			} else {
				return nil, fmt.Errorf("unsupported expression")
			}
		case st.If != nil:
			cond, err := convertExpr(st.If.Cond, env)
			if err != nil {
				return nil, err
			}
			thenStmts, err := transpileStmts(st.If.Then, env)
			if err != nil {
				return nil, err
			}
			elseStmts, err := transpileStmts(st.If.Else, env)
			if err != nil {
				return nil, err
			}
			pr.addStmt(&IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	_ = env
	return pr, nil
}

func transpileStmts(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		switch {
		case s.Assign != nil:
			ex, err := convertExpr(s.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			out = append(out, &AssignStmt{Name: s.Assign.Name, Expr: ex})
		case s.Expr != nil:
			call := s.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(call.Args) == 1 && len(s.Expr.Expr.Binary.Right) == 0 {
				ex, err := convertExpr(call.Args[0], env)
				if err != nil {
					return nil, err
				}
				temp := !isSimpleExpr(ex)
				t := types.TypeOfExpr(call.Args[0], env)
				isStr := false
				if _, ok := t.(types.StringType); ok {
					isStr = true
				}
				out = append(out, &DisplayStmt{Expr: ex, Temp: temp, IsString: isStr})
			} else {
				return nil, fmt.Errorf("unsupported expression")
			}
		case s.If != nil:
			cond, err := convertExpr(s.If.Cond, env)
			if err != nil {
				return nil, err
			}
			thenStmts, err := transpileStmts(s.If.Then, env)
			if err != nil {
				return nil, err
			}
			elseStmts, err := transpileStmts(s.If.Else, env)
			if err != nil {
				return nil, err
			}
			out = append(out, &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func convertVar(name string, t *parser.TypeRef, val *parser.Expr, env *types.Env) (VarDecl, Stmt, error) {
	pic := "PIC 9(9)"
	if t != nil && t.Simple != nil {
		switch *t.Simple {
		case "int":
			pic = "PIC 9"
		case "string":
			pic = "PIC X(100)"
		case "bool":
			pic = "PIC 9"
		}
	}
	if val != nil {
		if lit := literalExpr(val); lit != nil {
			if il, ok := lit.(*IntLit); ok {
				if il.Value >= -9 && il.Value <= 9 {
					pic = "PIC 9"
				} else {
					pic = "PIC 9(9)"
				}
			}
		}
	}
	var constVal Expr
	if val != nil {
		if lit := literalExpr(val); lit != nil {
			constVal = lit
		} else {
			ex, err := convertExpr(val, env)
			if err != nil {
				return VarDecl{}, nil, err
			}
			return VarDecl{Name: name, Pic: pic}, &AssignStmt{Name: name, Expr: ex}, nil
		}
	}
	return VarDecl{Name: name, Pic: pic, Val: constVal}, nil, nil
}

func literalExpr(e *parser.Expr) Expr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	pf := u.Value
	if pf == nil || len(pf.Ops) != 0 {
		return nil
	}
	lit := pf.Target.Lit
	if lit == nil {
		return nil
	}
	switch {
	case lit.Int != nil:
		return &IntLit{Value: int(*lit.Int)}
	case lit.Str != nil:
		return &StringLit{Value: *lit.Str}
	default:
		return nil
	}
}

func convertExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expr")
	}
	cur, err := convertUnary(e.Binary.Left, env)
	if err != nil {
		return nil, err
	}
	for _, op := range e.Binary.Right {
		r, err := convertPostfix(op.Right, env)
		if err != nil {
			return nil, err
		}
		cur = &BinaryExpr{Op: op.Op, Left: cur, Right: r}
	}
	return cur, nil
}

func convertUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	ex, err := convertPostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		if u.Ops[i] != "-" {
			return nil, fmt.Errorf("unsupported unary op")
		}
		ex = &UnaryExpr{Op: "-", Expr: ex}
	}
	return ex, nil
}

func convertPostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil || len(pf.Ops) > 0 {
		return nil, fmt.Errorf("unsupported postfix")
	}
	return convertPrimary(pf.Target, env)
}

func convertPrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	case p.Group != nil:
		return convertExpr(p.Group, env)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: int(*l.Int)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

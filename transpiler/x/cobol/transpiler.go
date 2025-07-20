//go:build slow

package cobol

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
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

func (p *Program) ensureIntVar(name string) {
	upper := strings.ToUpper(name)
	for _, v := range p.Vars {
		if strings.ToUpper(v.Name) == upper {
			return
		}
	}
	p.Vars = append(p.Vars, VarDecl{Name: name, Pic: "PIC 9"})
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
	case *WhileStmt:
		for _, b := range st.Body {
			if stmtNeedsTmp(b) {
				return true
			}
		}
	case *ForRangeStmt:
		for _, b := range st.Body {
			if stmtNeedsTmp(b) {
				return true
			}
		}
	}
	return false
}

func needsStr(s Stmt) bool {
	switch st := s.(type) {
	case *DisplayStmt:
		if st.IsString {
			return false
		}
		if st.Temp {
			return true
		}
		return !isDirectNumber(st.Expr)
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
	case *WhileStmt:
		for _, b := range st.Body {
			if needsStr(b) {
				return true
			}
		}
	case *ForRangeStmt:
		for _, b := range st.Body {
			if needsStr(b) {
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
	Name     string
	Expr     Expr
	IsString bool
}

// IfStmt represents a simple IF/ELSE statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

// WhileStmt represents a basic while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents a numeric for loop like `for i in a..b {}` where b is exclusive.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

// --- Expressions ---

type VarRef struct{ Name string }

type IntLit struct{ Value int }

type StringLit struct{ Value string }

// LenExpr represents a builtin len() call.
type LenExpr struct{ Value Expr }

// StrExpr represents a builtin str() call on a constant.
type StrExpr struct{ Value string }

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

func (s *StrExpr) emitExpr(w io.Writer) {
	esc := strings.ReplaceAll(s.Value, "\"", "\"\"")
	fmt.Fprintf(w, "\"%s\"", esc)
}

func (l *LenExpr) emitExpr(w io.Writer) {
	io.WriteString(w, "FUNCTION LENGTH(")
	l.Value.emitExpr(w)
	io.WriteString(w, ")")
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
	if b.Op == "%" {
		io.WriteString(w, "FUNCTION MOD(")
		b.Left.emitExpr(w)
		io.WriteString(w, ", ")
		b.Right.emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "&&" || b.Op == "||" {
		if l, ok := b.Left.(*BinaryExpr); ok && binaryPrec(l.Op) < binaryPrec(b.Op) {
			io.WriteString(w, "(")
			l.emitExpr(w)
			io.WriteString(w, ")")
		} else {
			b.Left.emitExpr(w)
		}
		if b.Op == "&&" {
			io.WriteString(w, " AND ")
		} else {
			io.WriteString(w, " OR ")
		}
		if r, ok := b.Right.(*BinaryExpr); ok && binaryPrec(r.Op) < binaryPrec(b.Op) {
			io.WriteString(w, "(")
			r.emitExpr(w)
			io.WriteString(w, ")")
		} else {
			b.Right.emitExpr(w)
		}
		return
	}
	if l, ok := b.Left.(*BinaryExpr); ok && binaryPrec(l.Op) < binaryPrec(b.Op) {
		io.WriteString(w, "(")
		l.emitExpr(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emitExpr(w)
	}
	op := b.Op
	switch b.Op {
	case "==":
		op = "="
	case "!=":
		op = "<>"
	}
	fmt.Fprintf(w, " %s ", op)
	if r, ok := b.Right.(*BinaryExpr); ok && binaryPrec(r.Op) < binaryPrec(b.Op) {
		io.WriteString(w, "(")
		r.emitExpr(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emitExpr(w)
	}
}

func binaryPrec(op string) int {
	switch op {
	case "*", "/", "%":
		return 2
	case "+", "-":
		return 1
	default:
		return 0
	}
}

func isStringLit(e Expr) bool {
	switch e.(type) {
	case *StringLit:
		return true
	case *StrExpr:
		return true
	default:
		return false
	}
}

func isSimpleExpr(e Expr) bool {
	switch v := e.(type) {
	case *IntLit, *StringLit, *VarRef, *StrExpr:
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
		if v.Op == "&&" || v.Op == "||" {
			return isBoolExpr(v.Left) && isBoolExpr(v.Right)
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
			return isDirectNumber(v.Expr)
		}
	case *BinaryExpr:
		if v.Op == "+" || v.Op == "-" {
			return isDirectNumber(v.Left) && isDirectNumber(v.Right)
		}
	}
	return false
}

func relOpExpr(e Expr) (*BinaryExpr, bool) {
	b, ok := e.(*BinaryExpr)
	if !ok {
		return nil, false
	}
	switch b.Op {
	case "==", "!=", "<", "<=", ">", ">=":
		return b, true
	default:
		return nil, false
	}
}

func isBoolExpr(e Expr) bool {
	switch b := e.(type) {
	case *BinaryExpr:
		switch b.Op {
		case "==", "!=", "<", "<=", ">", ">=":
			return true
		case "&&", "||":
			return isBoolExpr(b.Left) && isBoolExpr(b.Right)
		}
	}
	return false
}

func emitCondExpr(w io.Writer, e Expr) {
	switch b := e.(type) {
	case *BinaryExpr:
		switch b.Op {
		case "&&", "||":
			emitCondExpr(w, b.Left)
			if b.Op == "&&" {
				io.WriteString(w, " AND ")
			} else {
				io.WriteString(w, " OR ")
			}
			emitCondExpr(w, b.Right)
		case "==", "!=", "<", "<=", ">", ">=":
			b.Left.emitExpr(w)
			op := b.Op
			switch op {
			case "==":
				op = "="
			case "!=":
				op = "<>"
			}
			fmt.Fprintf(w, " %s ", op)
			b.Right.emitExpr(w)
		default:
			b.emitExpr(w)
		}
	default:
		e.emitExpr(w)
	}
}

// --- Statement emitters ---

func (d *DisplayStmt) emit(w io.Writer) {
	if isBoolExpr(d.Expr) {
		io.WriteString(w, "IF ")
		emitCondExpr(w, d.Expr)
		io.WriteString(w, "\n        DISPLAY 1\n    ELSE\n        DISPLAY 0\n    END-IF")
		return
	}
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
		if il, ok := d.Expr.(*IntLit); ok {
			fmt.Fprintf(w, "DISPLAY %d", il.Value)
			return
		}
		if ue, ok := d.Expr.(*UnaryExpr); ok && ue.Op == "-" {
			if il, ok := ue.Expr.(*IntLit); ok {
				fmt.Fprintf(w, "DISPLAY -%d", il.Value)
				return
			}
		}
		if vr, ok := d.Expr.(*VarRef); ok {
			io.WriteString(w, "DISPLAY ")
			vr.emitExpr(w)
			return
		}
		io.WriteString(w, "MOVE ")
		d.Expr.emitExpr(w)
		io.WriteString(w, " TO TMP-STR")
		io.WriteString(w, "\n    DISPLAY FUNCTION TRIM(TMP-STR)")
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
	if a.IsString {
		io.WriteString(w, "MOVE ")
		a.Expr.emitExpr(w)
		io.WriteString(w, " TO ")
		io.WriteString(w, strings.ToUpper(a.Name))
		return
	}
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

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "PERFORM UNTIL NOT(")
	wst.Cond.emitExpr(w)
	io.WriteString(w, ")\n")
	for _, s := range wst.Body {
		io.WriteString(w, "    ")
		s.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "END-PERFORM")
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "PERFORM VARYING ")
	io.WriteString(w, strings.ToUpper(fr.Var))
	io.WriteString(w, " FROM ")
	if fr.Start != nil {
		fr.Start.emitExpr(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, " BY 1 UNTIL ")
	io.WriteString(w, strings.ToUpper(fr.Var))
	io.WriteString(w, " >= ")
	if fr.End != nil {
		fr.End.emitExpr(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, "\n")
	for _, s := range fr.Body {
		io.WriteString(w, "    ")
		s.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "END-PERFORM")
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
	if len(p.Vars) > 0 || p.NeedTmp || p.NeedStr {
		buf.WriteString("\nDATA DIVISION.\n")
		buf.WriteString("WORKING-STORAGE SECTION.\n")
		for _, v := range p.Vars {
			fmt.Fprintf(&buf, "01 %s %s", strings.ToUpper(v.Name), v.Pic)
			if v.Val != nil {
				buf.WriteString(" VALUE ")
				v.Val.emitExpr(&buf)
			} else if strings.HasPrefix(v.Pic, "PIC 9") {
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
			isStr := false
			if _, ok := types.TypeOfExpr(st.Assign.Value, env).(types.StringType); ok {
				isStr = true
			}
			pr.addStmt(&AssignStmt{Name: st.Assign.Name, Expr: ex, IsString: isStr})
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
		case st.While != nil:
			cond, err := convertExpr(st.While.Cond, env)
			if err != nil {
				return nil, err
			}
			body, err := transpileStmts(st.While.Body, env)
			if err != nil {
				return nil, err
			}
			pr.addStmt(&WhileStmt{Cond: cond, Body: body})
		case st.For != nil:
			if st.For.RangeEnd != nil {
				pr.ensureIntVar(st.For.Name)
				start, err := convertExpr(st.For.Source, env)
				if err != nil {
					return nil, err
				}
				end, err := convertExpr(st.For.RangeEnd, env)
				if err != nil {
					return nil, err
				}
				body, err := transpileStmts(st.For.Body, env)
				if err != nil {
					return nil, err
				}
				pr.addStmt(&ForRangeStmt{Var: st.For.Name, Start: start, End: end, Body: body})
			} else {
				return nil, fmt.Errorf("unsupported statement")
			}
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
			isStr := false
			if _, ok := types.TypeOfExpr(s.Assign.Value, env).(types.StringType); ok {
				isStr = true
			}
			out = append(out, &AssignStmt{Name: s.Assign.Name, Expr: ex, IsString: isStr})
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
		case s.While != nil:
			cond, err := convertExpr(s.While.Cond, env)
			if err != nil {
				return nil, err
			}
			body, err := transpileStmts(s.While.Body, env)
			if err != nil {
				return nil, err
			}
			out = append(out, &WhileStmt{Cond: cond, Body: body})
		case s.For != nil:
			if s.For.RangeEnd != nil {
				start, err := convertExpr(s.For.Source, env)
				if err != nil {
					return nil, err
				}
				end, err := convertExpr(s.For.RangeEnd, env)
				if err != nil {
					return nil, err
				}
				body, err := transpileStmts(s.For.Body, env)
				if err != nil {
					return nil, err
				}
				out = append(out, &ForRangeStmt{Var: s.For.Name, Start: start, End: end, Body: body})
			} else {
				return nil, fmt.Errorf("unsupported statement")
			}
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
			switch v := lit.(type) {
			case *IntLit:
				n := len(strconv.Itoa(absInt(v.Value)))
				if v.Value < 0 {
					n++
				}
				if n <= 1 {
					pic = "PIC 9"
				} else {
					pic = fmt.Sprintf("PIC 9(%d)", n)
				}
			case *StringLit:
				l := len(v.Value)
				if l == 0 {
					l = 1
				}
				pic = fmt.Sprintf("PIC X(%d)", l)
			}
		}
	}
	var constVal Expr
	if val != nil {
		if lit := literalExpr(val); lit != nil {
			constVal = lit
		} else if ie := ifExpr(val); ie != nil {
			stmt, err := convertIfExprAssign(name, ie, env)
			if err != nil {
				return VarDecl{}, nil, err
			}
			tval := types.TypeOfExpr(val, env)
			if _, ok := tval.(types.StringType); ok {
				l := maxIfStringLen(ie)
				if l > 0 {
					pic = fmt.Sprintf("PIC X(%d)", l)
				} else {
					pic = "PIC X(100)"
				}
			}
			return VarDecl{Name: name, Pic: pic}, stmt, nil
		} else {
			ex, err := convertExpr(val, env)
			if err != nil {
				return VarDecl{}, nil, err
			}
			tval := types.TypeOfExpr(val, env)
			isStr := false
			if _, ok := tval.(types.StringType); ok {
				if sl, ok := literalExpr(val).(*StringLit); ok {
					l := len(sl.Value)
					if l == 0 {
						l = 1
					}
					pic = fmt.Sprintf("PIC X(%d)", l)
				} else {
					pic = "PIC X(100)"
				}
				isStr = true
			}
			return VarDecl{Name: name, Pic: pic}, &AssignStmt{Name: name, Expr: ex, IsString: isStr}, nil
		}
	}
	return VarDecl{Name: name, Pic: pic, Val: constVal}, nil, nil
}

func ifExpr(e *parser.Expr) *parser.IfExpr {
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
	return pf.Target.If
}

func convertIfExprAssign(name string, ie *parser.IfExpr, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(ie.Cond, env)
	if err != nil {
		return nil, err
	}
	thenEx, err := convertExpr(ie.Then, env)
	if err != nil {
		return nil, err
	}
	isStrThen := false
	if _, ok := types.TypeOfExpr(ie.Then, env).(types.StringType); ok {
		isStrThen = true
	}
	thenStmts := []Stmt{&AssignStmt{Name: name, Expr: thenEx, IsString: isStrThen}}
	var elseStmts []Stmt
	if ie.ElseIf != nil {
		st, err := convertIfExprAssign(name, ie.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{st}
	} else if ie.Else != nil {
		elseEx, err := convertExpr(ie.Else, env)
		if err != nil {
			return nil, err
		}
		isStrElse := false
		if _, ok := types.TypeOfExpr(ie.Else, env).(types.StringType); ok {
			isStrElse = true
		}
		elseStmts = []Stmt{&AssignStmt{Name: name, Expr: elseEx, IsString: isStrElse}}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func maxIfStringLen(ie *parser.IfExpr) int {
	max := 0
	if lit := literalExpr(ie.Then); lit != nil {
		if s, ok := lit.(*StringLit); ok {
			if len(s.Value) > max {
				max = len(s.Value)
			}
		}
	}
	if ie.ElseIf != nil {
		if l := maxIfStringLen(ie.ElseIf); l > max {
			max = l
		}
	} else if ie.Else != nil {
		if lit := literalExpr(ie.Else); lit != nil {
			if s, ok := lit.(*StringLit); ok {
				if len(s.Value) > max {
					max = len(s.Value)
				}
			}
		}
	}
	return max
}

func literalExpr(e *parser.Expr) Expr {
	if v, ok := intConstExpr(e); ok {
		return &IntLit{Value: v}
	}
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

func intConstExpr(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil {
		return 0, false
	}
	val, ok := intConstUnary(e.Binary.Left)
	if !ok {
		return 0, false
	}
	cur := val
	for _, op := range e.Binary.Right {
		r, ok := intConstPostfix(op.Right)
		if !ok {
			return 0, false
		}
		switch op.Op {
		case "+":
			cur += r
		case "-":
			cur -= r
		case "*":
			cur *= r
		case "/":
			if r == 0 {
				return 0, false
			}
			cur /= r
		case "%":
			if r == 0 {
				return 0, false
			}
			cur %= r
		default:
			return 0, false
		}
	}
	return cur, true
}

func intConstUnary(u *parser.Unary) (int, bool) {
	if u == nil {
		return 0, false
	}
	v, ok := intConstPostfix(u.Value)
	if !ok {
		return 0, false
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		if u.Ops[i] != "-" {
			return 0, false
		}
		v = -v
	}
	return v, true
}

func intConstPostfix(pf *parser.PostfixExpr) (int, bool) {
	if pf == nil || len(pf.Ops) != 0 || pf.Target == nil {
		return 0, false
	}
	lit := pf.Target.Lit
	if lit == nil || lit.Int == nil {
		return 0, false
	}
	return int(*lit.Int), true
}

func convertExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expr")
	}
	first, err := convertUnary(e.Binary.Left, env)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := make([]*parser.BinaryOp, len(e.Binary.Right))
	for i, op := range e.Binary.Right {
		ex, err := convertPostfix(op.Right, env)
		if err != nil {
			return nil, err
		}
		operands = append(operands, ex)
		ops[i] = op
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!="},
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
			if contains(level, ops[i].Op) {
				left := operands[i]
				right := operands[i+1]
				newExpr := &BinaryExpr{Op: ops[i].Op, Left: left, Right: right}
				operands[i] = newExpr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	return operands[0], nil
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
	case p.Call != nil && p.Call.Func == "len" && len(p.Call.Args) == 1:
		arg, err := convertExpr(p.Call.Args[0], env)
		if err != nil {
			return nil, err
		}
		return &LenExpr{Value: arg}, nil
	case p.Call != nil && p.Call.Func == "str" && len(p.Call.Args) == 1:
		if lit := literalExpr(p.Call.Args[0]); lit != nil {
			switch v := lit.(type) {
			case *IntLit:
				return &StrExpr{Value: fmt.Sprintf("%d", v.Value)}, nil
			case *StringLit:
				return &StrExpr{Value: v.Value}, nil
			}
		}
		return nil, fmt.Errorf("unsupported primary")
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

func absInt(v int) int {
	if v < 0 {
		return -v
	}
	return v
}

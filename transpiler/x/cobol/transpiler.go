//go:build slow

package cobol

import (
	"bytes"
	"fmt"
	"io"
	"mochi/parser"
	"mochi/types"
	"strconv"
	"strings"
)

// --- Simple COBOL AST ---

// Program contains variable declarations and a sequence of statements.
type Program struct {
	Vars  []VarDecl
	Stmts []Stmt
}

var constVars map[string]interface{}
var currentProgram *Program

func (p *Program) addStmt(s Stmt) {
	p.Stmts = append(p.Stmts, s)
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

// FloatLit represents a floating point literal.
type FloatLit struct{ Value float64 }

type StringLit struct{ Value string }

// LenExpr represents a builtin len() call.
type LenExpr struct{ Value Expr }

// StrExpr represents a builtin str() call on a constant.
type StrExpr struct{ Value string }

// ContainsExpr represents s.contains(sub).
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

// SubstringExpr represents substring(str, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

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

func (f *FloatLit) emitExpr(w io.Writer) {
	if f.Value == float64(int64(f.Value)) {
		fmt.Fprintf(w, "%.1f", f.Value)
	} else {
		fmt.Fprintf(w, "%g", f.Value)
	}
}

func (s *StringLit) emitExpr(w io.Writer) {
	esc := strings.ReplaceAll(s.Value, "\"", "\"\"")
	fmt.Fprintf(w, "\"%s\"", esc)
}

func (s *StrExpr) emitExpr(w io.Writer) {
	esc := strings.ReplaceAll(s.Value, "\"", "\"\"")
	fmt.Fprintf(w, "\"%s\"", esc)
}

func (c *ContainsExpr) emitExpr(w io.Writer) {
	io.WriteString(w, "FUNCTION POSITION(")
	c.Sub.emitExpr(w)
	io.WriteString(w, ", ")
	c.Str.emitExpr(w)
	io.WriteString(w, ") > 0")
}

func (s *SubstringExpr) emitExpr(w io.Writer) {
	s.Str.emitExpr(w)
	io.WriteString(w, "(")
	s.Start.emitExpr(w)
	io.WriteString(w, " + 1:")
	s.End.emitExpr(w)
	io.WriteString(w, " - ")
	s.Start.emitExpr(w)
	io.WriteString(w, ")")
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
		} else if u, ok := b.Left.(*UnaryExpr); ok && strings.TrimSpace(u.Op) == "-" {
			io.WriteString(w, "(")
			u.emitExpr(w)
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
		} else if u, ok := b.Right.(*UnaryExpr); ok && strings.TrimSpace(u.Op) == "-" {
			io.WriteString(w, "(")
			u.emitExpr(w)
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
	} else if u, ok := b.Left.(*UnaryExpr); ok && strings.TrimSpace(u.Op) == "-" {
		io.WriteString(w, "(")
		u.emitExpr(w)
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
	} else if u, ok := b.Right.(*UnaryExpr); ok && strings.TrimSpace(u.Op) == "-" {
		io.WriteString(w, "(")
		u.emitExpr(w)
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
	case *IntLit, *FloatLit, *StringLit, *VarRef, *StrExpr:
		return true
	case *SubstringExpr:
		return true
	case *UnaryExpr:
		if v.Op == "-" {
			if _, ok := v.Expr.(*IntLit); ok {
				return true
			}
		} else if strings.TrimSpace(v.Op) == "NOT" {
			return isBoolExpr(v.Expr)
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
	case *IntLit, *FloatLit, *VarRef:
		return true
	case *UnaryExpr:
		if v.Op == "-" {
			return isDirectNumber(v.Expr)
		}
	case *BinaryExpr:
		switch v.Op {
		case "+", "-", "*", "/", "%":
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
	case *UnaryExpr:
		if strings.TrimSpace(b.Op) == "NOT" {
			return isBoolExpr(b.Expr)
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
	if ce, ok := d.Expr.(*ContainsExpr); ok {
		io.WriteString(w, "IF ")
		ce.emitExpr(w)
		io.WriteString(w, " THEN\n        DISPLAY 1\n    ELSE\n        DISPLAY 0\n    END-IF")
		return
	}
	if isBoolExpr(d.Expr) {
		io.WriteString(w, "IF ")
		emitCondExpr(w, d.Expr)
		io.WriteString(w, "\n        DISPLAY 1\n    ELSE\n        DISPLAY 0\n    END-IF")
		return
	}
	io.WriteString(w, "DISPLAY ")
	d.Expr.emitExpr(w)
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

// Emit renders COBOL source code.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(">>SOURCE FORMAT FREE\n")
	buf.WriteString("IDENTIFICATION DIVISION.\n")
	buf.WriteString("PROGRAM-ID. MAIN.\n")
	if len(p.Vars) > 0 {
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
	constVars = map[string]interface{}{}
	currentProgram = pr
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
			if vals, ok := listLiteralInts(st.Assign.Value); ok {
				constVars[st.Assign.Name] = vals
			} else {
				delete(constVars, st.Assign.Name)
			}
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(call.Args) == 1 && len(st.Expr.Expr.Binary.Right) == 0 {
				ex, err := convertExpr(call.Args[0], env)
				if err != nil {
					return nil, err
				}
				isStr := false
				if _, ok := types.TypeOfExpr(call.Args[0], env).(types.StringType); ok {
					isStr = true
				}
				if _, ok := ex.(*StringLit); ok {
					isStr = true
				}
				pr.addStmt(&DisplayStmt{Expr: ex, IsString: isStr})
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
			} else if vals, ok := listLiteralInts(st.For.Source); ok {
				pr.ensureIntVar(st.For.Name)
				body, err := transpileStmts(st.For.Body, env)
				if err != nil {
					return nil, err
				}
				for _, v := range vals {
					pr.addStmt(&AssignStmt{Name: st.For.Name, Expr: &IntLit{Value: v}})
					for _, b := range body {
						pr.addStmt(b)
					}
				}
			} else {
				return nil, fmt.Errorf("unsupported statement")
			}
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	_ = env
	normalizeSubstringLiterals(pr)
	currentProgram = nil
	return pr, nil
}

func normalizeSubstringLiterals(p *Program) {
	var count int

	var fixExpr func(e Expr) Expr
	fixExpr = func(e Expr) Expr {
		switch v := e.(type) {
		case *SubstringExpr:
			if lit, ok := v.Str.(*StringLit); ok {
				count++
				name := fmt.Sprintf("STR%d", count)
				pic := fmt.Sprintf("PIC X(%d)", len(lit.Value))
				p.Vars = append(p.Vars, VarDecl{Name: name, Pic: pic, Val: lit})
				v.Str = &VarRef{Name: name}
			}
			v.Start = fixExpr(v.Start)
			v.End = fixExpr(v.End)
			return v
		case *BinaryExpr:
			v.Left = fixExpr(v.Left)
			v.Right = fixExpr(v.Right)
			return v
		case *UnaryExpr:
			v.Expr = fixExpr(v.Expr)
			return v
		case *ContainsExpr:
			v.Str = fixExpr(v.Str)
			v.Sub = fixExpr(v.Sub)
			return v
		case *LenExpr:
			v.Value = fixExpr(v.Value)
			return v
		default:
			return e
		}
	}

	var fixStmt func(s Stmt)
	fixStmt = func(s Stmt) {
		switch st := s.(type) {
		case *DisplayStmt:
			st.Expr = fixExpr(st.Expr)
		case *AssignStmt:
			st.Expr = fixExpr(st.Expr)
		case *IfStmt:
			st.Cond = fixExpr(st.Cond)
			for i := range st.Then {
				fixStmt(st.Then[i])
			}
			for i := range st.Else {
				fixStmt(st.Else[i])
			}
		case *WhileStmt:
			st.Cond = fixExpr(st.Cond)
			for i := range st.Body {
				fixStmt(st.Body[i])
			}
		case *ForRangeStmt:
			st.Start = fixExpr(st.Start)
			st.End = fixExpr(st.End)
			for i := range st.Body {
				fixStmt(st.Body[i])
			}
		}
	}

	for i := range p.Stmts {
		fixStmt(p.Stmts[i])
	}
}

func transpileStmts(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		switch {
		case s.Let != nil:
			vd, initStmt, err := convertVar(s.Let.Name, s.Let.Type, s.Let.Value, env)
			if err != nil {
				return nil, err
			}
			if currentProgram != nil {
				currentProgram.Vars = append(currentProgram.Vars, vd)
			}
			if initStmt != nil {
				out = append(out, initStmt)
			}
		case s.Var != nil:
			vd, initStmt, err := convertVar(s.Var.Name, s.Var.Type, s.Var.Value, env)
			if err != nil {
				return nil, err
			}
			if currentProgram != nil {
				currentProgram.Vars = append(currentProgram.Vars, vd)
			}
			if initStmt != nil {
				out = append(out, initStmt)
			}
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
				isStr := false
				if _, ok := types.TypeOfExpr(call.Args[0], env).(types.StringType); ok {
					isStr = true
				}
				out = append(out, &DisplayStmt{Expr: ex, IsString: isStr})
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
			} else if vals, ok := listLiteralInts(s.For.Source); ok {
				body, err := transpileStmts(s.For.Body, env)
				if err != nil {
					return nil, err
				}
				for _, v := range vals {
					out = append(out, &AssignStmt{Name: s.For.Name, Expr: &IntLit{Value: v}})
					out = append(out, body...)
				}
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
	if t == nil && val != nil {
		switch types.TypeOfExpr(val, env).(type) {
		case types.StringType:
			pic = "PIC X(100)"
		case types.BoolType, types.IntType:
			pic = "PIC 9"
		case types.FloatType:
			pic = "PIC 9V9"
		}
	}
	if t != nil && t.Simple != nil {
		switch *t.Simple {
		case "int":
			pic = "PIC 9"
		case "string":
			pic = "PIC X(100)"
		case "bool":
			pic = "PIC 9"
		case "float":
			pic = "PIC 9V9"
		}
	}
	var constVal Expr
	if val != nil {
		if lit := literalExpr(val); lit != nil {
			switch v := lit.(type) {
			case *IntLit:
				abs := v.Value
				if abs < 0 {
					abs = -abs
				}
				n := len(strconv.Itoa(abs))
				if v.Value < 0 {
					n++
				}
				if n <= 1 {
					pic = "PIC 9"
				} else {
					pic = fmt.Sprintf("PIC 9(%d)", n)
				}
			case *FloatLit:
				pic = "PIC 9V9"
			case *StringLit:
				l := len(v.Value)
				if l == 0 {
					l = 1
				}
				pic = fmt.Sprintf("PIC X(%d)", l)
			}
		} else if vals, ok := listLiteralInts(val); ok {
			parts := make([]string, len(vals))
			for i, n := range vals {
				parts[i] = strconv.Itoa(n)
			}
			s := "[" + strings.Join(parts, ", ") + "]"
			pic = fmt.Sprintf("PIC X(%d)", len(s))
			constVal = &StringLit{Value: s}
			constVars[name] = vals
		} else if maps, ok := listLiteralMaps(val); ok {
			s := formatConstMapSlice(maps)
			pic = fmt.Sprintf("PIC X(%d)", len(s))
			constVal = &StringLit{Value: s}
			constVars[name] = maps
		} else if m, ok := mapLiteral(val); ok {
			s := formatConstMap(m)
			pic = fmt.Sprintf("PIC X(%d)", len(s))
			constVal = &StringLit{Value: s}
			constVars[name] = m
		} else {
			delete(constVars, name)
		}
	}
	if val != nil {
		if lit := literalExpr(val); lit != nil {
			constVal = lit
		} else if vals, ok := listLiteralInts(val); ok {
			parts := make([]string, len(vals))
			for i, n := range vals {
				parts[i] = strconv.Itoa(n)
			}
			s := "[" + strings.Join(parts, ", ") + "]"
			constVal = &StringLit{Value: s}
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
	case lit.Float != nil:
		return &FloatLit{Value: *lit.Float}
	case lit.Str != nil:
		return &StringLit{Value: *lit.Str}
	default:
		return nil
	}
}

func stringConstExpr(e *parser.Expr) (string, bool) {
	if lit := literalExpr(e); lit != nil {
		if s, ok := lit.(*StringLit); ok {
			return s.Value, true
		}
	}
	return "", false
}

func identExpr(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
		return sel.Root, true
	}
	return "", false
}

func matchConstString(m *parser.MatchExpr) (string, bool) {
	tgt, ok := intConstExpr(m.Target)
	if !ok {
		return "", false
	}
	var def string
	for _, c := range m.Cases {
		if name, ok := identExpr(c.Pattern); ok && name == "_" {
			if s, ok := stringConstExpr(c.Result); ok {
				def = s
			}
			continue
		}
		v, ok := intConstExpr(c.Pattern)
		if !ok || v != tgt {
			continue
		}
		if s, ok := stringConstExpr(c.Result); ok {
			return s, true
		}
		return "", false
	}
	if def != "" {
		return def, true
	}
	return "", false
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

func listLiteralLen(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return 0, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return 0, false
	}
	if lst := u.Value.Target.List; lst != nil {
		return len(lst.Elems), true
	}
	return 0, false
}

func listLiteralInts(e *parser.Expr) ([]int, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil, false
	}
	if lst := u.Value.Target.List; lst != nil {
		out := make([]int, len(lst.Elems))
		for i, el := range lst.Elems {
			v, ok := intConstExpr(el)
			if !ok {
				return nil, false
			}
			out[i] = v
		}
		return out, true
	}
	return nil, false
}

func listLiteralMaps(e *parser.Expr) ([]map[string]any, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil, false
	}
	if lst := u.Value.Target.List; lst != nil {
		out := make([]map[string]any, len(lst.Elems))
		for i, el := range lst.Elems {
			m, ok := mapLiteral(el)
			if !ok {
				return nil, false
			}
			sm := map[string]any{}
			for k, v := range m {
				ks, ok := k.(string)
				if !ok {
					return nil, false
				}
				sm[ks] = v
			}
			out[i] = sm
		}
		return out, true
	}
	return nil, false
}

func mapLiteral(e *parser.Expr) (map[interface{}]interface{}, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil, false
	}
	ml := u.Value.Target.Map
	if ml == nil {
		return nil, false
	}
	m := make(map[interface{}]interface{}, len(ml.Items))
	for _, it := range ml.Items {
		key, ok := constMapKey(it.Key)
		if !ok {
			return nil, false
		}
		val, ok := constMapVal(it.Value)
		if !ok {
			return nil, false
		}
		m[key] = val
	}
	return m, true
}

func constMapKey(e *parser.Expr) (interface{}, bool) {
	if v, ok := intConstExpr(e); ok {
		return v, true
	}
	if s, ok := stringConstExpr(e); ok {
		return s, true
	}
	if name, ok := identExpr(e); ok {
		if v, ok := constVars[name]; ok {
			switch v := v.(type) {
			case int, string:
				return v, true
			}
		}
	}
	return nil, false
}

func constMapVal(e *parser.Expr) (interface{}, bool) {
	if v, ok := intConstExpr(e); ok {
		return v, true
	}
	if s, ok := stringConstExpr(e); ok {
		return s, true
	}
	if m, ok := mapLiteral(e); ok {
		return m, true
	}
	if name, ok := identExpr(e); ok {
		if v, ok := constVars[name]; ok {
			switch v := v.(type) {
			case int, string, map[interface{}]interface{}:
				return v, true
			}
		}
	}
	return nil, false
}

func convertExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expr")
	}
	if len(e.Binary.Right) == 1 && e.Binary.Right[0].Op == "in" {
		leftExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: e.Binary.Left}}
		rightExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: e.Binary.Right[0].Right}}}

		// list membership
		if leftVal, lok := intConstExpr(leftExpr); lok {
			var lst []int
			if vals, ok := listLiteralInts(rightExpr); ok {
				lst = vals
			} else if name, ok := identExpr(rightExpr); ok {
				if vals, ok := constVars[name].([]int); ok {
					lst = vals
				}
			}
			if lst != nil {
				found := false
				for _, v := range lst {
					if v == leftVal {
						found = true
						break
					}
				}
				if found {
					return &StringLit{Value: "True"}, nil
				}
				return &StringLit{Value: "False"}, nil
			}
		}

		// map membership
		if key, ok := constMapKey(leftExpr); ok {
			var m map[interface{}]interface{}
			if mm, ok := mapLiteral(rightExpr); ok {
				m = mm
			} else if name, ok := identExpr(rightExpr); ok {
				if mm, ok := constVars[name].(map[interface{}]interface{}); ok {
					m = mm
				}
			}
			if m != nil {
				if _, found := m[key]; found {
					return &StringLit{Value: "True"}, nil
				}
				return &StringLit{Value: "False"}, nil
			}
		}
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
	res := operands[0]
	if v, ok := evalInt(res); ok {
		return &IntLit{Value: v}, nil
	}
	return res, nil
}

func convertUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	ex, err := convertPostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			ex = &UnaryExpr{Op: "-", Expr: ex}
		case "!":
			if sl, ok := ex.(*StringLit); ok {
				if sl.Value == "True" {
					ex = &IntLit{Value: 0}
				} else if sl.Value == "False" {
					ex = &IntLit{Value: 1}
				} else {
					return nil, fmt.Errorf("unsupported unary op")
				}
			} else {
				ex = &UnaryExpr{Op: "NOT ", Expr: ex}
			}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return ex, nil
}

func convertPostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		base := &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
		recv, err := convertPrimary(base, env)
		if err != nil {
			return nil, err
		}
		method := pf.Target.Selector.Tail[0]
		args := pf.Ops[0].Call.Args
		switch method {
		case "contains":
			if len(args) != 1 {
				return nil, fmt.Errorf("unsupported call")
			}
			arg, err := convertExpr(args[0], env)
			if err != nil {
				return nil, err
			}
			return &ContainsExpr{Str: recv, Sub: arg}, nil
		default:
			return nil, fmt.Errorf("unsupported method")
		}
	}

	ex, err := convertPrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		if op.Index != nil {
			idx := op.Index
			if idx.Colon == nil && idx.Colon2 == nil {
				if idx.Start == nil {
					return nil, fmt.Errorf("unsupported index")
				}
				// constant list indexing
				if pf.Target != nil && pf.Target.List != nil {
					elems, ok := listLiteralInts(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: pf}}})
					if ok {
						if pos, ok := intConstExpr(idx.Start); ok {
							if pos >= 0 && pos < len(elems) {
								ex = &IntLit{Value: elems[pos]}
								continue
							}
						}
					}
				} else if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 0 {
					name := pf.Target.Selector.Root
					if elems, ok := constVars[name].([]int); ok {
						if pos, ok := intConstExpr(idx.Start); ok {
							if pos >= 0 && pos < len(elems) {
								ex = &IntLit{Value: elems[pos]}
								continue
							}
						}
					} else if m, ok := constVars[name].(map[interface{}]interface{}); ok {
						if key, ok := constMapKey(idx.Start); ok {
							if val, ok := m[key]; ok {
								switch v := val.(type) {
								case int:
									ex = &IntLit{Value: v}
								case string:
									ex = &StringLit{Value: v}
								case map[interface{}]interface{}:
									ex = &StringLit{Value: formatConstMap(v)}
								}
								continue
							}
						}
					}
				} else if pf.Target != nil && pf.Target.Map != nil {
					if m, ok := mapLiteral(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: pf}}}); ok {
						if key, ok := constMapKey(idx.Start); ok {
							if val, ok := m[key]; ok {
								switch v := val.(type) {
								case int:
									ex = &IntLit{Value: v}
								case string:
									ex = &StringLit{Value: v}
								case map[interface{}]interface{}:
									ex = &StringLit{Value: formatConstMap(v)}
								}
								continue
							}
						}
					}
				}
				start, err := convertExpr(idx.Start, env)
				if err != nil {
					return nil, err
				}
				end := &BinaryExpr{Op: "+", Left: start, Right: &IntLit{Value: 1}}
				ex = &SubstringExpr{Str: ex, Start: start, End: end}
			} else if idx.Colon != nil && idx.Colon2 == nil {
				var startExpr Expr = &IntLit{Value: 0}
				var endExpr Expr
				if idx.Start != nil {
					startExpr, err = convertExpr(idx.Start, env)
					if err != nil {
						return nil, err
					}
				}
				if idx.End != nil {
					endExpr, err = convertExpr(idx.End, env)
					if err != nil {
						return nil, err
					}
				} else {
					return nil, fmt.Errorf("unsupported index")
				}
				ex = &SubstringExpr{Str: ex, Start: startExpr, End: endExpr}
			} else {
				return nil, fmt.Errorf("unsupported postfix")
			}
		} else if op.Cast != nil {
			if op.Cast.Type == nil || op.Cast.Type.Simple == nil {
				return nil, fmt.Errorf("unsupported cast")
			}
			switch *op.Cast.Type.Simple {
			case "int":
				sl, ok := ex.(*StringLit)
				if !ok {
					return nil, fmt.Errorf("unsupported cast")
				}
				v, err := strconv.Atoi(sl.Value)
				if err != nil {
					return nil, fmt.Errorf("invalid int cast")
				}
				ex = &IntLit{Value: v}
			default:
				return nil, fmt.Errorf("unsupported cast")
			}
		} else {
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return ex, nil
}

func convertPrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Call != nil && p.Call.Func == "len" && len(p.Call.Args) == 1:
		if n, ok := listLiteralLen(p.Call.Args[0]); ok {
			return &IntLit{Value: n}, nil
		}
		if m, ok := mapLiteral(p.Call.Args[0]); ok {
			return &IntLit{Value: len(m)}, nil
		}
		if name, ok := identExpr(p.Call.Args[0]); ok {
			if m, ok := constVars[name].(map[interface{}]interface{}); ok {
				return &IntLit{Value: len(m)}, nil
			}
		}
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
	case p.Call != nil && p.Call.Func == "sum" && len(p.Call.Args) == 1:
		if vals, ok := listLiteralInts(p.Call.Args[0]); ok {
			total := 0
			for _, v := range vals {
				total += v
			}
			return &IntLit{Value: total}, nil
		}
		return nil, fmt.Errorf("unsupported primary")
	case p.Call != nil && p.Call.Func == "avg" && len(p.Call.Args) == 1:
		if vals, ok := listLiteralInts(p.Call.Args[0]); ok {
			total := 0
			for _, v := range vals {
				total += v
			}
			if len(vals) == 0 {
				return &FloatLit{Value: 0}, nil
			}
			return &FloatLit{Value: float64(total) / float64(len(vals))}, nil
		}
		return nil, fmt.Errorf("unsupported primary")
	case p.Call != nil && (p.Call.Func == "min" || p.Call.Func == "max") && len(p.Call.Args) == 1:
		if vals, ok := listLiteralInts(p.Call.Args[0]); ok {
			if len(vals) == 0 {
				return &IntLit{Value: 0}, nil
			}
			m := vals[0]
			for _, v := range vals[1:] {
				if p.Call.Func == "min" {
					if v < m {
						m = v
					}
				} else {
					if v > m {
						m = v
					}
				}
			}
			return &IntLit{Value: m}, nil
		} else if ref, ok := identExpr(p.Call.Args[0]); ok {
			if lst, ok := constVars[ref].([]int); ok {
				if len(lst) == 0 {
					return &IntLit{Value: 0}, nil
				}
				m := lst[0]
				for _, v := range lst[1:] {
					if p.Call.Func == "min" {
						if v < m {
							m = v
						}
					} else {
						if v > m {
							m = v
						}
					}
				}
				return &IntLit{Value: m}, nil
			}
		}
		return nil, fmt.Errorf("unsupported primary")
	case p.Call != nil && p.Call.Func == "append" && len(p.Call.Args) == 2:
		if vals, ok := listLiteralInts(p.Call.Args[0]); ok {
			if v, ok := intConstExpr(p.Call.Args[1]); ok {
				vals = append(vals, v)
				parts := make([]string, len(vals))
				for i, n := range vals {
					parts[i] = strconv.Itoa(n)
				}
				return &StringLit{Value: "[" + strings.Join(parts, ", ") + "]"}, nil
			}
		} else if ref, ok := identExpr(p.Call.Args[0]); ok {
			if lst, ok := constVars[ref].([]int); ok {
				if v, ok := intConstExpr(p.Call.Args[1]); ok {
					lst = append(lst, v)
					parts := make([]string, len(lst))
					for i, n := range lst {
						parts[i] = strconv.Itoa(n)
					}
					return &StringLit{Value: "[" + strings.Join(parts, ", ") + "]"}, nil
				}
			}
		}
		return nil, fmt.Errorf("unsupported primary")
	case p.Call != nil && p.Call.Func == "count" && len(p.Call.Args) == 1:
		if vals, ok := listLiteralInts(p.Call.Args[0]); ok {
			return &IntLit{Value: len(vals)}, nil
		} else if ref, ok := identExpr(p.Call.Args[0]); ok {
			if lst, ok := constVars[ref].([]int); ok {
				return &IntLit{Value: len(lst)}, nil
			}
		}
		return nil, fmt.Errorf("unsupported primary")
	case p.Call != nil && p.Call.Func == "values" && len(p.Call.Args) == 1:
		if m, ok := mapLiteral(p.Call.Args[0]); ok {
			return &StringLit{Value: formatConstValues(m)}, nil
		}
		if name, ok := identExpr(p.Call.Args[0]); ok {
			if m, ok := constVars[name].(map[interface{}]interface{}); ok {
				return &StringLit{Value: formatConstValues(m)}, nil
			}
		}
		return nil, fmt.Errorf("unsupported primary")
	case p.Match != nil:
		if s, ok := matchConstString(p.Match); ok {
			return &StringLit{Value: s}, nil
		}
		return nil, fmt.Errorf("unsupported primary")
	case p.Call != nil && p.Call.Func == "substring" && len(p.Call.Args) == 3:
		strArg, err := convertExpr(p.Call.Args[0], env)
		if err != nil {
			return nil, err
		}
		startArg, err := convertExpr(p.Call.Args[1], env)
		if err != nil {
			return nil, err
		}
		endArg, err := convertExpr(p.Call.Args[2], env)
		if err != nil {
			return nil, err
		}
		return &SubstringExpr{Str: strArg, Start: startArg, End: endArg}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if vals, ok := constVars[p.Selector.Root].([]int); ok {
			parts := make([]string, len(vals))
			for i, n := range vals {
				parts[i] = strconv.Itoa(n)
			}
			return &StringLit{Value: "[" + strings.Join(parts, ", ") + "]"}, nil
		}
		return &VarRef{Name: p.Selector.Root}, nil
	case p.Query != nil:
		if s, err := evalQueryString(p.Query); err == nil {
			return &StringLit{Value: s}, nil
		}
		return nil, fmt.Errorf("unsupported primary")
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

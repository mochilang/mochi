//go:build slow

package ocaml

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Program represents a sequence of OCaml statements.
// Program represents an OCaml program. All statements are emitted inside
// a `let () =` block. Helper functions may be emitted before the block.
type Program struct {
	Stmts []Stmt
}

const prelude = `let string_contains s sub =
  let len_s = String.length s and len_sub = String.length sub in
  let rec aux i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else aux (i + 1)
  in aux 0

let string_of_int_list lst =
  String.concat " " (List.map string_of_int lst)
`

type VarInfo struct {
	typ string
	ref bool
}

// Stmt can emit itself as OCaml code.
type Stmt interface{ emit(io.Writer) }

// LetStmt represents a simple variable binding.
type LetStmt struct {
	Name string
	Expr Expr
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  let %s = ", l.Name)
	l.Expr.emit(w)
	io.WriteString(w, " in\n")
}

// VarStmt represents a mutable variable binding using OCaml references.
type VarStmt struct {
	Name string
	Expr Expr
}

func (v *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  let %s = ref ", v.Name)
	v.Expr.emit(w)
	io.WriteString(w, " in\n")
}

// AssignStmt represents an update to a mutable variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  %s := ", a.Name)
	a.Expr.emit(w)
	io.WriteString(w, ";\n")
}

// PrintStmt represents a call to print_endline.
type PrintStmt struct{ Expr Expr }

func (p *PrintStmt) emit(w io.Writer) {
	io.WriteString(w, "  print_endline (")
	p.Expr.emitPrint(w)
	io.WriteString(w, ");\n")
}

// IfStmt represents a basic if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "  if ")
	i.Cond.emit(w)
	io.WriteString(w, " then (\n")
	for _, st := range i.Then {
		st.emit(w)
	}
	io.WriteString(w, "  )")
	if len(i.Else) > 0 {
		io.WriteString(w, " else (\n")
		for _, st := range i.Else {
			st.emit(w)
		}
		io.WriteString(w, "  )")
	}
	io.WriteString(w, ";\n")
}

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "  while ")
	ws.Cond.emit(w)
	io.WriteString(w, " do\n")
	for _, st := range ws.Body {
		st.emit(w)
	}
	io.WriteString(w, "  done;\n")
}

// ForRangeStmt represents a numeric for-loop like `for i in a..b {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "  for ")
	io.WriteString(w, fr.Name)
	io.WriteString(w, " = ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, " to (")
	fr.End.emit(w)
	io.WriteString(w, " - 1) do\n")
	for _, st := range fr.Body {
		st.emit(w)
	}
	io.WriteString(w, "  done;\n")
}

// ForEachStmt represents iteration over a collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (fe *ForEachStmt) emit(w io.Writer) {
	io.WriteString(w, "  List.iter (fun ")
	io.WriteString(w, fe.Name)
	io.WriteString(w, " ->\n")
	for _, st := range fe.Body {
		st.emit(w)
	}
	io.WriteString(w, "  ) ")
	fe.Iterable.emit(w)
	io.WriteString(w, ";\n")
}

// Expr is any OCaml expression that can emit itself.
type Expr interface {
	emit(io.Writer)
	emitPrint(io.Writer)
}

// StrBuiltin represents a call to the builtin str() function.
type StrBuiltin struct{ Expr Expr }

func (s *StrBuiltin) emit(w io.Writer) {
	io.WriteString(w, "string_of_int ")
	s.Expr.emit(w)
}

func (s *StrBuiltin) emitPrint(w io.Writer) { s.emit(w) }

// LenBuiltin represents a call to len() builtin.
// Mode specifies whether we are operating on a string or list.
type LenBuiltin struct {
	Arg  Expr
	Mode string
}

func (l *LenBuiltin) emit(w io.Writer) {
	if l.Mode == "string" {
		io.WriteString(w, "String.length ")
	} else {
		io.WriteString(w, "List.length ")
	}
	l.Arg.emit(w)
}

func (l *LenBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int (")
	l.emit(w)
	io.WriteString(w, ")")
}

// SubstringBuiltin represents substring(str, start, end).
type SubstringBuiltin struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringBuiltin) emit(w io.Writer) {
	io.WriteString(w, "String.sub ")
	s.Str.emit(w)
	io.WriteString(w, " ")
	s.Start.emit(w)
	io.WriteString(w, " (")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ")")
}

func (s *SubstringBuiltin) emitPrint(w io.Writer) { s.emit(w) }

// SumBuiltin represents sum(list).
type SumBuiltin struct{ List Expr }

func (s *SumBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.fold_left (fun acc x -> acc + x) 0 ")
	s.List.emit(w)
	io.WriteString(w, ")")
}

func (s *SumBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int ")
	s.emit(w)
}

// AppendBuiltin represents append(list, elem).
type AppendBuiltin struct {
	List Expr
	Elem Expr
}

func (a *AppendBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(")
	a.List.emit(w)
	io.WriteString(w, " @ [")
	a.Elem.emit(w)
	io.WriteString(w, "])")
}

func (a *AppendBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int_list ")
	a.emit(w)
}

// StringContainsBuiltin represents s.contains(sub).
type StringContainsBuiltin struct {
	Str Expr
	Sub Expr
}

func (s *StringContainsBuiltin) emit(w io.Writer) {
	io.WriteString(w, "string_contains ")
	s.Str.emit(w)
	io.WriteString(w, " ")
	s.Sub.emit(w)
}

func (s *StringContainsBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int (if ")
	s.emit(w)
	io.WriteString(w, " then 1 else 0)")
}

// ListLit represents a list literal of integers.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, "; ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

func (l *ListLit) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int_list ")
	l.emit(w)
}

// UnaryMinus represents negation of an integer expression.
type UnaryMinus struct{ Expr Expr }

func (u *UnaryMinus) emit(w io.Writer) {
	io.WriteString(w, "(-")
	u.Expr.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryMinus) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int (-")
	u.Expr.emit(w)
	io.WriteString(w, ")")
}

// IfExpr represents a conditional expression.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
	Typ  string
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then ")
	i.Then.emit(w)
	io.WriteString(w, " else ")
	i.Else.emit(w)
}

func (i *IfExpr) emitPrint(w io.Writer) {
	switch i.Typ {
	case "int":
		io.WriteString(w, "string_of_int ")
		i.emit(w)
	case "bool":
		io.WriteString(w, "string_of_int (if ")
		i.Cond.emit(w)
		io.WriteString(w, " then (if ")
		i.Then.emit(w)
		io.WriteString(w, " then 1 else 0) else (if ")
		i.Else.emit(w)
		io.WriteString(w, " then 1 else 0))")
	default:
		i.emit(w)
	}
}

// Name represents a variable reference.
type Name struct {
	Ident string
	Typ   string
	Ref   bool
}

func (n *Name) emit(w io.Writer) {
	if n.Ref {
		fmt.Fprintf(w, "!%s", n.Ident)
	} else {
		io.WriteString(w, n.Ident)
	}
}
func (n *Name) emitPrint(w io.Writer) {
	ident := n.Ident
	if n.Ref {
		ident = "!" + n.Ident
	}
	switch n.Typ {
	case "int":
		fmt.Fprintf(w, "string_of_int %s", ident)
	case "bool":
		fmt.Fprintf(w, "string_of_int (if %s then 1 else 0)", ident)
	case "list":
		fmt.Fprintf(w, "string_of_int_list %s", ident)
	default:
		io.WriteString(w, ident)
	}
}

// IntLit represents an integer literal.
type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer)      { fmt.Fprintf(w, "%d", i.Value) }
func (i *IntLit) emitPrint(w io.Writer) { fmt.Fprintf(w, "string_of_int %d", i.Value) }

// BoolLit represents a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprintf(w, "%t", b.Value) }
func (b *BoolLit) emitPrint(w io.Writer) {
	if b.Value {
		io.WriteString(w, "string_of_int 1")
	} else {
		io.WriteString(w, "string_of_int 0")
	}
}

// StringLit represents a string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer)      { fmt.Fprintf(w, "%q", s.Value) }
func (s *StringLit) emitPrint(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

// BinaryExpr represents a binary operation.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
	Typ   string
}

func (b *BinaryExpr) emit(w io.Writer) {
	op := b.Op
	if b.Op == "%" {
		op = "mod"
	}
	if b.Op == "+" && b.Typ == "string" {
		op = "^"
	}
	fmt.Fprintf(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", op)
	b.Right.emit(w)
	fmt.Fprintf(w, ")")
}

func (b *BinaryExpr) emitPrint(w io.Writer) {
	switch b.Typ {
	case "bool":
		io.WriteString(w, "string_of_int (if ")
		b.emit(w)
		io.WriteString(w, " then 1 else 0)")
	case "string":
		b.emit(w)
	default:
		io.WriteString(w, "string_of_int ")
		b.emit(w)
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
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(b))
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf("(* Generated by Mochi transpiler v%s on %s *)\n", version(), t.Format("2006-01-02 15:04:05 MST"))
}

func defaultValueExpr(typ string) Expr {
	switch typ {
	case "int":
		return &IntLit{Value: 0}
	case "bool":
		return &BoolLit{Value: false}
	case "string":
		return &StringLit{Value: ""}
	}
	return &IntLit{Value: 0}
}

// Emit renders OCaml source code for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	buf.WriteString("\n")
	buf.WriteString(prelude)
	buf.WriteString("\n")
	buf.WriteString("let () =\n")
	for _, s := range p.Stmts {
		s.emit(&buf)
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a simple OCaml AST.
func transpileStmt(st *parser.Statement, env *types.Env, vars map[string]VarInfo) (Stmt, error) {
	switch {
	case st.Let != nil:
		var expr Expr
		var typ string
		var err error
		if st.Let.Value != nil {
			expr, typ, err = convertExpr(st.Let.Value, env, vars)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil {
			typ = *st.Let.Type.Simple
			expr = defaultValueExpr(typ)
		} else {
			return nil, fmt.Errorf("let without value not supported")
		}
		vars[st.Let.Name] = VarInfo{typ: typ}
		return &LetStmt{Name: st.Let.Name, Expr: expr}, nil
	case st.Var != nil:
		var expr Expr
		var typ string
		var err error
		if st.Var.Value != nil {
			expr, typ, err = convertExpr(st.Var.Value, env, vars)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil {
			typ = *st.Var.Type.Simple
			expr = defaultValueExpr(typ)
		} else {
			return nil, fmt.Errorf("var without type or value not supported")
		}
		vars[st.Var.Name] = VarInfo{typ: typ, ref: true}
		return &VarStmt{Name: st.Var.Name, Expr: expr}, nil
	case st.Assign != nil:
		info, ok := vars[st.Assign.Name]
		if !ok || !info.ref {
			return nil, fmt.Errorf("assignment to unknown or immutable variable")
		}
		expr, _, err := convertExpr(st.Assign.Value, env, vars)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: expr}, nil
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" && len(call.Args) == 1 {
			expr, _, err := convertExpr(call.Args[0], env, vars)
			if err != nil {
				return nil, err
			}
			return &PrintStmt{Expr: expr}, nil
		}
		return nil, fmt.Errorf("unsupported expression")
	case st.If != nil:
		cond, _, err := convertExpr(st.If.Cond, env, vars)
		if err != nil {
			return nil, err
		}
		thenStmts, err := transpileStmts(st.If.Then, env, vars)
		if err != nil {
			return nil, err
		}
		var elseStmts []Stmt
		if st.If.ElseIf != nil {
			elseStmt, err := transpileStmt(&parser.Statement{If: st.If.ElseIf}, env, vars)
			if err != nil {
				return nil, err
			}
			if elseStmt != nil {
				elseStmts = []Stmt{elseStmt}
			}
		} else if len(st.If.Else) > 0 {
			elseStmts, err = transpileStmts(st.If.Else, env, vars)
			if err != nil {
				return nil, err
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case st.While != nil:
		cond, _, err := convertExpr(st.While.Cond, env, vars)
		if err != nil {
			return nil, err
		}
		body, err := transpileStmts(st.While.Body, env, vars)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		if st.For.RangeEnd != nil {
			start, _, err := convertExpr(st.For.Source, env, vars)
			if err != nil {
				return nil, err
			}
			endExpr, _, err := convertExpr(st.For.RangeEnd, env, vars)
			if err != nil {
				return nil, err
			}
			vars[st.For.Name] = VarInfo{typ: "int"}
			body, err := transpileStmts(st.For.Body, env, vars)
			if err != nil {
				return nil, err
			}
			delete(vars, st.For.Name)
			return &ForRangeStmt{Name: st.For.Name, Start: start, End: endExpr, Body: body}, nil
		}
		iter, _, err := convertExpr(st.For.Source, env, vars)
		if err != nil {
			return nil, err
		}
		vars[st.For.Name] = VarInfo{typ: "int"}
		body, err := transpileStmts(st.For.Body, env, vars)
		if err != nil {
			return nil, err
		}
		delete(vars, st.For.Name)
		return &ForEachStmt{Name: st.For.Name, Iterable: iter, Body: body}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func transpileStmts(list []*parser.Statement, env *types.Env, vars map[string]VarInfo) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		compiled, err := transpileStmt(st, env, vars)
		if err != nil {
			return nil, err
		}
		if compiled != nil {
			out = append(out, compiled)
		}
	}
	return out, nil
}

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	pr := &Program{}
	vars := map[string]VarInfo{}
	stmts, err := transpileStmts(prog.Statements, env, vars)
	if err != nil {
		return nil, err
	}
	pr.Stmts = stmts
	return pr, nil
}

// Print converts the custom AST into a generic ast.Node and prints it.
func Print(p *Program) {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		switch s := st.(type) {
		case *PrintStmt:
			n.Children = append(n.Children, &ast.Node{Kind: "print"})
		case *LetStmt:
			n.Children = append(n.Children, &ast.Node{Kind: "let", Value: s.Name})
		case *VarStmt:
			n.Children = append(n.Children, &ast.Node{Kind: "var", Value: s.Name})
		case *AssignStmt:
			n.Children = append(n.Children, &ast.Node{Kind: "assign", Value: s.Name})
		default:
			n.Children = append(n.Children, &ast.Node{Kind: "stmt"})
		}
	}
	n.Print("")
}

// --- Conversion helpers ---

func convertExpr(e *parser.Expr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if e == nil {
		return nil, "", fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary, env, vars)
}

func convertBinary(b *parser.BinaryExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if b == nil {
		return nil, "", fmt.Errorf("nil binary")
	}
	// Convert all operands first
	left, typ, err := convertUnary(b.Left, env, vars)
	if err != nil {
		return nil, "", err
	}
	operands := []Expr{left}
	typesList := []string{typ}
	ops := []string{}
	for _, op := range b.Right {
		right, rtyp, err := convertPostfix(op.Right, env, vars)
		if err != nil {
			return nil, "", err
		}
		operands = append(operands, right)
		typesList = append(typesList, rtyp)
		ops = append(ops, op.Op)
	}

	prec := func(op string) int {
		switch op {
		case "*", "/", "%":
			return 2
		case "+", "-":
			return 1
		case "==", "!=", "<", "<=", ">", ">=":
			return 0
		default:
			return -1
		}
	}

	// Shunting-yard like evaluation
	var exprStack []Expr
	var typeStack []string
	var opStack []string

	pushResult := func() {
		if len(opStack) == 0 {
			return
		}
		op := opStack[len(opStack)-1]
		opStack = opStack[:len(opStack)-1]
		if len(exprStack) < 2 || len(typeStack) < 2 {
			return
		}
		right := exprStack[len(exprStack)-1]
		exprStack = exprStack[:len(exprStack)-1]
		rtyp := typeStack[len(typeStack)-1]
		typeStack = typeStack[:len(typeStack)-1]
		left := exprStack[len(exprStack)-1]
		exprStack = exprStack[:len(exprStack)-1]
		ltyp := typeStack[len(typeStack)-1]
		typeStack = typeStack[:len(typeStack)-1]

		resTyp := ltyp
		switch op {
		case "+", "-", "*", "/", "%":
			if op == "+" && ltyp == "string" && rtyp == "string" {
				resTyp = "string"
			} else {
				resTyp = "int"
			}
		case "==", "!=", "<", "<=", ">", ">=":
			resTyp = "bool"
		}

		exprStack = append(exprStack, &BinaryExpr{Left: left, Op: op, Right: right, Typ: resTyp})
		typeStack = append(typeStack, resTyp)
	}

	exprStack = append(exprStack, operands[0])
	typeStack = append(typeStack, typesList[0])
	for i, op := range ops {
		for len(opStack) > 0 && prec(opStack[len(opStack)-1]) >= prec(op) {
			pushResult()
		}
		opStack = append(opStack, op)
		exprStack = append(exprStack, operands[i+1])
		typeStack = append(typeStack, typesList[i+1])
	}
	for len(opStack) > 0 {
		pushResult()
	}
	if len(exprStack) != 1 {
		return nil, "", fmt.Errorf("binary conversion failed")
	}
	return exprStack[0], typeStack[0], nil
}

func convertUnary(u *parser.Unary, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if u == nil {
		return nil, "", fmt.Errorf("nil unary")
	}
	if len(u.Ops) > 0 {
		if len(u.Ops) == 1 && u.Ops[0] == "-" {
			expr, typ, err := convertPostfix(u.Value, env, vars)
			if err != nil {
				return nil, "", err
			}
			if typ != "int" {
				return nil, "", fmt.Errorf("unary - only for ints")
			}
			return &UnaryMinus{Expr: expr}, "int", nil
		}
		return nil, "", fmt.Errorf("unary ops not supported")
	}
	return convertPostfix(u.Value, env, vars)
}

func convertPostfix(p *parser.PostfixExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if p == nil {
		return nil, "", fmt.Errorf("nil postfix")
	}
	// Handle selector call patterns like s.contains("sub")
	if p.Target != nil && p.Target.Selector != nil &&
		len(p.Target.Selector.Tail) == 1 && p.Target.Selector.Tail[0] == "contains" &&
		len(p.Ops) == 1 && p.Ops[0].Call != nil {
		arg, at, err := convertExpr(p.Ops[0].Call.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		info := vars[p.Target.Selector.Root]
		if info.typ != "string" || at != "string" {
			return nil, "", fmt.Errorf("contains expects string")
		}
		root := &Name{Ident: p.Target.Selector.Root, Typ: info.typ, Ref: info.ref}
		return &StringContainsBuiltin{Str: root, Sub: arg}, "bool", nil
	}
	expr, typ, err := convertPrimary(p.Target, env, vars)
	if err != nil {
		return nil, "", err
	}
	for i := 0; i < len(p.Ops); {
		op := p.Ops[i]
		switch {
		case op.Field != nil && op.Field.Name == "contains":
			if i+1 >= len(p.Ops) || p.Ops[i+1].Call == nil {
				return nil, "", fmt.Errorf("contains must be called")
			}
			call := p.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, "", fmt.Errorf("contains expects 1 arg")
			}
			arg, aTyp, err := convertExpr(call.Args[0], env, vars)
			if err != nil {
				return nil, "", err
			}
			if typ != "string" || aTyp != "string" {
				return nil, "", fmt.Errorf("contains expects string")
			}
			expr = &StringContainsBuiltin{Str: expr, Sub: arg}
			typ = "bool"
			i += 2
			continue
		default:
			return nil, "", fmt.Errorf("postfix op not supported")
		}
	}
	return expr, typ, nil
}

func convertPrimary(p *parser.Primary, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Group != nil:
		return convertExpr(p.Group, env, vars)
	case p.If != nil:
		return convertIf(p.If, env, vars)
	case p.Call != nil:
		return convertCall(p.Call, env, vars)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, _, err := convertExpr(e, env, vars)
			if err != nil {
				return nil, "", err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, "list", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			info := vars[p.Selector.Root]
			return &Name{Ident: p.Selector.Root, Typ: info.typ, Ref: info.ref}, info.typ, nil
		}
	}
	return nil, "", fmt.Errorf("unsupported expression")
}

func convertLiteral(l *parser.Literal) (Expr, string, error) {
	if l.Int != nil {
		return &IntLit{Value: int(*l.Int)}, "int", nil
	}
	if l.Bool != nil {
		return &BoolLit{Value: bool(*l.Bool)}, "bool", nil
	}
	if l.Str != nil {
		return &StringLit{Value: *l.Str}, "string", nil
	}
	return nil, "", fmt.Errorf("unsupported literal")
}

func convertIf(ifx *parser.IfExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	cond, _, err := convertExpr(ifx.Cond, env, vars)
	if err != nil {
		return nil, "", err
	}
	thenExpr, thenTyp, err := convertExpr(ifx.Then, env, vars)
	if err != nil {
		return nil, "", err
	}
	var elseExpr Expr
	var elseTyp string
	if ifx.ElseIf != nil {
		elseExpr, elseTyp, err = convertIf(ifx.ElseIf, env, vars)
		if err != nil {
			return nil, "", err
		}
	} else if ifx.Else != nil {
		elseExpr, elseTyp, err = convertExpr(ifx.Else, env, vars)
		if err != nil {
			return nil, "", err
		}
	} else {
		return nil, "", fmt.Errorf("if expression missing else")
	}
	if thenTyp != elseTyp {
		return nil, "", fmt.Errorf("if branches have different types")
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr, Typ: thenTyp}, thenTyp, nil
}

func convertCall(c *parser.CallExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if c.Func == "str" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "int" {
			return nil, "", fmt.Errorf("str only supports int")
		}
		return &StrBuiltin{Expr: arg}, "string", nil
	}
	if c.Func == "len" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		switch typ {
		case "string":
			return &LenBuiltin{Arg: arg, Mode: "string"}, "int", nil
		case "list":
			return &LenBuiltin{Arg: arg, Mode: "list"}, "int", nil
		default:
			return nil, "", fmt.Errorf("len only supports string or list")
		}
	}
	if c.Func == "count" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "list" {
			return nil, "", fmt.Errorf("count expects list")
		}
		return &LenBuiltin{Arg: arg, Mode: "list"}, "int", nil
	}
	if c.Func == "substring" && len(c.Args) == 3 {
		strArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "string" {
			return nil, "", fmt.Errorf("substring expects string")
		}
		startArg, typ2, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ2 != "int" {
			return nil, "", fmt.Errorf("substring start expects int")
		}
		endArg, typ3, err := convertExpr(c.Args[2], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ3 != "int" {
			return nil, "", fmt.Errorf("substring end expects int")
		}
		return &SubstringBuiltin{Str: strArg, Start: startArg, End: endArg}, "string", nil
	}
	if c.Func == "append" && len(c.Args) == 2 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "list" {
			return nil, "", fmt.Errorf("append expects list")
		}
		elemArg, etyp, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		if etyp != "int" {
			return nil, "", fmt.Errorf("append only supports int elements")
		}
		return &AppendBuiltin{List: listArg, Elem: elemArg}, "list", nil
	}
	if c.Func == "sum" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "list" {
			return nil, "", fmt.Errorf("sum expects list")
		}
		return &SumBuiltin{List: listArg}, "int", nil
	}
	return nil, "", fmt.Errorf("call %s not supported", c.Func)
}

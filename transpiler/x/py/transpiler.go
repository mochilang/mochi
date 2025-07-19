//go:build slow

package py

import (
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

// Python AST definitions

type Program struct {
	Stmts []Stmt
}

type Stmt interface{ isStmt() }

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
        Target Expr
        Name   string
}

func (f *FieldExpr) emit(w io.Writer) error {
        if err := emitExpr(w, f.Target); err != nil {
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

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := emitExpr(w, b.Left); err != nil {
		return err
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
	if err := emitExpr(w, b.Right); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
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

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf("# Generated by Mochi transpiler v%s on %s\n",
		version(), t.Format("2006-01-02 15:04:05 MST"))
}

// Emit renders Python code from AST
func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	for _, s := range p.Stmts {
		switch st := s.(type) {
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
		}
	}
	return nil
}

// Transpile converts a Mochi program to a Python AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	p := &Program{}
	for _, st := range prog.Statements {
		switch {
		case st.Expr != nil:
			e, err := convertExpr(st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ExprStmt{Expr: e})
		case st.Let != nil:
			e, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Expr: e})
               case st.Var != nil:
                       e, err := convertExpr(st.Var.Value)
                       if err != nil {
                               return nil, err
                       }
                       p.Stmts = append(p.Stmts, &VarStmt{Name: st.Var.Name, Expr: e})
               case st.While != nil:
                       cond, err := convertExpr(st.While.Cond)
                       if err != nil {
                               return nil, err
                       }
                       body, err := convertStmts(st.While.Body)
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
                       body, err := convertStmts(st.For.Body)
                       if err != nil {
                               return nil, err
                       }
                       p.Stmts = append(p.Stmts, &ForStmt{Var: st.For.Name, Iter: iter, Body: body})
               case st.Assign != nil:
			e, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			// TODO: ignore index and field for now
			p.Stmts = append(p.Stmts, &AssignStmt{Name: st.Assign.Name, Expr: e})
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
		case st.Fun != nil:
			body, err := convertStmts(st.Fun.Body)
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

func convertStmts(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		switch {
		case s.Expr != nil:
			e, err := convertExpr(s.Expr.Expr)
			if err != nil {
				return nil, err
			}
			out = append(out, &ExprStmt{Expr: e})
		case s.Let != nil:
			e, err := convertExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &LetStmt{Name: s.Let.Name, Expr: e})
               case s.Var != nil:
                       e, err := convertExpr(s.Var.Value)
                       if err != nil {
                               return nil, err
                       }
                       out = append(out, &VarStmt{Name: s.Var.Name, Expr: e})
               case s.While != nil:
                       cond, err := convertExpr(s.While.Cond)
                       if err != nil {
                               return nil, err
                       }
                       body, err := convertStmts(s.While.Body)
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
                       body, err := convertStmts(s.For.Body)
                       if err != nil {
                               return nil, err
                       }
                       out = append(out, &ForStmt{Var: s.For.Name, Iter: iter, Body: body})
               case s.Assign != nil:
			e, err := convertExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &AssignStmt{Name: s.Assign.Name, Expr: e})
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
		case s.Fun != nil:
			b, err := convertStmts(s.Fun.Body)
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

	levels := [][]string{
		{"*", "/", "%"},
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

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(p.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range p.Ops {
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
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
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
		switch p.Call.Func {
		case "append":
			if len(args) == 2 {
				return &BinaryExpr{Left: args[0], Op: "+", Right: &ListLit{Elems: []Expr{args[1]}}}, nil
			}
		case "avg":
			if len(args) == 1 {
				sumCall := &CallExpr{Func: &Name{Name: "sum"}, Args: []Expr{args[0]}}
				lenCall := &CallExpr{Func: &Name{Name: "len"}, Args: []Expr{args[0]}}
				div := &BinaryExpr{Left: sumCall, Op: "/", Right: lenCall}
				return &CondExpr{Cond: args[0], Then: div, Else: &IntLit{Value: "0"}}, nil
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
               case "substring":
                       if len(args) == 3 {
                               return &SliceExpr{Target: args[0], Start: args[1], End: args[2]}, nil
                       }
               }
               return &CallExpr{Func: &Name{Name: p.Call.Func}, Args: args}, nil
	case p.Selector != nil:
		expr := Expr(&Name{Name: p.Selector.Root})
		for _, t := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: t}
		}
		return expr, nil
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
			ke, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			ve, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			keys = append(keys, ke)
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
		return &FloatLit{Value: fmt.Sprintf("%g", *l.Float)}, nil
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
       case *FuncDef:
               n := &ast.Node{Kind: "func", Value: st.Name}
               for _, p := range st.Params {
                       n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
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
	case *CondExpr:
		return &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the Python AST in Lisp-like form to stdout.
func Print(p *Program) {
	toNode(p).Print("")
}

package lint

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"

	"mochi/parser"
)

// scope represents a lexical scope with declared variables and usage info.
type scope struct {
	vars map[string]lexer.Position
	used map[string]bool
}

type linter struct {
	scopes []*scope
	errs   []string
}

func (l *linter) push() {
	l.scopes = append(l.scopes, &scope{vars: map[string]lexer.Position{}, used: map[string]bool{}})
}

func (l *linter) pop() {
	s := l.scopes[len(l.scopes)-1]
	l.scopes = l.scopes[:len(l.scopes)-1]
	for name, pos := range s.vars {
		if !s.used[name] {
			l.errs = append(l.errs, fmt.Sprintf("%s:%d:%d: unused variable %q", pos.Filename, pos.Line, pos.Column, name))
		}
	}
}

func (l *linter) declare(name string, pos lexer.Position) {
	if len(l.scopes) == 0 {
		l.push()
	}
	s := l.scopes[len(l.scopes)-1]
	if _, exists := s.vars[name]; !exists {
		s.vars[name] = pos
	}
}

func (l *linter) use(name string) {
	for i := len(l.scopes) - 1; i >= 0; i-- {
		if _, ok := l.scopes[i].vars[name]; ok {
			l.scopes[i].used[name] = true
			return
		}
	}
}

func (l *linter) visitProgram(p *parser.Program) {
	l.push()
	for _, stmt := range p.Statements {
		l.visitStmt(stmt)
	}
	l.pop()
}

func (l *linter) visitBlock(stmts []*parser.Statement) {
	l.push()
	terminated := false
	for _, s := range stmts {
		if terminated {
			l.errs = append(l.errs, fmt.Sprintf("%s:%d:%d: unreachable code", s.Pos.Filename, s.Pos.Line, s.Pos.Column))
			break
		}
		if l.visitStmt(s) {
			terminated = true
		}
	}
	l.pop()
}

// visitStmt returns true if the statement stops execution of the current block.
func (l *linter) visitStmt(s *parser.Statement) bool {
	switch {
	case s.Let != nil:
		l.declare(s.Let.Name, s.Let.Pos)
		l.visitExpr(s.Let.Value)
	case s.Var != nil:
		l.declare(s.Var.Name, s.Var.Pos)
		l.visitExpr(s.Var.Value)
	case s.Assign != nil:
		l.use(s.Assign.Name)
		for _, idx := range s.Assign.Index {
			l.visitExpr(idx.Start)
			l.visitExpr(idx.End)
			l.visitExpr(idx.Step)
		}
		l.visitExpr(s.Assign.Value)
	case s.Return != nil:
		l.visitExpr(s.Return.Value)
		return true
	case s.Break != nil, s.Continue != nil:
		return true
	case s.Expr != nil:
		l.visitExpr(s.Expr.Expr)
	case s.Fun != nil:
		l.declare(s.Fun.Name, s.Fun.Pos)
		l.push()
		for _, p := range s.Fun.Params {
			l.declare(p.Name, s.Fun.Pos)
		}
		l.visitBlock(s.Fun.Body)
		l.pop()
	case s.If != nil:
		l.visitExpr(s.If.Cond)
		l.visitBlock(s.If.Then)
		if s.If.ElseIf != nil {
			l.visitStmt(&parser.Statement{If: s.If.ElseIf})
		}
		if len(s.If.Else) > 0 {
			l.visitBlock(s.If.Else)
		}
	case s.While != nil:
		l.visitExpr(s.While.Cond)
		l.visitBlock(s.While.Body)
	case s.For != nil:
		l.visitExpr(s.For.Source)
		l.visitExpr(s.For.RangeEnd)
		l.push()
		l.declare(s.For.Name, s.For.Pos)
		l.visitBlock(s.For.Body)
		l.pop()
	default:
		// other statement types are ignored for lint purposes
	}
	return false
}

func (l *linter) visitExpr(e *parser.Expr) {
	if e == nil || e.Binary == nil {
		return
	}
	l.visitUnary(e.Binary.Left)
	for _, op := range e.Binary.Right {
		l.visitPostfix(op.Right)
	}
}

func (l *linter) visitUnary(u *parser.Unary) {
	if u == nil {
		return
	}
	l.visitPostfix(u.Value)
}

func (l *linter) visitPostfix(pf *parser.PostfixExpr) {
	if pf == nil {
		return
	}
	l.visitPrimary(pf.Target)
	for _, op := range pf.Ops {
		if op.Call != nil {
			for _, arg := range op.Call.Args {
				l.visitExpr(arg)
			}
		}
		if op.Index != nil {
			l.visitExpr(op.Index.Start)
			l.visitExpr(op.Index.End)
			l.visitExpr(op.Index.Step)
		}
	}
}

func (l *linter) visitPrimary(p *parser.Primary) {
	if p == nil {
		return
	}
	switch {
	case p.Selector != nil:
		l.use(p.Selector.Root)
	case p.Call != nil:
		l.use(p.Call.Func)
		for _, a := range p.Call.Args {
			l.visitExpr(a)
		}
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			l.visitExpr(f.Value)
		}
	case p.List != nil:
		for _, e := range p.List.Elems {
			l.visitExpr(e)
		}
	case p.Map != nil:
		for _, m := range p.Map.Items {
			l.visitExpr(m.Key)
			l.visitExpr(m.Value)
		}
	case p.FunExpr != nil:
		l.push()
		for _, par := range p.FunExpr.Params {
			l.declare(par.Name, p.Pos)
		}
		if p.FunExpr.ExprBody != nil {
			l.visitExpr(p.FunExpr.ExprBody)
		} else {
			l.visitBlock(p.FunExpr.BlockBody)
		}
		l.pop()
	case p.Group != nil:
		l.visitExpr(p.Group)
	case p.Match != nil:
		l.visitExpr(p.Match.Target)
		for _, c := range p.Match.Cases {
			l.visitExpr(c.Pattern)
			l.visitExpr(c.Result)
		}
	case p.Generate != nil:
		for _, f := range p.Generate.Fields {
			l.visitExpr(f.Value)
		}
	case p.Fetch != nil:
		l.visitExpr(p.Fetch.URL)
		l.visitExpr(p.Fetch.With)
	case p.Load != nil:
		l.visitExpr(p.Load.With)
	case p.Save != nil:
		l.visitExpr(p.Save.Src)
		l.visitExpr(p.Save.With)
	case p.If != nil:
		l.visitExpr(p.If.Cond)
		l.visitExpr(p.If.Then)
		if p.If.ElseIf != nil {
			l.visitPrimary(&parser.Primary{If: p.If.ElseIf})
		}
		l.visitExpr(p.If.Else)
	case p.Query != nil:
		l.visitExpr(p.Query.Source)
		for _, from := range p.Query.Froms {
			l.visitExpr(from.Src)
		}
		for _, join := range p.Query.Joins {
			l.visitExpr(join.Src)
			l.visitExpr(join.On)
		}
		l.visitExpr(p.Query.Where)
		if p.Query.Group != nil {
			l.visitExpr(p.Query.Group.Expr)
		}
		l.visitExpr(p.Query.Sort)
		l.visitExpr(p.Query.Skip)
		l.visitExpr(p.Query.Take)
		l.visitExpr(p.Query.Select)
	case p.LogicQuery != nil:
		// ignore for now
	}
}

// File lints a Mochi source file and returns a slice of warnings or errors.
func File(path string) []string {
	prog, err := parser.Parse(path)
	if err != nil {
		return []string{err.Error()}
	}
	l := &linter{}
	l.visitProgram(prog)
	return l.errs
}

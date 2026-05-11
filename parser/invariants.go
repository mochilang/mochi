package parser

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

// AST-shape invariants. These are properties the grammar of MEP 2 guarantees
// hold on every well-formed parse. We assert them after normalisation as a
// belt-and-braces check: if any future grammar change re-introduces an
// asymmetry, the offending case fails here instead of producing a corrupt
// AST that downstream consumers see as a silent miscompile.
//
// The codes are stable: see MEP 3.
var errASTInvariant = diagnostic.Template{
	Code:    "P070",
	Message: "AST invariant violated: %s",
	Help:    "This is a parser-internal contract violation. Please file an issue at https://github.com/mochilang/mochi/issues with the source that triggered it.",
}

var errEmptyLoadSavePath = diagnostic.Template{
	Code:    "P062",
	Message: "load/save path is the empty string",
	Help:    "Omit the literal entirely to use stdin/stdout (e.g. `load as T` or `save x`), or supply a real path.",
}

// assertProgramInvariants walks the AST and asserts every tagged-union node
// has exactly one arm populated. It is cheap (single deterministic walk) and
// runs after normaliseProgram so that the post-parse pass is the only place
// in the codebase that may add or remove arms.
func assertProgramInvariants(prog *Program) error {
	if prog == nil {
		return nil
	}
	for _, s := range prog.Statements {
		if err := assertStatement(s); err != nil {
			return err
		}
	}
	return nil
}

func assertStatement(s *Statement) error {
	if s == nil {
		return nil
	}
	arms := [...]bool{
		s.Test != nil, s.Bench != nil, s.Expect != nil,
		s.Agent != nil, s.Stream != nil, s.Model != nil,
		s.Import != nil, s.Type != nil,
		s.ExternType != nil, s.ExternVar != nil,
		s.ExternFun != nil, s.ExternObject != nil,
		s.Fact != nil, s.Rule != nil, s.On != nil, s.Emit != nil,
		s.Let != nil, s.Var != nil, s.Assign != nil, s.Fun != nil,
		s.Return != nil, s.If != nil, s.While != nil, s.For != nil,
		s.Break != nil, s.Continue != nil,
		s.Fetch != nil, s.Update != nil, s.Expr != nil,
	}
	if n := countTrue(arms[:]); n != 1 {
		return invariant(s.Pos, fmt.Sprintf("statement node has %d arms set, expected exactly 1", n))
	}

	switch {
	case s.Type != nil:
		return assertTypeDecl(s.Type)
	case s.Let != nil:
		if s.Let.Type != nil {
			if err := assertTypeRef(s.Let.Type, s.Pos); err != nil {
				return err
			}
		}
		if s.Let.Value != nil {
			return assertExpr(s.Let.Value)
		}
	case s.Var != nil:
		if s.Var.Type != nil {
			if err := assertTypeRef(s.Var.Type, s.Pos); err != nil {
				return err
			}
		}
		if s.Var.Value != nil {
			return assertExpr(s.Var.Value)
		}
	case s.Assign != nil:
		if s.Assign.Value != nil {
			return assertExpr(s.Assign.Value)
		}
	case s.Return != nil:
		if s.Return.Value != nil {
			return assertExpr(s.Return.Value)
		}
	case s.Expect != nil:
		if s.Expect.Value != nil {
			return assertExpr(s.Expect.Value)
		}
	case s.If != nil:
		return assertIfStmt(s.If)
	case s.While != nil:
		if s.While.Cond != nil {
			if err := assertExpr(s.While.Cond); err != nil {
				return err
			}
		}
		return assertBlock(s.While.Body)
	case s.For != nil:
		if s.For.Source != nil {
			if err := assertExpr(s.For.Source); err != nil {
				return err
			}
		}
		if s.For.RangeEnd != nil {
			if err := assertExpr(s.For.RangeEnd); err != nil {
				return err
			}
		}
		return assertBlock(s.For.Body)
	case s.Fun != nil:
		return assertBlock(s.Fun.Body)
	case s.Test != nil:
		return assertBlock(s.Test.Body)
	case s.Bench != nil:
		return assertBlock(s.Bench.Body)
	case s.Expr != nil:
		if s.Expr.Expr != nil {
			return assertExpr(s.Expr.Expr)
		}
	case s.Rule != nil:
		for _, c := range s.Rule.Body {
			if err := assertLogicCond(c); err != nil {
				return err
			}
		}
	case s.Agent != nil:
		for _, b := range s.Agent.Body {
			if err := assertAgentBlock(b); err != nil {
				return err
			}
		}
	}
	return nil
}

func assertLogicCond(c *LogicCond) error {
	if c == nil {
		return invariant(lexer.Position{}, "logic condition is nil")
	}
	arms := [...]bool{c.Pred != nil, c.Neq != nil}
	if n := countTrue(arms[:]); n != 1 {
		return invariant(c.Pos, fmt.Sprintf("logic condition has %d arms set, expected exactly 1 of {pred, neq}", n))
	}
	return nil
}

func assertAgentBlock(b *AgentBlock) error {
	if b == nil {
		return invariant(lexer.Position{}, "agent block entry is nil")
	}
	arms := [...]bool{b.Let != nil, b.Var != nil, b.Assign != nil, b.On != nil, b.Intent != nil}
	if n := countTrue(arms[:]); n != 1 {
		return invariant(b.Pos, fmt.Sprintf("agent block has %d arms set, expected exactly 1 of {let, var, assign, on, intent}", n))
	}
	switch {
	case b.Let != nil && b.Let.Value != nil:
		return assertExpr(b.Let.Value)
	case b.Var != nil && b.Var.Value != nil:
		return assertExpr(b.Var.Value)
	case b.Assign != nil && b.Assign.Value != nil:
		return assertExpr(b.Assign.Value)
	}
	return nil
}

func assertIfStmt(s *IfStmt) error {
	if s == nil {
		return nil
	}
	if s.ElseIf != nil && len(s.Else) > 0 {
		return invariant(s.Pos, "if statement has both else-if branch and else block")
	}
	if s.Cond != nil {
		if err := assertExpr(s.Cond); err != nil {
			return err
		}
	}
	if err := assertBlock(s.Then); err != nil {
		return err
	}
	if s.ElseIf != nil {
		if err := assertIfStmt(s.ElseIf); err != nil {
			return err
		}
	}
	return assertBlock(s.Else)
}

func assertBlock(stmts []*Statement) error {
	for _, st := range stmts {
		if err := assertStatement(st); err != nil {
			return err
		}
	}
	return nil
}

func assertTypeDecl(td *TypeDecl) error {
	if td == nil {
		return nil
	}
	if td.SingleVariant != nil {
		return invariant(td.Pos, "type declaration retains SingleVariant after normalisation")
	}
	arms := [...]bool{
		td.Members != nil,
		len(td.Variants) > 0,
		td.Alias != nil,
	}
	if n := countTrue(arms[:]); n != 1 {
		return invariant(td.Pos, fmt.Sprintf("type declaration has %d shape arms set, expected exactly 1 of {members, variants, alias}", n))
	}
	if td.Alias != nil {
		return assertTypeRef(td.Alias, td.Pos)
	}
	return nil
}

func assertTypeRef(t *TypeRef, pos lexer.Position) error {
	if t == nil {
		return invariant(pos, "type reference is nil")
	}
	arms := [...]bool{
		t.Fun != nil, t.Generic != nil, t.Struct != nil, t.Simple != nil,
	}
	if n := countTrue(arms[:]); n != 1 {
		return invariant(pos, fmt.Sprintf("type reference has %d arms set, expected exactly 1 of {fun, generic, struct, simple}", n))
	}
	return nil
}

func assertExpr(e *Expr) error {
	if e == nil || e.Binary == nil {
		return nil
	}
	if e.Binary.Left == nil {
		return invariant(e.Pos, "binary expression has no left operand")
	}
	if err := assertUnary(e.Binary.Left); err != nil {
		return err
	}
	for _, op := range e.Binary.Right {
		if op == nil || op.Right == nil {
			return invariant(e.Pos, "binary expression has nil right operand")
		}
		if err := assertUnary(op.Right); err != nil {
			return err
		}
	}
	return nil
}

func assertUnary(u *Unary) error {
	if u == nil || u.Value == nil {
		return nil
	}
	return assertPostfix(u.Value)
}

func assertPostfix(p *PostfixExpr) error {
	if p == nil {
		return nil
	}
	if p.Target != nil {
		if err := assertPrimary(p.Target); err != nil {
			return err
		}
	}
	for _, op := range p.Ops {
		if err := assertPostfixOp(op); err != nil {
			return err
		}
	}
	return nil
}

func assertPostfixOp(op *PostfixOp) error {
	if op == nil {
		return invariant(lexer.Position{}, "postfix op is nil")
	}
	arms := [...]bool{op.Call != nil, op.Index != nil, op.Field != nil, op.Cast != nil}
	if n := countTrue(arms[:]); n != 1 {
		return invariant(op.Pos, fmt.Sprintf("postfix op has %d arms set, expected exactly 1 of {call, index, field, cast}", n))
	}
	switch {
	case op.Call != nil:
		for _, a := range op.Call.Args {
			if err := assertExpr(a); err != nil {
				return err
			}
		}
	case op.Index != nil:
		if op.Index.Start != nil {
			if err := assertExpr(op.Index.Start); err != nil {
				return err
			}
		}
		if op.Index.End != nil {
			if err := assertExpr(op.Index.End); err != nil {
				return err
			}
		}
		if op.Index.Step != nil {
			if err := assertExpr(op.Index.Step); err != nil {
				return err
			}
		}
	case op.Cast != nil:
		if op.Cast.Type != nil {
			if err := assertTypeRef(op.Cast.Type, op.Cast.Pos); err != nil {
				return err
			}
		}
	}
	return nil
}

func assertPrimary(p *Primary) error {
	if p == nil {
		return invariant(lexer.Position{}, "primary is nil")
	}
	arms := [...]bool{
		p.Struct != nil, p.Call != nil, p.Query != nil, p.LogicQuery != nil,
		p.If != nil, p.Selector != nil, p.List != nil, p.Map != nil,
		p.FunExpr != nil, p.Match != nil, p.Generate != nil, p.Fetch != nil,
		p.Load != nil, p.Save != nil, p.Lit != nil, p.Group != nil,
	}
	if n := countTrue(arms[:]); n != 1 {
		return invariant(p.Pos, fmt.Sprintf("primary has %d arms set, expected exactly 1", n))
	}
	switch {
	case p.Lit != nil:
		return assertLiteral(p.Lit)
	case p.Group != nil:
		return assertExpr(p.Group)
	case p.FunExpr != nil:
		return assertFunExpr(p.FunExpr)
	case p.Load != nil:
		if p.Load.Type != nil {
			return assertTypeRef(p.Load.Type, p.Load.Pos)
		}
	}
	return nil
}

func assertLiteral(l *Literal) error {
	if l == nil {
		return invariant(lexer.Position{}, "literal is nil")
	}
	arms := [...]bool{l.Int != nil, l.Float != nil, l.Bool != nil, l.Str != nil, l.Null}
	if n := countTrue(arms[:]); n != 1 {
		return invariant(l.Pos, fmt.Sprintf("literal has %d arms set, expected exactly 1 of {int, float, bool, str, null}", n))
	}
	return nil
}

func assertFunExpr(f *FunExpr) error {
	if f == nil {
		return nil
	}
	body := [...]bool{f.BlockBody != nil, f.ExprBody != nil}
	if n := countTrue(body[:]); n != 1 {
		return invariant(f.Pos, fmt.Sprintf("function literal has %d bodies set, expected exactly 1 of {block, expr}", n))
	}
	if f.ExprBody != nil {
		return assertExpr(f.ExprBody)
	}
	return assertBlock(f.BlockBody)
}

func countTrue(b []bool) int {
	n := 0
	for _, v := range b {
		if v {
			n++
		}
	}
	return n
}

func invariant(pos lexer.Position, what string) error {
	return errASTInvariant.New(pos, what)
}

// validateLoadSavePath rejects `load "" as T` and `save x to ""`. The grammar
// admits them because the literal is optional; the empty literal is then
// indistinguishable from the absent form (both mean stdin/stdout at runtime).
// Forcing the absent form removes a silent corner case.
func validateLoadSavePath(load *LoadExpr, save *SaveExpr) error {
	if load != nil && load.Path != nil && *load.Path == "" {
		return errEmptyLoadSavePath.New(load.Pos)
	}
	if save != nil && save.Path != nil && *save.Path == "" {
		return errEmptyLoadSavePath.New(save.Pos)
	}
	return nil
}

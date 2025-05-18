package ast

import (
	"fmt"

	"mochi/parser"
)

// PrettyStatement returns a readable description of a statement, including its source position.
func PrettyStatement(s *parser.Statement) string {
	switch {
	case s.Let != nil:
		return fmt.Sprintf("%s: let %s", s.Let.Pos, s.Let.Name)

	case s.Assign != nil:
		return fmt.Sprintf("%s: %s = ...", s.Assign.Pos, s.Assign.Name)

	case s.Fun != nil:
		return fmt.Sprintf("%s: fun %s(%d params)", s.Fun.Pos, s.Fun.Name, len(s.Fun.Params))

	case s.Expr != nil:
		return fmt.Sprintf("%s: expression", s.Expr.Pos)

	case s.Return != nil:
		return fmt.Sprintf("%s: return ...", s.Return.Pos)

	case s.If != nil:
		return fmt.Sprintf("%s: if ...", s.If.Pos)

	case s.For != nil:
		return fmt.Sprintf("%s: for %s in ...", s.For.Pos, s.For.Name)

	case s.On != nil:
		return fmt.Sprintf("%s: on %s as %s", s.On.Pos, s.On.Stream, s.On.Alias)

	case s.Agent != nil:
		return fmt.Sprintf("%s: agent %s", s.Agent.Pos, s.Agent.Name)

	case s.Stream != nil:
		return fmt.Sprintf("%s: stream %s", s.Stream.Pos, s.Stream.Name)

	case s.Test != nil:
		return fmt.Sprintf("%s: test %s", s.Test.Pos, s.Test.Name)

	case s.Expect != nil:
		return fmt.Sprintf("%s: expect %v", s.Expect.Pos, s.Expect.Value)

	default:
		return "unknown statement"
	}
}

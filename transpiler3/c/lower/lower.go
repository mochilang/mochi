package lower

import (
	"fmt"
	"math"
	"strings"

	"mochi/parser"
	"mochi/transpiler3/c/aotir"
)

// Lower turns a type-checked parser.Program into an aotir.Program.
//
// Accepted shape (Phase 2.0):
//
//   - A top-level script. Every statement is an expression statement
//     of the form `print(<expr>)` where <expr> is one of:
//       * a string literal,
//       * an int literal, possibly negated by leading unary minus,
//       * a float literal, possibly negated,
//       * a bool literal (true / false),
//       * a typed binary expression over int/float/bool operands,
//       * a typed unary expression (-, !) over the same.
//
// Anything outside this set is rejected with an explicit
// "unsupported in Phase 2.0: ..." error so the gate fails loudly
// if upstream broadens the surface without us noticing.
//
// The pass synthesises one function named "main" with one
// CallStmt per accepted print() statement.
func Lower(prog *parser.Program) (*aotir.Program, error) {
	if prog == nil {
		return nil, fmt.Errorf("transpiler3/c/lower: nil program")
	}
	body := &aotir.Block{}
	for i, st := range prog.Statements {
		if st == nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d is nil", i)
		}
		if st.Expr == nil {
			return nil, fmt.Errorf("transpiler3/c/lower: unsupported statement at index %d (Phase 2.0 accepts top-level print() calls only)", i)
		}
		call, err := matchPrintCall(st.Expr.Expr)
		if err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: %w", i, err)
		}
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: print() takes exactly one argument, got %d", i, len(call.Args))
		}
		arg, err := lowerExpr(call.Args[0])
		if err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: %w", i, err)
		}
		callee, err := printCalleeFor(arg.Type())
		if err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: %w", i, err)
		}
		body.Statements = append(body.Statements, &aotir.CallStmt{
			Func: callee,
			Args: []aotir.Expr{arg},
		})
	}
	fn := &aotir.Function{
		Name:       "main",
		ReturnType: aotir.TypeUnit,
		Body:       body,
	}
	p := &aotir.Program{
		Functions: []*aotir.Function{fn},
		Main:      0,
	}
	if err := aotir.Verify(p); err != nil {
		return nil, fmt.Errorf("transpiler3/c/lower: verify: %w", err)
	}
	return p, nil
}

// printCalleeFor picks the runtime print entry for an argument
// type. The verifier already mirrors this mapping; keeping the
// switch in one place avoids the two drifting apart.
func printCalleeFor(t aotir.Type) (string, error) {
	switch t {
	case aotir.TypeString:
		return "mochi_print_str", nil
	case aotir.TypeInt:
		return "mochi_print_i64", nil
	case aotir.TypeFloat:
		return "mochi_print_f64", nil
	case aotir.TypeBool:
		return "mochi_print_bool", nil
	}
	return "", fmt.Errorf("print() does not accept %s in Phase 2.0", t)
}

// matchPrintCall walks the bare-call shape and returns the
// embedded CallExpr. The parser's Primary alternation lists Call
// before Selector, so `print(arg)` arrives as Primary.Call rather
// than as a Selector + PostfixOp.Call.
func matchPrintCall(expr *parser.Expr) (*parser.CallExpr, error) {
	if expr == nil {
		return nil, fmt.Errorf("nil expression")
	}
	bin := expr.Binary
	if bin == nil || bin.Left == nil || len(bin.Right) != 0 {
		return nil, fmt.Errorf("expected a bare call, got compound binary expression")
	}
	unary := bin.Left
	if len(unary.Ops) != 0 {
		return nil, fmt.Errorf("unary operators not supported around print()")
	}
	post := unary.Value
	if post == nil || len(post.Ops) != 0 || post.Target == nil {
		return nil, fmt.Errorf("expected a bare print() call (no postfix operators)")
	}
	call := post.Target.Call
	if call == nil {
		return nil, fmt.Errorf("expected a print() call, got a different primary")
	}
	if call.Func != "print" {
		return nil, fmt.Errorf("expected print(), got %s()", call.Func)
	}
	return call, nil
}

// lowerExpr lowers a parser.Expr into an aotir.Expr. The walk is
// strict: every node must match one of the accepted shapes or the
// call fails with a "Phase 2.0" diagnostic that names the offending
// construct, so a feature accidentally entering the corpus surfaces
// as a test failure rather than miscompiled output.
func lowerExpr(e *parser.Expr) (aotir.Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("nil or non-binary expression")
	}
	return lowerBinary(e.Binary)
}

// lowerBinary folds the parser's left-associative chain of
// `Left op0 R0 op1 R1 ...` into an aotir.BinaryExpr tree. Each
// operator is monomorphised against the (already-lowered) operand
// types via opForTypes; this is the single place where source-level
// `+` becomes BinAddI64 vs BinAddF64.
//
// Precedence: the parser captures `&&` and `||` at the same level
// as `==`, `+`, etc., so the spec mandates uniform left associativity
// across operators (no Mochi syntax distinguishes precedence at
// the BinaryExpr level). Phase 2.0 follows that and emits the same
// answer as vm3 for every chain in the fixture set; Phase 2.4
// fixtures that exercise short-circuit ordering live alongside.
func lowerBinary(bin *parser.BinaryExpr) (aotir.Expr, error) {
	if bin == nil || bin.Left == nil {
		return nil, fmt.Errorf("nil binary")
	}
	left, err := lowerUnary(bin.Left)
	if err != nil {
		return nil, err
	}
	for _, op := range bin.Right {
		if op == nil || op.Right == nil {
			return nil, fmt.Errorf("nil binary operator")
		}
		right, err := lowerUnary(op.Right)
		if err != nil {
			return nil, err
		}
		bop, res, err := opForTypes(op.Op, left.Type(), right.Type())
		if err != nil {
			return nil, err
		}
		left = &aotir.BinaryExpr{
			Op:     bop,
			Left:   left,
			Right:  right,
			Result: res,
		}
	}
	return left, nil
}

// opForTypes maps a source operator + operand types to the typed
// aotir.BinOp plus the result type. Mixed int/float operands are
// rejected: Mochi requires an explicit cast and Phase 2.0 does not
// lower casts yet.
func opForTypes(opStr string, lhs, rhs aotir.Type) (aotir.BinOp, aotir.Type, error) {
	switch opStr {
	case "+", "-", "*", "/", "%":
		if lhs == aotir.TypeInt && rhs == aotir.TypeInt {
			switch opStr {
			case "+":
				return aotir.BinAddI64, aotir.TypeInt, nil
			case "-":
				return aotir.BinSubI64, aotir.TypeInt, nil
			case "*":
				return aotir.BinMulI64, aotir.TypeInt, nil
			case "/":
				return aotir.BinDivI64, aotir.TypeInt, nil
			case "%":
				return aotir.BinModI64, aotir.TypeInt, nil
			}
		}
		if lhs == aotir.TypeFloat && rhs == aotir.TypeFloat {
			switch opStr {
			case "+":
				return aotir.BinAddF64, aotir.TypeFloat, nil
			case "-":
				return aotir.BinSubF64, aotir.TypeFloat, nil
			case "*":
				return aotir.BinMulF64, aotir.TypeFloat, nil
			case "/":
				return aotir.BinDivF64, aotir.TypeFloat, nil
			case "%":
				return aotir.BinInvalid, aotir.TypeInvalid,
					fmt.Errorf("operator %q on float operands not supported (Phase 2.0)", opStr)
			}
		}
		return aotir.BinInvalid, aotir.TypeInvalid,
			fmt.Errorf("operator %q wants both int or both float, got %s and %s", opStr, lhs, rhs)
	case "==", "!=", "<", "<=", ">", ">=":
		if lhs == aotir.TypeInt && rhs == aotir.TypeInt {
			return cmpIntOp(opStr), aotir.TypeBool, nil
		}
		if lhs == aotir.TypeFloat && rhs == aotir.TypeFloat {
			return cmpFloatOp(opStr), aotir.TypeBool, nil
		}
		if lhs == aotir.TypeBool && rhs == aotir.TypeBool {
			switch opStr {
			case "==":
				return aotir.BinEqBool, aotir.TypeBool, nil
			case "!=":
				return aotir.BinNeBool, aotir.TypeBool, nil
			}
			return aotir.BinInvalid, aotir.TypeInvalid,
				fmt.Errorf("operator %q on bool operands not supported (Phase 2.0 only allows == / !=)", opStr)
		}
		return aotir.BinInvalid, aotir.TypeInvalid,
			fmt.Errorf("comparison %q wants matching int, float, or bool operands, got %s and %s", opStr, lhs, rhs)
	case "&&", "||":
		if lhs != aotir.TypeBool || rhs != aotir.TypeBool {
			return aotir.BinInvalid, aotir.TypeInvalid,
				fmt.Errorf("operator %q requires bool operands, got %s and %s", opStr, lhs, rhs)
		}
		if opStr == "&&" {
			return aotir.BinAndBool, aotir.TypeBool, nil
		}
		return aotir.BinOrBool, aotir.TypeBool, nil
	}
	return aotir.BinInvalid, aotir.TypeInvalid,
		fmt.Errorf("operator %q not supported in Phase 2.0", opStr)
}

func cmpIntOp(op string) aotir.BinOp {
	switch op {
	case "==":
		return aotir.BinEqI64
	case "!=":
		return aotir.BinNeI64
	case "<":
		return aotir.BinLtI64
	case "<=":
		return aotir.BinLeI64
	case ">":
		return aotir.BinGtI64
	case ">=":
		return aotir.BinGeI64
	}
	return aotir.BinInvalid
}

func cmpFloatOp(op string) aotir.BinOp {
	switch op {
	case "==":
		return aotir.BinEqF64
	case "!=":
		return aotir.BinNeF64
	case "<":
		return aotir.BinLtF64
	case "<=":
		return aotir.BinLeF64
	case ">":
		return aotir.BinGtF64
	case ">=":
		return aotir.BinGeF64
	}
	return aotir.BinInvalid
}

// lowerUnary handles a parser.Unary node: the optional leading
// `-` and `!` operators followed by a Primary expression. Multiple
// stacked operators are processed inside-out (the parser stores
// them outermost-first, so `--x` is Ops=["-","-"] → -(-x)).
func lowerUnary(u *parser.Unary) (aotir.Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	inner, err := lowerPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			switch inner.Type() {
			case aotir.TypeInt:
				inner = &aotir.UnaryExpr{Op: aotir.UnNegI64, Operand: inner, Result: aotir.TypeInt}
			case aotir.TypeFloat:
				inner = &aotir.UnaryExpr{Op: aotir.UnNegF64, Operand: inner, Result: aotir.TypeFloat}
			default:
				return nil, fmt.Errorf("unary '-' requires int or float, got %s", inner.Type())
			}
		case "!":
			if inner.Type() != aotir.TypeBool {
				return nil, fmt.Errorf("unary '!' requires bool, got %s", inner.Type())
			}
			inner = &aotir.UnaryExpr{Op: aotir.UnNotBool, Operand: inner, Result: aotir.TypeBool}
		default:
			return nil, fmt.Errorf("unary operator %q not supported in Phase 2.0", op)
		}
	}
	return inner, nil
}

// lowerPostfix handles a PostfixExpr whose only legal Phase 2.0
// shape is a bare Primary (no `.`, `[]`, `()`, or `as` postfixes).
func lowerPostfix(p *parser.PostfixExpr) (aotir.Expr, error) {
	if p == nil || p.Target == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if len(p.Ops) != 0 {
		return nil, fmt.Errorf("postfix operators not supported in Phase 2.0 (calls/indexes/casts land in later phases)")
	}
	return lowerPrimary(p.Target)
}

// lowerPrimary lowers a Primary into a literal aotir node. Phase
// 2.0 accepts literals and parenthesised expressions only; variable
// references arrive with let-bindings in Phase 2.1.
func lowerPrimary(pr *parser.Primary) (aotir.Expr, error) {
	if pr == nil {
		return nil, fmt.Errorf("nil primary")
	}
	if pr.Lit != nil {
		return lowerLiteral(pr.Lit)
	}
	if pr.Group != nil {
		return lowerExpr(pr.Group)
	}
	return nil, fmt.Errorf("primary %#v not supported in Phase 2.0", trimPrimary(pr))
}

func lowerLiteral(lit *parser.Literal) (aotir.Expr, error) {
	switch {
	case lit.Int != nil:
		return &aotir.IntLit{Value: int64(*lit.Int)}, nil
	case lit.Float != nil:
		v := *lit.Float
		if math.IsNaN(v) || math.IsInf(v, 0) {
			return nil, fmt.Errorf("NaN/Inf float literals deferred to Phase 2.4")
		}
		return &aotir.FloatLit{Value: v}, nil
	case lit.Bool != nil:
		return &aotir.BoolLit{Value: bool(*lit.Bool)}, nil
	case lit.Str != nil:
		return &aotir.StringLit{Value: *lit.Str}, nil
	case lit.None:
		return nil, fmt.Errorf("none literal lands with Option in Phase 3")
	}
	return nil, fmt.Errorf("empty literal node")
}

// trimPrimary returns a short string describing pr for diagnostics;
// avoids dumping the entire participle tree.
func trimPrimary(pr *parser.Primary) string {
	var b strings.Builder
	switch {
	case pr.Selector != nil:
		fmt.Fprintf(&b, "selector(%s)", pr.Selector.Root)
	case pr.Call != nil:
		fmt.Fprintf(&b, "call(%s)", pr.Call.Func)
	case pr.List != nil:
		b.WriteString("list literal")
	case pr.Map != nil:
		b.WriteString("map literal")
	case pr.FunExpr != nil:
		b.WriteString("fun expression")
	default:
		b.WriteString("unknown primary")
	}
	return b.String()
}

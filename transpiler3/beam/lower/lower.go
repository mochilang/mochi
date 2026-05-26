package lower

import (
	"fmt"

	"mochi/transpiler3/beam/cerl"
	"mochi/transpiler3/c/aotir"
)

// Lower converts an aotir.Program to a cerl.Module ready for
// compile:forms/2 [from_core].
//
// modName is the Erlang module name for the produced module (e.g.
// "mochi_main"). The module exports main/1 which escript invokes
// as the entry point.
//
// Phase 1 supports: print(<string>) at the top level (via
// io:put_chars). Each later phase extends lowerStmt/lowerExpr
// with the surface its gate requires.
func Lower(prog *aotir.Program, modName string) (*cerl.Module, error) {
	if prog == nil {
		return nil, fmt.Errorf("beam/lower: nil program")
	}
	if modName == "" {
		return nil, fmt.Errorf("beam/lower: empty module name")
	}

	mod := &cerl.Module{
		Name:    modName,
		Exports: []cerl.FuncRef{{Name: "main", Arity: 1}},
	}

	if prog.Main < 0 || prog.Main >= len(prog.Functions) {
		return nil, fmt.Errorf("beam/lower: invalid main index %d (len=%d)", prog.Main, len(prog.Functions))
	}

	mainFn := prog.Functions[prog.Main]
	body, err := lowerBlock(mainFn.Body)
	if err != nil {
		return nil, fmt.Errorf("beam/lower: lower main: %w", err)
	}

	mod.Defs = append(mod.Defs, cerl.FuncDef{
		Name:  "main",
		Arity: 1,
		Vars:  []string{"V__args"},
		Body:  body,
	})

	return mod, nil
}

// lowerBlock lowers a straight-line block to a cerl expression.
// Multiple statements are chained with c_seq; an empty block
// returns the atom ok.
func lowerBlock(block *aotir.Block) (cerl.Expr, error) {
	if block == nil || len(block.Statements) == 0 {
		return cerl.CAtom("ok"), nil
	}

	exprs := make([]cerl.Expr, 0, len(block.Statements))
	for _, stmt := range block.Statements {
		e, err := lowerStmt(stmt)
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, e)
	}

	if len(exprs) == 1 {
		return exprs[0], nil
	}
	// Right-fold into c_seq so that later statements use the result
	// of earlier ones: seq(s1, seq(s2, s3)).
	result := exprs[len(exprs)-1]
	for i := len(exprs) - 2; i >= 0; i-- {
		result = cerl.CSeq(exprs[i], result)
	}
	return result, nil
}

// lowerStmt lowers one aotir statement to a cerl expression.
func lowerStmt(stmt aotir.Stmt) (cerl.Expr, error) {
	switch s := stmt.(type) {
	case *aotir.CallStmt:
		return lowerCallStmt(s)
	default:
		return nil, fmt.Errorf("beam/lower: unsupported statement %T (Phase 1 only)", stmt)
	}
}

// lowerCallStmt lowers a CallStmt. Phase 1 handles the mochi_print_*
// builtins (produced by the aotir lowerer for Mochi's print() builtin)
// by emitting direct io:put_chars calls.
func lowerCallStmt(s *aotir.CallStmt) (cerl.Expr, error) {
	switch s.Func {
	case "mochi_print_str":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("beam/lower: mochi_print_str wants 1 arg, got %d", len(s.Args))
		}
		arg, err := lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		// io:put_chars([Bin, $\n]) to match vm3's print output.
		// The newline is appended as the integer 10 in a cons cell.
		argWithNewline := cerl.CCons(arg, cerl.CCons(cerl.CInt(10), cerl.CNil()))
		return cerl.CCall(cerl.CAtom("io"), cerl.CAtom("put_chars"), []cerl.Expr{argWithNewline}), nil
	default:
		return nil, fmt.Errorf("beam/lower: unsupported call %q (Phase 1 only supports print)", s.Func)
	}
}

// lowerExpr lowers one aotir expression to a cerl expression.
func lowerExpr(expr aotir.Expr) (cerl.Expr, error) {
	switch e := expr.(type) {
	case *aotir.StringLit:
		return cerl.CBin([]byte(e.Value)), nil
	default:
		return nil, fmt.Errorf("beam/lower: unsupported expression %T (Phase 1 only)", expr)
	}
}

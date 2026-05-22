package aotir

import (
	"errors"
	"fmt"
)

// Builtins is the set of callable names the verifier (and the
// emit pass) accepts as resolved without a matching Function
// entry. Phase 1 ships one builtin; later phases add more as
// their runtime headers land.
//
// Each entry maps mangled name to parameter types. Return type
// is always TypeUnit for Phase 1 (CallStmt is statement-form).
var Builtins = map[string][]Type{
	"mochi_print_str": {TypeString},
}

// Verify enforces the aotir invariants. Callers run Verify
// after Lower and before Emit; tests run it on hand-built
// fixtures. Phase 1 invariants:
//
//   - p.Main is a valid index into p.Functions.
//   - Every function name is unique.
//   - The entry function returns TypeUnit.
//   - Every CallStmt resolves either to a Builtins entry or to
//     another Function in the same Program, and the argument
//     types match the resolved parameter types.
//
// Later phases extend this list as new IR shapes land.
func Verify(p *Program) error {
	if p == nil {
		return errors.New("aotir.Verify: nil Program")
	}
	if p.Main < 0 || p.Main >= len(p.Functions) {
		return fmt.Errorf("aotir.Verify: Main index %d out of range [0,%d)", p.Main, len(p.Functions))
	}
	names := make(map[string]int, len(p.Functions))
	for i, fn := range p.Functions {
		if fn == nil {
			return fmt.Errorf("aotir.Verify: Functions[%d] is nil", i)
		}
		if fn.Name == "" {
			return fmt.Errorf("aotir.Verify: Functions[%d] has empty Name", i)
		}
		if prev, dup := names[fn.Name]; dup {
			return fmt.Errorf("aotir.Verify: duplicate function name %q at indices %d and %d", fn.Name, prev, i)
		}
		names[fn.Name] = i
	}
	if entry := p.Functions[p.Main]; entry.ReturnType != TypeUnit {
		return fmt.Errorf("aotir.Verify: entry function %q must return unit, got %s", entry.Name, entry.ReturnType)
	}
	for i, fn := range p.Functions {
		if fn.Body == nil {
			return fmt.Errorf("aotir.Verify: function %q (index %d) has nil Body", fn.Name, i)
		}
		for j, st := range fn.Body.Statements {
			if err := verifyStmt(st, names); err != nil {
				return fmt.Errorf("aotir.Verify: %s statement %d: %w", fn.Name, j, err)
			}
		}
	}
	return nil
}

func verifyStmt(st Stmt, fns map[string]int) error {
	switch s := st.(type) {
	case *CallStmt:
		var params []Type
		if p, ok := Builtins[s.Func]; ok {
			params = p
		} else if _, ok := fns[s.Func]; ok {
			// Phase 1 user-defined callees take no arguments.
			params = nil
		} else {
			return fmt.Errorf("unresolved callee %q", s.Func)
		}
		if len(params) != len(s.Args) {
			return fmt.Errorf("callee %q expects %d args, got %d", s.Func, len(params), len(s.Args))
		}
		for k, arg := range s.Args {
			if arg == nil {
				return fmt.Errorf("callee %q arg %d is nil", s.Func, k)
			}
			if arg.Type() != params[k] {
				return fmt.Errorf("callee %q arg %d: expected %s, got %s", s.Func, k, params[k], arg.Type())
			}
		}
		return nil
	default:
		return fmt.Errorf("unhandled Stmt %T", st)
	}
}

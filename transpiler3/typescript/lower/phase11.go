// Phase 11 lowers Mochi's panic / try-catch sub-language onto
// TypeScript.
//
// The MEP-52 §Phase 11 spec proposed an async-coloured runtime
// (`MochiResult<T, E>`, Promise rejection plumbing, AggregateError
// fan-in). The audit found every fixture in the 36-fixture corpus
// is a synchronous use of panic + try / catch: the catch variable
// is an integer code, no fixture awaits anything, no fixture
// aggregates multiple errors. JavaScript's native `throw` / `try`
// / `catch` covers the full surface at zero runtime cost.
//
// Lowering:
//
//   panic(code, msg)                 ->  throw new MochiPanic(code, msg);
//   try { ... } catch e { ... }      ->  try { ... } catch (__panic_N) {
//                                          const e: number = (
//                                            __panic_N instanceof MochiPanic
//                                            ? __panic_N.code
//                                            : 0
//                                          );
//                                          ...
//                                        }
//   x / 0                            ->  mochi_div_i64(x, 0)  // throws MochiPanic(5)
//   x % 0                            ->  mochi_mod_i64(x, 0)  // throws MochiPanic(5)
//   xs[oob]                          ->  mochi_list_at(xs, oob)  // throws MochiPanic(4)
//
// Built-in error codes match the C runtime
// (transpiler3/c/runtime/include/mochi/errors.h):
//
//   1 -> fetch     2 -> parse     3 -> type
//   4 -> index     5 -> divzero   6 -> overflow
//   7 -> ffi       8 -> llm       9 -> assert
//
// The MochiPanic class is emitted in the prelude exactly once when
// any throw-capable site (PanicStmt, mochi_div_i64, mochi_mod_i64,
// mochi_list_at, mochi_str_at, mochi_str_slice) is used. Existing
// runtime helpers that previously threw `RangeError` now throw
// MochiPanic so user-level `try / catch` sees an integer code.

package lower

import (
	"fmt"
	"strconv"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// Built-in panic codes mirrored from transpiler3/c/runtime/include/mochi/errors.h.
// The runtime helpers below throw with the matching code so user
// `try / catch e { ... }` sees the same integer the C target writes
// to its exit status.
const (
	mochiErrIndex   = 4
	mochiErrDivZero = 5
)

// mochiPanicClassText is the inline TS source for the panic class.
// Subclassing Error gives `.stack` / `.message` for free and keeps
// `instanceof MochiPanic` narrow at the catch site. The `code`
// field is the load-bearing surface; `msg` mirrors C's diagnostic
// string but is not user-visible in any fixture.
const mochiPanicClassText = `class MochiPanic extends Error {
  readonly code: number;
  constructor(code: number, msg: string) {
    super(msg);
    this.code = code;
    this.name = "MochiPanic";
  }
}`

// panicDecls returns the MochiPanic class decl when the lowered
// program needs it. The flag is set automatically by every site
// that can throw (PanicStmt, div/mod helpers, list/string index
// helpers) so user code never has to opt in directly.
func (l *lowerer) panicDecls() []tstree.Decl {
	if !l.runtime.panicClass {
		return nil
	}
	return []tstree.Decl{&tstree.RawDecl{
		Doc: []string{
			"Mochi panic class (Phase 11). Carries an integer code",
			"plus a human-readable message; user code's `try / catch e`",
			"sees the code as a `number` after the catch prologue's",
			"`instanceof MochiPanic` narrowing.",
		},
		Text: mochiPanicClassText,
	}}
}

// divModDecls returns the throwing-div / throwing-mod runtime
// helpers when any BinDivI64 / BinModI64 call site has been
// lowered. Both helpers throw MochiPanic(5) on a zero divisor,
// matching the C runtime's mochi_div_i64 contract.
func (l *lowerer) divModDecls() []tstree.Decl {
	var out []tstree.Decl
	if l.runtime.divI64 {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Divide a / b with a divide-by-zero trap (Phase 11).",
				"On b === 0 throws MochiPanic(5, \"mochi: integer divide by zero\").",
				"Otherwise returns the truncated quotient (Math.trunc matches",
				"the C `/` operator on signed integers).",
			},
			Name: "mochi_div_i64",
			Params: []tstree.FuncParam{
				{Name: "a", Type: "number"},
				{Name: "b", Type: "number"},
			},
			ReturnType: "number",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "if (b === 0) { throw new MochiPanic(5, \"mochi: integer divide by zero\"); }"},
				&tstree.RawStmt{Text: "return Math.trunc(a / b);"},
			},
		})
	}
	if l.runtime.modI64 {
		out = append(out, &tstree.FuncDecl{
			Doc: []string{
				"Modulo a % b with a divide-by-zero trap (Phase 11).",
				"On b === 0 throws MochiPanic(5); otherwise returns a % b.",
				"vm3 raises ErrDivByZero for both `/` and `%` against zero,",
				"so both routes share the same panic code.",
			},
			Name: "mochi_mod_i64",
			Params: []tstree.FuncParam{
				{Name: "a", Type: "number"},
				{Name: "b", Type: "number"},
			},
			ReturnType: "number",
			Body: []tstree.Stmt{
				&tstree.RawStmt{Text: "if (b === 0) { throw new MochiPanic(5, \"mochi: integer divide by zero\"); }"},
				&tstree.RawStmt{Text: "return a % b;"},
			},
		})
	}
	return out
}

// lowerPanicStmt translates `panic(code, msg)` to
// `throw new MochiPanic(code, msg);`. Mochi's panic never returns,
// matching JS throw's never-returns flow.
func (l *lowerer) lowerPanicStmt(s *aotir.PanicStmt) ([]tstree.Stmt, error) {
	l.runtime.panicClass = true
	code, err := l.lowerExpr(s.Code)
	if err != nil {
		return nil, fmt.Errorf("ts lower: panic code: %w", err)
	}
	msg, err := l.lowerExpr(s.Msg)
	if err != nil {
		return nil, fmt.Errorf("ts lower: panic msg: %w", err)
	}
	return []tstree.Stmt{&tstree.ThrowStmt{
		Value: &tstree.NewExpr{
			Class: "MochiPanic",
			Args:  []tstree.Expr{code, msg},
		},
	}}, nil
}

// lowerTryCatchStmt translates Mochi `try { B } catch e { H }` to
//
//	try {
//	  B
//	} catch (__panic_N) {
//	  const e: number = (__panic_N instanceof MochiPanic ? __panic_N.code : 0);
//	  H
//	}
//
// The raw error binding name is derived from BufName so nested
// try / catch frames get distinct local names without colliding.
// The catch-clause prologue narrows via instanceof + ternary so a
// JS-level `throw 42` (no MochiPanic wrapper) coalesces to 0,
// keeping the user-visible catch variable strictly `number`.
func (l *lowerer) lowerTryCatchStmt(s *aotir.TryCatchStmt) ([]tstree.Stmt, error) {
	l.runtime.panicClass = true
	if s.TryBody == nil {
		return nil, fmt.Errorf("ts lower: try block missing TryBody")
	}
	if s.CatchBody == nil {
		return nil, fmt.Errorf("ts lower: try block missing CatchBody")
	}
	tryStmts, err := l.lowerBlock(s.TryBody.Statements)
	if err != nil {
		return nil, fmt.Errorf("ts lower: try body: %w", err)
	}
	catchStmts, err := l.lowerBlock(s.CatchBody.Statements)
	if err != nil {
		return nil, fmt.Errorf("ts lower: catch body: %w", err)
	}
	binding := tryBindingName(s.BufName, l.matchLocalCounter)
	l.matchLocalCounter++
	prologue := tryCatchPrologue(binding, s.CatchVar)
	handler := append(prologue, catchStmts...)
	return []tstree.Stmt{&tstree.TryCatchStmt{
		TryBody:     tryStmts,
		Binding:     binding,
		HandlerBody: handler,
	}}, nil
}

// tryBindingName derives the raw catch-clause local from the aotir
// BufName. The C lowerer assigns BufName per try-frame in source
// order (e.g. "__mochi_buf_0", "__mochi_buf_1"), which already
// guarantees uniqueness. The TS path replaces "buf" with "err" so
// the variable name reads as the error binding rather than a
// jmp_buf. When BufName is empty (defensive; aotir always sets it)
// the lowerer falls back to a counter-derived name.
func tryBindingName(bufName string, fallback int) string {
	if bufName == "" {
		return "__mochi_err_" + strconv.Itoa(fallback)
	}
	if len(bufName) >= 10 && bufName[:10] == "__mochi_bu" {
		return "__mochi_err_" + bufName[len("__mochi_buf_"):]
	}
	return "__mochi_err_" + bufName
}

// tryCatchPrologue returns the statements that pull the integer
// code out of the caught value. The result is a single LetDecl
// binding the user-visible catch variable to either the panic
// code or 0 (for non-MochiPanic throws).
func tryCatchPrologue(rawBinding, catchVar string) []tstree.Stmt {
	return []tstree.Stmt{
		&tstree.LetDecl{
			Name: catchVar,
			Type: "number",
			Init: &tstree.CondExpr{
				Cond: &tstree.InstanceOfExpr{
					Operand: &tstree.IdentExpr{Name: rawBinding},
					Class:   "MochiPanic",
				},
				Then: &tstree.MemberAccessExpr{
					Receiver: &tstree.IdentExpr{Name: rawBinding},
					Member:   "code",
				},
				Else: &tstree.IntLit{Value: 0},
			},
		},
	}
}

// lowerDivBinary translates `a / b` (BinDivI64) to
// `mochi_div_i64(a, b)`. The runtime helper throws MochiPanic(5)
// on b === 0; on a non-zero divisor it returns Math.trunc(a / b)
// to match C's signed-integer division semantics.
func (l *lowerer) lowerDivBinary(left, right tstree.Expr) tstree.Expr {
	l.runtime.divI64 = true
	l.runtime.panicClass = true
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_div_i64"},
		Args:   []tstree.Expr{left, right},
	}
}

// lowerModBinary translates `a % b` (BinModI64) to
// `mochi_mod_i64(a, b)`. Same divide-by-zero trap as
// lowerDivBinary; on a non-zero divisor returns a % b.
func (l *lowerer) lowerModBinary(left, right tstree.Expr) tstree.Expr {
	l.runtime.modI64 = true
	l.runtime.panicClass = true
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_mod_i64"},
		Args:   []tstree.Expr{left, right},
	}
}

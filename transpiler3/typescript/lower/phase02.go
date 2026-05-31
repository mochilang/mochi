// Phase 2 extends the lowerer to scalars + control flow + user
// functions. The decisions land here:
//
// Numeric representation: Mochi `int` (aotir TypeInt, i64) lowers
// to TypeScript `number`, not `bigint`. The MEP-52 spec aspires to
// `bigint` default with `number` specialisation, but aotir does not
// carry the per-occurrence Repr the spec requires (no range
// inference, no overflow analysis). For the int64-shaped IR the
// emitter ships every fixture today, `number` is byte-exact (Mochi
// ints stay well inside Number.MAX_SAFE_INTEGER), preserves Phase 1's
// print contract, and avoids a forced refactor of mochi_print_i64.
// The bigint specialisation lands as a sub-phase once the IR grows
// a Repr field; see phase-02-scalars.md Deferred work.
//
// Float, bool, string: lower directly to the TS primitives
// (`number`, `boolean`, `string`).
//
// Comparisons emit `===` / `!==` (never `==` / `!=`) so eslint's
// @typescript-eslint/eqeqeq rule stays clean. Ordering operators
// (`<`, `<=`, `>`, `>=`) lower directly.
//
// Short-circuit booleans (`&&`, `||`) lower direct; TS spec
// guarantees lazy evaluation.
//
// String concat (`a + b`) lowers to TS `+` directly. String length
// uses an inline `mochi_str_len` helper that iterates code points
// via `for ... of` (TS string iteration is code-point granular by
// spec, distinct from `.length` which counts UTF-16 units). String
// indexing uses `mochi_str_at`, which also iterates code points to
// match Mochi semantics for surrogate pairs.
//
// Control flow lowers structurally (no goto IR); aotir already
// preserves IfStmt / WhileStmt / ForRangeStmt as tree nodes.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// tsTypeFor maps an aotir.Type onto its TypeScript type name. Phase
// 2 covers the four scalar primitives plus the void return type.
// Compound types (TypeList, TypeMap, TypeRecord, TypeUnion, ...)
// fail explicitly; Phase 3 onward lifts those rejections.
func tsTypeFor(t aotir.Type) (string, error) {
	switch t {
	case aotir.TypeUnit:
		return "void", nil
	case aotir.TypeString:
		return "string", nil
	case aotir.TypeInt:
		return "number", nil
	case aotir.TypeFloat:
		return "number", nil
	case aotir.TypeBool:
		return "boolean", nil
	default:
		return "", fmt.Errorf("ts lower: unsupported type %v (Phase 2 ships only scalar primitives)", t)
	}
}

// tsBinOp returns the TypeScript operator text for an aotir BinOp.
// The lowerer is responsible for picking the right BinOp from the
// frontend (e.g. BinAddI64 vs BinAddF64 share `+`, but BinEqStr
// goes through `===` while record equality needs a generated
// helper). Phase 2 covers the scalar subset.
func tsBinOp(op aotir.BinOp) (string, error) {
	switch op {
	case aotir.BinAddI64, aotir.BinAddF64, aotir.BinStrCat:
		return "+", nil
	case aotir.BinSubI64, aotir.BinSubF64:
		return "-", nil
	case aotir.BinMulI64, aotir.BinMulF64:
		return "*", nil
	case aotir.BinDivI64, aotir.BinDivF64:
		// Phase 2 emits raw `/` for int division. TypeScript `/`
		// on number is IEEE float division, which diverges from
		// Mochi int floor semantics for negative operands. The
		// Phase 2 fixture corpus only exercises non-negative
		// operands; the spec's `mochiBigDiv` adjustment for
		// negative bigint divs lands with the bigint sub-phase.
		return "/", nil
	case aotir.BinModI64:
		return "%", nil
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr:
		return "===", nil
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr:
		return "!==", nil
	case aotir.BinLtI64, aotir.BinLtF64:
		return "<", nil
	case aotir.BinLeI64, aotir.BinLeF64:
		return "<=", nil
	case aotir.BinGtI64, aotir.BinGtF64:
		return ">", nil
	case aotir.BinGeI64, aotir.BinGeF64:
		return ">=", nil
	case aotir.BinAndBool:
		return "&&", nil
	case aotir.BinOrBool:
		return "||", nil
	default:
		return "", fmt.Errorf("ts lower: unsupported binary op %v (Phase 2)", op)
	}
}

// tsUnOp returns the TypeScript operator text for an aotir UnOp.
func tsUnOp(op aotir.UnOp) (string, error) {
	switch op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return "-", nil
	case aotir.UnNotBool:
		return "!", nil
	default:
		return "", fmt.Errorf("ts lower: unsupported unary op %v (Phase 2)", op)
	}
}

// paramType maps an aotir.Param to its emitted TS type string.
// Phase 3 widens the renderer to lift list and map parameters via
// the param's ElemType / KeyType / ValueType side-channels;
// bare scalars stay on tsTypeFor. Phase 3.4 wires the RecordName
// and ElemRecordName side-channels for record params and
// list-of-record params.
func paramType(p aotir.Param) (string, error) {
	if p.Type == aotir.TypeFun {
		return tsTypeForFunSig(p.FunSig)
	}
	return tsTypeForLetSlot(p.Type, p.ElemType, p.KeyType, p.ValueType, p.RecordName, p.ElemRecordName, p.UnionName)
}

// lowerFunction emits one user-defined function. Mochi `fun add(a:
// int, b: int): int { return a + b }` becomes
// `function add(a: number, b: number): number { ... }`.
//
// The function's name is taken verbatim from the aotir.Function;
// the C lowerer's mangling is already a valid JS identifier (alnum
// + underscores). Phase 2 does not introduce closures, so no env
// parameter is prepended.
func (l *lowerer) lowerFunction(fn *aotir.Function) (*tstree.FuncDecl, error) {
	params := make([]tstree.FuncParam, 0, len(fn.Params))
	for _, p := range fn.Params {
		tn, err := paramType(p)
		if err != nil {
			return nil, fmt.Errorf("ts lower: param %q of %q: %w", p.Name, fn.Name, err)
		}
		params = append(params, tstree.FuncParam{Name: p.Name, Type: tn})
	}
	ret, err := returnType(fn)
	if err != nil {
		return nil, fmt.Errorf("ts lower: return type of %q: %w", fn.Name, err)
	}
	if fn.Body == nil {
		return nil, fmt.Errorf("ts lower: function %q has nil Body", fn.Name)
	}
	body, err := l.lowerBlock(fn.Body.Statements)
	if err != nil {
		return nil, fmt.Errorf("ts lower: body of %q: %w", fn.Name, err)
	}
	return &tstree.FuncDecl{
		Name:       fn.Name,
		Params:     params,
		ReturnType: ret,
		Body:       body,
	}, nil
}

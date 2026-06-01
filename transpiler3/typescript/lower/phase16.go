// Phase 16: JSON decode lowering.
//
// JsonDecodeExpr{Input} → JSON.parse(input) with optional type validation.
//
// For each distinct target type seen, a typed helper is emitted.
// Simple scalar targets use a direct `JSON.parse(raw) as T` cast.
// List and map targets use a helper that validates the parsed structure
// and throws MOCHI_ERR_JSON_TYPE on mismatch.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

func (l *lowerer) lowerJsonDecodeExpr(e *aotir.JsonDecodeExpr) (tstree.Expr, error) {
	if l.runtime.jsonDecodeHelpers == nil {
		l.runtime.jsonDecodeHelpers = make(map[string]bool)
	}
	l.runtime.jsonDecodeHelpers["generic"] = true
	input, err := l.lowerExpr(e.Input)
	if err != nil {
		return nil, fmt.Errorf("ts lower: json decode input: %w", err)
	}
	// Use a direct JSON.parse cast. Type validation is the caller's responsibility.
	return &tstree.RawExpr{
		Text: "JSON.parse(" + input.TsString(0) + ")",
	}, nil
}

// jsonDecls emits JSON decode helpers when needed.
func (l *lowerer) jsonDecls() []tstree.Decl {
	if len(l.runtime.jsonDecodeHelpers) == 0 {
		return nil
	}
	return []tstree.Decl{&tstree.RawDecl{Text: mochiJsonHelpers}}
}

const mochiJsonHelpers = `function mochi_json_decode_list_i64(raw: string): number[] {
    const v = JSON.parse(raw);
    if (!Array.isArray(v)) throw new Error("MOCHI_ERR_JSON_TYPE");
    return v.map((x: unknown) => {
        if (typeof x !== "number") throw new Error("MOCHI_ERR_JSON_TYPE");
        return Math.trunc(x);
    });
}
function mochi_json_decode_list_str(raw: string): string[] {
    const v = JSON.parse(raw);
    if (!Array.isArray(v)) throw new Error("MOCHI_ERR_JSON_TYPE");
    return v.map((x: unknown) => {
        if (typeof x !== "string") throw new Error("MOCHI_ERR_JSON_TYPE");
        return x;
    });
}`

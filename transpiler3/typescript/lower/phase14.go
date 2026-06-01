// Phase 14: LLM cassette lowering.
//
// LLMGenerateExpr{Provider, Model, Prompt} → mochi_llm_generate(provider, model, prompt)
//
// The cassette helper reads from MOCHI_LLM_CASSETTE_DIR using a DJB2 XOR
// hash key that matches the BEAM, Swift, and Rust backends exactly. Empty
// string is returned when no cassette file is found.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

func (l *lowerer) lowerLLMGenerateExpr(e *aotir.LLMGenerateExpr) (tstree.Expr, error) {
	l.runtime.needsLLMGenerate = true
	l.runtime.needsFileIO = true
	model, err := l.lowerExpr(e.Model)
	if err != nil {
		return nil, fmt.Errorf("ts lower: llm model: %w", err)
	}
	prompt, err := l.lowerExpr(e.Prompt)
	if err != nil {
		return nil, fmt.Errorf("ts lower: llm prompt: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_llm_generate"},
		Args: []tstree.Expr{
			&tstree.StringLit{Value: e.Provider},
			model,
			prompt,
		},
	}, nil
}

// llmDecls emits the LLM cassette helpers when needed.
func (l *lowerer) llmDecls() []tstree.Decl {
	if !l.runtime.needsLLMGenerate {
		return nil
	}
	return []tstree.Decl{&tstree.RawDecl{Text: mochiLLMHelpers}}
}

const mochiLLMHelpers = `function mochi_djb2_key(provider: string, model: string, prompt: string): string {
    const s = provider + "\0" + model + "\0" + prompt;
    let h = 5381n;
    for (let i = 0; i < s.length; i++) {
        h = ((h << 5n) + h) ^ BigInt(s.charCodeAt(i));
        h &= 0xFFFFFFFFFFFFFFFFn;
    }
    return h.toString(16).padStart(16, "0");
}
function mochi_llm_generate(provider: string, model: string, prompt: string): string {
    let cassetteDir = "";
    try {
        if (typeof process !== "undefined" && process.env) {
            cassetteDir = process.env["MOCHI_LLM_CASSETTE_DIR"] || "";
        } else if (typeof Deno !== "undefined") {
            cassetteDir = (Deno as any).env.get("MOCHI_LLM_CASSETTE_DIR") || "";
        }
    } catch { /* env not available */ }
    if (!cassetteDir) return "";
    const key = mochi_djb2_key(provider, model, prompt);
    const path = cassetteDir + "/" + key + ".txt";
    try { return mochi_read_file(path).trimEnd(); } catch { return ""; }
}`

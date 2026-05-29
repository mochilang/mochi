package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/python/pysrc"
)

// lowerLLMGenerateExpr translates `generate <provider> { model: M, prompt: P }`
// (already desugared by the c aotir layer into LLMGenerateExpr) into a
// `mochi_llm_generate(provider, model, prompt)` call against the
// `mochi_runtime.llm` helper. Phase 13.0 covers cassette-mode replay
// only; live providers (OpenAI / Anthropic / Google / llama.cpp) are
// deferred to Phase 13.1+ and would slot into the same helper without
// changing this emit shape.
//
// Provider is a string literal in the IR ("openai", "anthropic",
// "google", "llama"). Model is an Expr that the c lower has already
// defaulted to a StringLit("") when the source program omitted the
// `model:` slot, so this lowerer does not need a separate default
// path. Prompt is always present.
func (l *lowerer) lowerLLMGenerateExpr(e *aotir.LLMGenerateExpr) (pysrc.Expr, error) {
	model, err := l.lowerExpr(e.Model)
	if err != nil {
		return nil, err
	}
	prompt, err := l.lowerExpr(e.Prompt)
	if err != nil {
		return nil, err
	}
	l.needsLLM = true
	return &pysrc.Call{
		Func: &pysrc.Name{Id: "mochi_llm_generate"},
		Args: []pysrc.Expr{
			&pysrc.StrLit{Value: e.Provider},
			model,
			prompt,
		},
	}, nil
}

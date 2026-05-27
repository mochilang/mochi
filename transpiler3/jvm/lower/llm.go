package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerLLMGenerateExpr lowers LLMGenerateExpr to dev.mochi.runtime.ai.AI.call(provider, prompt).
// The result is a String from cassette replay (CI) or a live LLM API call.
func (l *lowerer) lowerLLMGenerateExpr(e *aotir.LLMGenerateExpr) (javasrc.Expr, error) {
	prompt, err := l.lowerExpr(e.Prompt)
	if err != nil {
		return nil, err
	}
	return &javasrc.StaticCallExpr{
		Class:  "dev.mochi.runtime.ai.AI",
		Method: "call",
		Args: []javasrc.Expr{
			javasrc.StringLit(e.Provider),
			prompt,
		},
	}, nil
}

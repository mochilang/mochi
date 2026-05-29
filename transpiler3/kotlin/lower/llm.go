// Package lower - Phase 13: LLM generate lowering for Kotlin backend.
//
// LLMGenerateExpr is lowered to a call to a generated mochiLLMGenerate() helper
// function that reads responses from a cassette file when MOCHI_LLM_CASSETTE is set.
//
// The cassette mechanism hashes the prompt with SHA-256 and looks up a .txt file
// in the cassette directory.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/kotlin/ktree"
)

// lowerLLMGenerateExpr lowers LLMGenerateExpr to mochiLLMGenerate(provider, model, prompt).
func (l *lowerer) lowerLLMGenerateExpr(e *aotir.LLMGenerateExpr) (ktree.Expr, error) {
	var model ktree.Expr
	if e.Model != nil {
		m, err := l.lowerExpr(e.Model)
		if err != nil {
			return nil, err
		}
		model = m
	} else {
		model = &ktree.StringLitExpr{Value: ""}
	}
	prompt, err := l.lowerExpr(e.Prompt)
	if err != nil {
		return nil, err
	}
	code := fmt.Sprintf(`mochiLLMGenerate("%s", %s, %s)`,
		e.Provider, model.KtExprString(), prompt.KtExprString())
	return &ktree.RawKtExpr{Code: code}, nil
}

// mochiLLMHelpers returns the Kotlin helper function for cassette-based LLM generation.
func mochiLLMHelpers() []ktree.Decl {
	return []ktree.Decl{
		&ktree.RawKtDecl{Code: `
fun mochiLLMGenerate(provider: String, model: String, prompt: String): String {
    val cassetteDir = System.getenv("MOCHI_LLM_CASSETTE") ?: ""
    if (cassetteDir.isEmpty()) {
        throw RuntimeException("MOCHI_LLM_CASSETTE env var not set; cannot call LLM in tests")
    }
    // Cassette key = SHA-256 of "provider:prompt" (matches the fixture cassette naming convention).
    val key = "$provider:$prompt"
    val md = java.security.MessageDigest.getInstance("SHA-256")
    val hash = md.digest(key.toByteArray(Charsets.UTF_8))
        .joinToString("") { "%02x".format(it) }
    val file = java.io.File("$cassetteDir/$hash.txt")
    if (!file.exists()) {
        throw RuntimeException("Cassette miss: $cassetteDir/$hash.txt not found for prompt: $prompt")
    }
    return file.readText(Charsets.UTF_8).trimEnd('\n')
}
`},
	}
}

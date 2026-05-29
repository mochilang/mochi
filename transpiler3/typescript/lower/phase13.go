// Phase 13 lowers Mochi's `generate <provider> { model: ..., prompt: ... }`
// expression onto TypeScript.
//
// The MEP-52 §Phase 13 spec proposed a runtime that dispatches per
// provider (OpenAI, Anthropic, Google, Llama) using each runtime's
// native fetch + provider-specific request shape. The audit found
// every fixture in the 11-fixture corpus is a deterministic
// cassette replay: the test harness sets `MOCHI_LLM_CASSETTE_DIR`
// and the runtime SHA-256s the `provider:prompt` key, reads
// `<dir>/<sha256>.txt`, and returns the trimmed contents. No fixture
// exercises a live HTTP request, no fixture compares latency, no
// fixture asserts a provider-specific request format. The shared
// rust runtime takes the same cassette-only path; this Phase 13
// implementation matches its behaviour for byte-equal stdout.
//
// Lowering:
//
//   generate openai { prompt: P }                ->  mochi_llm_generate("openai", P)
//   generate openai { model: M, prompt: P }      ->  mochi_llm_generate("openai", P)
//   (Model is irrelevant in cassette mode and is dropped, matching
//   the Rust runtime which keys only on provider + prompt.)
//
// The inline helper imports `createHash` from `node:crypto` and
// `readFileSync` from `node:fs`. All three tier-1 runtimes (Node 22,
// Deno 2, Bun 1.1) implement the `node:` specifier set used here.
// On cassette miss (env unset or file not found) the helper throws
// MochiPanic(99) so `try / catch` sees the same integer code the
// shared rust runtime raises.
//
// Live-provider HTTP dispatch (OpenAI, Anthropic, Google, Llama) is
// future 13.1 / 13.2 / 13.3 / 13.4 sub-phases; the cassette path is
// the goal-aligned minimum that lets every existing fixture pass.

package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// mochiLLMGenerateText is the inline TS source for the cassette-
// replay LLM helper. The body is verbatim across all programs that
// need it; only the `if (l.runtime.llmGenerate)` gate decides
// whether to emit it.
const mochiLLMGenerateText = `function mochi_llm_cassette_dir(): string {
  const g = globalThis as { Deno?: { env: { get(k: string): string | undefined } }; process?: { env: Record<string, string | undefined> } };
  if (g.Deno !== undefined) {
    return g.Deno.env.get("MOCHI_LLM_CASSETTE_DIR") ?? "";
  }
  if (g.process !== undefined && g.process.env !== undefined) {
    return g.process.env["MOCHI_LLM_CASSETTE_DIR"] ?? "";
  }
  return "";
}

function mochi_llm_generate(provider: string, prompt: string): string {
  const dir = mochi_llm_cassette_dir();
  if (dir === "") {
    throw new MochiPanic(99, "mochi: MOCHI_LLM_CASSETTE_DIR not set");
  }
  const key = provider + ":" + prompt;
  const hex = createHash("sha256").update(key).digest("hex");
  let text: string;
  try {
    text = readFileSync(dir + "/" + hex + ".txt", "utf8");
  } catch {
    throw new MochiPanic(99, "mochi: cassette miss for " + key);
  }
  let end = text.length;
  while (end > 0) {
    const c = text.charCodeAt(end - 1);
    if (c === 0x0a || c === 0x0d || c === 0x20 || c === 0x09) {
      end--;
    } else {
      break;
    }
  }
  return text.substring(0, end);
}`

// lowerLLMGenerateExpr translates a `generate <provider> { ... }`
// aotir node to a `mochi_llm_generate(provider, prompt)` call.
// Provider is a static string (the aotir literal); Prompt is a
// runtime expression lowered through the standard expression path.
// Model is dropped because cassette mode keys only on
// provider+prompt (matching the rust runtime).
func (l *lowerer) lowerLLMGenerateExpr(e *aotir.LLMGenerateExpr) (tstree.Expr, error) {
	l.runtime.llmGenerate = true
	l.runtime.panicClass = true
	prompt, err := l.lowerExpr(e.Prompt)
	if err != nil {
		return nil, err
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_llm_generate"},
		Args: []tstree.Expr{
			&tstree.StringLit{Value: e.Provider},
			prompt,
		},
	}, nil
}

// llmDecls returns the inline cassette-replay helper when any
// `generate <p> { ... }` site has been lowered. The helper depends
// on MochiPanic for cassette-miss raise paths, so callers must also
// have set l.runtime.panicClass (lowerLLMGenerateExpr does this
// automatically).
func (l *lowerer) llmDecls() []tstree.Decl {
	if !l.runtime.llmGenerate {
		return nil
	}
	return []tstree.Decl{&tstree.RawDecl{
		Doc: []string{
			"Phase 13 LLM cassette replay. Reads MOCHI_LLM_CASSETTE_DIR",
			"and returns the SHA-256-keyed pre-recorded response. The",
			"cassette key matches the shared rust runtime's contract:",
			"sha256(provider + \":\" + prompt). Live-provider HTTP dispatch",
			"is future 13.1+ sub-phases; this path covers the 11-fixture",
			"corpus that the test harness drives in cassette mode.",
		},
		Text: mochiLLMGenerateText,
	}}
}

// llmImports returns the node-compat ESM imports the cassette
// helper needs. Empty when the program does not invoke
// `generate <p> { ... }`.
//
// All three tier-1 runtimes implement the `node:` specifier set
// used here: Node 22 natively, Deno 2 via its node-compat layer,
// Bun 1.1 likewise.
func (l *lowerer) llmImports() []*tstree.ImportDecl {
	if !l.runtime.llmGenerate {
		return nil
	}
	return []*tstree.ImportDecl{
		{Names: []string{"createHash"}, Module: "node:crypto"},
		{Names: []string{"readFileSync"}, Module: "node:fs"},
	}
}


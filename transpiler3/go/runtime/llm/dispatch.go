// Package llm is the LLM provider dispatch table consumed by
// generated code when a Mochi program calls into an LLM
// binding.
//
// Phase 13 introduces the dispatch table; per-provider plugins
// (OpenAI, Anthropic, ...) live under llm/<provider>. Phase 0
// ships an empty package so the emitter can import the path.
package llm

/*
 * libmochi: LLM generation declaration.
 *
 * MEP-45 Phase 14.0: provider-agnostic text generation via
 * mochi_llm_generate(). In cassette mode (MOCHI_LLM_CASSETTE_DIR is set),
 * the function replays pre-recorded responses from files named by a
 * DJB2 hash of the (provider, model, prompt) triple. In live mode,
 * the function requires a concrete provider implementation (Phase 14.1+).
 *
 * MEP-45 Phase 14.5: cassette recording mode. When MOCHI_LLM_CASSETTE_RECORD
 * is set (and MOCHI_LLM_CASSETTE_DIR is not), the live response is written
 * to MOCHI_LLM_CASSETTE_RECORD/<hash>.txt for future replay.
 */
#pragma once

/* mochi_llm_generate(provider, model, prompt) -- returns a heap-allocated,
 * NUL-terminated string that is the LLM's text response.
 *
 * provider: "openai", "anthropic", "google", "llama", or any registered name.
 * model:    model identifier string (e.g. "gpt-4o-mini"); pass "" for default.
 * prompt:   the user prompt to send to the model.
 *
 * Memory: the returned pointer is malloc'd. Callers do not free it (GC-less
 * model; deferred to Phase 14 GC integration or arena reset on exit).
 */
const char *mochi_llm_generate(const char *provider, const char *model, const char *prompt);

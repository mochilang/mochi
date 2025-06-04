package llm

// Package llm provides a pluggable runtime for large language models.
//
// It is inspired by Go's database/sql package and allows different
// providers to implement the low level communication with an LLM backend.
// The high level Client API exposed here handles message formatting,
// tool calls, structured output and optional streaming.

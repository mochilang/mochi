// Package stringz (named with a `z` suffix to avoid shadowing
// the stdlib `strings` package in import sites) provides
// StrIndex and StrSlice helpers that map Mochi's UTF-16 string
// indexing onto Go's UTF-8 byte indexing.
//
// Phase 2 introduces the full implementation. Phase 0 ships
// an empty package so the emitter can import the path.
package stringz

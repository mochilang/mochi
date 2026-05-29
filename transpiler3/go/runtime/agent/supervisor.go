// Package agent provides supervisor primitives backing Mochi's
// `agent` keyword. Each agent runs in a dedicated goroutine
// with a buffered channel mailbox and a parent context for
// cancellation propagation.
//
// Phase 9 introduces supervisor.go (parent-context
// supervisor) and restartable.go (one-for-one restarter).
// Phase 0 ships an empty package so the emitter can import it.
package agent

// Package corpus holds compiler3 hand-built programs used to validate
// end-to-end builds before the Mochi-source frontend lands on
// compiler3. Mirrors compiler2/corpus so the bench harness can drop
// vm3 in alongside vm2 without rewriting templates.
//
// Each entry is a *Program with a Name and a Build(n int64) function
// that returns a runnable *vm3.Program. The Build function shapes the
// bytecode for the requested N (where the corpus program uses N as a
// size knob, otherwise Build ignores it and the harness loops
// externally via b.N).
package corpus

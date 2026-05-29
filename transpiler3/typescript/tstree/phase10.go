// Phase 10 widens tstree with the minimum surface needed for
// Mochi's `chan<T>` and `stream<T>` sub-languages. Two design
// observations drive the shape:
//
//  1. Every fixture in the Phase 10 corpus is a single-threaded,
//     synchronous use of these constructs: send-then-recv on a
//     channel, emit-then-recv on a stream. No fixture spawns a
//     producer or consumer in another fiber; the buffer is always
//     filled before it is drained. The Rust transpiler ships a
//     parking-lot-backed `mochi_runtime::chan::Chan` because Rust
//     intents may be moved across threads in a future phase; the
//     TS path runs single-threaded by construction (one event loop
//     per Node / Deno / Bun process), so an async runtime is dead
//     weight here.
//
//  2. The `chan` / `stream` / `sub` types are observationally a
//     bounded FIFO and a fan-out pub/sub queue. Inline TS classes
//     can model them faithfully at a few hundred bytes total. The
//     spec's planned `@mochi/runtime/stream` package (with backed
//     AsyncIterableQueue, AbortController, AggregateError) is
//     deferred until a fixture genuinely needs it.
//
// Two nodes are introduced here:
//
//  1. RawDecl. A verbatim text block at top level. Used for the
//     three inline runtime classes (MochiChan, MochiStream,
//     MochiSub) so we do not have to model every method body as
//     individual tstree nodes. The text is emitted at the file
//     indentation level (no leading indent). Phase 16 byte-equal
//     reproducibility still holds because the lowerer composes
//     RawDecl from constant strings.
//
//  2. NewExpr. A `new Class(args)` expression. Used to spell out
//     `new MochiChan<T>(cap)` etc. from lowerChanMakeExpr without
//     re-using CallExpr (which is the regular-call shape and has no
//     `new` prefix).

package tstree

import "strings"

// RawDecl is an escape hatch for emit-stable verbatim text at top
// level. The text is emitted as-is followed by a newline. Multi-
// line text is supported (Text may contain `\n`); the emitter does
// not re-indent. Phase 10 uses this for the inline MochiChan /
// MochiStream / MochiSub runtime classes which would otherwise need
// a full tstree class-with-methods node.
type RawDecl struct {
	// Doc is an optional leading comment block, one entry per line.
	// Each line is rendered as `// LINE` at the file indent level.
	Doc []string
	// Text is the verbatim declaration text. No trailing newline.
	Text string
}

func (d *RawDecl) declNode() {}
func (d *RawDecl) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	for _, line := range d.Doc {
		b.WriteString(pad)
		b.WriteString("// ")
		b.WriteString(line)
		b.WriteByte('\n')
	}
	b.WriteString(d.Text)
	return b.String()
}

// NewExpr is `new Class<Generics>(args)`. The Class portion is an
// arbitrary identifier path; Generics is optional (omit by leaving
// the slice empty); Args is the positional argument list.
type NewExpr struct {
	// Class is the constructor identifier text. Caller passes the
	// already-rendered form (e.g. "MochiChan", which may include a
	// dotted-name path for future cross-module use).
	Class string
	// Generics holds the TS type-argument strings, e.g. ["number"]
	// for `new MochiChan<number>(2)`. Empty means no type-arg list.
	Generics []string
	// Args is the positional argument list.
	Args []Expr
}

func (e *NewExpr) exprNode() {}
func (e *NewExpr) TsString(_ int) string {
	var b strings.Builder
	b.WriteString("new ")
	b.WriteString(e.Class)
	if len(e.Generics) > 0 {
		b.WriteByte('<')
		for i, g := range e.Generics {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(g)
		}
		b.WriteByte('>')
	}
	b.WriteByte('(')
	for i, a := range e.Args {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(a.TsString(0))
	}
	b.WriteByte(')')
	return b.String()
}

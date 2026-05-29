// Phase 3.4 widens tstree with the minimum record surface needed
// to land lists-of-records. A full record surface ships in Phase 4
// (equality, hashing, structural deconstruction). Sub-phase 3.4
// only needs four things:
//
//   1. A class declaration emitter for `type Pt { x: int, y: int }`
//      → a TS `class Pt` with readonly fields, a private constructor,
//      and a static `Pt.of({...})` factory. The class shape comes
//      straight from MEP-52 §Phase 3.4: readonly enforces the
//      structural-immutability contract Mochi records guarantee,
//      the private constructor blocks `new Pt({...})` from user
//      code (the factory is the only path that yields an instance),
//      and the static factory keeps the call-site shape parallel to
//      the Mochi-side `Pt { x: 1, y: 2 }` literal (one named-arg
//      bag in, one Pt out).
//
//   2. An object literal node (`{ x: 1, y: 2 }`) to carry the
//      factory's argument bag. Field order in the emit follows the
//      record-decl source order (set by aotir's RecordLit lowerer),
//      so two builds of the same program emit byte-identical text
//      (Phase 16 reproducibility gate).
//
//   3. A `Pt.of({...})` factory-call expression. This is shorthand
//      for `MemberCallExpr{Receiver: IdentExpr{"Pt"}, Method: "of",
//      Args: [ObjectLit{...}]}`; a dedicated node reads cleaner at
//      lowering sites and keeps the emit pass independent of the
//      shape its callee picks (the factory could move to a free
//      function `Pt_of` in a later phase without invalidating the
//      caller AST).
//
//   4. Member access on records is already covered by
//      MemberAccessExpr (phase 3 added it for `xs.length`); the
//      lowerer just reuses it for `p.x` field reads.
package tstree

import "strings"

// ClassDecl is a top-level `class NAME { readonly F: T; ...
// private constructor(opts: { F: T; ... }) { this.F = opts.F; ... }
// static of(opts: { F: T; ... }): NAME { return new NAME(opts); } }`.
//
// MEP-52 §Phase 3.4 fixes this shape:
//
//   - `readonly` fields rather than mutable ones. Mochi records are
//     structurally immutable (no per-field mutation; the only way
//     to "update" a field is to construct a fresh record), so TS
//     `readonly` is the closest match. `tsc --strict` enforces it.
//   - Private constructor. Mochi has no `new` keyword; the only
//     literal form is `Pt { x: 1, y: 2 }`, which lowers via the
//     factory. Hiding the constructor behind `private` keeps user-
//     written `new Pt({...})` from compiling even if a future emit
//     decision exposes the type name.
//   - Static `of(opts)` factory. The factory takes one named-args
//     object so the emit lines up with the Mochi-side literal shape;
//     it returns `new ClassName(opts)`. The factory is `static` so
//     `Pt.of({...})` is the call site (parallel to `Pt { ... }`).
//
// Field order is the order Fields holds; the lowerer is responsible
// for stamping it in record-decl source order (matches the aotir
// RecordLit's argument-reorder rule). Determinism + Phase 16
// reproducibility live on that invariant.
type ClassDecl struct {
	// Doc is the leading `//` comment block (one line per entry).
	Doc []string
	// Name is the emitted class identifier (record name).
	Name string
	// Fields lists (Name, Type) pairs in record-decl source order.
	Fields []ClassField
}

// ClassField is one (Name, Type) pair inside a ClassDecl.
type ClassField struct {
	Name string
	Type string
}

func (d *ClassDecl) declNode() {}
func (d *ClassDecl) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	inner := pad + "  "
	innerInner := inner + "  "
	var b strings.Builder
	for _, line := range d.Doc {
		b.WriteString(pad)
		b.WriteString("// ")
		b.WriteString(line)
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteString("class ")
	b.WriteString(d.Name)
	b.WriteString(" {\n")
	// readonly fields, source order
	for _, f := range d.Fields {
		b.WriteString(inner)
		b.WriteString("readonly ")
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(f.Type)
		b.WriteString(";\n")
	}
	// private constructor
	b.WriteString(inner)
	b.WriteString("private constructor(opts: { ")
	for i, f := range d.Fields {
		if i > 0 {
			b.WriteString("; ")
		}
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(f.Type)
	}
	b.WriteString("; }) {\n")
	for _, f := range d.Fields {
		b.WriteString(innerInner)
		b.WriteString("this.")
		b.WriteString(f.Name)
		b.WriteString(" = opts.")
		b.WriteString(f.Name)
		b.WriteString(";\n")
	}
	b.WriteString(inner)
	b.WriteString("}\n")
	// static factory
	b.WriteString(inner)
	b.WriteString("static of(opts: { ")
	for i, f := range d.Fields {
		if i > 0 {
			b.WriteString("; ")
		}
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(f.Type)
	}
	b.WriteString("; }): ")
	b.WriteString(d.Name)
	b.WriteString(" {\n")
	b.WriteString(innerInner)
	b.WriteString("return new ")
	b.WriteString(d.Name)
	b.WriteString("(opts);\n")
	b.WriteString(inner)
	b.WriteString("}\n")
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// ObjectLit is `{ k1: v1, k2: v2, ... }`. The lowerer constructs
// these in record-decl source order so the emit is deterministic.
// Empty literals render `{}`.
type ObjectLit struct {
	Fields []ObjectLitField
}

// ObjectLitField is one (Name, Value) pair inside an ObjectLit.
type ObjectLitField struct {
	Name  string
	Value Expr
}

func (e *ObjectLit) exprNode() {}
func (e *ObjectLit) TsString(_ int) string {
	if len(e.Fields) == 0 {
		return "{}"
	}
	var b strings.Builder
	b.WriteString("{ ")
	for i, f := range e.Fields {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(f.Value.TsString(0))
	}
	b.WriteString(" }")
	return b.String()
}

// RecordOfCallExpr is `RECORDNAME.of({ ... })`. It carries the
// record name (the factory receiver) and the field bag the factory
// will splat into `new RECORDNAME(opts)`. A dedicated node keeps
// the lowering site readable and decouples the emit from the
// factory's exact shape (a later phase could pivot to a free
// function `mochi_RECORDNAME_of` without touching call-site code).
type RecordOfCallExpr struct {
	RecordName string
	Fields     *ObjectLit
}

func (e *RecordOfCallExpr) exprNode() {}
func (e *RecordOfCallExpr) TsString(_ int) string {
	return e.RecordName + ".of(" + e.Fields.TsString(0) + ")"
}

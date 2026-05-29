// Phase 3.4 widens the lowerer with the minimum record surface
// needed for lists of records (the data shape Phase 7's query DSL
// and Phase 8's datalog rules will iterate). The full record
// surface ships in Phase 4; this sub-phase ships only what 3.4
// fixtures exercise:
//
//   - Record declarations (prog.Records) → top-level ClassDecl
//     emit with readonly fields, private constructor, and static
//     `of(opts)` factory. Emit order matches source order so two
//     builds of the same program produce a byte-stable file
//     (Phase 16 reproducibility gate).
//
//   - RecordLit (`Pt { x: 1, y: 2 }`) → `Pt.of({ x: 1, y: 2 })`.
//     The aotir lowerer already reordered the user's literal args
//     into record-decl source order, so the emit copies that order
//     directly into the ObjectLit.
//
//   - FieldAccess (`p.x`) → `p.x`. No runtime helper needed: TS
//     property reads on a readonly class field are byte-for-byte
//     what Mochi means by `p.x` (a single, lossless load).
//
//   - List slots typed `list<R>` → `R[]`. The Phase 3.1 list
//     helpers (`mochi_list_at`, `mochi_list_sum`, etc.) are
//     already polymorphic in T, so they cover record elements
//     without per-record specialisation. The few helpers that
//     specialise on the element type (sum/min/max) don't apply
//     to records and stay numeric-only.
//
//   - Record-typed function params and returns. The existing
//     param/return path already routes through tsTypeForLetSlot;
//     Phase 3.4 extends that helper to honour RecordName.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// tsTypeForRecord returns the TS type for a record-typed slot.
// The record name is the TS class name verbatim; aotir's
// RecordDecl.Name is already a valid JS identifier (Mochi's
// frontend rejects names with non-ident characters).
func tsTypeForRecord(name string) (string, error) {
	if name == "" {
		return "", fmt.Errorf("ts lower: record slot missing record name")
	}
	return name, nil
}

// tsTypeForListElemWithRecord widens tsTypeForList to accept a
// record-typed element. When elem is TypeRecord the helper reads
// the record name from elemRecord; otherwise it falls through to
// the scalar list-elem renderer.
func tsTypeForListElemWithRecord(elem aotir.Type, elemRecord string) (string, error) {
	if elem == aotir.TypeRecord {
		return tsTypeForRecord(elemRecord)
	}
	return tsTypeForList(elem)
}

// lowerRecordLit translates `R { f1: v1, f2: v2 }` to
// `R.of({ f1: v1, f2: v2 })`. The aotir RecordLit already holds
// Fields in record-decl source order (the lowerer reorders user-
// literal args), so the emit copies that order directly.
func (l *lowerer) lowerRecordLit(e *aotir.RecordLit) (tstree.Expr, error) {
	if e.TypeName == "" {
		return nil, fmt.Errorf("ts lower: record literal missing TypeName")
	}
	fields := make([]tstree.ObjectLitField, 0, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("ts lower: record %q field %d (%q): %w", e.TypeName, i, f.Name, err)
		}
		fields = append(fields, tstree.ObjectLitField{Name: f.Name, Value: v})
	}
	return &tstree.RecordOfCallExpr{
		RecordName: e.TypeName,
		Fields:     &tstree.ObjectLit{Fields: fields},
	}, nil
}

// lowerFieldAccess translates `r.f` to `r.f`. The aotir FieldAccess
// has already resolved the field against the record decl and
// stamped Result with the field's declared type; the TS emit
// reads the property directly. There is no runtime helper because
// TS property reads on a readonly class field are byte-for-byte
// what Mochi means by `r.f` (a single, lossless load with no
// bounds check or unwrap step).
func (l *lowerer) lowerFieldAccess(e *aotir.FieldAccess) (tstree.Expr, error) {
	if e.FieldName == "" {
		return nil, fmt.Errorf("ts lower: field access missing FieldName")
	}
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: field access receiver: %w", err)
	}
	return &tstree.MemberAccessExpr{Receiver: recv, Member: e.FieldName}, nil
}

// recordDecls walks prog.Records in source order and emits one
// ClassDecl per record. The emit order is fixed by aotir's source-
// order traversal, so two builds of the same program produce a
// byte-stable prelude (Phase 16 reproducibility gate). Scalar-only
// field types are guaranteed by the aotir verifier (Phase 3.0's
// buildRecordDecl rejects nested-record / list / map fields).
func (l *lowerer) recordDecls() ([]tstree.Decl, error) {
	if len(l.prog.Records) == 0 {
		return nil, nil
	}
	out := make([]tstree.Decl, 0, len(l.prog.Records))
	for _, rd := range l.prog.Records {
		if rd == nil {
			continue
		}
		fields := make([]tstree.ClassField, 0, len(rd.Fields))
		for _, f := range rd.Fields {
			ty, err := tsTypeFor(f.Type)
			if err != nil {
				return nil, fmt.Errorf("ts lower: record %q field %q: %w", rd.Name, f.Name, err)
			}
			fields = append(fields, tstree.ClassField{Name: f.Name, Type: ty})
		}
		out = append(out, &tstree.ClassDecl{
			Doc: []string{
				"Generated record class for Mochi `type " + rd.Name + "`.",
				"Fields are readonly; the private constructor blocks `new`",
				"from user code so `" + rd.Name + ".of({...})` is the only construction path.",
			},
			Name:   rd.Name,
			Fields: fields,
		})
	}
	return out, nil
}

// Package typemap implements the closed RBS-to-Mochi type translation table
// described in [website/docs/research/0076/05-type-mapping.md]. Items whose
// entire RBS signature falls inside the table are translated; items with any
// out-of-table type produce a SkipReport.
//
// Phase 5 of MEP-76 fills in the Translate function.
package typemap

import (
	pkgerr "mochi/package3/ruby/errors"
	"mochi/package3/ruby/rbs"
)

// MochiType is a simplified Mochi type string as it appears in an extern fn
// declaration, e.g. "int", "string", "list<string>", "map<string, int>",
// "bool", "int?".
type MochiType = string

// Translate converts an RBS Type to a Mochi type string. It returns a
// SkipReport if the type is outside the closed translation table.
//
// The gem and item parameters are used for the SkipReport message only.
func Translate(t *rbs.Type, gem, item string) (MochiType, *pkgerr.SkipReport) {
	if t == nil {
		return "unit", nil
	}
	switch t.Kind {
	case rbs.TypeInteger:
		return "int", nil
	case rbs.TypeFloat:
		return "float", nil
	case rbs.TypeString:
		return "string", nil
	case rbs.TypeSymbol:
		// Symbol maps to string; a shim comment notes the to_proc limitation.
		return "string", nil
	case rbs.TypeBool:
		return "bool", nil
	case rbs.TypeNil:
		return "nil", nil
	case rbs.TypeVoid:
		return "unit", nil
	case rbs.TypeOptional:
		if t.Elem == nil {
			return skip(gem, item, pkgerr.SkipComplexUnion, "T? with nil elem")
		}
		inner, s := Translate(t.Elem, gem, item)
		if s != nil {
			return skip(gem, item, pkgerr.SkipComplexUnion, "optional with untranslatable elem")
		}
		return inner + "?", nil
	case rbs.TypeArray:
		if t.Elem == nil {
			return skip(gem, item, pkgerr.SkipUntyped, "Array[] with no element type")
		}
		elem, s := Translate(t.Elem, gem, item)
		if s != nil {
			return skip(gem, item, s.Reason, "Array element: "+s.RBSType)
		}
		return "list<" + elem + ">", nil
	case rbs.TypeHash:
		if t.Key == nil || t.Value == nil {
			return skip(gem, item, pkgerr.SkipUntyped, "Hash with nil key or value")
		}
		key, ks := Translate(t.Key, gem, item)
		if ks != nil {
			return skip(gem, item, ks.Reason, "Hash key: "+ks.RBSType)
		}
		val, vs := Translate(t.Value, gem, item)
		if vs != nil {
			return skip(gem, item, vs.Reason, "Hash value: "+vs.RBSType)
		}
		return "map<" + key + ", " + val + ">", nil
	case rbs.TypeTuple:
		if len(t.Members) == 0 {
			return skip(gem, item, pkgerr.SkipUntyped, "empty tuple")
		}
		parts := make([]string, 0, len(t.Members))
		for _, m := range t.Members {
			mt, s := Translate(m, gem, item)
			if s != nil {
				return skip(gem, item, s.Reason, "tuple member: "+s.RBSType)
			}
			parts = append(parts, mt)
		}
		return "tuple<" + join(parts) + ">", nil
	case rbs.TypeProc:
		if len(t.Params) > 5 {
			return skip(gem, item, pkgerr.SkipProc, "proc arity > 5")
		}
		// Proc translation is phase 5; stub returns skip for now.
		return skip(gem, item, pkgerr.SkipProc, "proc (phase 5 pending)")
	case rbs.TypeUntyped:
		return skip(gem, item, pkgerr.SkipUntyped, "untyped")
	case rbs.TypeTop, rbs.TypeBot:
		return skip(gem, item, pkgerr.SkipTopBot, t.Name)
	case rbs.TypeSelf, rbs.TypeInstance, rbs.TypeClass:
		return skip(gem, item, pkgerr.SkipSelfType, t.Name)
	case rbs.TypeNamed:
		// Named class types: phase 5 will walk the ClassDecl table to find
		// if the named type itself translates to a Mochi record.
		return skip(gem, item, pkgerr.SkipUnknown, "named type "+t.Name+" (phase 5)")
	default:
		return skip(gem, item, pkgerr.SkipUnknown, "unknown RBS type kind")
	}
}

func skip(gem, item string, reason pkgerr.SkipReason, rbsType string) (MochiType, *pkgerr.SkipReport) {
	return "", &pkgerr.SkipReport{
		Gem:     gem,
		Item:    item,
		Reason:  reason,
		RBSType: rbsType,
	}
}

func join(ss []string) string {
	out := ""
	for i, s := range ss {
		if i > 0 {
			out += ", "
		}
		out += s
	}
	return out
}

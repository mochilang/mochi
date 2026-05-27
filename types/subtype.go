package types

// Subtype reports whether s is a subtype of t under the rules laid out
// in MEP-11 §Specification. The predicate is strict: AnyType only acts
// as a top type when t is AnyType (T-Top). The reverse direction
// (AnyType flowing into a concrete type without an explicit cast) is
// rejected, which closes MEP-10 A1 at every call site that routes
// through Subtype.
//
// Subtype is purely structural and has no side effects. It does not
// allocate, and it does not produce substitutions; unification with
// type variables is the job of the unifier in check.go. A caller that
// needs both behaviors (the common case at a call site) runs the
// unifier first, applies the substitution, and then asks Subtype
// whether the inferred argument type is acceptable in the parameter
// position.
func Subtype(s, t Type) bool {
	// T-Refl on identical kinds with no children handled by the
	// per-kind branches below. The fast equality test catches the
	// common case of two AnyType{} values, two IntType{} values, and
	// so on without descending into structural rules.
	if equalKinds(s, t) {
		return true
	}

	// T-Top. Any concrete type widens to AnyType. The converse is not
	// a rule: AnyType -> T requires an explicit cast (MEP-10 A1).
	if _, ok := t.(AnyType); ok {
		return true
	}

	switch sv := s.(type) {

	case IntType:
		switch t.(type) {
		case Int64Type, BigIntType, BigRatType:
			return true
		}
		return false

	case Int64Type:
		switch t.(type) {
		case BigIntType, BigRatType:
			return true
		}
		return false

	case BigIntType:
		_, ok := t.(BigRatType)
		return ok

	case FloatType:
		_, ok := t.(BigRatType)
		return ok

	case SetType:
		// Set elements are covariant in read position (same rationale as ListType).
		tv, ok := t.(SetType)
		if !ok {
			return false
		}
		return Subtype(sv.Elem, tv.Elem)

	case ListType:
		// MEP-11 §T-List-Read. List elements are covariant in read
		// position only. Routing through Subtype is read position by
		// construction; write position (assignment into an index) uses
		// invariance via Equal (see MEP-11 §T-List-Inv).
		tv, ok := t.(ListType)
		if !ok {
			return false
		}
		return Subtype(sv.Elem, tv.Elem)

	case MapType:
		// MEP-11 §T-Map-Inv. Maps are invariant in both key and value.
		// We model invariance as Subtype(s, t) iff Equal(s.K, t.K) &&
		// Equal(s.V, t.V). The Subtype call here intentionally bottoms
		// out at structural equality on the children.
		tv, ok := t.(MapType)
		if !ok {
			return false
		}
		return equalKinds(sv.Key, tv.Key) && equalKinds(sv.Value, tv.Value)

	case OMapType:
		// OMapType is invariant in both key and value (same rationale as MapType).
		tv, ok := t.(OMapType)
		if !ok {
			return false
		}
		return equalKinds(sv.Key, tv.Key) && equalKinds(sv.Value, tv.Value)

	case ChanType:
		// chan<T> is invariant: send and recv both require exact element match.
		tv, ok := t.(ChanType)
		if !ok {
			return false
		}
		return equalKinds(sv.Elem, tv.Elem)

	case StreamType:
		// stream<T> is invariant: emit and recv_sub both require exact element match.
		tv, ok := t.(StreamType)
		if !ok {
			return false
		}
		return equalKinds(sv.Elem, tv.Elem)

	case SubType:
		// sub<T> is invariant.
		tv, ok := t.(SubType)
		if !ok {
			return false
		}
		return equalKinds(sv.Elem, tv.Elem)

	case OptionType:
		// MEP-11 §T-Option-Cov. option[S] <: option[T] when S <: T.
		tv, ok := t.(OptionType)
		if !ok {
			return false
		}
		return Subtype(sv.Elem, tv.Elem)

	case StructType:
		// MEP-11 §T-Struct-Nominal. Struct typing is nominal: two
		// structs are related iff they share a declared name. The
		// variant-to-union rule below handles the cross-kind case.
		switch tv := t.(type) {
		case StructType:
			return sv.Name != "" && sv.Name == tv.Name
		case UnionType:
			// MEP-11 §T-Variant. A variant struct is a subtype of its
			// declared union.
			_, ok := tv.Variants[sv.Name]
			return ok
		}
		return false

	case UnionType:
		tv, ok := t.(UnionType)
		if !ok {
			return false
		}
		return sv.Name != "" && sv.Name == tv.Name

	case FuncType:
		// MEP-11 §T-Fun. Functions are contravariant in arguments and
		// covariant in the return.
		tv, ok := t.(FuncType)
		if !ok {
			return false
		}
		if len(sv.Params) != len(tv.Params) {
			return false
		}
		if (sv.Variadic == nil) != (tv.Variadic == nil) {
			return false
		}
		for i := range sv.Params {
			if !Subtype(tv.Params[i], sv.Params[i]) {
				return false
			}
		}
		if sv.Variadic != nil && !Subtype(tv.Variadic, sv.Variadic) {
			return false
		}
		if sv.Return == nil || tv.Return == nil {
			return sv.Return == tv.Return
		}
		return Subtype(sv.Return, tv.Return)

	case GroupType:
		tv, ok := t.(GroupType)
		if !ok {
			return false
		}
		return equalKinds(sv.Key, tv.Key) && equalKinds(sv.Elem, tv.Elem)
	}

	return false
}

// Assignable reports whether a value of type src can flow into a
// position declared to accept dst, under the relaxed rules used today
// while the MEP-10 A1 any-audit is in flight (MEP-11.2). It is
// Subtype plus an explicit allowance for AnyType on either side.
//
// The helper exists so that let / var / assign / argument / return
// all share one call site. The MEP-11.3 (any-tightening) task then
// removes the AnyType-on-source escape to close MEP-10 A1 without
// having to re-route every individual call site again. Callers that
// need permanent looseness route through an explicit `as any` cast
// once #79 lands.
func Assignable(src, dst Type) bool {
	// MEP-11.3 (MEP-10 A1). A value typed `any` no longer flows into a
	// concrete slot without an explicit `as T` cast. The dst=any
	// direction stays valid (covered by Subtype's T-Top rule below).
	return assignableAt(src, dst, false)
}

// assignableAt is the recursion that powers Assignable. The
// elementContext flag carries the "empty collection literal" carve-out
// across structural descents: `[]` is `[any]`, `{}` is `{any: any}`,
// and `null` is `option[any]`. Inside those positions we still admit
// src=Any because it represents the literal's polymorphic element
// type, not a user-typed `any` value. At the top level the flag is
// false and the bare-any escape is closed.
func assignableAt(src, dst Type, elementContext bool) bool {
	if _, ok := dst.(AnyType); ok {
		return true
	}
	if elementContext {
		if _, ok := src.(AnyType); ok {
			return true
		}
	}
	switch sv := src.(type) {
	case ListType:
		if dv, ok := dst.(ListType); ok {
			return assignableAt(sv.Elem, dv.Elem, true)
		}
	case MapType:
		if dv, ok := dst.(MapType); ok {
			return assignableAt(sv.Key, dv.Key, true) && assignableAt(sv.Value, dv.Value, true)
		}
	case OMapType:
		if dv, ok := dst.(OMapType); ok {
			return assignableAt(sv.Key, dv.Key, true) && assignableAt(sv.Value, dv.Value, true)
		}
	case ChanType:
		if dv, ok := dst.(ChanType); ok {
			return assignableAt(sv.Elem, dv.Elem, true)
		}
	case StreamType:
		if dv, ok := dst.(StreamType); ok {
			return assignableAt(sv.Elem, dv.Elem, true)
		}
	case SubType:
		if dv, ok := dst.(SubType); ok {
			return assignableAt(sv.Elem, dv.Elem, true)
		}
	case OptionType:
		if dv, ok := dst.(OptionType); ok {
			return assignableAt(sv.Elem, dv.Elem, true)
		}
	}
	// MEP-16 R1: a non-option `src` flows into an option `dst` when it
	// would flow into the wrapped element. The wrap is silent at the
	// type layer; the value carries itself as the `Some` payload.
	if dv, ok := dst.(OptionType); ok {
		if _, srcOpt := src.(OptionType); !srcOpt {
			return assignableAt(src, dv.Elem, elementContext)
		}
	}
	return Subtype(src, dst)
}

// equalKinds is a small structural equality used by Subtype to discharge
// T-Refl on compound kinds and to enforce the invariance rules on Map
// and Group children. It deliberately does not call Subtype recursively
// because invariance is *not* expressible as bidirectional subtyping
// once AnyType is in play (Subtype(int, any) holds but Subtype(any, int)
// does not, so the bidirectional formulation would silently re-admit
// the implicit widening Subtype is designed to reject).
func equalKinds(a, b Type) bool {
	switch av := a.(type) {
	case AnyType:
		_, ok := b.(AnyType)
		return ok
	case ValueType:
		_, ok := b.(ValueType)
		return ok
	case IntType:
		_, ok := b.(IntType)
		return ok
	case Int64Type:
		_, ok := b.(Int64Type)
		return ok
	case BigIntType:
		_, ok := b.(BigIntType)
		return ok
	case BigRatType:
		_, ok := b.(BigRatType)
		return ok
	case FloatType:
		_, ok := b.(FloatType)
		return ok
	case StringType:
		_, ok := b.(StringType)
		return ok
	case BoolType:
		_, ok := b.(BoolType)
		return ok
	case UnitType:
		_, ok := b.(UnitType)
		return ok
	case SetType:
		bv, ok := b.(SetType)
		return ok && equalKinds(av.Elem, bv.Elem)
	case ListType:
		bv, ok := b.(ListType)
		return ok && equalKinds(av.Elem, bv.Elem)
	case MapType:
		bv, ok := b.(MapType)
		return ok && equalKinds(av.Key, bv.Key) && equalKinds(av.Value, bv.Value)
	case OMapType:
		bv, ok := b.(OMapType)
		return ok && equalKinds(av.Key, bv.Key) && equalKinds(av.Value, bv.Value)
	case ChanType:
		bv, ok := b.(ChanType)
		return ok && equalKinds(av.Elem, bv.Elem)
	case StreamType:
		bv, ok := b.(StreamType)
		return ok && equalKinds(av.Elem, bv.Elem)
	case SubType:
		bv, ok := b.(SubType)
		return ok && equalKinds(av.Elem, bv.Elem)
	case OptionType:
		bv, ok := b.(OptionType)
		return ok && equalKinds(av.Elem, bv.Elem)
	case GroupType:
		bv, ok := b.(GroupType)
		return ok && equalKinds(av.Key, bv.Key) && equalKinds(av.Elem, bv.Elem)
	case StructType:
		bv, ok := b.(StructType)
		if !ok {
			return false
		}
		if av.Name != "" || bv.Name != "" {
			return av.Name == bv.Name
		}
		if len(av.Fields) != len(bv.Fields) {
			return false
		}
		for i, f := range av.Fields {
			if f.Name != bv.Fields[i].Name {
				return false
			}
			if !equalKinds(f.Type, bv.Fields[i].Type) {
				return false
			}
		}
		return true
	case UnionType:
		bv, ok := b.(UnionType)
		return ok && av.Name == bv.Name
	case FuncType:
		bv, ok := b.(FuncType)
		if !ok {
			return false
		}
		if len(av.Params) != len(bv.Params) {
			return false
		}
		if (av.Variadic == nil) != (bv.Variadic == nil) {
			return false
		}
		for i := range av.Params {
			if !equalKinds(av.Params[i], bv.Params[i]) {
				return false
			}
		}
		if av.Variadic != nil && !equalKinds(av.Variadic, bv.Variadic) {
			return false
		}
		if av.Return == nil || bv.Return == nil {
			return av.Return == bv.Return
		}
		return equalKinds(av.Return, bv.Return)
	case *TypeVar:
		bv, ok := b.(*TypeVar)
		return ok && av.Name == bv.Name
	}
	return false
}

package types

import "fmt"

// MEP-12.1 substitution-aware unification.
//
// The pre-MEP-12 unifier in check.go takes a Subst map but uses it only
// as a side-channel for fresh TypeVar binding; it returns a bool and
// gives no diagnostic when unification fails. The MEP-12 algorithm
// needs three things the legacy path does not provide:
//
//  1. A proper occurs check, so we cannot bind `T ↦ List<T>` and loop.
//  2. A way to compose substitutions, so a single call site that binds
//     `T` from argument 1 can apply that binding when typing argument 2.
//  3. A non-bool failure signal, so the call-site walk can attach a
//     diagnostic (T047) to a specific argument position.
//
// `Apply`, `Compose`, `Unify` here form the algebra the algorithm in
// website/docs/mep/mep-0012.md §Algorithm needs. They live next to the
// legacy `unify` rather than replacing it because the legacy path is
// shared with non-generic call sites; the cut-over is the MEP-11.2 /
// MEP-12.2 task. See website/docs/mep/mep-0011.md §"Splitting unify"
// for the long-term layering.

// Apply walks t and substitutes every bound *TypeVar with its image
// under sub. Free type variables (those without a binding) are left
// alone. The returned type shares structure with t where no
// substitution fires; do not mutate it.
func (sub Subst) Apply(t Type) Type {
	if sub == nil || t == nil {
		return t
	}
	switch v := t.(type) {
	case *TypeVar:
		if u, ok := sub[v.Name]; ok {
			// Chase transitively so Apply is idempotent even when
			// substitutions chain (T1 ↦ T2 ↦ int).
			return sub.Apply(u)
		}
		return t
	case SetType:
		return SetType{Elem: sub.Apply(v.Elem)}
	case ListType:
		return ListType{Elem: sub.Apply(v.Elem)}
	case MapType:
		return MapType{Key: sub.Apply(v.Key), Value: sub.Apply(v.Value)}
	case OMapType:
		return OMapType{Key: sub.Apply(v.Key), Value: sub.Apply(v.Value)}
	case ChanType:
		return ChanType{Elem: sub.Apply(v.Elem)}
	case StreamType:
		return StreamType{Elem: sub.Apply(v.Elem)}
	case SubType:
		return SubType{Elem: sub.Apply(v.Elem)}
	case FutureType:
		return FutureType{Elem: sub.Apply(v.Elem)}
	case OptionType:
		return OptionType{Elem: sub.Apply(v.Elem)}
	case GroupType:
		return GroupType{Key: sub.Apply(v.Key), Elem: sub.Apply(v.Elem)}
	case FuncType:
		params := make([]Type, len(v.Params))
		for i, p := range v.Params {
			params[i] = sub.Apply(p)
		}
		out := FuncType{
			Params:     params,
			Return:     sub.Apply(v.Return),
			Effects:    v.Effects,
			TypeParams: append([]string(nil), v.TypeParams...),
		}
		if v.Variadic != nil {
			out.Variadic = sub.Apply(v.Variadic)
		}
		return out
	case StructType:
		// Struct field types may contain free type variables once
		// generic struct declarations land (MEP-12 Open Questions).
		// Walk them now so the helper is ready.
		fields := make([]StructField, len(v.Fields))
		for i, f := range v.Fields {
			fields[i] = StructField{Name: f.Name, Type: sub.Apply(f.Type)}
		}
		out := v
		out.Fields = fields
		return out
	case UnionType:
		// Unions are nominal and their variant set is fixed at
		// declaration; we return as-is rather than rebuilding the
		// variant map. Variant struct field types do not refer to
		// type variables that escape the declaration site today.
		return v
	}
	return t
}

// Compose returns sub ∘ other: the substitution obtained by applying
// `sub` to every image in `other` and then taking the union, with
// `sub`'s direct bindings winning on conflict. This matches the usual
// left-to-right reading where `sub` is the most recent solver state.
func (sub Subst) Compose(other Subst) Subst {
	out := make(Subst, len(sub)+len(other))
	for k, v := range other {
		out[k] = sub.Apply(v)
	}
	for k, v := range sub {
		out[k] = v
	}
	return out
}

// Bind adds the mapping name ↦ t to sub. It runs the occurs check
// described in MEP-12 §Algorithm: binding `T ↦ τ` where `τ` mentions
// `T` fails with a structural error rather than producing an infinite
// type. Binding a variable to itself is a no-op.
func (sub Subst) Bind(name string, t Type) error {
	if v, ok := t.(*TypeVar); ok && v.Name == name {
		return nil
	}
	if occurs(name, t, sub) {
		return fmt.Errorf("occurs check failed: %s in %s", name, t)
	}
	sub[name] = t
	return nil
}

// occurs reports whether the variable named `name` appears free in t
// under the current substitution. The substitution is followed so that
// `α ↦ β, β ↦ List<α>` is detected as a cycle even though the direct
// image of α does not mention α.
func occurs(name string, t Type, sub Subst) bool {
	switch v := t.(type) {
	case *TypeVar:
		if v.Name == name {
			return true
		}
		if u, ok := sub[v.Name]; ok {
			return occurs(name, u, sub)
		}
		return false
	case SetType:
		return occurs(name, v.Elem, sub)
	case ListType:
		return occurs(name, v.Elem, sub)
	case MapType:
		return occurs(name, v.Key, sub) || occurs(name, v.Value, sub)
	case OMapType:
		return occurs(name, v.Key, sub) || occurs(name, v.Value, sub)
	case OptionType:
		return occurs(name, v.Elem, sub)
	case GroupType:
		return occurs(name, v.Key, sub) || occurs(name, v.Elem, sub)
	case FuncType:
		for _, p := range v.Params {
			if occurs(name, p, sub) {
				return true
			}
		}
		if v.Variadic != nil && occurs(name, v.Variadic, sub) {
			return true
		}
		return occurs(name, v.Return, sub)
	case StructType:
		for _, f := range v.Fields {
			if occurs(name, f.Type, sub) {
				return true
			}
		}
		return false
	}
	return false
}

// Unify is the MEP-12 §Algorithm substitution-aware unifier. Given two
// types and a starting substitution, it returns the extended
// substitution that makes the two types equal under Apply, or an error
// describing the structural mismatch.
//
// Unify is symmetric on every kind except the AnyType / TypeVar
// boundary cases:
//
//   - Two TypeVars bind one to the other (the surviving one is the
//     left operand by convention).
//   - A TypeVar on either side binds to the other type with an occurs
//     check, regardless of which side it sits on.
//   - AnyType against any other kind succeeds without binding. This
//     mirrors the legacy unifier so call sites that escape into `any`
//     at the boundary keep working; MEP-10 A1 closeout (rejecting the
//     reverse direction implicitly) happens in the Subtype predicate,
//     not here. The two concerns are deliberately separated per
//     MEP-11 §"Splitting unify".
//
// The returned substitution is a fresh map; the input `sub` is not
// mutated. Callers that want in-place behaviour can simply rebind.
func Unify(a, b Type, sub Subst) (Subst, error) {
	if sub == nil {
		sub = Subst{}
	}
	out := make(Subst, len(sub))
	for k, v := range sub {
		out[k] = v
	}
	if err := unifyInto(a, b, out); err != nil {
		return nil, err
	}
	return out, nil
}

func unifyInto(a, b Type, sub Subst) error {
	a = sub.Apply(a)
	b = sub.Apply(b)

	// TypeVar before AnyType: when one side is a TypeVar and the other
	// is AnyType, bind the var to any. Otherwise the any short-circuit
	// below would leave the var unbound and the call-site escape check
	// (T048) would fire even though the call is well-formed.
	if av, ok := a.(*TypeVar); ok {
		if bv, ok := b.(*TypeVar); ok && av.Name == bv.Name {
			return nil
		}
		return sub.Bind(av.Name, b)
	}
	if bv, ok := b.(*TypeVar); ok {
		return sub.Bind(bv.Name, a)
	}

	if _, ok := a.(AnyType); ok {
		return nil
	}
	if _, ok := b.(AnyType); ok {
		return nil
	}

	switch av := a.(type) {
	case SetType:
		bv, ok := b.(SetType)
		if !ok {
			return mismatch(a, b)
		}
		return unifyInto(av.Elem, bv.Elem, sub)
	case ListType:
		bv, ok := b.(ListType)
		if !ok {
			return mismatch(a, b)
		}
		return unifyInto(av.Elem, bv.Elem, sub)
	case MapType:
		bv, ok := b.(MapType)
		if !ok {
			return mismatch(a, b)
		}
		if err := unifyInto(av.Key, bv.Key, sub); err != nil {
			return err
		}
		return unifyInto(av.Value, bv.Value, sub)
	case OMapType:
		bv, ok := b.(OMapType)
		if !ok {
			return mismatch(a, b)
		}
		if err := unifyInto(av.Key, bv.Key, sub); err != nil {
			return err
		}
		return unifyInto(av.Value, bv.Value, sub)
	case ChanType:
		bv, ok := b.(ChanType)
		if !ok {
			return mismatch(a, b)
		}
		return unifyInto(av.Elem, bv.Elem, sub)
	case StreamType:
		bv, ok := b.(StreamType)
		if !ok {
			return mismatch(a, b)
		}
		return unifyInto(av.Elem, bv.Elem, sub)
	case SubType:
		bv, ok := b.(SubType)
		if !ok {
			return mismatch(a, b)
		}
		return unifyInto(av.Elem, bv.Elem, sub)
	case FutureType:
		bv, ok := b.(FutureType)
		if !ok {
			return mismatch(a, b)
		}
		return unifyInto(av.Elem, bv.Elem, sub)
	case OptionType:
		bv, ok := b.(OptionType)
		if !ok {
			return mismatch(a, b)
		}
		return unifyInto(av.Elem, bv.Elem, sub)
	case GroupType:
		bv, ok := b.(GroupType)
		if !ok {
			return mismatch(a, b)
		}
		if err := unifyInto(av.Key, bv.Key, sub); err != nil {
			return err
		}
		return unifyInto(av.Elem, bv.Elem, sub)
	case FuncType:
		bv, ok := b.(FuncType)
		if !ok {
			return mismatch(a, b)
		}
		if len(av.Params) != len(bv.Params) {
			return mismatch(a, b)
		}
		if (av.Variadic == nil) != (bv.Variadic == nil) {
			return mismatch(a, b)
		}
		for i := range av.Params {
			if err := unifyInto(av.Params[i], bv.Params[i], sub); err != nil {
				return err
			}
		}
		if av.Variadic != nil {
			if err := unifyInto(av.Variadic, bv.Variadic, sub); err != nil {
				return err
			}
		}
		return unifyInto(av.Return, bv.Return, sub)
	case StructType:
		bv, ok := b.(StructType)
		if !ok {
			return mismatch(a, b)
		}
		if av.Name != bv.Name {
			return mismatch(a, b)
		}
		return nil
	case UnionType:
		bv, ok := b.(UnionType)
		if !ok {
			return mismatch(a, b)
		}
		if av.Name != bv.Name {
			return mismatch(a, b)
		}
		return nil
	}

	if sameAtomic(a, b) {
		return nil
	}
	return mismatch(a, b)
}

// sameAtomic compares two atomic kinds (those without children). Only
// atomic kinds reach this branch; compound kinds are handled by the
// switch in unifyInto.
func sameAtomic(a, b Type) bool {
	switch a.(type) {
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
	}
	return false
}

func mismatch(a, b Type) error {
	return fmt.Errorf("cannot unify %s with %s", a, b)
}

package types

import (
	"fmt"
	"sort"
	"sync/atomic"
)

// Polymorphism helpers used by the MEP-12 call-site algorithm. The
// pieces here are the textbook generalise / instantiate primitives.
// They are kept in their own file so the call-site walk in check.go
// can read like the pseudocode in website/docs/mep/mep-0012.md
// §Algorithm rather than re-deriving the substitution mechanics every
// time. See also website/docs/mep/mep-0005.md P11.

// freshCounter is the monotonic source of unique TypeVar suffixes. A
// counter (rather than a UUID or random string) keeps test output
// stable and short. Reset is exported only for tests.
var freshCounter uint64

// FreshTypeVar returns a TypeVar whose name is `base` decorated with a
// monotonically increasing suffix, so distinct call sites of a generic
// function get distinct variables.
//
//	FreshTypeVar("T") -> *TypeVar{Name: "T#1"}
//	FreshTypeVar("T") -> *TypeVar{Name: "T#2"}
//
// The `#` separator is not a valid identifier character in surface
// Mochi, which guarantees a fresh variable can never collide with a
// user-declared name. The pointer-receiver identity rule on TypeVar
// (see check.go) ensures that two FreshTypeVar results with the same
// suffix would still be distinct values; the unique suffix is purely
// for diagnostic clarity.
func FreshTypeVar(base string) *TypeVar {
	if base == "" {
		base = "T"
	}
	n := atomic.AddUint64(&freshCounter, 1)
	return &TypeVar{Name: fmt.Sprintf("%s#%d", base, n)}
}

// ResetFreshCounter rewinds the fresh-variable counter. Test-only.
func ResetFreshCounter() { atomic.StoreUint64(&freshCounter, 0) }

// Instantiate replaces every TypeVar in t whose name is listed in
// `params` with a fresh TypeVar, and returns both the rewritten type
// and the substitution used. This is the operation the MEP-12 call
// site runs to turn a declared scheme `<T, U>(T, U) : T` into a fresh
// pair of variables at each call.
//
// Variables in t whose names are not in `params` are left alone.
// That matches the prenex-only design from MEP-12 §"Why prenex only":
// only the function-level parameters are quantified; any nested
// variables are either captured from the outer scope or already
// flagged as escaping by T048.
func Instantiate(t Type, params []string) (Type, Subst) {
	if len(params) == 0 || t == nil {
		return t, Subst{}
	}
	sub := Subst{}
	for _, name := range params {
		sub[name] = FreshTypeVar(name)
	}
	return sub.Apply(t), sub
}

// FreeTypeVars returns the names of every TypeVar that appears free
// in t under sub. The result is deterministic: names are returned in
// sorted order so callers building diagnostic strings get a stable
// rendering.
func FreeTypeVars(t Type, sub Subst) []string {
	seen := map[string]struct{}{}
	collectFreeVars(t, sub, seen)
	out := make([]string, 0, len(seen))
	for name := range seen {
		out = append(out, name)
	}
	sort.Strings(out)
	return out
}

func collectFreeVars(t Type, sub Subst, seen map[string]struct{}) {
	if t == nil {
		return
	}
	switch v := t.(type) {
	case *TypeVar:
		if u, ok := sub[v.Name]; ok {
			collectFreeVars(u, sub, seen)
			return
		}
		seen[v.Name] = struct{}{}
	case SetType:
		collectFreeVars(v.Elem, sub, seen)
	case ListType:
		collectFreeVars(v.Elem, sub, seen)
	case MapType:
		collectFreeVars(v.Key, sub, seen)
		collectFreeVars(v.Value, sub, seen)
	case OMapType:
		collectFreeVars(v.Key, sub, seen)
		collectFreeVars(v.Value, sub, seen)
	case ChanType:
		collectFreeVars(v.Elem, sub, seen)
	case StreamType:
		collectFreeVars(v.Elem, sub, seen)
	case SubType:
		collectFreeVars(v.Elem, sub, seen)
	case FutureType:
		collectFreeVars(v.Elem, sub, seen)
	case OptionType:
		collectFreeVars(v.Elem, sub, seen)
	case GroupType:
		collectFreeVars(v.Key, sub, seen)
		collectFreeVars(v.Elem, sub, seen)
	case FuncType:
		for _, p := range v.Params {
			collectFreeVars(p, sub, seen)
		}
		if v.Variadic != nil {
			collectFreeVars(v.Variadic, sub, seen)
		}
		collectFreeVars(v.Return, sub, seen)
	case StructType:
		for _, f := range v.Fields {
			collectFreeVars(f.Type, sub, seen)
		}
	}
}

// Generalise returns the list of TypeVar names that are free in t but
// not free in any binding currently visible through `env`. Those names
// are the candidates to quantify at the declaration site. Names free
// in env are *not* generalised because they are captured from an outer
// scope and would change meaning if rebound at a call.
//
// The returned names are sorted. The returned type is t unchanged;
// generalisation in Mochi does not rewrite the type itself because
// type parameters are explicit at the FunDecl syntax level (MEP-12
// §"Why not Hindley-Milner"). The helper is still useful as a check
// for escaping variables (MEP-12 §Diagnostics T048): if Generalise
// returns names that the declaration did not list as parameters, those
// are escaping variables.
func Generalise(t Type, env *Env) ([]string, Type) {
	free := FreeTypeVars(t, nil)
	if env == nil {
		return free, t
	}
	captured := envFreeVars(env)
	out := free[:0]
	for _, name := range free {
		if _, ok := captured[name]; ok {
			continue
		}
		out = append(out, name)
	}
	return out, t
}

// envFreeVars returns the set of TypeVar names that appear free in any
// binding visible through env. We walk every name in env, including
// names shadowed by inner scopes, because a variable captured by an
// outer binding remains captured even if the inner scope rebinds the
// surface name to a different type.
func envFreeVars(env *Env) map[string]struct{} {
	seen := map[string]struct{}{}
	for e := env; e != nil; e = e.parent {
		for _, t := range e.types {
			collectFreeVars(t, nil, seen)
		}
	}
	return seen
}

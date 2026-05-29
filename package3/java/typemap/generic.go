package typemap

import "strings"

// GenericSignature holds a parsed Java generic type signature.
// It covers the common cases the bridge needs: raw class, single-param
// generics (List<T>, Optional<T>, CompletableFuture<T>), and two-param
// generics (Map<K,V>).
type GenericSignature struct {
	// Base is the fully-qualified class name without type parameters.
	Base string
	// Params are the type parameter strings (possibly themselves generic).
	Params []string
}

// ParseGenericSignature parses a Java generic type name like
// "java.util.List<java.lang.String>" into its base class and type params.
// On success ok is true. Raw (non-generic) types return the name as Base
// with an empty Params slice.
func ParseGenericSignature(name string) (sig GenericSignature, ok bool) {
	name = strings.TrimSpace(name)
	lt := strings.Index(name, "<")
	if lt < 0 {
		return GenericSignature{Base: name}, true
	}
	if !strings.HasSuffix(name, ">") {
		return GenericSignature{}, false
	}
	base := name[:lt]
	inner := name[lt+1 : len(name)-1]
	params := splitTopLevelParams(inner)
	return GenericSignature{Base: base, Params: params}, true
}

// IsRaw reports whether the signature has no type parameters.
func (s GenericSignature) IsRaw() bool { return len(s.Params) == 0 }

// Arity returns the number of type parameters.
func (s GenericSignature) Arity() int { return len(s.Params) }

// String reconstructs the full Java type name from the signature.
func (s GenericSignature) String() string {
	if s.IsRaw() {
		return s.Base
	}
	return s.Base + "<" + strings.Join(s.Params, ",") + ">"
}

// EraseParams returns the raw (erased) form of the signature.
func (s GenericSignature) EraseParams() GenericSignature {
	return GenericSignature{Base: s.Base}
}

// WildcardErased reports whether any parameter is a wildcard ("?", "? extends ...",
// "? super ..."). Such types cannot be safely bridged.
func (s GenericSignature) WildcardErased() bool {
	for _, p := range s.Params {
		tp := strings.TrimSpace(p)
		if tp == "?" || strings.HasPrefix(tp, "? ") {
			return true
		}
	}
	return false
}

// splitTopLevelParams splits a comma-separated list of type parameters,
// ignoring commas nested inside angle brackets.
func splitTopLevelParams(s string) []string {
	var out []string
	depth := 0
	last := 0
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '<':
			depth++
		case '>':
			depth--
		case ',':
			if depth == 0 {
				out = append(out, strings.TrimSpace(s[last:i]))
				last = i + 1
			}
		}
	}
	out = append(out, strings.TrimSpace(s[last:]))
	return out
}

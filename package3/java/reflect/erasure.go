package reflect

import "strings"

// EraseType returns the type-erased (raw) form of a Java type name. For
// generic types (e.g. "java.util.List<java.lang.String>") it strips the
// angle-bracket parameters and returns the base class name. Non-generic
// types are returned unchanged.
//
// This matches the JVM's type-erasure semantics: at runtime all generic
// information is removed and the JVM operates on the raw class.
func EraseType(javaTypeName string) string {
	idx := strings.IndexByte(javaTypeName, '<')
	if idx < 0 {
		return javaTypeName
	}
	return javaTypeName[:idx]
}

// IsErased reports whether a type name contains no generic parameters.
func IsErased(javaTypeName string) bool {
	return !strings.ContainsAny(javaTypeName, "<>")
}

// ErasedParams extracts the type parameters from a generic type name.
// For "java.util.Map<K,V>" it returns ["K", "V"]. For raw types it returns nil.
// This is the counterpart to EraseType for callers that need the parameter list.
func ErasedParams(javaTypeName string) []string {
	lt := strings.IndexByte(javaTypeName, '<')
	if lt < 0 {
		return nil
	}
	if !strings.HasSuffix(javaTypeName, ">") {
		return nil
	}
	inner := javaTypeName[lt+1 : len(javaTypeName)-1]
	return splitParams(inner)
}

// NormaliseTypeName applies a standard set of transformations to a raw type
// name extracted from bytecode metadata (as seen in the reflection surface):
//
//   - Replaces '$' inner-class separators with '.' for consistency.
//   - Strips array suffixes "[]" (returns the element type only).
//   - Strips leading/trailing whitespace.
func NormaliseTypeName(name string) string {
	name = strings.TrimSpace(name)
	name = strings.ReplaceAll(name, "$", ".")
	name = strings.TrimSuffix(name, "[]")
	return name
}

// splitParams splits a comma-separated list of type parameters, respecting
// nested angle brackets.
func splitParams(s string) []string {
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

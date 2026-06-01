// Package typemap converts Swift type strings to Mochi type mappings.
// It implements the closed type-mapping table described in MEP-69 §type-mapping.
package typemap

import (
	"strings"

	swifterrors "mochi/package3/swift/errors"
)

// Kind classifies a Mochi type category.
type Kind int

const (
	KindInt    Kind = iota // Int, Int32, Int64, UInt, etc.
	KindFloat              // Float, Double
	KindBool               // Bool
	KindString             // String
	KindUnit               // Void or ()
	KindList               // [T]
	KindMap                // [K: V]
	KindSet                // Set<T>
	KindOption             // Optional<T> or T?
	KindResult             // Result<T, E>
	KindRecord             // struct/class
	KindAsync              // async wrapper
	KindSkip               // cannot be mapped
)

// Mapping is the result of mapping a Swift type to a Mochi type.
type Mapping struct {
	Kind      Kind
	MochiType string            // canonical Mochi type string
	Elem      *Mapping          // for KindList, KindSet, KindOption
	Key       *Mapping          // for KindMap
	Value     *Mapping          // for KindMap
	Inner     *Mapping          // for KindAsync
	SkipReason swifterrors.SkipReason // when Kind==KindSkip
}

// Map converts a Swift type string to a Mapping.
func Map(swiftType string) *Mapping {
	return mapType(strings.TrimSpace(swiftType))
}

func mapType(t string) *Mapping {
	if t == "" {
		return &Mapping{Kind: KindUnit, MochiType: "unit"}
	}

	// Void / ()
	if t == "Void" || t == "()" {
		return &Mapping{Kind: KindUnit, MochiType: "unit"}
	}

	// Bool
	if t == "Bool" {
		return &Mapping{Kind: KindBool, MochiType: "bool"}
	}

	// Integer types
	switch t {
	case "Int", "Int8", "Int16", "Int32", "Int64",
		"UInt", "UInt8", "UInt16", "UInt32", "UInt64":
		mochiT := intMochiType(t)
		return &Mapping{Kind: KindInt, MochiType: mochiT}
	}

	// Float types
	if t == "Float" || t == "Float32" {
		return &Mapping{Kind: KindFloat, MochiType: "f32"}
	}
	if t == "Double" || t == "Float64" {
		return &Mapping{Kind: KindFloat, MochiType: "f64"}
	}

	// String
	if t == "String" {
		return &Mapping{Kind: KindString, MochiType: "str"}
	}

	// Unsafe pointer types -> skip
	if strings.Contains(t, "UnsafePointer") || strings.Contains(t, "UnsafeMutablePointer") ||
		strings.Contains(t, "UnsafeRawPointer") || strings.Contains(t, "UnsafeMutableRawPointer") {
		return &Mapping{Kind: KindSkip, SkipReason: swifterrors.SkipUnsafePointer}
	}

	// Closure types: (A, B) -> C
	if looksLikeClosure(t) {
		return &Mapping{Kind: KindSkip, SkipReason: swifterrors.SkipClosure}
	}

	// Optional: T? or Optional<T>
	if strings.HasSuffix(t, "?") {
		inner := mapType(strings.TrimSuffix(t, "?"))
		return &Mapping{Kind: KindOption, MochiType: "option<" + inner.MochiType + ">", Elem: inner}
	}
	if after, ok := strings.CutPrefix(t, "Optional<"); ok {
		inner := mapType(strings.TrimSuffix(after, ">"))
		return &Mapping{Kind: KindOption, MochiType: "option<" + inner.MochiType + ">", Elem: inner}
	}

	// Array: [T]
	if strings.HasPrefix(t, "[") && !strings.Contains(t, ":") {
		inner := mapType(t[1 : len(t)-1])
		return &Mapping{Kind: KindList, MochiType: "list<" + inner.MochiType + ">", Elem: inner}
	}

	// Dictionary: [K: V]
	if strings.HasPrefix(t, "[") && strings.Contains(t, ":") {
		colonIdx := strings.Index(t, ":")
		k := mapType(strings.TrimSpace(t[1:colonIdx]))
		v := mapType(strings.TrimSpace(t[colonIdx+1 : len(t)-1]))
		return &Mapping{
			Kind:      KindMap,
			MochiType: "map<" + k.MochiType + ", " + v.MochiType + ">",
			Key:       k,
			Value:     v,
		}
	}

	// Set<T>
	if after, ok := strings.CutPrefix(t, "Set<"); ok {
		inner := mapType(strings.TrimSuffix(after, ">"))
		return &Mapping{Kind: KindSet, MochiType: "set<" + inner.MochiType + ">", Elem: inner}
	}

	// Result<T, E>
	if strings.HasPrefix(t, "Result<") {
		inner := t[7 : len(t)-1]
		parts := splitAngleBracket(inner)
		if len(parts) == 2 {
			ok := mapType(strings.TrimSpace(parts[0]))
			_ = mapType(strings.TrimSpace(parts[1])) // error type, ignored for now
			return &Mapping{
				Kind:      KindResult,
				MochiType: "result<" + ok.MochiType + ", str>",
				Elem:      ok,
			}
		}
	}

	// any Protocol -> existential -> skip
	if strings.HasPrefix(t, "any ") {
		return &Mapping{Kind: KindSkip, SkipReason: swifterrors.SkipExistential}
	}

	// some Protocol -> opaque -> skip
	if strings.HasPrefix(t, "some ") {
		return &Mapping{Kind: KindSkip, SkipReason: swifterrors.SkipOpaque}
	}

	// Generic type parameters (single uppercase letter or contains '<')
	if isGenericParam(t) {
		return &Mapping{Kind: KindSkip, SkipReason: swifterrors.SkipGeneric}
	}

	// Actor types
	if strings.HasSuffix(t, "Actor") {
		return &Mapping{Kind: KindSkip, SkipReason: swifterrors.SkipActor}
	}

	// Concurrency types
	switch t {
	case "TaskGroup", "AsyncStream", "AsyncThrowingStream", "AsyncSequence",
		"CheckedContinuation", "UnsafeContinuation":
		return &Mapping{Kind: KindSkip, SkipReason: swifterrors.SkipConcurrencyType}
	}

	// Default: treat as a record type
	return &Mapping{Kind: KindRecord, MochiType: t}
}

// intMochiType maps Swift integer type names to Mochi type names.
func intMochiType(t string) string {
	switch t {
	case "Int8":
		return "i8"
	case "Int16":
		return "i16"
	case "Int32":
		return "i32"
	case "Int64":
		return "i64"
	case "UInt8":
		return "u8"
	case "UInt16":
		return "u16"
	case "UInt32":
		return "u32"
	case "UInt64":
		return "u64"
	case "UInt":
		return "u64"
	default: // Int
		return "i64"
	}
}

// looksLikeClosure returns true if t looks like a Swift closure type "(A) -> B".
func looksLikeClosure(t string) bool {
	if !strings.HasPrefix(t, "(") {
		return false
	}
	// Find matching close paren
	depth := 0
	for i, c := range t {
		switch c {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				rest := strings.TrimSpace(t[i+1:])
				return strings.HasPrefix(rest, "->")
			}
		}
	}
	return false
}

// isGenericParam returns true if t looks like a generic type parameter.
func isGenericParam(t string) bool {
	if len(t) == 1 && t[0] >= 'A' && t[0] <= 'Z' {
		return true
	}
	// Contains unresolved angle brackets
	if strings.Contains(t, "<") {
		return true
	}
	return false
}

// splitAngleBracket splits a comma-separated list, respecting nested angle brackets.
func splitAngleBracket(s string) []string {
	var out []string
	depth := 0
	last := 0
	for i, c := range s {
		switch c {
		case '<':
			depth++
		case '>':
			depth--
		case ',':
			if depth == 0 {
				out = append(out, s[last:i])
				last = i + 1
			}
		}
	}
	out = append(out, s[last:])
	return out
}

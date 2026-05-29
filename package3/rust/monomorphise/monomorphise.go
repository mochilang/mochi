// Package monomorphise owns the MEP-73 Phase 12 generic-instantiation
// pipeline. Phase 3's type-mapping table refuses any function whose
// signature contains a generic parameter (`fn from_str<T>(s: &str)`)
// because the bridge cannot synthesise a wrapper without a concrete
// substitution for `T`. Phase 12 unlocks this case via an explicit
// `[rust.monomorphise]` table in `mochi.toml`: the user enumerates the
// concretisations they need, and the bridge emits one extern "C"
// wrapper per (item, type-args) pair.
//
// This package owns:
//
//   - The on-wire shape of the `[rust.monomorphise]` table (Spec, Entry).
//   - The TOML parser that reads it.
//   - The per-instantiation symbol mangler (extern name + Rust call
//     site path).
//   - The lookup index from "<item-path>" + sorted type-args to the
//     matching Entry, so wrapper.Synth can decide whether a generic
//     fn was monomorphised or should be refused.
//
// The package intentionally only sees the user's enumerated set: no
// inference, no auto-monomorphisation. The combinatorial-explosion
// risk called out in MEP-73 §Risks is hard-bounded by the manifest's
// own length.
package monomorphise

import (
	"fmt"
	"sort"
	"strings"

	"mochi/package3/rust/rustdoc"
)

// Spec is the parsed `[rust.monomorphise]` table.
type Spec struct {
	// Entries is the list in declaration order. Iteration order
	// matches the user's manifest layout for deterministic emit.
	Entries []Entry
}

// Entry is one explicit instantiation. Item is the upstream Rust
// path of the generic item (e.g. "serde_json::from_str" or
// "Vec::new"). TypeArgs is the per-type-parameter substitution,
// keyed by the type-parameter name as it appears in the rustdoc
// surface (e.g. "T", "K", "V").
//
// Example manifest line:
//
//	monomorphise = [
//	    { item = "serde_json::from_str", T = "MyStruct" },
//	    { item = "Vec::new", T = "i64" },
//	]
//
// becomes:
//
//	Entry{Item: "serde_json::from_str", TypeArgs: {"T": "MyStruct"}}
//	Entry{Item: "Vec::new",              TypeArgs: {"T": "i64"}}
type Entry struct {
	Item     string
	TypeArgs map[string]string
}

// ErrInvalid is returned when an Entry is structurally bad. The
// Reason field carries the human-readable diagnostic.
type ErrInvalid struct{ Reason string }

// Error implements error.
func (e ErrInvalid) Error() string { return "monomorphise: " + e.Reason }

// Validate checks that every Entry has a non-empty Item and at least
// one TypeArg. Empty items would mangle to ambiguous symbols; empty
// TypeArgs would defeat the point of an instantiation entry (the
// generic refusal would still fire).
func (s Spec) Validate() error {
	for i, e := range s.Entries {
		if strings.TrimSpace(e.Item) == "" {
			return ErrInvalid{Reason: fmt.Sprintf("entry %d: empty item path", i)}
		}
		if len(e.TypeArgs) == 0 {
			return ErrInvalid{Reason: fmt.Sprintf("entry %d (%s): no type-args", i, e.Item)}
		}
		for k, v := range e.TypeArgs {
			if strings.TrimSpace(k) == "" {
				return ErrInvalid{Reason: fmt.Sprintf("entry %d (%s): empty type-parameter name", i, e.Item)}
			}
			if strings.TrimSpace(v) == "" {
				return ErrInvalid{Reason: fmt.Sprintf("entry %d (%s): empty substitution for %s", i, e.Item, k)}
			}
		}
	}
	return nil
}

// Lookup finds an Entry by Item path. Returns nil if no monomorphise
// entry was declared for the path. The Spec is intentionally a flat
// slice (small N, manifest-bounded); a linear scan is faster than
// any map for the expected sizes (a-few-tens of entries).
func (s Spec) Lookup(item string) []Entry {
	out := make([]Entry, 0, len(s.Entries))
	for _, e := range s.Entries {
		if e.Item == item {
			out = append(out, e)
		}
	}
	return out
}

// ExternName mangles an (item, type-args) pair into a stable
// extern "C" symbol. The shape mirrors wrapper.externName for sync
// fns and appends a `_of_` suffix carrying the type-args in sorted
// (type-param-name) order. Sorting keeps the symbol byte-stable
// across runs even though TypeArgs is a map.
//
// Example: ("serde_json::from_str", {"T": "MyStruct"}) becomes
// "mochi_serde_json_from_str_of_t_my_struct".
func ExternName(upstream, item string, typeArgs map[string]string) string {
	out := "mochi_" + sanitize(upstream)
	for _, seg := range itemSegments(upstream, item) {
		out += "_" + sanitize(seg)
	}
	if len(typeArgs) == 0 {
		return out
	}
	out += "_of"
	keys := sortedKeys(typeArgs)
	for _, k := range keys {
		out += "_" + sanitize(k) + "_" + sanitize(typeArgs[k])
	}
	return out
}

// CallSite renders the concretised Rust path the wrapper body calls
// for an Entry. For "serde_json::from_str" with T=MyStruct this is
// "serde_json::from_str::<MyStruct>"; the turbofish ordering matches
// the rustdoc-types declaration order via sortedKeys.
//
// Multi-arg substitutions render as "<A, B>" with the type-parameter
// names sorted for byte stability.
func CallSite(item string, typeArgs map[string]string) string {
	if len(typeArgs) == 0 {
		return item
	}
	keys := sortedKeys(typeArgs)
	parts := make([]string, len(keys))
	for i, k := range keys {
		parts[i] = typeArgs[k]
	}
	return item + "::<" + strings.Join(parts, ", ") + ">"
}

// Substitute recursively walks a rustdoc.Type and replaces every
// Generic node whose name appears in `subs` with a concrete type
// derived from the substitution string. Substitutions that match a
// known Rust primitive (e.g. "i64", "bool") become primitive types;
// anything else becomes a resolved_path with the substitution as the
// path text. Non-generic positions are returned unchanged, so the
// caller can run typemap.Map on the result without further dispatch.
//
// Substitute is intentionally a shallow lowering: it does not parse
// nested turbofish (`Vec<i64>` substitutions remain a single
// resolved_path string) because typemap rejects such constructs at
// the path-name layer anyway. Phase 12's contract is one-level
// concretisation.
func Substitute(t rustdoc.Type, subs map[string]string) rustdoc.Type {
	if t.Generic != "" {
		if sub, ok := subs[t.Generic]; ok {
			return concretise(sub)
		}
		return t
	}
	if t.Tuple != nil {
		out := make([]rustdoc.Type, len(t.Tuple))
		for i, e := range t.Tuple {
			out[i] = Substitute(e, subs)
		}
		t.Tuple = out
	}
	if t.Slice != nil {
		s := Substitute(*t.Slice, subs)
		t.Slice = &s
	}
	if t.Array != nil {
		a := Substitute(t.Array.Type, subs)
		t.Array.Type = a
	}
	if t.BorrowedRef != nil {
		b := Substitute(t.BorrowedRef.Type, subs)
		t.BorrowedRef.Type = b
	}
	if t.ResolvedPath != nil && t.ResolvedPath.Args != nil && t.ResolvedPath.Args.AngleBracketed != nil {
		args := t.ResolvedPath.Args.AngleBracketed.Args
		out := make([]rustdoc.GenericArg, len(args))
		for i, a := range args {
			if a.Type != nil {
				sub := Substitute(*a.Type, subs)
				a.Type = &sub
			}
			out[i] = a
		}
		t.ResolvedPath.Args.AngleBracketed.Args = out
	}
	return t
}

func concretise(sub string) rustdoc.Type {
	if isKnownPrimitive(sub) {
		return rustdoc.Type{Primitive: sub}
	}
	return rustdoc.Type{ResolvedPath: &rustdoc.PathType{Path: sub}}
}

func isKnownPrimitive(s string) bool {
	switch s {
	case "bool",
		"i8", "i16", "i32", "i64", "i128", "isize",
		"u8", "u16", "u32", "u64", "u128", "usize",
		"f32", "f64",
		"char", "str":
		return true
	}
	return false
}

// ParseTOMLEntries parses the inline-array body of a `monomorphise`
// manifest row. The accepted shape is the verbatim Cargo-style
// inline array of inline tables:
//
//	[
//	    { item = "serde_json::from_str", T = "MyStruct" },
//	    { item = "Vec::new", T = "i64" },
//	]
//
// The leading `[` and trailing `]` may be present or stripped. Each
// inline table must have an `item` key plus at least one capitalised
// type-parameter key. The parser is intentionally narrow; it rejects
// anything that does not match the literal shape so the manifest
// stays a closed surface.
//
// This parser is hand-rolled (no third-party TOML dep) to keep the
// package layering-conservative.
func ParseTOMLEntries(body string) (Spec, error) {
	body = strings.TrimSpace(body)
	body = strings.TrimPrefix(body, "[")
	body = strings.TrimSuffix(body, "]")
	tables, err := splitTables(body)
	if err != nil {
		return Spec{}, err
	}
	out := Spec{Entries: make([]Entry, 0, len(tables))}
	for i, tbl := range tables {
		entry, err := parseTable(tbl)
		if err != nil {
			return Spec{}, ErrInvalid{Reason: fmt.Sprintf("entry %d: %s", i, err.Error())}
		}
		out.Entries = append(out.Entries, entry)
	}
	return out, nil
}

func splitTables(body string) ([]string, error) {
	body = strings.TrimSpace(body)
	if body == "" {
		return nil, nil
	}
	var out []string
	depth := 0
	start := -1
	for i := 0; i < len(body); i++ {
		c := body[i]
		switch c {
		case '{':
			if depth == 0 {
				start = i + 1
			}
			depth++
		case '}':
			depth--
			if depth == 0 && start >= 0 {
				out = append(out, body[start:i])
				start = -1
			}
		}
	}
	if depth != 0 {
		return nil, ErrInvalid{Reason: "unbalanced braces"}
	}
	return out, nil
}

func parseTable(body string) (Entry, error) {
	e := Entry{TypeArgs: map[string]string{}}
	for _, part := range splitKVs(body) {
		k, v, ok := strings.Cut(part, "=")
		if !ok {
			return Entry{}, fmt.Errorf("missing `=` in %q", part)
		}
		k = strings.TrimSpace(k)
		v = strings.TrimSpace(v)
		if strings.HasPrefix(v, `"`) && strings.HasSuffix(v, `"`) {
			v = v[1 : len(v)-1]
		} else {
			return Entry{}, fmt.Errorf("value %q is not a quoted string", v)
		}
		if k == "item" {
			e.Item = v
		} else {
			e.TypeArgs[k] = v
		}
	}
	return e, nil
}

func splitKVs(body string) []string {
	var out []string
	depth := 0
	start := 0
	inStr := false
	for i := 0; i < len(body); i++ {
		c := body[i]
		switch c {
		case '"':
			inStr = !inStr
		case '{', '[':
			if !inStr {
				depth++
			}
		case '}', ']':
			if !inStr {
				depth--
			}
		case ',':
			if !inStr && depth == 0 {
				chunk := strings.TrimSpace(body[start:i])
				if chunk != "" {
					out = append(out, chunk)
				}
				start = i + 1
			}
		}
	}
	tail := strings.TrimSpace(body[start:])
	if tail != "" {
		out = append(out, tail)
	}
	return out
}

func sortedKeys(m map[string]string) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

func sanitize(s string) string {
	b := make([]byte, 0, len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c >= 'a' && c <= 'z', c >= '0' && c <= '9', c == '_':
			b = append(b, c)
		case c >= 'A' && c <= 'Z':
			b = append(b, c-'A'+'a')
		default:
			b = append(b, '_')
		}
	}
	return string(b)
}

// itemSegments splits the upstream-relative tail of `item`. When
// `item` does not start with `<upstream>::`, the whole path is
// treated as the tail (e.g. "Vec::new" inside the "smallvec"
// upstream is just ["Vec", "new"]).
func itemSegments(upstream, item string) []string {
	prefix := upstream + "::"
	if strings.HasPrefix(item, prefix) {
		item = item[len(prefix):]
	}
	return strings.Split(item, "::")
}

// Package monomorphise is the MEP-74 phase 15 generic-
// instantiation surface. It covers the bridge-side handling of
// Go 1.18+ generic functions and types: the mochi.toml
// `[go.monomorphise]` table lists concrete instantiations per
// generic item, and the wrapper synthesiser emits one
// per-instantiation Go function (and lockfile entry) so the
// rest of the bridge can treat the instantiation as a plain
// non-generic export.
//
// The package has three layers:
//
//  1. A parser for the [go.monomorphise] table that produces a
//     SpecSet from inline TOML-array-of-tables fragments. The
//     parser is intentionally tiny and table-only (no full TOML
//     parser dependency); the wider MEP-57 mochi.toml driver
//     extracts the slice of inline tables and feeds them to
//     ParseSpecs here.
//  2. A resolver that walks an apisurface.Package, matches
//     each Spec against an exported generic Func (or generic
//     Type method), validates the type-argument count, and
//     produces a fully-typed Instance for the wrapper
//     synthesiser.
//  3. A renderer that emits the Go source for one Instance: a
//     non-generic wrapper function whose body calls the
//     generic original with the resolved type arguments
//     spliced into the call. The output is byte-deterministic
//     so the phase 10 wrapper-sha256 pin stays stable.
//
// Out of scope for v1 (deferred to 15.1+): auto-monomorphisation
// from a usage-site `slices.Sort([]int{})` call, multi-param
// generic type instantiations declared via positional indexing,
// and constraint-checking against the type-parameter's interface.
package monomorphise

import (
	"errors"
	"fmt"
	"sort"
	"strings"

	"mochi/package3/go/apisurface"
)

// ErrMonomorphise is the package-wide error sentinel.
var ErrMonomorphise = errors.New("monomorphise")

// Spec is one entry of the mochi.toml `[go.monomorphise]`
// table. Item is the canonical generic identifier:
// `<package-import-path>.<Identifier>` (e.g.
// "encoding/json.Unmarshal" or "golang.org/x/exp/slices.Sort").
// TypeArgs is the comma-split list of concrete type
// arguments (single-arg generics have len(TypeArgs)==1).
type Spec struct {
	Item     string
	TypeArgs []string
}

// SpecSet is an ordered list of monomorphise Specs. The order
// is preserved verbatim from the manifest so a downstream
// `mochi pkg lock` diff is interpretable.
type SpecSet struct {
	Specs []Spec
}

// ParseSpecs turns a slice of `{item, T}` table fragments into
// a SpecSet. Each fragment is a `key = "value"` line list (no
// nested tables, no comments). The wider mochi.toml driver
// extracts and passes the fragments; this function does the
// final shape check.
//
// Format per fragment:
//
//	item = "<pkg-path>.<Ident>"
//	T = "<type>" | "<t1>, <t2>"
//
// Returns ErrMonomorphise for any unparseable fragment.
func ParseSpecs(fragments []string) (*SpecSet, error) {
	out := &SpecSet{}
	for i, frag := range fragments {
		s, err := parseFragment(frag)
		if err != nil {
			return nil, fmt.Errorf("%w: entry %d: %v", ErrMonomorphise, i, err)
		}
		out.Specs = append(out.Specs, s)
	}
	if err := out.Validate(); err != nil {
		return nil, err
	}
	return out, nil
}

func parseFragment(s string) (Spec, error) {
	spec := Spec{}
	for _, raw := range strings.Split(s, "\n") {
		line := strings.TrimSpace(raw)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		key, val, ok := cutEquals(line)
		if !ok {
			return Spec{}, fmt.Errorf("malformed entry line %q (expected 'key = value')", line)
		}
		val = strings.Trim(val, ",")
		val = strings.TrimSpace(val)
		val = strings.Trim(val, "\"")
		switch key {
		case "item":
			spec.Item = val
		case "T":
			parts := strings.Split(val, ",")
			for _, p := range parts {
				p = strings.TrimSpace(p)
				if p != "" {
					spec.TypeArgs = append(spec.TypeArgs, p)
				}
			}
		default:
			return Spec{}, fmt.Errorf("unknown entry key %q", key)
		}
	}
	return spec, nil
}

func cutEquals(s string) (string, string, bool) {
	idx := strings.Index(s, "=")
	if idx < 0 {
		return "", "", false
	}
	return strings.TrimSpace(s[:idx]), strings.TrimSpace(s[idx+1:]), true
}

// Validate checks that every Spec is structurally well-formed:
// non-empty Item, a dot separating PackagePath from Ident, at
// least one TypeArg. Returns nil for an empty SpecSet.
func (set *SpecSet) Validate() error {
	for i, s := range set.Specs {
		if strings.TrimSpace(s.Item) == "" {
			return fmt.Errorf("%w: entry %d: item is required", ErrMonomorphise, i)
		}
		if dot := strings.LastIndex(s.Item, "."); dot <= 0 {
			return fmt.Errorf("%w: entry %d: item %q must be 'package/path.Ident'", ErrMonomorphise, i, s.Item)
		}
		if len(s.TypeArgs) == 0 {
			return fmt.Errorf("%w: entry %d: T is required", ErrMonomorphise, i)
		}
		for j, ta := range s.TypeArgs {
			if strings.TrimSpace(ta) == "" {
				return fmt.Errorf("%w: entry %d, T[%d]: empty type argument", ErrMonomorphise, i, j)
			}
		}
	}
	return nil
}

// PackagePath returns the import-path portion of Item, i.e.
// everything before the final dot.
func (s Spec) PackagePath() string {
	if dot := strings.LastIndex(s.Item, "."); dot > 0 {
		return s.Item[:dot]
	}
	return ""
}

// Ident returns the identifier portion of Item, i.e.
// everything after the final dot.
func (s Spec) Ident() string {
	if dot := strings.LastIndex(s.Item, "."); dot > 0 {
		return s.Item[dot+1:]
	}
	return s.Item
}

// Instance is a fully-resolved monomorphisation: the matched
// generic Func plus the parsed-and-validated type arguments
// and a renderer-ready symbol suffix.
type Instance struct {
	// Spec is the manifest entry this instance derives from.
	Spec Spec
	// Func is the matched generic function in the surface.
	Func apisurface.Func
	// SymbolSuffix is the wrapper symbol suffix (e.g.
	// "MyStruct" for a single-arg, "string_int64" for two
	// args). Derived from TypeArgs with non-identifier
	// characters replaced by `_`.
	SymbolSuffix string
}

// Resolve matches every Spec in set against an exported
// generic Func or generic Type-method in pkg and returns the
// resolved Instances plus a list of error strings for specs
// that did not match (so the caller can surface them as
// SkipReport entries without aborting the whole synthesise).
// A successful Resolve produces deterministic output: the
// Instances slice is sorted by Spec.Item then by
// SymbolSuffix.
func Resolve(pkg apisurface.Package, set *SpecSet) ([]Instance, []string, error) {
	if set == nil {
		return nil, nil, nil
	}
	if err := set.Validate(); err != nil {
		return nil, nil, err
	}

	// Build a quick lookup table of generic funcs in pkg keyed
	// by `<pkg-path>.<Ident>`. The pkg-path of a function
	// declared in the package itself is pkg.Path. Methods on
	// generic types are keyed by `<pkg-path>.<TypeName>.<Method>`.
	lookup := map[string]apisurface.Func{}
	for _, f := range pkg.Funcs {
		if len(f.TypeParams) == 0 {
			continue
		}
		lookup[pkg.ImportPath+"."+f.Name] = f
	}
	for _, t := range pkg.Types {
		// Generic type's methods are themselves generic via the
		// type-parameter inheritance; we synthesise the key as
		// `<pkg-path>.<Type>.<Method>` so the manifest can refer
		// to them explicitly.
		if len(t.TypeParams) == 0 {
			continue
		}
		for _, m := range t.Methods {
			lookup[pkg.ImportPath+"."+t.Name+"."+m.Name] = m
		}
	}

	var out []Instance
	var missing []string
	for _, s := range set.Specs {
		f, ok := lookup[s.Item]
		if !ok {
			missing = append(missing, fmt.Sprintf("%s: no generic identifier matches", s.Item))
			continue
		}
		// The number of type arguments must match the function's
		// generic arity.
		if len(s.TypeArgs) != len(f.TypeParams) {
			missing = append(missing, fmt.Sprintf("%s: needs %d type argument(s), got %d", s.Item, len(f.TypeParams), len(s.TypeArgs)))
			continue
		}
		out = append(out, Instance{
			Spec:         s,
			Func:         f,
			SymbolSuffix: sanitiseSuffix(s.TypeArgs),
		})
	}
	sort.SliceStable(out, func(i, j int) bool {
		if out[i].Spec.Item != out[j].Spec.Item {
			return out[i].Spec.Item < out[j].Spec.Item
		}
		return out[i].SymbolSuffix < out[j].SymbolSuffix
	})
	return out, missing, nil
}

// sanitiseSuffix converts a list of type arguments into an
// identifier-safe symbol suffix: dots, slashes, brackets, and
// commas become `_`; existing identifier chars pass through.
// "MyStruct" -> "MyStruct"
// "encoding/json.Decoder" -> "encoding_json_Decoder"
// ["string", "int64"] -> "string_int64"
func sanitiseSuffix(args []string) string {
	parts := make([]string, len(args))
	for i, a := range args {
		parts[i] = sanitiseIdent(strings.TrimSpace(a))
	}
	return strings.Join(parts, "_")
}

func sanitiseIdent(s string) string {
	var sb strings.Builder
	for _, r := range s {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_' {
			sb.WriteRune(r)
		} else {
			sb.WriteRune('_')
		}
	}
	return sb.String()
}

// RenderInstance returns the Go source for one Instance: a
// non-generic wrapper function `mochi_<module>_<Ident>_<suffix>`
// whose body calls the original generic with `[T1, T2, ...]`
// type arguments spliced into the call site. The wrapper's
// signature mirrors the original's signature with every
// TypeParam name replaced by its corresponding TypeArg.
//
// The wrapper is decorated with a `//export` directive so the
// cgo wrapper synthesiser (phase 6) picks it up like any other
// non-generic export. The package clause is the wrapper's,
// not the source module's: callers are expected to drop the
// rendered source into the wrapper package's directory.
//
// alias is the wrapper's import alias for the source module
// (e.g. "src_slices" for `import src_slices "golang.org/x/exp/slices"`).
func RenderInstance(inst Instance, moduleFlatName, alias string) (string, error) {
	if moduleFlatName == "" {
		return "", fmt.Errorf("%w: moduleFlatName is required", ErrMonomorphise)
	}
	if alias == "" {
		return "", fmt.Errorf("%w: alias is required", ErrMonomorphise)
	}
	if len(inst.Func.TypeParams) != len(inst.Spec.TypeArgs) {
		return "", fmt.Errorf("%w: arity mismatch: func has %d type params, spec has %d args", ErrMonomorphise, len(inst.Func.TypeParams), len(inst.Spec.TypeArgs))
	}

	// Map type-param name -> concrete type expression.
	tpMap := map[string]string{}
	for i, tp := range inst.Func.TypeParams {
		tpMap[tp.Name] = inst.Spec.TypeArgs[i]
	}

	prefix := "mochi_" + moduleFlatName + "_" + inst.Func.Name + "_" + inst.SymbolSuffix

	var sb strings.Builder
	sb.WriteString("// " + prefix + " is the monomorphised wrapper for\n")
	sb.WriteString("// " + inst.Spec.Item + "[" + strings.Join(inst.Spec.TypeArgs, ", ") + "].\n")
	sb.WriteString("// Generated by mochi MEP-74 phase 15. DO NOT EDIT.\n")
	sb.WriteString("//\n")
	sb.WriteString("//export " + prefix + "\n")
	sb.WriteString("func " + prefix + "(")
	for i, p := range inst.Func.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		name := p.Name
		if name == "" {
			name = fmt.Sprintf("p%d", i)
		}
		sb.WriteString(name + " " + concretiseType(p.Type, tpMap))
	}
	sb.WriteString(")")
	switch len(inst.Func.Results) {
	case 0:
	case 1:
		sb.WriteString(" " + concretiseType(inst.Func.Results[0].Type, tpMap))
	default:
		sb.WriteString(" (")
		for i, r := range inst.Func.Results {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(concretiseType(r.Type, tpMap))
		}
		sb.WriteString(")")
	}
	sb.WriteString(" {\n")
	// Body: `return alias.Ident[T1, T2, ...](p0, p1, ...)`
	callArgs := make([]string, len(inst.Func.Params))
	for i, p := range inst.Func.Params {
		name := p.Name
		if name == "" {
			name = fmt.Sprintf("p%d", i)
		}
		callArgs[i] = name
	}
	call := alias + "." + inst.Func.Name + "[" + strings.Join(inst.Spec.TypeArgs, ", ") + "](" + strings.Join(callArgs, ", ") + ")"
	if len(inst.Func.Results) == 0 {
		sb.WriteString("\t" + call + "\n")
	} else {
		sb.WriteString("\treturn " + call + "\n")
	}
	sb.WriteString("}\n")
	return sb.String(), nil
}

// concretiseType replaces type-parameter names in a type
// expression with their concrete type. The substitution is
// textual but identifier-bounded so a TypeParam name "T" does
// not match a longer ident like "Truthy".
func concretiseType(t string, tpMap map[string]string) string {
	if len(tpMap) == 0 || t == "" {
		return t
	}
	out := t
	// Apply longer names first so a substitution of "T" does
	// not eat the leading "T" of "TX". For Go generics, type
	// params are always single capital identifiers in practice,
	// but the rule applies in general.
	keys := make([]string, 0, len(tpMap))
	for k := range tpMap {
		keys = append(keys, k)
	}
	sort.SliceStable(keys, func(i, j int) bool {
		return len(keys[i]) > len(keys[j])
	})
	for _, k := range keys {
		out = replaceIdentBoundary(out, k, tpMap[k])
	}
	return out
}

// replaceIdentBoundary replaces every occurrence of `old`
// that's bounded by non-identifier characters with `new`. Used
// so substituting "T" in "[]T" hits the standalone T but
// substituting "T" in "TX" or "list[T2]" leaves the longer
// ident alone.
func replaceIdentBoundary(s, old, new string) string {
	if old == "" {
		return s
	}
	var sb strings.Builder
	i := 0
	for i < len(s) {
		j := strings.Index(s[i:], old)
		if j < 0 {
			sb.WriteString(s[i:])
			break
		}
		j += i
		// Left boundary.
		if j > 0 && isIdentByte(s[j-1]) {
			sb.WriteString(s[i : j+1])
			i = j + 1
			continue
		}
		// Right boundary.
		end := j + len(old)
		if end < len(s) && isIdentByte(s[end]) {
			sb.WriteString(s[i : j+1])
			i = j + 1
			continue
		}
		sb.WriteString(s[i:j])
		sb.WriteString(new)
		i = end
	}
	return sb.String()
}

func isIdentByte(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9') || b == '_'
}

// Package generics handles .NET reified generic monomorphisation for the
// MEP-68 bridge. .NET generics are reified: List<int> and List<string> are
// distinct runtime types. The bridge must explicitly enumerate which generic
// instantiations to generate wrappers for via the [dotnet.monomorphise] table
// in mochi.toml.
package generics

import (
	"fmt"
	"slices"
	"strings"

	doterrors "mochi/package3/dotnet/errors"
	"mochi/package3/dotnet/metacli"
	"mochi/package3/dotnet/shimgen"
	"mochi/package3/dotnet/typemap"
)

// MonoEntry is one explicit generic instantiation declared in mochi.toml.
type MonoEntry struct {
	// Item is the fully-qualified .NET type+method, e.g.
	// "System.Linq.Enumerable.Select" or "System.Collections.Generic.List`1"
	Item string
	// TypeArgs is the list of concrete type argument full names,
	// e.g. ["System.String", "System.Int32"]
	TypeArgs []string
}

// MonoTable is the parsed [dotnet.monomorphise] table from mochi.toml.
type MonoTable struct {
	Entries []MonoEntry
}

// ParseMonoTable parses the TOML array of inline tables used in mochi.toml:
//
//	monomorphise = [
//	    { item = "System.Linq.Enumerable.Select", type_args = ["System.String", "System.Int32"] },
//	]
//
// The parser handles simple single-line inline table arrays. Each entry must
// have an item key and a type_args key.
func ParseMonoTable(toml string) (*MonoTable, error) {
	// Find the monomorphise = [...] value.
	const key = "monomorphise"
	_, afterKey, found := strings.Cut(toml, key)
	if !found {
		// No monomorphise table: return empty.
		return &MonoTable{}, nil
	}
	rest := strings.TrimSpace(afterKey)
	if len(rest) == 0 || rest[0] != '=' {
		return nil, fmt.Errorf("generics: malformed monomorphise entry: expected '=' after key")
	}
	rest = strings.TrimSpace(rest[1:])
	// Expect '['.
	if len(rest) == 0 || rest[0] != '[' {
		return nil, fmt.Errorf("generics: monomorphise value is not an array")
	}

	// Extract the full array content (may span multiple lines).
	arrayContent, err := extractBracketContent(rest)
	if err != nil {
		return nil, fmt.Errorf("generics: extract array: %w", err)
	}

	// Split into inline table entries at top-level commas between '}'.
	entries, err := splitInlineTables(arrayContent)
	if err != nil {
		return nil, fmt.Errorf("generics: split entries: %w", err)
	}

	table := &MonoTable{}
	for i, entry := range entries {
		e, err := parseInlineEntry(entry)
		if err != nil {
			return nil, fmt.Errorf("generics: entry %d: %w", i, err)
		}
		table.Entries = append(table.Entries, e)
	}
	return table, nil
}

// extractBracketContent returns everything between the first '[' and matching ']'.
func extractBracketContent(s string) (string, error) {
	if len(s) == 0 || s[0] != '[' {
		return "", fmt.Errorf("expected '[', got %q", s)
	}
	depth := 0
	for i, c := range s {
		switch c {
		case '[', '{':
			depth++
		case ']', '}':
			depth--
			if depth == 0 {
				return s[1:i], nil
			}
		}
	}
	return "", fmt.Errorf("unmatched '['")
}

// splitInlineTables splits the content of a TOML array into individual inline
// table strings separated by top-level commas.
func splitInlineTables(content string) ([]string, error) {
	var out []string
	depth := 0
	start := 0
	for i := 0; i < len(content); i++ {
		switch content[i] {
		case '{', '[':
			depth++
		case '}', ']':
			depth--
		case ',':
			if depth == 0 {
				segment := strings.TrimSpace(content[start:i])
				if segment != "" {
					out = append(out, segment)
				}
				start = i + 1
			}
		}
	}
	// Last segment.
	if segment := strings.TrimSpace(content[start:]); segment != "" {
		out = append(out, segment)
	}
	return out, nil
}

// parseInlineEntry parses one TOML inline table like:
// { item = "System.Linq.Enumerable.Select", type_args = ["System.String"] }
func parseInlineEntry(s string) (MonoEntry, error) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "{") || !strings.HasSuffix(s, "}") {
		return MonoEntry{}, fmt.Errorf("expected inline table { ... }, got %q", s)
	}
	inner := s[1 : len(s)-1]

	// Split by top-level commas.
	parts := splitAtTopLevelCommas(inner)
	var e MonoEntry
	for _, part := range parts {
		k, v, ok := strings.Cut(part, "=")
		if !ok {
			return MonoEntry{}, fmt.Errorf("missing '=' in key-value pair %q", part)
		}
		k = strings.TrimSpace(k)
		v = strings.TrimSpace(v)
		switch k {
		case "item":
			s, err := parseTomlString(v)
			if err != nil {
				return MonoEntry{}, fmt.Errorf("item: %w", err)
			}
			e.Item = s
		case "type_args":
			args, err := parseTomlStringArray(v)
			if err != nil {
				return MonoEntry{}, fmt.Errorf("type_args: %w", err)
			}
			e.TypeArgs = args
		}
	}
	if e.Item == "" {
		return MonoEntry{}, fmt.Errorf("missing required key 'item'")
	}
	if len(e.TypeArgs) == 0 {
		return MonoEntry{}, fmt.Errorf("missing required key 'type_args' or empty for item %q", e.Item)
	}
	return e, nil
}

// splitAtTopLevelCommas splits s at commas that are not inside brackets or braces.
func splitAtTopLevelCommas(s string) []string {
	var out []string
	depth := 0
	inStr := false
	start := 0
	for i := 0; i < len(s); i++ {
		c := s[i]
		if inStr {
			if c == '"' && (i == 0 || s[i-1] != '\\') {
				inStr = false
			}
			continue
		}
		switch c {
		case '"':
			inStr = true
		case '{', '[':
			depth++
		case '}', ']':
			depth--
		case ',':
			if depth == 0 {
				out = append(out, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if last := strings.TrimSpace(s[start:]); last != "" {
		out = append(out, last)
	}
	return out
}

// parseTomlString extracts the string value from a TOML quoted string.
func parseTomlString(v string) (string, error) {
	v = strings.TrimSpace(v)
	if len(v) < 2 || v[0] != '"' || v[len(v)-1] != '"' {
		return "", fmt.Errorf("expected quoted string, got %q", v)
	}
	return v[1 : len(v)-1], nil
}

// parseTomlStringArray parses a TOML string array like ["a", "b", "c"].
func parseTomlStringArray(v string) ([]string, error) {
	v = strings.TrimSpace(v)
	if !strings.HasPrefix(v, "[") || !strings.HasSuffix(v, "]") {
		return nil, fmt.Errorf("expected array [..], got %q", v)
	}
	inner := strings.TrimSpace(v[1 : len(v)-1])
	if inner == "" {
		return nil, nil
	}
	parts := splitAtTopLevelCommas(inner)
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		s, err := parseTomlString(p)
		if err != nil {
			return nil, fmt.Errorf("array element: %w", err)
		}
		out = append(out, s)
	}
	return out, nil
}

// Instantiate checks whether an Item has an entry in the MonoTable.
// Returns the concrete TypeArgs if found, nil if not.
func (t *MonoTable) Instantiate(item string) ([]string, bool) {
	if t == nil {
		return nil, false
	}
	for _, e := range t.Entries {
		if e.Item == item {
			return e.TypeArgs, true
		}
	}
	return nil, false
}

// MonomorphiseMethod attempts to produce a concrete shimgen.ShimMethod for a
// generic MethodDef by substituting TypeArgs from the MonoTable.
// Returns (method, true) if the method was successfully monomorphised,
// or (zero, false) if no matching entry exists or substitution fails.
func MonomorphiseMethod(
	typeName, methodName string,
	method metacli.MethodDef,
	table *MonoTable,
	dir typemap.Direction,
) (shimgen.ShimMethod, bool) {
	fullItem := typeName + "." + methodName
	typeArgs, ok := table.Instantiate(fullItem)
	if !ok {
		return shimgen.ShimMethod{}, false
	}
	if len(typeArgs) == 0 {
		return shimgen.ShimMethod{}, false
	}

	// Substitute the type args into the method's generic parameters.
	// Build a substitution map from the method's TypeParams.
	subst := map[string]string{}
	for i, tp := range method.TypeParams {
		if i < len(typeArgs) {
			subst[tp] = typeArgs[i]
		}
	}

	// Build the ShimMethod params.
	params := make([]shimgen.ShimParam, 0, len(method.Parameters))
	for _, p := range method.Parameters {
		typeRef := substituteTypeRef(p.Type, subst)
		mapping, sr, _ := typemap.Map(typeRef, dir)
		if sr != doterrors.SkipUnknown {
			return shimgen.ShimMethod{}, false
		}
		params = append(params, shimgen.ShimParam{
			Name:    p.Name,
			Mapping: *mapping,
		})
	}

	// Map return type.
	retRef := substituteTypeRef(method.ReturnType, subst)
	retMapping, sr, _ := typemap.Map(retRef, typemap.DirectionOut)
	if sr != doterrors.SkipUnknown {
		return shimgen.ShimMethod{}, false
	}
	var finalReturn *typemap.Mapping
	if retMapping != nil && retMapping.Kind != typemap.KindUnit {
		finalReturn = retMapping
	}

	// Build a mangled method name that includes the type args.
	mangledArgs := strings.Join(typeArgs, "_")
	mangledArgs = sanitizeForIdent(mangledArgs)
	csMethodName := shortName(typeName) + "_" + methodName + "_" + mangledArgs

	entryPoint := "mochi_" + sanitizeForIdent(typeName) + "_" + sanitizeForIdent(methodName) + "_" + mangledArgs

	return shimgen.ShimMethod{
		EntryPoint:   entryPoint,
		CSMethodName: csMethodName,
		UpstreamCall: typeName + "." + methodName,
		Params:       params,
		Return:       finalReturn,
		IsAsync:      method.IsAsync,
		IsStatic:     method.IsStatic,
	}, true
}

// substituteTypeRef replaces TypeRefGenericParam references with concrete types.
func substituteTypeRef(ref metacli.TypeRef, subst map[string]string) metacli.TypeRef {
	if ref.Kind == metacli.TypeRefGenericParam {
		if concrete, ok := subst[ref.FullName]; ok {
			return metacli.TypeRef{
				FullName: concrete,
				Kind:     metacli.TypeRefClass,
			}
		}
	}
	// Substitute recursively in TypeArgs.
	if len(ref.TypeArgs) > 0 {
		newArgs := make([]metacli.TypeRef, len(ref.TypeArgs))
		for i, a := range ref.TypeArgs {
			newArgs[i] = substituteTypeRef(a, subst)
		}
		ref.TypeArgs = newArgs
	}
	return ref
}

// ApplyToSurface rewrites an ApiSurface by attempting to monomorphise all
// generic methods that have entries in the MonoTable. Non-generic methods are
// passed through unchanged. Generic methods without a MonoTable entry
// accumulate SkipReport entries in the returned slice.
func ApplyToSurface(surface *metacli.ApiSurface, table *MonoTable) (*metacli.ApiSurface, []doterrors.SkipReport) {
	if surface == nil {
		return &metacli.ApiSurface{}, nil
	}
	var skipped []doterrors.SkipReport
	out := &metacli.ApiSurface{
		Assembly:        surface.Assembly,
		Version:         surface.Version,
		TargetFramework: surface.TargetFramework,
	}

	for _, st := range surface.Types {
		newType := metacli.SurfaceType{
			FullName:   st.FullName,
			ShortName:  st.ShortName,
			Namespace:  st.Namespace,
			Kind:       st.Kind,
			IsAbstract: st.IsAbstract,
			IsStatic:   st.IsStatic,
		}

		for _, m := range st.Methods {
			// Find the MethodDef from the surface for generic processing.
			if !isGenericMethod(m) {
				// Non-generic: pass through.
				newType.Methods = append(newType.Methods, m)
				continue
			}

			// Generic method: look up in MonoTable.
			fullItem := st.FullName + "." + m.Name
			if _, ok := table.Instantiate(fullItem); !ok {
				skipped = append(skipped, doterrors.SkipReport{
					ItemPath: fullItem,
					Reason:   doterrors.SkipGeneric,
					Detail:   fmt.Sprintf("generic method %s has no monomorphise entry", fullItem),
					Override: fmt.Sprintf("add { item = %q, type_args = [\"T\"] } under [dotnet.monomorphise]", fullItem),
				})
				continue
			}

			// Has a MonoTable entry: include it but note it still appears in
			// the surface (actual shim generation via MonomorphiseMethod is
			// the caller's responsibility). We keep the method as-is in the
			// surface to allow callers to then invoke MonomorphiseMethod.
			newType.Methods = append(newType.Methods, m)
		}

		// Properties pass through unchanged.
		newType.Properties = append(newType.Properties, st.Properties...)
		out.Types = append(out.Types, newType)
	}

	return out, skipped
}

// isGenericMethod reports whether a SurfaceMethod is generic.
// We check the return type and parameter types for generic params.
func isGenericMethod(m metacli.SurfaceMethod) bool {
	if containsGenericParam(m.ReturnType) {
		return true
	}
	for _, p := range m.Params {
		if containsGenericParam(p.Type) {
			return true
		}
	}
	return false
}

// containsGenericParam reports whether a TypeRef contains a generic parameter.
func containsGenericParam(ref metacli.TypeRef) bool {
	if ref.Kind == metacli.TypeRefGenericParam {
		return true
	}
	return slices.ContainsFunc(ref.TypeArgs, containsGenericParam)
}

// sanitizeForIdent converts a CLR full name to a safe identifier fragment.
func sanitizeForIdent(s string) string {
	var b strings.Builder
	for _, r := range s {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9', r == '_':
			b.WriteRune(r)
		case r >= 'A' && r <= 'Z':
			b.WriteRune(r + 32) // tolower
		case r == '.', r == ',', r == ' ', r == '[', r == ']':
			b.WriteRune('_')
		case r == '`':
			// drop backtick arity
		default:
			b.WriteRune('_')
		}
	}
	return b.String()
}

// shortName returns the last dot-separated segment of a CLR full name.
func shortName(full string) string {
	if i := strings.LastIndex(full, "."); i >= 0 {
		return full[i+1:]
	}
	return full
}

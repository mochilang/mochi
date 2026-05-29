package shimgen

import (
	"fmt"
	"strings"
	"unicode"

	"mochi/package3/dotnet/errors"
	"mochi/package3/dotnet/metacli"
	"mochi/package3/dotnet/typemap"
)

// Synth synthesises a Shim from a metacli.ApiSurface. It walks each
// SurfaceType's Methods and Properties, applies typemap.Map to each parameter
// and return type, and emits a ShimMethod for every translatable item.
// Items that cannot be translated are recorded in Shim.Skipped.
func Synth(pkg, version, targetFramework string, surface *metacli.ApiSurface) *Shim {
	s := &Shim{
		Package:         pkg,
		PackageVersion:  version,
		TargetFramework: targetFramework,
	}
	if surface == nil {
		return s
	}
	for _, st := range surface.Types {
		// Skip abstract non-static classes: no concrete factory to call.
		if st.IsAbstract && !st.IsStatic {
			s.Skipped = append(s.Skipped, errors.SkipReport{
				ItemPath: st.FullName,
				Reason:   errors.SkipAbstract,
				Detail:   fmt.Sprintf("abstract class %s has no concrete factory in v1", st.FullName),
			})
			continue
		}
		for _, m := range st.Methods {
			sm, skip := synthMethod(pkg, st, m)
			if skip != nil {
				s.Skipped = append(s.Skipped, *skip)
				continue
			}
			s.Methods = append(s.Methods, *sm)
		}
		for _, p := range st.Properties {
			if !p.HasGetter {
				continue
			}
			sm, skip := synthPropertyGetter(pkg, st, p)
			if skip != nil {
				s.Skipped = append(s.Skipped, *skip)
				continue
			}
			s.Methods = append(s.Methods, *sm)
		}
	}
	return s
}

// synthMethod synthesises a ShimMethod from a SurfaceMethod.
func synthMethod(pkg string, st metacli.SurfaceType, m metacli.SurfaceMethod) (*ShimMethod, *errors.SkipReport) {
	itemPath := st.FullName + "." + m.Name

	// Map each parameter.
	params := make([]ShimParam, 0, len(m.Params))
	for i, p := range m.Params {
		if p.IsOut || p.IsRef {
			return nil, &errors.SkipReport{
				ItemPath: itemPath,
				Reason:   errors.SkipRefType,
				Detail:   fmt.Sprintf("param %d (%s): ref/out parameter requires special marshalling", i, p.Name),
			}
		}
		mapping, sr, det := typemap.Map(p.Type, typemap.DirectionIn)
		if sr != errors.SkipUnknown || det != "" {
			return nil, &errors.SkipReport{
				ItemPath: itemPath,
				Reason:   sr,
				Detail:   fmt.Sprintf("param %d (%s): %s", i, p.Name, det),
			}
		}
		name := p.Name
		if name == "" {
			name = fmt.Sprintf("arg%d", i)
		}
		params = append(params, ShimParam{Name: name, Mapping: *mapping})
	}

	// Map the return type.
	retMapping, sr, det := typemap.Map(m.ReturnType, typemap.DirectionOut)
	if sr != errors.SkipUnknown || det != "" {
		return nil, &errors.SkipReport{
			ItemPath: itemPath,
			Reason:   sr,
			Detail:   fmt.Sprintf("return: %s", det),
		}
	}

	// Detect async: if the return is Task or Task<T>.
	isAsync := m.IsAsync
	var finalReturn *typemap.Mapping
	if retMapping != nil && retMapping.Kind != typemap.KindUnit {
		if retMapping.Kind == typemap.KindTask {
			isAsync = true
			// Unwrap the inner type.
			if retMapping.Inner != nil {
				finalReturn = retMapping.Inner
			}
			// else void (non-generic Task)
		} else {
			finalReturn = retMapping
		}
	}
	// Also check the raw return type for Task (non-generic).
	if m.ReturnType.FullName == "System.Threading.Tasks.Task" ||
		m.ReturnType.FullName == "System.Threading.Tasks.Task`1" {
		isAsync = true
		if retMapping != nil && retMapping.Kind == typemap.KindTask && retMapping.Inner != nil {
			finalReturn = retMapping.Inner
		}
	}

	entryPoint := buildEntryPoint(pkg, st.FullName, m.Name)
	csMethodName := st.ShortName + "_" + m.Name
	upstreamCall := st.FullName + "." + m.Name

	return &ShimMethod{
		EntryPoint:   entryPoint,
		CSMethodName: csMethodName,
		UpstreamCall: upstreamCall,
		Params:       params,
		Return:       finalReturn,
		IsAsync:      isAsync,
		IsStatic:     m.IsStatic,
		DocComment:   stripXML(m.XmlDoc),
	}, nil
}

// synthPropertyGetter synthesises a ShimMethod for a property getter.
func synthPropertyGetter(pkg string, st metacli.SurfaceType, p metacli.SurfaceProp) (*ShimMethod, *errors.SkipReport) {
	itemPath := st.FullName + "." + p.Name

	mapping, sr, det := typemap.Map(p.Type, typemap.DirectionOut)
	if sr != errors.SkipUnknown || det != "" {
		return nil, &errors.SkipReport{
			ItemPath: itemPath,
			Reason:   sr,
			Detail:   fmt.Sprintf("property getter return: %s", det),
		}
	}

	var finalReturn *typemap.Mapping
	if mapping != nil && mapping.Kind != typemap.KindUnit {
		finalReturn = mapping
	}

	getterName := "get_" + p.Name
	entryPoint := buildEntryPoint(pkg, st.FullName, getterName)
	csMethodName := st.ShortName + "_get_" + p.Name
	upstreamCall := st.FullName + "." + p.Name

	return &ShimMethod{
		EntryPoint:   entryPoint,
		CSMethodName: csMethodName,
		UpstreamCall: upstreamCall,
		Params:       nil,
		Return:       finalReturn,
		IsAsync:      false,
		IsStatic:     p.IsStatic,
		DocComment:   stripXML(p.XmlDoc),
	}, nil
}

// buildEntryPoint constructs the canonical C symbol entry-point name.
// Format: "mochi_" + sanitize(pkg) + "_" + sanitize(typeName) + "_" + sanitize(methodName)
// all lowercase with underscores.
func buildEntryPoint(pkg, fullTypeName, methodName string) string {
	return "mochi_" + sanitize(pkg) + "_" + sanitize(fullTypeName) + "_" + sanitize(methodName)
}

// sanitize lowercases a string, removes dots and backticks and spaces,
// and replaces hyphens with underscores. Used for entry-point mangling.
func sanitize(s string) string {
	var b strings.Builder
	for _, r := range s {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9':
			b.WriteRune(r)
		case r >= 'A' && r <= 'Z':
			b.WriteRune(unicode.ToLower(r))
		case r == '-':
			b.WriteRune('_')
		case r == '.', r == '`', r == ' ', r == '+':
			// drop
		default:
			b.WriteRune('_')
		}
	}
	return b.String()
}

// sanitizeIdent sanitizes a single identifier segment (no dots).
func sanitizeIdent(s string) string {
	var b strings.Builder
	for _, r := range s {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9', r == '_':
			b.WriteRune(r)
		case r >= 'A' && r <= 'Z':
			b.WriteRune(unicode.ToLower(r))
		default:
			b.WriteRune('_')
		}
	}
	return b.String()
}

// sanitizeDotted returns a dot-free lowercase identifier from a potentially
// dotted name (e.g. "JsonConvert" stays "JsonConvert" as is, no dots to strip).
func sanitizeDotted(s string) string {
	return s
}

// stripXML removes XML doc comment tags, returning plain text.
func stripXML(doc string) string {
	if doc == "" {
		return ""
	}
	var b strings.Builder
	inTag := false
	for _, r := range doc {
		switch {
		case r == '<':
			inTag = true
		case r == '>':
			inTag = false
		case !inTag:
			b.WriteRune(r)
		}
	}
	return strings.TrimSpace(b.String())
}

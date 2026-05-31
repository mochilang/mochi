// Package typemap implements the closed TypeScript-to-Mochi type table.
// Lands in MEP-72 Phase 5.
package typemap

import (
	"strings"

	"mochi/package3/typescript/apisurface"
	brerrors "mochi/package3/typescript/errors"
)

// MochiType is a Mochi type string.
type MochiType string

const (
	TypeFloat  MochiType = "float"
	TypeInt    MochiType = "int"
	TypeString MochiType = "string"
	TypeBool   MochiType = "bool"
	TypeUnit   MochiType = "unit"
	TypeAny    MochiType = "any"
	TypeAsync  MochiType = "async"
	TypeStream MochiType = "stream"
)

// MapResult holds the Mochi type or a skip reason for a TypeScript type.
type MapResult struct {
	Type      MochiType
	Container string // "list", "map", "set", "option", "async", "stream", or ""
	Inner     []MochiType
	Skipped   bool
	Reason    brerrors.SkipReason
}

// Mapped is a successfully mapped type.
func Mapped(t MochiType) MapResult {
	return MapResult{Type: t}
}

// Skipped is a skipped type.
func Skipped(r brerrors.SkipReason) MapResult {
	return MapResult{Skipped: true, Reason: r}
}

// MapType maps a TypeScript type string to a Mochi type.
// paramName is used for the number→int heuristic.
func MapType(tsType string, paramName string) MapResult {
	tsType = strings.TrimSpace(tsType)

	switch tsType {
	case "string":
		return Mapped(TypeString)
	case "boolean":
		return Mapped(TypeBool)
	case "bigint":
		return Mapped(TypeInt)
	case "void":
		return Mapped(TypeUnit)
	case "null", "undefined":
		return Mapped(TypeUnit)
	case "any":
		return Skipped(brerrors.SkipAnyType)
	case "unknown":
		return Skipped(brerrors.SkipUnknownType)
	case "never":
		return Skipped(brerrors.SkipNeverType)
	case "symbol", "unique symbol":
		return Skipped(brerrors.SkipSymbol)
	case "number":
		if isIntegerParamName(paramName) {
			return Mapped(TypeInt)
		}
		return Mapped(TypeFloat)
	}

	// Template literal types: contain `${`
	if strings.Contains(tsType, "${") {
		return Skipped(brerrors.SkipTemplateLiteral)
	}

	// Promise<T>
	if strings.HasPrefix(tsType, "Promise<") && strings.HasSuffix(tsType, ">") {
		inner := tsType[len("Promise<") : len(tsType)-1]
		innerResult := MapType(inner, "")
		if innerResult.Skipped {
			return innerResult
		}
		return MapResult{Type: TypeAsync, Container: "async", Inner: []MochiType{innerResult.Type}}
	}

	// AsyncIterable<T>
	if strings.HasPrefix(tsType, "AsyncIterable<") && strings.HasSuffix(tsType, ">") {
		inner := tsType[len("AsyncIterable<") : len(tsType)-1]
		innerResult := MapType(inner, "")
		if innerResult.Skipped {
			return innerResult
		}
		return MapResult{Type: TypeStream, Container: "stream", Inner: []MochiType{innerResult.Type}}
	}

	// Array<T> or T[]
	if strings.HasPrefix(tsType, "Array<") && strings.HasSuffix(tsType, ">") {
		inner := tsType[len("Array<") : len(tsType)-1]
		innerResult := MapType(inner, "")
		if innerResult.Skipped {
			return innerResult
		}
		return MapResult{Type: "list", Container: "list", Inner: []MochiType{innerResult.Type}}
	}
	if strings.HasPrefix(tsType, "ReadonlyArray<") && strings.HasSuffix(tsType, ">") {
		inner := tsType[len("ReadonlyArray<") : len(tsType)-1]
		innerResult := MapType(inner, "")
		if innerResult.Skipped {
			return innerResult
		}
		return MapResult{Type: "list", Container: "list", Inner: []MochiType{innerResult.Type}}
	}
	if strings.HasSuffix(tsType, "[]") {
		inner := tsType[:len(tsType)-2]
		innerResult := MapType(inner, "")
		if innerResult.Skipped {
			return innerResult
		}
		return MapResult{Type: "list", Container: "list", Inner: []MochiType{innerResult.Type}}
	}

	// Map<K, V>
	if strings.HasPrefix(tsType, "Map<") && strings.HasSuffix(tsType, ">") {
		kv := tsType[len("Map<") : len(tsType)-1]
		parts := splitTypeArgs(kv)
		if len(parts) == 2 {
			kr := MapType(strings.TrimSpace(parts[0]), "")
			vr := MapType(strings.TrimSpace(parts[1]), "")
			if kr.Skipped {
				return kr
			}
			if vr.Skipped {
				return vr
			}
			return MapResult{Type: "map", Container: "map", Inner: []MochiType{kr.Type, vr.Type}}
		}
	}

	// Set<T>
	if strings.HasPrefix(tsType, "Set<") && strings.HasSuffix(tsType, ">") {
		inner := tsType[len("Set<") : len(tsType)-1]
		innerResult := MapType(inner, "")
		if innerResult.Skipped {
			return innerResult
		}
		return MapResult{Type: "set", Container: "set", Inner: []MochiType{innerResult.Type}}
	}

	// T | null or T | undefined → option[T]
	if strings.Contains(tsType, " | ") {
		parts := strings.Split(tsType, " | ")
		var nonNull []string
		for _, p := range parts {
			p = strings.TrimSpace(p)
			if p != "null" && p != "undefined" {
				nonNull = append(nonNull, p)
			}
		}
		if len(nonNull) == 1 {
			inner := MapType(nonNull[0], paramName)
			if inner.Skipped {
				return inner
			}
			return MapResult{Type: inner.Type, Container: "option", Inner: []MochiType{inner.Type}}
		}
		// Complex union — skip
		return Skipped(brerrors.SkipUnknownType)
	}

	// Generic type parameter (single uppercase letter or T/U/V pattern)
	if isGenericParam(tsType) {
		return Skipped(brerrors.SkipGeneric)
	}

	// Conditional type
	if strings.Contains(tsType, " extends ") && strings.Contains(tsType, "?") {
		return Skipped(brerrors.SkipConditional)
	}

	// Mapped type { [K in ...]: ... }
	if strings.HasPrefix(tsType, "{") && strings.Contains(tsType, " in ") {
		return Skipped(brerrors.SkipMappedType)
	}

	// Intersection
	if strings.Contains(tsType, " & ") {
		return Skipped(brerrors.SkipIntersection)
	}

	// Unknown complex type
	return Skipped(brerrors.SkipUnknownType)
}

func isIntegerParamName(name string) bool {
	lower := strings.ToLower(name)
	suffixes := []string{"count", "length", "index", "size", "offset", "capacity", "limit", "start", "end"}
	for _, s := range suffixes {
		if strings.HasSuffix(lower, s) {
			return true
		}
	}
	return false
}

func isGenericParam(s string) bool {
	if len(s) == 1 {
		return s[0] >= 'A' && s[0] <= 'Z'
	}
	// Common generic names
	generics := []string{"T", "U", "V", "K", "R", "E", "TKey", "TValue", "TResult", "TError"}
	for _, g := range generics {
		if s == g {
			return true
		}
	}
	return false
}

// splitTypeArgs splits "K, V" respecting angle bracket nesting.
func splitTypeArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, c := range s {
		switch c {
		case '<':
			depth++
		case '>':
			depth--
		case ',':
			if depth == 0 {
				parts = append(parts, s[start:i])
				start = i + 1
			}
		}
	}
	parts = append(parts, s[start:])
	return parts
}

// MappedExport is an export with its mapped Mochi types resolved.
type MappedExport struct {
	Export     apisurface.Export
	ModulePath string
	MochiName  string
	ParamTypes []MapResult
	ReturnType MapResult
	Skipped    bool
	SkipReport brerrors.SkipReport
}

// MapSurface maps all exports in an ApiSurface to Mochi types.
func MapSurface(surface *apisurface.ApiSurface) (mapped []MappedExport, skipped []brerrors.SkipReport) {
	pkgName := sanitizePkgName(surface.PackageName)
	for modPath, exports := range surface.Exports {
		for _, exp := range exports {
			me := MappedExport{
				Export:     exp,
				ModulePath: modPath,
				MochiName:  "ts_" + pkgName + "_" + exp.Name,
			}

			// Skip deprecated/internal/browser immediately.
			if exp.Deprecated {
				sr := brerrors.SkipReport{ItemPath: exp.Name, Reason: brerrors.SkipDeprecated}
				skipped = append(skipped, sr)
				continue
			}
			if exp.Internal {
				sr := brerrors.SkipReport{ItemPath: exp.Name, Reason: brerrors.SkipInternal}
				skipped = append(skipped, sr)
				continue
			}
			if exp.BrowserOnly {
				sr := brerrors.SkipReport{ItemPath: exp.Name, Reason: brerrors.SkipBrowser}
				skipped = append(skipped, sr)
				continue
			}
			if len(exp.TypeParams) > 0 {
				sr := brerrors.SkipReport{ItemPath: exp.Name, Reason: brerrors.SkipGeneric}
				skipped = append(skipped, sr)
				continue
			}

			// Map return type.
			me.ReturnType = MapType(exp.ReturnType, "")
			if me.ReturnType.Skipped {
				sr := brerrors.SkipReport{ItemPath: exp.Name, Reason: me.ReturnType.Reason}
				skipped = append(skipped, sr)
				me.Skipped = true
				me.SkipReport = sr
				continue
			}

			// Map param types.
			skip := false
			for _, p := range exp.Params {
				r := MapType(p.TypeStr, p.Name)
				me.ParamTypes = append(me.ParamTypes, r)
				if r.Skipped {
					sr := brerrors.SkipReport{ItemPath: exp.Name, Reason: r.Reason, Detail: p.Name}
					skipped = append(skipped, sr)
					me.Skipped = true
					me.SkipReport = sr
					skip = true
					break
				}
			}
			if skip {
				continue
			}

			mapped = append(mapped, me)
		}
	}
	return mapped, skipped
}

func sanitizePkgName(name string) string {
	// @scope/pkg → scope_pkg; lodash → lodash
	name = strings.TrimPrefix(name, "@")
	name = strings.ReplaceAll(name, "/", "_")
	name = strings.ReplaceAll(name, "-", "_")
	name = strings.ReplaceAll(name, ".", "_")
	return name
}

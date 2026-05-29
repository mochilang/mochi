// Package externemit generates the Mochi .mochi shim files that declare
// extern fn and extern type bindings for each NuGet package's translated
// surface. These shims are imported by user Mochi source via
// `import dotnet "<pkg>" as <alias>` after the MEP-53 emit pass rewrites the
// import to `import "./dotnet_wrap/<pkg>/shim.mochi" as <alias>`.
package externemit

import (
	"fmt"
	"strings"

	"mochi/package3/dotnet/typemap"
)

// ShimFile holds the generated Mochi shim for one NuGet package.
type ShimFile struct {
	PackageID string
	Body      string
}

// Emit generates the Mochi extern fn / extern type shim file for the given
// TranslatedSurface. The shim declares every translated item so the Mochi
// type checker and code generator can resolve `<alias>.<Item>` references.
func Emit(surface *typemap.TranslatedSurface) *ShimFile {
	var b strings.Builder
	b.WriteString(fmt.Sprintf("// Auto-generated extern shim for NuGet package %q.\n", surface.PackageID))
	b.WriteString("// MEP-68. Do not edit by hand; regenerate with `mochi pkg sync dotnet`.\n\n")

	// Emit extern type declarations first.
	emitted := map[string]bool{}
	for _, item := range surface.Items {
		if item.Kind == typemap.ItemType {
			name := item.ReturnType.Name
			if name == "" {
				continue
			}
			if emitted[name] {
				continue
			}
			emitted[name] = true
			if item.DocSummary != "" {
				b.WriteString("/// " + item.DocSummary + "\n")
			}
			switch item.ReturnType.Kind {
			case typemap.MochiExtern:
				b.WriteString(fmt.Sprintf("extern type %s\n\n", name))
			case typemap.MochiRecord:
				b.WriteString(fmt.Sprintf("extern record %s {}\n\n", name))
			case typemap.MochiADT:
				b.WriteString(fmt.Sprintf("extern type %s\n\n", name))
			}
		}
	}

	// Emit extern fn declarations.
	for _, item := range surface.Items {
		switch item.Kind {
		case typemap.ItemFn, typemap.ItemMethod, typemap.ItemConstructor, typemap.ItemFreeSymbol:
			emitFn(&b, item)
		}
	}

	return &ShimFile{
		PackageID: surface.PackageID,
		Body:      b.String(),
	}
}

func emitFn(b *strings.Builder, item typemap.TranslatedItem) {
	if item.DocSummary != "" {
		b.WriteString("/// " + item.DocSummary + "\n")
	}
	b.WriteString(fmt.Sprintf("extern fn %s(", item.MochiName))

	var params []string
	if item.ReceiverType != nil {
		params = append(params, "self: "+mochiTypeName(*item.ReceiverType))
	}
	for _, p := range item.Params {
		params = append(params, p.Name+": "+mochiTypeName(p.Type))
	}
	b.WriteString(strings.Join(params, ", "))
	b.WriteString("): ")
	b.WriteString(mochiTypeName(item.ReturnType))

	if item.Kind != typemap.ItemFreeSymbol {
		b.WriteString(fmt.Sprintf(" = \"%s\"", item.Symbol))
	} else {
		b.WriteString(fmt.Sprintf(" = \"%s\"", item.Symbol))
	}
	b.WriteString("\n\n")
}

func mochiTypeName(t typemap.MochiType) string {
	switch t.Kind {
	case typemap.MochiUnit:
		return "unit"
	case typemap.MochiBool:
		return "bool"
	case typemap.MochiInt:
		return "int"
	case typemap.MochiByte:
		return "byte"
	case typemap.MochiLong:
		return "long"
	case typemap.MochiFloat:
		return "float"
	case typemap.MochiDecimal:
		return "decimal"
	case typemap.MochiString:
		return "string"
	case typemap.MochiList:
		if t.Elem != nil {
			return "list<" + mochiTypeName(*t.Elem) + ">"
		}
		return "list<any>"
	case typemap.MochiSet:
		if t.Elem != nil {
			return "set<" + mochiTypeName(*t.Elem) + ">"
		}
		return "set<any>"
	case typemap.MochiOSet:
		if t.Elem != nil {
			return "oset<" + mochiTypeName(*t.Elem) + ">"
		}
		return "oset<any>"
	case typemap.MochiMap:
		if t.Key != nil && t.Value != nil {
			return "map<" + mochiTypeName(*t.Key) + ", " + mochiTypeName(*t.Value) + ">"
		}
		return "map<string, any>"
	case typemap.MochiOMap:
		if t.Key != nil && t.Value != nil {
			return "omap<" + mochiTypeName(*t.Key) + ", " + mochiTypeName(*t.Value) + ">"
		}
		return "omap<string, any>"
	case typemap.MochiOptional:
		if t.Elem != nil {
			return mochiTypeName(*t.Elem) + "|nil"
		}
		return "any|nil"
	case typemap.MochiAsync:
		if t.Elem != nil {
			return "async " + mochiTypeName(*t.Elem)
		}
		return "async unit"
	case typemap.MochiExtern, typemap.MochiRecord, typemap.MochiADT:
		return t.Name
	case typemap.MochiFn:
		return "fn"
	case typemap.MochiTuple:
		return "tuple"
	default:
		return "any"
	}
}

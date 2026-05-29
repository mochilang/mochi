package pyodide

import (
	"fmt"
	"sort"
	"strings"
)

// WITType is one node in the type half of the WIT IDL the WASI
// component model uses. Only the variants the Mochi <-> Python type
// table emits today are listed; future predicates (resource, stream,
// future, list-of-resource) will land in sub-phase 16.1 as the
// wrapper synthesiser grows.
type WITType struct {
	Kind     WITKind
	Name     string   // record name (for KindRef / KindRecord)
	ListOf   *WITType // element type (KindList)
	Optional *WITType // inner type (KindOption)
}

// WITKind enumerates the WIT primitives + composites Phase 16 emits.
type WITKind int

const (
	WITUnknown WITKind = iota
	WITBool
	WITS8
	WITS16
	WITS32
	WITS64
	WITU8
	WITU16
	WITU32
	WITU64
	WITF32
	WITF64
	WITChar
	WITString
	WITList
	WITOption
	WITRecord
	WITRef
)

// Render emits the WIT type fragment for t. The emitter is total over
// the kinds it claims to support; unknown kinds panic so a renderer
// bug fails loudly during the umbrella gate.
func (t WITType) Render() string {
	switch t.Kind {
	case WITBool:
		return "bool"
	case WITS8:
		return "s8"
	case WITS16:
		return "s16"
	case WITS32:
		return "s32"
	case WITS64:
		return "s64"
	case WITU8:
		return "u8"
	case WITU16:
		return "u16"
	case WITU32:
		return "u32"
	case WITU64:
		return "u64"
	case WITF32:
		return "f32"
	case WITF64:
		return "f64"
	case WITChar:
		return "char"
	case WITString:
		return "string"
	case WITRef:
		if t.Name == "" {
			panic("pyodide: WITRef must carry Name")
		}
		return t.Name
	case WITList:
		if t.ListOf == nil {
			panic("pyodide: WITList must carry ListOf")
		}
		return "list<" + t.ListOf.Render() + ">"
	case WITOption:
		if t.Optional == nil {
			panic("pyodide: WITOption must carry Optional")
		}
		return "option<" + t.Optional.Render() + ">"
	default:
		panic(fmt.Sprintf("pyodide: unhandled WIT kind %d", t.Kind))
	}
}

// WITField is one field of a WITRecord.
type WITField struct {
	Name string
	Type WITType
}

// WITRecordDecl is a top-level record declaration in a world.
type WITRecordDecl struct {
	Name   string
	Fields []WITField
}

// Render emits the `record <name> { <fields> }` block.
func (r WITRecordDecl) Render() string {
	var b strings.Builder
	fmt.Fprintf(&b, "  record %s {\n", r.Name)
	for _, f := range r.Fields {
		fmt.Fprintf(&b, "    %s: %s,\n", f.Name, f.Type.Render())
	}
	b.WriteString("  }\n")
	return b.String()
}

// WITFunc is one exported (or imported) function. Params is in
// declaration order. Return is the WIT result type; KindUnknown
// means the function returns nothing.
type WITFunc struct {
	Name   string
	Params []WITField
	Return WITType
}

// Render emits a single `<name>: func(<params>) -> <return>` line.
// Functions with Return.Kind == WITUnknown omit the result clause.
func (f WITFunc) Render() string {
	parts := make([]string, len(f.Params))
	for i, p := range f.Params {
		parts[i] = fmt.Sprintf("%s: %s", p.Name, p.Type.Render())
	}
	line := fmt.Sprintf("%s: func(%s)", f.Name, strings.Join(parts, ", "))
	if f.Return.Kind != WITUnknown {
		line += " -> " + f.Return.Render()
	}
	return line
}

// WITWorld is the unit the host imports / exports against. World
// names follow WIT IDL casing (kebab-case).
type WITWorld struct {
	Package string // "mochi:py-bridge"
	Name    string // world identifier
	Records []WITRecordDecl
	Imports []WITFunc
	Exports []WITFunc
}

// Validate enforces the WIT identifier rules Phase 16 emits against.
// Names must be non-empty kebab-case (ASCII lowercase + digits +
// hyphens, starting with a letter). Records can collide on name only
// if their fields match; Validate checks for clashes.
func (w WITWorld) Validate() error {
	if w.Package == "" {
		return fmt.Errorf("pyodide: WITWorld Package must be non-empty")
	}
	if !isKebab(w.Name) {
		return fmt.Errorf("pyodide: WITWorld Name %q is not kebab-case", w.Name)
	}
	seenRecord := map[string]struct{}{}
	for _, r := range w.Records {
		if !isKebab(r.Name) {
			return fmt.Errorf("pyodide: record %q is not kebab-case", r.Name)
		}
		if _, dup := seenRecord[r.Name]; dup {
			return fmt.Errorf("pyodide: duplicate record %q", r.Name)
		}
		seenRecord[r.Name] = struct{}{}
		for _, f := range r.Fields {
			if !isKebab(f.Name) {
				return fmt.Errorf("pyodide: record %q field %q is not kebab-case", r.Name, f.Name)
			}
		}
	}
	seenFn := map[string]struct{}{}
	for _, f := range append(append([]WITFunc{}, w.Imports...), w.Exports...) {
		if !isKebab(f.Name) {
			return fmt.Errorf("pyodide: function %q is not kebab-case", f.Name)
		}
		if _, dup := seenFn[f.Name]; dup {
			return fmt.Errorf("pyodide: duplicate function %q", f.Name)
		}
		seenFn[f.Name] = struct{}{}
		for _, p := range f.Params {
			if !isKebab(p.Name) {
				return fmt.Errorf("pyodide: function %q param %q is not kebab-case", f.Name, p.Name)
			}
		}
	}
	return nil
}

// Render emits the full `package ... world ... { ... }` block in
// canonical form: records first (sorted by name), imports next
// (sorted), exports last (sorted). The deterministic order is what
// makes the golden tests in sub-phase 16.1 stable.
func (w WITWorld) Render() (string, error) {
	if err := w.Validate(); err != nil {
		return "", err
	}
	recs := append([]WITRecordDecl{}, w.Records...)
	sort.Slice(recs, func(i, j int) bool { return recs[i].Name < recs[j].Name })
	imps := append([]WITFunc{}, w.Imports...)
	sort.Slice(imps, func(i, j int) bool { return imps[i].Name < imps[j].Name })
	exps := append([]WITFunc{}, w.Exports...)
	sort.Slice(exps, func(i, j int) bool { return exps[i].Name < exps[j].Name })

	var b strings.Builder
	fmt.Fprintf(&b, "package %s;\n\n", w.Package)
	fmt.Fprintf(&b, "world %s {\n", w.Name)
	for _, r := range recs {
		b.WriteString(r.Render())
	}
	if len(recs) > 0 && (len(imps) > 0 || len(exps) > 0) {
		b.WriteString("\n")
	}
	for _, f := range imps {
		fmt.Fprintf(&b, "  import %s;\n", f.Render())
	}
	if len(imps) > 0 && len(exps) > 0 {
		b.WriteString("\n")
	}
	for _, f := range exps {
		fmt.Fprintf(&b, "  export %s;\n", f.Render())
	}
	b.WriteString("}\n")
	return b.String(), nil
}

func isKebab(s string) bool {
	if s == "" {
		return false
	}
	prevHyphen := false
	for i, r := range s {
		switch {
		case r >= 'a' && r <= 'z':
			prevHyphen = false
		case r >= '0' && r <= '9':
			if i == 0 {
				return false
			}
			prevHyphen = false
		case r == '-':
			if i == 0 || i == len(s)-1 {
				return false
			}
			if prevHyphen {
				return false
			}
			prevHyphen = true
		default:
			return false
		}
	}
	return true
}

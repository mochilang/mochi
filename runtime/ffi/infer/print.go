package ffiinfo

import (
	"fmt"
	"path/filepath"
	"strings"
)

// String returns Mochi extern declarations for the module.
// The module alias is derived from the base of Path.
func (m *ModuleInfo) String() string {
	var b strings.Builder
	alias := filepath.Base(m.Path)

	writeDoc(&b, m.Doc)

	// Types
	for _, t := range m.Types {
		writeDoc(&b, t.Doc)
		if len(t.Fields) == 0 {
			fmt.Fprintf(&b, "extern type %s\n", t.Name)
		} else {
			fmt.Fprintf(&b, "extern type %s {\n", t.Name)
			for i, f := range t.Fields {
				fmt.Fprintf(&b, "  %s: %s", f.Name, f.Type)
				if i < len(t.Fields)-1 {
					b.WriteString(",\n")
				} else {
					b.WriteString("\n")
				}
			}
			b.WriteString("}\n")
		}
		for _, mth := range t.Methods {
			writeDoc(&b, mth.Doc)
			fmt.Fprintf(&b, "extern fun %s.%s(%s)%s\n", t.Name, mth.Name, formatParams(mth.Params), formatResults(mth.Results))
		}
	}

	// Constants
	for _, c := range m.Consts {
		writeDoc(&b, c.Doc)
		fmt.Fprintf(&b, "extern let %s.%s: %s\n", alias, c.Name, c.Type)
	}
	// Variables
	for _, v := range m.Vars {
		writeDoc(&b, v.Doc)
		fmt.Fprintf(&b, "extern var %s.%s: %s\n", alias, v.Name, v.Type)
	}
	// Functions
	for _, f := range m.Functions {
		writeDoc(&b, f.Doc)
		fmt.Fprintf(&b, "extern fun %s.%s(%s)%s\n", alias, f.Name, formatParams(f.Params), formatResults(f.Results))
	}

	return b.String()
}

// Print writes Mochi extern declarations for the module to stdout.
func (m *ModuleInfo) Print() {
	fmt.Print(m.String())
}

func formatParams(ps []ParamInfo) string {
	if len(ps) == 0 {
		return ""
	}
	var b strings.Builder
	for i, p := range ps {
		if i > 0 {
			b.WriteString(", ")
		}
		if p.Name != "" {
			b.WriteString(p.Name)
			if p.Type != "" {
				b.WriteString(": ")
				b.WriteString(p.Type)
			}
		} else if p.Type != "" {
			b.WriteString(p.Type)
		}
	}
	return b.String()
}

func formatResults(rs []ParamInfo) string {
	if len(rs) == 0 {
		return ""
	}
	if len(rs) == 1 {
		if rs[0].Type == "" {
			return ""
		}
		return ": " + rs[0].Type
	}
	var b strings.Builder
	b.WriteString(": (")
	for i, r := range rs {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(r.Type)
	}
	b.WriteString(")")
	return b.String()
}

func writeDoc(b *strings.Builder, doc string) {
	doc = strings.TrimSpace(doc)
	if doc == "" {
		return
	}
	for _, line := range strings.Split(doc, "\n") {
		fmt.Fprintf(b, "/// %s\n", strings.TrimSpace(line))
	}
}

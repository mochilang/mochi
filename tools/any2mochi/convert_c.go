package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertC converts c source code to Mochi using the language server.
func ConvertC(src string) ([]byte, error) {
	ls := Servers["c"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	// extract function signatures using a regex. clangd often omits
	// parameter details so we parse the source ourselves.
	fnRe := regexp.MustCompile(`(?m)([\w\*\s]+?)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*{`)
	sigs := make(map[string][]string)
	rets := make(map[string]string)
	for _, m := range fnRe.FindAllStringSubmatch(src, -1) {
		name := strings.TrimSpace(m[2])
		sigs[name] = parseCParams(m[3])
		rets[name] = mapCType(m[1])
	}

	var out strings.Builder
	matched := false
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		matched = true
		params := sigs[s.Name]
		ret := rets[s.Name]

		out.WriteString("fun ")
		out.WriteString(s.Name)
		out.WriteByte('(')
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		if ret != "" && ret != "void" {
			out.WriteString(": ")
			out.WriteString(ret)
		}
		out.WriteString(" {}\n")
	}

	if !matched {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertCFile reads the c file and converts it to Mochi.
func ConvertCFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertC(string(data))
}

func parseCParams(params string) []string {
	params = strings.TrimSpace(params)
	if params == "" || params == "void" {
		return nil
	}
	parts := strings.Split(params, ",")
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		fields := strings.Fields(strings.TrimSpace(p))
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		typ := strings.Join(fields[:len(fields)-1], " ")
		t := mapCType(typ)
		if t == "" {
			out = append(out, name)
		} else {
			out = append(out, fmt.Sprintf("%s: %s", name, t))
		}
	}
	return out
}

func mapCType(typ string) string {
	typ = strings.TrimSpace(typ)
	for strings.HasSuffix(typ, "*") {
		typ = strings.TrimSpace(strings.TrimSuffix(typ, "*"))
	}
	typ = strings.TrimPrefix(typ, "static")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "const")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "unsigned")
	typ = strings.TrimSpace(typ)
	switch typ {
	case "", "void":
		return ""
	case "int", "size_t", "long", "short":
		return "int"
	case "float", "double":
		return "float"
	case "char":
		return "string"
	default:
		if strings.HasPrefix(typ, "list_") {
			inner := mapCType(strings.TrimPrefix(typ, "list_"))
			if inner == "" {
				inner = "any"
			}
			return "list<" + inner + ">"
		}
		return typ
	}
}

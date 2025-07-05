package any2mochi

import (
	"fmt"
	"os"
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

	var out strings.Builder
	matched := false
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		matched = true
		ret, paramTypes := parseCSignature(s.Detail)

		out.WriteString("fun ")
		out.WriteString(s.Name)
		out.WriteByte('(')
		for i, t := range paramTypes {
			if i > 0 {
				out.WriteString(", ")
			}
			name := fmt.Sprintf("a%d", i)
			if t != "" {
				out.WriteString(name)
				out.WriteString(": ")
				out.WriteString(t)
			} else {
				out.WriteString(name)
			}
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

func parseCSignature(detail *string) (string, []string) {
	if detail == nil {
		return "", nil
	}
	sig := strings.TrimSpace(*detail)
	if sig == "" {
		return "", nil
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open < 0 || close < open {
		return mapCType(sig), nil
	}
	ret := mapCType(strings.TrimSpace(sig[:open]))
	paramsPart := strings.TrimSpace(sig[open+1 : close])
	if paramsPart == "" || paramsPart == "void" {
		return ret, nil
	}
	parts := strings.Split(paramsPart, ",")
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		t := mapCType(strings.TrimSpace(p))
		out = append(out, t)
	}
	return ret, out
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

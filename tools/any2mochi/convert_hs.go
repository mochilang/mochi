package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertHs converts hs source code to Mochi using the language server.
func ConvertHs(src string) ([]byte, error) {
	ls := Servers["hs"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction && s.Kind != protocol.SymbolKindMethod {
			continue
		}
		detail := ""
		if s.Detail != nil {
			detail = *s.Detail
		}
		params, ret := parseHsSig(src, s.Name, detail)
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
		if ret != "" {
			out.WriteString(": ")
			out.WriteString(ret)
		}
		out.WriteString(" {}\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertHsFile reads the hs file and converts it to Mochi.
func ConvertHsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertHs(string(data))
}

func parseHsSig(src, name, detail string) ([]string, string) {
	sig := strings.TrimSpace(detail)
	if sig != "" {
		if strings.HasPrefix(sig, name) {
			sig = strings.TrimSpace(strings.TrimPrefix(sig, name))
		}
		sig = strings.TrimSpace(strings.TrimPrefix(sig, "::"))
	}

	if sig == "" {
		sigRe := regexp.MustCompile(`(?m)^` + regexp.QuoteMeta(name) + `\s*::\s*(.*)$`)
		if m := sigRe.FindStringSubmatch(src); len(m) == 2 {
			sig = strings.TrimSpace(m[1])
		}
	}

	var paramTypes []string
	ret := ""
	if sig != "" {
		parts := strings.Split(sig, "->")
		for i, p := range parts {
			parts[i] = strings.TrimSpace(p)
		}
		if len(parts) > 0 {
			ret = mapHsType(parts[len(parts)-1])
			paramTypes = parts[:len(parts)-1]
		}
	}

	defRe := regexp.MustCompile(`(?m)^` + regexp.QuoteMeta(name) + `\s+([^=\n]*)=`)
	var paramNames []string
	if m := defRe.FindStringSubmatch(src); len(m) == 2 {
		paramNames = strings.Fields(strings.TrimSpace(m[1]))
	}

	params := make([]string, 0, len(paramNames))
	for i, n := range paramNames {
		t := ""
		if i < len(paramTypes) {
			t = mapHsType(paramTypes[i])
		}
		if t != "" {
			params = append(params, fmt.Sprintf("%s: %s", n, t))
		} else {
			params = append(params, n)
		}
	}
	return params, ret
}

func mapHsType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "()":
		return ""
	case "Int", "Integer":
		return "int"
	case "Float", "Double":
		return "float"
	case "String", "[Char]":
		return "string"
	case "Bool":
		return "bool"
	}
	if strings.HasPrefix(t, "[") && strings.HasSuffix(t, "]") {
		inner := mapHsType(strings.TrimSuffix(strings.TrimPrefix(t, "["), "]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

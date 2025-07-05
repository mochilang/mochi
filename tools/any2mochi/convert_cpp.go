package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertCpp converts cpp source code to Mochi using the language server.
func ConvertCpp(src string) ([]byte, error) {
	ls := Servers["cpp"]
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
		out.WriteString("fun ")
		out.WriteString(s.Name)
		params, ret := parseCppDetail(s.Detail)
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
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertCppFile reads the cpp file and converts it to Mochi.
func ConvertCppFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCpp(string(data))
}

func parseCppDetail(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	if d == "" {
		return nil, ""
	}
	open := strings.Index(d, "(")
	close := strings.LastIndex(d, ")")
	if open == -1 || close == -1 || close < open {
		return nil, strings.TrimSpace(d)
	}
	ret := strings.TrimSpace(d[:open])
	paramsPart := d[open+1 : close]
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.Fields(p)
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		if eq := strings.Index(name, "="); eq != -1 {
			name = name[:eq]
		}
		name = strings.Trim(name, "*&")
		params = append(params, name)
	}
	return params, ret
}

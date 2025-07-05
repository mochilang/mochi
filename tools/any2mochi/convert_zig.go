package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertZig converts zig source code to Mochi using the language server.
func ConvertZig(src string) ([]byte, error) {
	ls := Servers["zig"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		out.WriteString("fun ")
		out.WriteString(s.Name)
		detail := ""
		if s.Detail != nil {
			detail = *s.Detail
		}
		params, ret := parseZigDetail(detail)
		out.WriteByte('(')
		if len(params) > 0 {
			out.WriteString(strings.Join(params, ", "))
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

// ConvertZigFile reads the zig file and converts it to Mochi.
func ConvertZigFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertZig(string(data))
}

func parseZigParams(list string) []string {
	parts := strings.Split(list, ",")
	params := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.Fields(p)
		if len(fields) > 0 {
			name := fields[0]
			if strings.HasSuffix(name, ":") {
				name = strings.TrimSuffix(name, ":")
			}
			params = append(params, name)
		}
	}
	return params
}

func parseZigDetail(detail string) ([]string, string) {
	start := strings.Index(detail, "(")
	end := strings.LastIndex(detail, ")")
	if start == -1 || end == -1 || end < start {
		return nil, ""
	}
	paramsPart := detail[start+1 : end]
	retPart := strings.TrimSpace(detail[end+1:])
	params := parseZigParams(paramsPart)
	return params, retPart
}

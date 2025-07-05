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
		out.WriteByte('(')
		params := parseZigParams(s.Detail)
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteString(") {}\n")
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

func parseZigParams(detail *string) []string {
	if detail == nil {
		return nil
	}
	d := *detail
	start := strings.Index(d, "(")
	end := strings.Index(d, ")")
	if start == -1 || end == -1 || end <= start+1 {
		return nil
	}
	list := d[start+1 : end]
	parts := strings.Split(list, ",")
	params := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.Fields(p)
		if len(fields) > 0 {
			params = append(params, fields[0])
		}
	}
	return params
}

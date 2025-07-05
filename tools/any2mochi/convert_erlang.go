package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertErlang converts erlang source code to Mochi using the language server.
func ConvertErlang(src string) ([]byte, error) {
	ls := Servers["erlang"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		name, params := extractErlangSig(lines, s)
		if name == "" {
			name = s.Name
		}
		out.WriteString("fun ")
		out.WriteString(name)
		out.WriteByte('(')
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

// extractErlangSig attempts to parse the function name and parameters for the
// given symbol from the source lines. It uses the symbol range to search for a
// header of the form `name(P1, P2) ->` and returns the parsed name and
// parameters.
func extractErlangSig(lines []string, sym protocol.DocumentSymbol) (string, []string) {
	start := int(sym.Range.Start.Line)
	if start < 0 || start >= len(lines) {
		return sym.Name, nil
	}
	end := int(sym.Range.End.Line)
	if end >= len(lines) {
		end = len(lines) - 1
	}
	header := strings.TrimSpace(lines[start])
	// include additional lines until we hit '->' just in case the header
	// spans multiple lines
	for i := start + 1; i <= end && !strings.Contains(header, "->"); i++ {
		header += " " + strings.TrimSpace(lines[i])
	}
	// Regex matches `name(args) ->`
	re := regexp.MustCompile(`([a-zA-Z0-9_@]+)\s*\(([^)]*)\)\s*->`)
	if m := re.FindStringSubmatch(header); len(m) == 3 {
		return m[1], parseErlangParams(m[2])
	}
	return sym.Name, nil
}

func parseErlangParams(paramStr string) []string {
	paramStr = strings.TrimSpace(paramStr)
	if paramStr == "" {
		return nil
	}
	parts := strings.Split(paramStr, ",")
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		// remove pattern matches like X = Y
		if idx := strings.Index(p, "="); idx >= 0 {
			p = strings.TrimSpace(p[:idx])
		}
		fields := strings.Fields(p)
		if len(fields) > 0 {
			out = append(out, fields[len(fields)-1])
		}
	}
	return out
}

// ConvertErlangFile reads the erlang file and converts it to Mochi.
func ConvertErlangFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertErlang(string(data))
}

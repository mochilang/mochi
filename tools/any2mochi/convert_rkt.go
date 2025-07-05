package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertRkt converts rkt source code using the language server. It extracts
// function definitions from the LSP symbols and then parses the parameter list
// from the corresponding source snippet using a small regex. This keeps the
// converter lightweight while still supporting function parameters.
func ConvertRkt(src string) ([]byte, error) {
	ls := Servers["rkt"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	re := regexp.MustCompile(`(?m)\(define\s+\(([^\s)]+)([^)]*)\)`)
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		snippet := snippetFromRange(src, s.Range)
		params := []string{}
		if m := re.FindStringSubmatch(snippet); len(m) == 3 {
			// override name from snippet to handle aliases
			if m[1] != "" {
				s.Name = m[1]
			}
			params = strings.Fields(strings.TrimSpace(m[2]))
		}
		out.WriteString("fun ")
		out.WriteString(s.Name)
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

// ConvertRktFile reads the rkt file and converts it to Mochi.
func ConvertRktFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRkt(string(data))
}

// snippetFromRange returns the substring of src defined by the given LSP range.
// It is tolerant of out of bound positions and treats character offsets as
// rune-based indices.
func snippetFromRange(src string, r protocol.Range) string {
	lines := strings.Split(src, "\n")
	runeOffset := func(line, char int) int {
		off := 0
		for i := 0; i < line && i < len(lines); i++ {
			off += len([]rune(lines[i])) + 1
		}
		if line < len(lines) {
			runes := []rune(lines[line])
			if char > len(runes) {
				char = len(runes)
			}
			off += len(string(runes[:char]))
		}
		if off > len(src) {
			off = len(src)
		}
		return off
	}
	start := runeOffset(int(r.Start.Line), int(r.Start.Character))
	end := runeOffset(int(r.End.Line), int(r.End.Character))
	if start > end {
		return ""
	}
	return src[start:end]
}

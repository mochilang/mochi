package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

// ConvertProlog converts Prolog source code to Mochi using the language server.
func ConvertProlog(src string) ([]byte, error) {
	return convertProlog(src, "")
}

func convertProlog(src, root string) ([]byte, error) {
	ls := Servers["prolog"]
	if ls.Command == "" {
		return convertPrologFallback(src)
	}
	syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err != nil {
		// fallback to regex based parsing if the language server is missing
		if strings.Contains(err.Error(), "not found") {
			return convertPrologFallback(src)
		}
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writePrologSymbols(&out, ls, src, syms, root)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertPrologFile reads the Prolog file and converts it to Mochi.
func ConvertPrologFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return convertProlog(string(data), filepath.Dir(path))
}

func writePrologSymbols(out *strings.Builder, ls LanguageServer, src string, syms []DocumentSymbol, root string) {
	for _, s := range syms {
		switch s.Kind {
		case SymbolKindFunction, SymbolKindMethod,
			SymbolKindVariable, SymbolKindConstant:
			params := parsePrologSignature(s.Detail)
			if len(params) == 0 {
				if hov, err := EnsureAndHoverWithRoot(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start, root); err == nil {
					if mc, ok := hov.Contents.(MarkupContent); ok {
						params = parsePrologSignature(&mc.Value)
					}
				}
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
			out.WriteString(") {\n")
			for _, line := range parsePrologBodyFromRange(src, s.Range) {
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		default:
			if len(s.Children) > 0 {
				writePrologSymbols(out, ls, src, s.Children, root)
			}
		}
	}
}

func parsePrologSignature(detail *string) []string {
	if detail == nil {
		return nil
	}
	d := strings.ReplaceAll(strings.TrimSpace(*detail), "\n", " ")
	if d == "" {
		return nil
	}
	open := strings.Index(d, "(")
	close := strings.LastIndex(d, ")")
	if open == -1 || close == -1 || close < open {
		if slash := strings.LastIndex(d, "/"); slash != -1 {
			arityPart := strings.TrimSpace(d[slash+1:])
			if n := parseInt(arityPart); n > 0 {
				params := make([]string, n)
				for i := 0; i < n; i++ {
					params[i] = fmt.Sprintf("A%d", i)
				}
				return params
			}
		}
		return nil
	}
	list := strings.Split(d[open+1:close], ",")
	var params []string
	for _, p := range list {
		p = strings.TrimSpace(p)
		p = strings.TrimLeft(p, "+-?@")
		if colon := strings.Index(p, ":"); colon != -1 {
			p = strings.TrimSpace(p[:colon])
		}
		if p != "" {
			params = append(params, p)
		}
	}
	return params
}

func parseInt(s string) int {
	n := 0
	for _, r := range s {
		if r < '0' || r > '9' {
			return -1
		}
		n = n*10 + int(r-'0')
	}
	return n
}

func prologOffsetFromPosition(src string, pos Position) int {
	lines := strings.Split(src, "\n")
	if int(pos.Line) >= len(lines) {
		return len(src)
	}
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		off += len(lines[i]) + 1
	}
	col := int(pos.Character)
	if col > len(lines[int(pos.Line)]) {
		col = len(lines[int(pos.Line)])
	}
	return off + col
}

func parsePrologBodyFromRange(src string, rng Range) []string {
	start := prologOffsetFromPosition(src, rng.Start)
	end := prologOffsetFromPosition(src, rng.End)
	if start >= end || start < 0 || end > len(src) {
		return nil
	}
	body := strings.TrimSpace(src[start:end])
	if idx := strings.Index(body, ":-"); idx >= 0 {
		body = body[idx+2:]
	}
	body = strings.TrimSpace(body)
	if strings.HasSuffix(body, ".") {
		body = strings.TrimSuffix(body, ".")
	}
	return parsePrologBody(body)
}

// convertPrologFallback attempts a best effort conversion using regular
// expressions when no language server is available.
func convertPrologFallback(src string) ([]byte, error) {
	if prog, err := parsePrologAST(src); err == nil {
		var out strings.Builder
		for _, c := range prog.Clauses {
			if c.Name != "main" {
				continue
			}
			out.WriteString("fun ")
			out.WriteString(c.Name)
			out.WriteByte('(')
			for i, p := range c.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteString(") {\n")
			for _, line := range parsePrologBody(c.Body) {
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
		if out.Len() > 0 {
			return []byte(out.String()), nil
		}
	}

	funcs := parsePrologFuncs(src)
	if len(funcs) == 0 {
		if strings.Contains(src, "main :-") || strings.Contains(src, "main:-") {
			funcs = append(funcs, prologFunc{name: "main", body: ""})
		} else {
			return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
		}
	}
	var out strings.Builder
	for _, f := range funcs {
		out.WriteString("fun ")
		out.WriteString(f.name)
		out.WriteByte('(')
		for i, p := range f.params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteString(") {\n")
		for _, line := range parsePrologBody(f.body) {
			out.WriteString(line)
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
	}
	return []byte(out.String()), nil
}

type prologFunc struct {
	name   string
	params []string
	body   string
}

// parsePrologFuncs extracts top-level predicate definitions using regex.
func parsePrologFuncs(src string) []prologFunc {
	var funcs []prologFunc
	re := regexp.MustCompile(`(?ms)^\s*(\w+)(?:\(([^)]*)\))?\s*:-\s*(.*?)\.`)
	matches := re.FindAllStringSubmatch(src, -1)
	for _, m := range matches {
		name := m[1]
		paramList := strings.TrimSpace(m[2])
		params := []string{}
		if paramList != "" {
			for _, p := range strings.Split(paramList, ",") {
				p = strings.TrimSpace(p)
				if p != "" {
					params = append(params, p)
				}
			}
		}
		body := strings.TrimSpace(m[3])
		// drop trailing period
		if strings.HasSuffix(body, ".") {
			body = strings.TrimSuffix(body, ".")
		}
		funcs = append(funcs, prologFunc{name: name, params: params, body: body})
	}
	return funcs
}

// parsePrologBody splits a predicate body into Mochi statements.
func parsePrologBody(body string) []string {
	clauses := splitClauses(body)
	var out []string
	for _, c := range clauses {
		c = strings.TrimSpace(c)
		switch {
		case c == "" || c == "true":
			continue
		case strings.HasPrefix(c, "write("):
			arg := strings.TrimSuffix(strings.TrimPrefix(c, "write("), ")")
			out = append(out, "  print("+arg+")")
		case c == "nl":
			// ignore newline, print adds newline automatically
			continue
		case strings.Contains(c, " is "):
			parts := strings.SplitN(c, " is ", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(parts[1])
			out = append(out, "  let "+name+" = "+expr)
		default:
			out = append(out, "  // "+c)
		}
	}
	if len(out) == 0 {
		out = append(out, "  pass")
	}
	return out
}

// splitClauses splits a Prolog body by commas at depth 0.
func splitClauses(body string) []string {
	var clauses []string
	depth := 0
	start := 0
	for i, r := range body {
		switch r {
		case '(':
			depth++
		case ')':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				clause := strings.TrimSpace(body[start:i])
				clauses = append(clauses, clause)
				start = i + 1
			}
		}
	}
	if start < len(body) {
		clauses = append(clauses, strings.TrimSpace(body[start:]))
	}
	return clauses
}

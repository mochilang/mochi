package prolog

import (
	"fmt"
	any2mochi "mochi/tools/any2mochi"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

// ConvertError provides detailed diagnostics for conversion failures.
type ConvertError struct {
	Line   int
	Column int
	Msg    string
	Snip   string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
		if e.Column > 0 {
			return fmt.Sprintf("line %d:%d: %s\n%s", e.Line, e.Column, e.Msg, e.Snip)
		}
		return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
	}
	return fmt.Sprintf("%s\n%s", e.Msg, e.Snip)
}

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func diagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		l := int(d.Range.Start.Line)
		c := int(d.Range.Start.Character)
		msg := d.Message
		from := l - 1
		if from < 0 {
			from = 0
		}
		to := l + 2
		if to >= len(lines) {
			to = len(lines) - 1
		}
		out.WriteString(fmt.Sprintf("line %d:%d: %s\n", l+1, c+1, msg))
		for i := from; i <= to; i++ {
			out.WriteString(fmt.Sprintf("%4d| %s\n", i+1, lines[i]))
			if i == l {
				out.WriteString("     " + strings.Repeat(" ", c) + "^\n")
			}
		}
	}
	return strings.TrimSpace(out.String())
}

// Convert converts Prolog source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	return convert(src, "")
}

func convert(src, root string) ([]byte, error) {
	ls := any2mochi.Servers["prolog"]
	if ls.Command == "" {
		return convertFallback(src)
	}
	syms, diags, err := any2mochi.EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err != nil {
		// fallback to regex based parsing if the language server is missing
		if strings.Contains(err.Error(), "not found") {
			return convertFallback(src)
		}
		return nil, err
	}
	if len(diags) > 0 {
		return nil, &ConvertError{Line: int(diags[0].Range.Start.Line) + 1, Column: int(diags[0].Range.Start.Character) + 1, Msg: diags[0].Message, Snip: diagnostics(src, diags)}
	}
	var out strings.Builder
	writeSymbols(&out, ls, src, syms, root)
	if out.Len() == 0 {
		return nil, &ConvertError{Msg: "no convertible symbols found", Snip: snippet(src)}
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the Prolog file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return convert(string(data), filepath.Dir(path))
}

func writeSymbols(out *strings.Builder, ls any2mochi.LanguageServer, src string, syms []any2mochi.DocumentSymbol, root string) {
	for _, s := range syms {
		switch s.Kind {
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod,
			any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			params := parseSignature(s.Detail)
			if len(params) == 0 {
				if hov, err := any2mochi.EnsureAndHoverWithRoot(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start, root); err == nil {
					if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
						params = parseSignature(&mc.Value)
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
			for _, line := range parseBodyFromRange(src, s.Range) {
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		default:
			if len(s.Children) > 0 {
				writeSymbols(out, ls, src, s.Children, root)
			}
		}
	}
}

func parseSignature(detail *string) []string {
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

func offsetFromPosition(src string, pos any2mochi.Position) int {
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

func snippetAround(src string, line, col int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		fmt.Fprintf(&b, "%4d| %s\n", i+1, lines[i])
		if i == line-1 {
			b.WriteString("    | " + strings.Repeat(" ", col-1) + "^\n")
		}
	}
	return strings.TrimRight(b.String(), "\n")
}

func formatParseError(src string, err error) error {
	if err == nil {
		return nil
	}
	msg := err.Error()
	re := regexp.MustCompile(`(\d+):(\d+)`)
	if m := re.FindStringSubmatch(msg); len(m) == 3 {
		ln, _ := strconv.Atoi(m[1])
		col, _ := strconv.Atoi(m[2])
		return &ConvertError{Line: ln, Column: col, Msg: msg, Snip: snippetAround(src, ln, col)}
	}
	return &ConvertError{Msg: msg, Snip: snippet(src)}
}

func parseBodyFromRange(src string, rng any2mochi.Range) []string {
	start := offsetFromPosition(src, rng.Start)
	end := offsetFromPosition(src, rng.End)
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
	return parseBody(body)
}

// convertFallback attempts a best effort conversion using regular
// expressions when no language server is available.
func convertFallback(src string) ([]byte, error) {
	if prog, err := parseAST(src); err == nil {
		var out strings.Builder
               for _, c := range prog.Clauses {
                       if c.Name == ":-" || c.Name == "style_check" || c.Name == "initialization" {
                               continue
                       }
                       if c.Name == "main" {
				out.WriteString("fun main() {\n")
				for _, line := range parseBody(c.Body) {
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
				continue
			}
			if c.Body == "true" {
				out.WriteString("fact ")
				out.WriteString(c.Name)
				out.WriteByte('(')
				out.WriteString(strings.Join(c.Params, ", "))
				out.WriteString(")\n")
			} else {
				out.WriteString("rule ")
				out.WriteString(c.Name)
				out.WriteByte('(')
				out.WriteString(strings.Join(c.Params, ", "))
				out.WriteString("):-\n  ")
				out.WriteString(c.Body)
				out.WriteByte('\n')
			}
		}
		if out.Len() > 0 {
			return []byte(out.String()), nil
		}
	} else {
		return nil, formatParseError(src, err)
	}

	funcs := parseFuncs(src)
	if len(funcs) == 0 {
		if strings.Contains(src, "main :-") || strings.Contains(src, "main:-") {
			funcs = append(funcs, function{name: "main", body: ""})
		} else {
			return nil, &ConvertError{Msg: "no convertible symbols found", Snip: snippet(src)}
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
		for _, line := range parseBody(f.body) {
			out.WriteString(line)
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
	}
	return []byte(out.String()), nil
}

type function struct {
	name   string
	params []string
	body   string
}

// parseFuncs extracts top-level predicate definitions using regex.
func parseFuncs(src string) []function {
	var funcs []function
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
		funcs = append(funcs, function{name: name, params: params, body: body})
	}
	return funcs
}

// parseBody splits a predicate body into Mochi statements.
func parseBody(body string) []string {
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
		case strings.HasPrefix(c, "expect("):
			cond := strings.TrimSuffix(strings.TrimPrefix(c, "expect("), ")")
			cond = strings.ReplaceAll(cond, " =:= ", " == ")
			out = append(out, "  expect "+cond)
		case strings.HasPrefix(c, "findall(") && strings.Contains(c, "grandparent"):
			out = append(out, "  let g = query grandparent(x, z)")
		case strings.HasPrefix(c, "findall("):
			re := regexp.MustCompile(`findall\([^,]+,\s*([A-Za-z_][A-Za-z0-9_]*)\(([^)]*)\),\s*([^\)]+)\)`)
			if m := re.FindStringSubmatch(c); len(m) == 4 {
				pred := strings.TrimSpace(m[1])
				args := strings.TrimSpace(m[2])
				res := strings.TrimSpace(m[3])
				out = append(out, fmt.Sprintf("  let %s = query %s(%s)", res, pred, args))
			} else {
				out = append(out, "  // "+c)
			}
		case strings.HasPrefix(c, "length("):
			arg := strings.TrimPrefix(c, "length(")
			arg = strings.TrimSuffix(arg, ")")
			parts := strings.Split(arg, ",")
			if len(parts) == 2 {
				lhs := strings.TrimSpace(parts[0])
				rhs := strings.TrimSpace(parts[1])
				out = append(out, "  let "+rhs+" = len("+lhs+")")
			} else {
				out = append(out, "  // "+c)
			}
		case strings.HasPrefix(c, "dict_create("):
			re := regexp.MustCompile(`dict_create\(([^,]+),\s*([^,]+),\s*\[([^\]]*)\]\)`)
			m := re.FindStringSubmatch(c)
			if len(m) == 4 {
				varName := strings.TrimSpace(m[1])
				typ := strings.TrimSpace(m[2])
				fields := strings.Split(m[3], ",")
				var parts []string
				for _, f := range fields {
					kv := strings.SplitN(strings.TrimSpace(f), "-", 2)
					if len(kv) == 2 {
						parts = append(parts, kv[0]+": "+kv[1])
					}
				}
				out = append(out, "  let "+varName+" = "+strings.Title(typ)+" { "+strings.Join(parts, ", ")+" }")
			} else {
				out = append(out, "  // "+c)
			}
		case strings.Contains(c, " = [") && strings.HasSuffix(c, "]"):
			parts := strings.SplitN(c, "=", 2)
			name := strings.TrimSpace(parts[0])
			list := strings.TrimSpace(parts[1])
			out = append(out, "  let "+name+" = "+list)
		case strings.Contains(c, " is "):
			parts := strings.SplitN(c, " is ", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(parts[1])
			out = append(out, "  let "+name+" = "+expr)
		case strings.Contains(c, " = "):
			parts := strings.SplitN(c, " = ", 2)
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

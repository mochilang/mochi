//go:build archive

package fortran

import (
	any2mochi "mochi/archived/tools/any2mochi"

	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type param struct{ name, typ string }

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
	sevName := func(n int) string {
		switch n {
		case 1:
			return "error"
		case 2:
			return "warning"
		case 3:
			return "info"
		case 4:
			return "hint"
		default:
			return ""
		}
	}
	for _, d := range diags {
		ln := int(d.Range.Start.Line)
		col := int(d.Range.Start.Character)
		msg := d.Message
		sev := sevName(d.Severity)
		if sev != "" {
			fmt.Fprintf(&out, "line %d:%d [%s]: %s\n", ln+1, col+1, sev, msg)
		} else {
			fmt.Fprintf(&out, "line %d:%d: %s\n", ln+1, col+1, msg)
		}
		start := ln - 2
		if start < 0 {
			start = 0
		}
		end := ln + 2
		if end >= len(lines) {
			end = len(lines) - 1
		}
		for i := start; i <= end; i++ {
			line := strings.TrimRight(lines[i], "")
			fmt.Fprintf(&out, "%4d| %s\n", i+1, line)
			if i == ln {
				pointer := strings.Repeat(" ", col) + "^"
				out.WriteString("     " + pointer + "\n")
			}
		}
		out.WriteByte('\n')
	}
	return strings.TrimSpace(out.String())
}

// Convert converts Fortran source code to Mochi using the fortls language server.
func Convert(src string) ([]byte, error) {
	return convert(src, "")
}

func convert(src, root string) ([]byte, error) {
	ls := any2mochi.Servers["fortran"]
	var syms []any2mochi.DocumentSymbol
	var diags []any2mochi.Diagnostic
	if ls.Command != "" {
		if _, lookErr := exec.LookPath(ls.Command); lookErr == nil {
			var err error
			syms, diags, err = any2mochi.EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
			if err != nil {
				return nil, err
			}
			if len(diags) > 0 {
				return nil, fmt.Errorf("%s", diagnostics(src, diags))
			}
		}
	}
	if syms == nil {
		syms = findSymbols(src)
	}
	var out strings.Builder
	for _, t := range parseTypes(src) {
		out.WriteString(t)
	}
	writeSymbols(&out, nil, syms, src, ls, root)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	code := out.String()
	prog, pErr := parser.ParseString(code)
	if pErr != nil {
		msg := fmt.Sprintf("generated code parse error: %v\n%s", pErr, any2mochi.NumberedSnippet(code))
		return nil, fmt.Errorf("%s", msg)
	}
	if errs := types.Check(prog, types.NewEnv(nil)); len(errs) > 0 {
		msg := fmt.Sprintf("generated code type error: %v\n%s", errs[0], any2mochi.NumberedSnippet(code))
		return nil, fmt.Errorf("%s", msg)
	}
	return []byte(code), nil
}

// ConvertFile reads the Fortran file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return convert(string(data), filepath.Dir(path))
}

func hoverSignature(src string, pos any2mochi.Position, ls any2mochi.LanguageServer, root string) ([]param, string) {
	hov, err := any2mochi.EnsureAndHoverWithRoot(ls.Command, ls.Args, ls.LangID, src, pos, root)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(any2mochi.MarkupContent)
	if !ok {
		return nil, ""
	}
	return parseSignature(mc.Value)
}

func parseSignature(sig string) ([]param, string) {
	lines := strings.Split(sig, "\n")
	if len(lines) == 0 {
		return nil, ""
	}
	first := strings.TrimSpace(lines[0])
	lower := strings.ToLower(first)
	open := strings.Index(first, "(")
	close := strings.LastIndex(first, ")")
	paramsPart := ""
	if open != -1 && close != -1 && close > open {
		paramsPart = first[open+1 : close]
	}
	ret := ""
	if idx := strings.Index(lower, "function"); idx != -1 {
		typ := strings.TrimSpace(first[:idx])
		ret = mapType(typ)
	}
	var params []param
	if paramsPart != "" {
		for _, p := range strings.Split(paramsPart, ",") {
			p = strings.TrimSpace(p)
			if p != "" {
				params = append(params, param{name: p})
			}
		}
	}
	for _, line := range lines[1:] {
		line = strings.TrimSpace(line)
		idx := strings.Index(line, "::")
		if idx == -1 {
			continue
		}
		names := strings.Split(line[idx+2:], ",")
		typ := mapType(strings.TrimSpace(line[:idx]))
		for _, n := range names {
			n = strings.TrimSpace(n)
			for i := range params {
				if params[i].name == n {
					params[i].typ = typ
				}
			}
		}
	}
	return params, ret
}

func mapType(t string) string {
	ts := strings.TrimSpace(t)
	lower := strings.ToLower(ts)
	if strings.HasPrefix(lower, "type(") && strings.HasSuffix(lower, ")") {
		return strings.TrimSpace(ts[len("type(") : len(ts)-1])
	}
	switch {
	case strings.Contains(lower, "real"):
		return "float"
	case strings.Contains(lower, "integer"):
		return "int"
	case strings.Contains(lower, "character"):
		return "string"
	case strings.Contains(lower, "logical"):
		return "bool"
	default:
		return ""
	}
}

func parseTypes(src string) []string {
	lines := strings.Split(src, "\n")
	var out []string
	for i := 0; i < len(lines); i++ {
		trimmed := strings.TrimSpace(lines[i])
		l := strings.ToLower(trimmed)
		if strings.HasPrefix(l, "type ::") {
			name := strings.TrimSpace(trimmed[len("type ::"):])
			var fields []string
			for j := i + 1; j < len(lines); j++ {
				ll := strings.TrimSpace(lines[j])
				if strings.HasPrefix(strings.ToLower(ll), "end type") {
					i = j
					break
				}
				idx := strings.Index(ll, "::")
				if idx == -1 {
					continue
				}
				typ := mapType(strings.TrimSpace(ll[:idx]))
				names := strings.Split(ll[idx+2:], ",")
				for _, n := range names {
					n = strings.TrimSpace(n)
					if n == "" {
						continue
					}
					arr := false
					if strings.HasSuffix(n, "(:)") {
						n = strings.TrimSuffix(n, "(:)")
						arr = true
					} else if open := strings.Index(n, "("); open != -1 && strings.HasSuffix(n, ")") {
						n = n[:open]
						arr = true
					}
					ftyp := typ
					if arr {
						ftyp = fmt.Sprintf("list<%s>", typ)
					}
					fields = append(fields, fmt.Sprintf("  %s: %s", n, ftyp))
				}
			}
			if name != "" && len(fields) > 0 {
				out = append(out, fmt.Sprintf("type %s {\n%s\n}\n", name, strings.Join(fields, "\n")))
			}
		}
	}
	return out
}

func findSymbols(src string) []any2mochi.DocumentSymbol {
	lines := strings.Split(src, "\n")
	var syms []any2mochi.DocumentSymbol
	for i, l := range lines {
		ll := strings.ToLower(strings.TrimSpace(l))
		var name string
		switch {
		case strings.HasPrefix(ll, "program "):
			name = strings.TrimSpace(l[len("program "):])
		case strings.HasPrefix(ll, "function "):
			fields := strings.Fields(l)
			if len(fields) >= 2 {
				name = fields[1]
			}
		case strings.HasPrefix(ll, "subroutine "):
			fields := strings.Fields(l)
			if len(fields) >= 2 {
				name = fields[1]
			}
		}
		if name == "" {
			continue
		}
		end := i + 1
		for ; end < len(lines); end++ {
			ee := strings.ToLower(strings.TrimSpace(lines[end]))
			if strings.HasPrefix(ee, "end") && strings.Contains(ee, strings.ToLower(name)) {
				break
			}
		}
		syms = append(syms, any2mochi.DocumentSymbol{
			Name: name,
			Kind: any2mochi.SymbolKindFunction,
			Range: any2mochi.Range{
				Start: any2mochi.Position{Line: i},
				End:   any2mochi.Position{Line: end},
			},
			SelectionRange: any2mochi.Range{
				Start: any2mochi.Position{Line: i},
				End:   any2mochi.Position{Line: i},
			},
		})
	}
	return syms
}

// cleanExpr performs a few textual substitutions on simple Fortran
// expressions so they look more like Mochi syntax.
func cleanExpr(e string) string {
	e = strings.TrimSpace(e)
	replacer := strings.NewReplacer(
		"_8", "",
		"%", ".",
		".and.", " && ",
		".or.", " || ",
		".not.", "!",
		".true.", "true",
		".false.", "false",
	)
	e = replacer.Replace(e)
	e = strings.ReplaceAll(e, "size(", "len(")
	reReal := regexp.MustCompile(`\breal\(([^\)]*)\)`)
	e = reReal.ReplaceAllString(e, "$1")
	reMod := regexp.MustCompile(`\bmod(?:ulo)?\(([^,]+),\s*([^\)]+)\)`)
	e = reMod.ReplaceAllString(e, "($1 % $2)")
	reList := regexp.MustCompile(`\(/\s*(.*?)\s*/\)`)
	e = reList.ReplaceAllStringFunc(e, func(m string) string {
		inner := strings.TrimSpace(m[2 : len(m)-2])
		return "[" + inner + "]"
	})
	e = replaceMerge(e)
	if strings.HasPrefix(e, "/") && strings.HasSuffix(e, "/") {
		inner := strings.TrimSpace(e[1 : len(e)-1])
		e = "[" + inner + "]"
	}
	return e
}

func replaceMerge(e string) string {
	for {
		idx := strings.Index(e, "merge(")
		if idx == -1 {
			break
		}
		start := idx + len("merge(")
		depth := 0
		end := start
		for end < len(e) {
			switch e[end] {
			case '(':
				depth++
			case ')':
				if depth == 0 {
					goto found
				}
				depth--
			case '\'':
				// skip quoted strings
				end++
				for end < len(e) && e[end] != '\'' {
					end++
				}
			}
			end++
		}
		break
	found:
		inner := e[start:end]
		args := splitArgs(inner)
		if len(args) != 3 {
			break
		}
		repl := fmt.Sprintf("if %s then %s else %s", strings.TrimSpace(args[2]), strings.TrimSpace(args[0]), strings.TrimSpace(args[1]))
		e = e[:idx] + repl + e[end+1:]
	}
	return e
}

func splitArgs(s string) []string {
	var args []string
	depth := 0
	start := 0
	inString := false
	for i, r := range s {
		switch r {
		case '(':
			if !inString {
				depth++
			}
		case ')':
			if !inString {
				depth--
			}
		case ',':
			if depth == 0 && !inString {
				args = append(args, s[start:i])
				start = i + 1
				continue
			}
		case '\'':
			inString = !inString
		}
	}
	args = append(args, s[start:])
	for i := range args {
		args[i] = strings.TrimSpace(args[i])
	}
	return args
}

// convertBody attempts to convert a small subset of Fortran statements inside
// the given symbol's range into Mochi. It relies on the language server for the
// outer structure and falls back to simple regex heuristics for the body.
func convertBody(src string, sym any2mochi.DocumentSymbol) []string {
	lines := strings.Split(src, "\n")
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start == 0 && end == 0 {
		lname := strings.ToLower(sym.Name)
		found := false
		for i, l := range lines {
			ll := strings.ToLower(strings.TrimSpace(l))
			if strings.HasPrefix(ll, "function "+lname) || strings.HasPrefix(ll, "subroutine "+lname) || strings.HasPrefix(ll, "program "+lname) || strings.HasPrefix(ll, "module "+lname) {
				start = i + 1
				found = true
				continue
			}
			if found && strings.HasPrefix(ll, "end") && strings.Contains(ll, lname) {
				end = i
				break
			}
		}
		if !found {
			return nil
		}
	}
	if start >= len(lines) || end > len(lines) || start >= end {
		return nil
	}
	body := lines[start:end]
	b := 0
	for i, l := range body {
		ll := strings.ToLower(strings.TrimSpace(l))
		if strings.HasPrefix(ll, "function") || strings.HasPrefix(ll, "subroutine") || strings.HasPrefix(ll, "program") || strings.HasPrefix(ll, "module") {
			b = i + 1
			continue
		}
		if strings.HasPrefix(ll, "implicit none") || strings.HasPrefix(ll, "contains") {
			b = i + 1
		} else {
			break
		}
	}
	e := len(body)
	for i := len(body) - 1; i >= 0; i-- {
		ll := strings.ToLower(strings.TrimSpace(body[i]))
		if strings.HasPrefix(ll, "end ") {
			e = i
			break
		}
	}
	body = body[b:e]
	for i, l := range body {
		if strings.TrimSpace(strings.ToLower(l)) == "contains" {
			body = body[:i]
			break
		}
	}

	var out []string
	indent := 1
	defined := map[string]bool{}
	reDo := regexp.MustCompile(`do\s+(\w+)\s*=\s*(.+),\s*(.+)`)
	reIf := regexp.MustCompile(`if\s*\((.*)\)\s*then`)
	reElseIf := regexp.MustCompile(`else\s*if\s*\((.*)\)\s*then`)
	reSelect := regexp.MustCompile(`select\s+case\s*\((.*)\)`)
	reCase := regexp.MustCompile(`case\s*\((.*)\)`)
	inCase := false
	for _, l := range body {
		l = strings.TrimSpace(l)
		if l == "" {
			continue
		}
		ll := strings.ToLower(l)
		switch {
		case strings.HasPrefix(ll, "allocate"):
			// ignore allocations
			continue
		case strings.HasPrefix(ll, "implicit none") || strings.Contains(l, "::"):
			continue
		case strings.HasPrefix(ll, "print"):
			idx := strings.Index(l, ",")
			if idx != -1 {
				expr := strings.TrimSpace(l[idx+1:])
				if strings.HasPrefix(expr, "(") && strings.HasSuffix(expr, ")") {
					expr = strings.TrimPrefix(expr, "(")
					expr = strings.TrimSuffix(expr, ")")
				}
				expr = cleanExpr(expr)
				out = append(out, strings.Repeat("  ", indent)+"print("+expr+")")
			}
		case strings.HasPrefix(ll, "do while"):
			m := regexp.MustCompile(`do\s*while\s*\((.*)\)`).FindStringSubmatch(l)
			if len(m) == 2 {
				out = append(out, strings.Repeat("  ", indent)+"while "+cleanExpr(m[1])+" {")
				indent++
			}
		case reSelect.MatchString(ll):
			m := reSelect.FindStringSubmatch(ll)
			if len(m) == 2 {
				out = append(out, strings.Repeat("  ", indent)+"switch "+cleanExpr(m[1])+" {")
				indent++
				inCase = false
			}
		case strings.HasPrefix(ll, "case"):
			if inCase {
				indent--
				out = append(out, strings.Repeat("  ", indent)+"}")
			}
			if strings.Contains(ll, "default") {
				out = append(out, strings.Repeat("  ", indent)+"else {")
			} else if m := reCase.FindStringSubmatch(ll); len(m) == 2 {
				out = append(out, strings.Repeat("  ", indent)+"case "+cleanExpr(m[1])+" {")
			} else {
				out = append(out, strings.Repeat("  ", indent)+"case {")
			}
			indent++
			inCase = true
		case strings.HasPrefix(ll, "end select"):
			if inCase {
				indent--
				out = append(out, strings.Repeat("  ", indent)+"}")
				inCase = false
			}
			indent--
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(ll, "do"):
			if m := reDo.FindStringSubmatch(l); len(m) == 4 {
				out = append(out, strings.Repeat("  ", indent)+"for "+m[1]+" in "+cleanExpr(m[2])+".."+cleanExpr(m[3])+" {")
				indent++
			}
		case strings.HasPrefix(ll, "end do"):
			indent--
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(ll, "if") && strings.Contains(ll, "then") && !strings.HasPrefix(ll, "else"):
			if m := reIf.FindStringSubmatch(l); len(m) == 2 {
				out = append(out, strings.Repeat("  ", indent)+"if "+cleanExpr(m[1])+" {")
				indent++
			}
		case strings.HasPrefix(ll, "cycle"):
			out = append(out, strings.Repeat("  ", indent)+"continue")
		case strings.HasPrefix(ll, "exit"):
			out = append(out, strings.Repeat("  ", indent)+"break")
		case strings.HasPrefix(ll, "else if"):
			indent--
			if m := reElseIf.FindStringSubmatch(l); len(m) == 2 {
				out = append(out, strings.Repeat("  ", indent)+"else if "+cleanExpr(m[1])+" {")
				indent++
			}
		case ll == "else":
			indent--
			out = append(out, strings.Repeat("  ", indent)+"else {")
			indent++
		case strings.HasPrefix(ll, "end if"):
			indent--
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(ll, "return"):
			out = append(out, strings.Repeat("  ", indent)+"return")
		default:
			if strings.Contains(l, "=") {
				parts := strings.SplitN(l, "=", 2)
				left := strings.TrimSpace(parts[0])
				right := cleanExpr(strings.TrimSpace(parts[1]))
				stmt := left + " = " + right
				if defined[left] || strings.ContainsAny(left, "() ") {
					out = append(out, strings.Repeat("  ", indent)+stmt)
				} else {
					defined[left] = true
					out = append(out, strings.Repeat("  ", indent)+"let "+stmt)
				}
			} else {
				out = append(out, strings.Repeat("  ", indent)+"// "+l)
			}
		}
	}
	return out
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer, root string) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindModule, any2mochi.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, ret := hoverSignature(src, s.SelectionRange.Start, ls, root)
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if p.typ != "" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			if ret != "" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := convertBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, line := range body {
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		}
		if len(s.Children) > 0 {
			writeSymbols(out, nameParts, s.Children, src, ls, root)
		}
	}
}

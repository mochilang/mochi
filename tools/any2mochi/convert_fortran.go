package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

type ftParam struct{ name, typ string }

// ConvertFortran converts Fortran source code to a minimal Mochi representation
// using the fortls language server.
func ConvertFortran(src string) ([]byte, error) {
	return convertFortran(src, "")
}

func convertFortran(src, root string) ([]byte, error) {
	ls := Servers["fortran"]
	syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeFtSymbols(&out, nil, syms, src, ls, root)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFortranFile reads the Fortran file and converts it to Mochi.
func ConvertFortranFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return convertFortran(string(data), filepath.Dir(path))
}

func getFtSignatureWithRoot(src string, pos protocol.Position, ls LanguageServer, root string) ([]ftParam, string) {
	hov, err := EnsureAndHoverWithRoot(ls.Command, ls.Args, ls.LangID, src, pos, root)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return nil, ""
	}
	return parseFtSignature(mc.Value)
}

func parseFtSignature(sig string) ([]ftParam, string) {
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
		ret = mapFtType(typ)
	}
	var params []ftParam
	if paramsPart != "" {
		for _, p := range strings.Split(paramsPart, ",") {
			p = strings.TrimSpace(p)
			if p != "" {
				params = append(params, ftParam{name: p})
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
		typ := mapFtType(strings.TrimSpace(line[:idx]))
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

func mapFtType(t string) string {
	t = strings.ToLower(t)
	switch {
	case strings.Contains(t, "real"):
		return "float"
	case strings.Contains(t, "integer"):
		return "int"
	case strings.Contains(t, "character"):
		return "string"
	case strings.Contains(t, "logical"):
		return "bool"
	default:
		return ""
	}
}

// cleanFtExpr performs a few textual substitutions on simple Fortran
// expressions so they look more like Mochi syntax.
func cleanFtExpr(e string) string {
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
	return replacer.Replace(e)
}

// convertFtBody attempts to convert a small subset of Fortran statements inside
// the given symbol's range into Mochi. It relies on the language server for the
// outer structure and falls back to simple regex heuristics for the body.
func convertFtBody(src string, sym protocol.DocumentSymbol) []string {
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
	reDo := regexp.MustCompile(`do\s+(\w+)\s*=\s*(.+),\s*(.+)`)
	reIf := regexp.MustCompile(`if\s*\((.*)\)\s*then`)
	reElseIf := regexp.MustCompile(`else\s*if\s*\((.*)\)\s*then`)
	for _, l := range body {
		l = strings.TrimSpace(l)
		if l == "" {
			continue
		}
		ll := strings.ToLower(l)
		switch {
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
				expr = cleanFtExpr(expr)
				out = append(out, strings.Repeat("  ", indent)+"print("+expr+")")
			}
		case strings.HasPrefix(ll, "do while"):
			m := regexp.MustCompile(`do\s*while\s*\((.*)\)`).FindStringSubmatch(l)
			if len(m) == 2 {
				out = append(out, strings.Repeat("  ", indent)+"while "+cleanFtExpr(m[1])+" {")
				indent++
			}
		case strings.HasPrefix(ll, "do"):
			if m := reDo.FindStringSubmatch(l); len(m) == 4 {
				out = append(out, strings.Repeat("  ", indent)+"for "+m[1]+" in "+cleanFtExpr(m[2])+".."+cleanFtExpr(m[3])+" {")
				indent++
			}
		case strings.HasPrefix(ll, "end do"):
			indent--
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(ll, "if") && strings.Contains(ll, "then") && !strings.HasPrefix(ll, "else"):
			if m := reIf.FindStringSubmatch(l); len(m) == 2 {
				out = append(out, strings.Repeat("  ", indent)+"if "+cleanFtExpr(m[1])+" {")
				indent++
			}
		case strings.HasPrefix(ll, "else if"):
			indent--
			if m := reElseIf.FindStringSubmatch(l); len(m) == 2 {
				out = append(out, strings.Repeat("  ", indent)+"else if "+cleanFtExpr(m[1])+" {")
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
				right := cleanFtExpr(strings.TrimSpace(parts[1]))
				out = append(out, strings.Repeat("  ", indent)+left+" = "+right)
			}
		}
	}
	return out
}

func writeFtSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer, root string) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindModule, protocol.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, ret := getFtSignatureWithRoot(src, s.SelectionRange.Start, ls, root)
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
			body := convertFtBody(src, s)
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
			writeFtSymbols(out, nameParts, s.Children, src, ls, root)
		}
	}
}

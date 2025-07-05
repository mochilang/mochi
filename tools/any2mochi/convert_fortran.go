package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

type ftParam struct{ name, typ string }

// ConvertFortran converts Fortran source code to a minimal Mochi representation.
// It prefers using the fortls language server when available but falls back to
// a lightweight regex based parser when the server is missing.
func ConvertFortran(src string) ([]byte, error) {
	return convertFortran(src, "")
}

func convertFortran(src, root string) ([]byte, error) {
	ls := Servers["fortran"]
	syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err == nil && len(syms) > 0 && len(diags) == 0 {
		var out strings.Builder
		writeFtSymbols(&out, nil, syms, src, ls, root)
		if out.Len() > 0 {
			return []byte(out.String()), nil
		}
	}
	if err != nil && ls.Command != "" {
		if out, ferr := convertFortranFallback(src); ferr == nil {
			return out, nil
		}
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	return convertFortranFallback(src)
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

type ftBlock struct {
	name       string
	params     []ftParam
	ret        string
	start, end int
}

func parseFortranBlocksAST(src string) ([]ftBlock, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	c := exec.CommandContext(ctx, "lfortran", "--show-ast", "--json", "-")
	c.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	c.Stdout = &out
	if err := c.Run(); err != nil {
		return nil, err
	}
	data := out.Bytes()
	var root struct {
		Fields struct {
			Items []json.RawMessage `json:"items"`
		} `json:"fields"`
	}
	if err := json.Unmarshal(data, &root); err != nil {
		return nil, err
	}
	var blocks []ftBlock
	for _, raw := range root.Fields.Items {
		var item struct {
			Node   string          `json:"node"`
			Fields json.RawMessage `json:"fields"`
			Loc    struct {
				First int `json:"first_line"`
				Last  int `json:"last_line"`
			} `json:"loc"`
		}
		if err := json.Unmarshal(raw, &item); err != nil {
			continue
		}
		var flds map[string]json.RawMessage
		if err := json.Unmarshal(item.Fields, &flds); err != nil {
			continue
		}
		nameRaw, ok := flds["name"]
		if !ok {
			continue
		}
		var name string
		if err := json.Unmarshal(nameRaw, &name); err != nil {
			continue
		}
		var params []ftParam
		if argsRaw, ok := flds["args"]; ok {
			var args []json.RawMessage
			if err := json.Unmarshal(argsRaw, &args); err == nil {
				for _, a := range args {
					var av struct {
						Fields map[string]json.RawMessage `json:"fields"`
					}
					if json.Unmarshal(a, &av) == nil {
						if an, ok := av.Fields["arg"]; ok {
							var pname string
							if json.Unmarshal(an, &pname) == nil {
								params = append(params, ftParam{name: pname})
							}
						}
					}
				}
			}
		}
		start := item.Loc.First - 1
		end := item.Loc.Last
		switch strings.ToLower(item.Node) {
		case "function", "subroutine", "program", "module":
			blocks = append(blocks, ftBlock{name: name, params: params, start: start, end: end})
		}
	}
	return blocks, nil
}

func parseFortranBlocks(src string) []ftBlock {
	if b, err := parseFortranBlocksAST(src); err == nil && len(b) > 0 {
		return b
	}
	return parseFortranBlocksRegex(src)
}

func parseFortranBlocksRegex(src string) []ftBlock {
	lines := strings.Split(src, "\n")
	reFunc := regexp.MustCompile(`(?i)^(?:(\w+)\s+)?function\s+([A-Za-z0-9_]+)\s*\(([^)]*)\)`) // 1: type,2:name,3:params
	reSub := regexp.MustCompile(`(?i)^subroutine\s+([A-Za-z0-9_]+)\s*(?:\(([^)]*)\))?`)
	reProg := regexp.MustCompile(`(?i)^program\s+([A-Za-z0-9_]+)`)
	reMod := regexp.MustCompile(`(?i)^module\s+([A-Za-z0-9_]+)`)
	reEnd := regexp.MustCompile(`(?i)^end\s+(function|subroutine|program|module)`)
	var blocks []ftBlock
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		if l == "" || strings.HasPrefix(l, "!") {
			continue
		}
		if m := reFunc.FindStringSubmatch(l); m != nil {
			name := m[2]
			params := parseFtParamList(m[3])
			ret := mapFtType(m[1])
			j := i + 1
			paramTypes := map[string]string{}
			for j < len(lines) {
				t := strings.TrimSpace(lines[j])
				lt := strings.ToLower(t)
				if reEnd.MatchString(lt) {
					break
				}
				if strings.Contains(t, "::") {
					idx := strings.Index(t, "::")
					typ := mapFtType(strings.TrimSpace(t[:idx]))
					names := strings.Split(t[idx+2:], ",")
					for _, n := range names {
						paramTypes[strings.TrimSpace(n)] = typ
					}
					j++
					continue
				}
				if strings.HasPrefix(lt, "implicit none") || t == "" {
					j++
					continue
				}
				break
			}
			for k := range params {
				if t, ok := paramTypes[params[k].name]; ok {
					params[k].typ = t
				}
			}
			for j < len(lines) {
				lt := strings.ToLower(strings.TrimSpace(lines[j]))
				if reEnd.MatchString(lt) && strings.Contains(lt, strings.ToLower(name)) {
					break
				}
				j++
			}
			blocks = append(blocks, ftBlock{name: name, params: params, ret: ret, start: i, end: j})
			i = j
			continue
		}
		if m := reSub.FindStringSubmatch(l); m != nil {
			name := m[1]
			params := parseFtParamList(m[2])
			j := i + 1
			paramTypes := map[string]string{}
			for j < len(lines) {
				t := strings.TrimSpace(lines[j])
				lt := strings.ToLower(t)
				if reEnd.MatchString(lt) {
					break
				}
				if strings.Contains(t, "::") {
					idx := strings.Index(t, "::")
					typ := mapFtType(strings.TrimSpace(t[:idx]))
					names := strings.Split(t[idx+2:], ",")
					for _, n := range names {
						paramTypes[strings.TrimSpace(n)] = typ
					}
					j++
					continue
				}
				if strings.HasPrefix(lt, "implicit none") || t == "" {
					j++
					continue
				}
				break
			}
			for k := range params {
				if t, ok := paramTypes[params[k].name]; ok {
					params[k].typ = t
				}
			}
			for j < len(lines) {
				lt := strings.ToLower(strings.TrimSpace(lines[j]))
				if reEnd.MatchString(lt) && strings.Contains(lt, strings.ToLower(name)) {
					break
				}
				j++
			}
			blocks = append(blocks, ftBlock{name: name, params: params, start: i, end: j})
			i = j
			continue
		}
		if m := reProg.FindStringSubmatch(l); m != nil {
			name := m[1]
			j := i + 1
			for j < len(lines) {
				lt := strings.ToLower(strings.TrimSpace(lines[j]))
				if reEnd.MatchString(lt) && strings.Contains(lt, strings.ToLower(name)) {
					break
				}
				j++
			}
			blocks = append(blocks, ftBlock{name: name, start: i, end: j})
			i = j
			continue
		}
		if m := reMod.FindStringSubmatch(l); m != nil {
			name := m[1]
			j := i + 1
			for j < len(lines) {
				lt := strings.ToLower(strings.TrimSpace(lines[j]))
				if reEnd.MatchString(lt) && strings.Contains(lt, strings.ToLower(name)) {
					break
				}
				j++
			}
			blocks = append(blocks, ftBlock{name: name, start: i, end: j})
			i = j
		}
	}
	return blocks
}

func parseFtParamList(s string) []ftParam {
	if strings.TrimSpace(s) == "" {
		return nil
	}
	var out []ftParam
	for _, p := range strings.Split(s, ",") {
		p = strings.TrimSpace(p)
		if p != "" {
			out = append(out, ftParam{name: p})
		}
	}
	return out
}

func convertFortranFallback(src string) ([]byte, error) {
	blocks := parseFortranBlocks(src)
	if len(blocks) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	var out strings.Builder
	for _, b := range blocks {
		out.WriteString("fun ")
		out.WriteString(b.name)
		out.WriteByte('(')
		for i, p := range b.params {
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
		if b.ret != "" {
			out.WriteString(": ")
			out.WriteString(b.ret)
		}
		sym := protocol.DocumentSymbol{Name: b.name, Range: protocol.Range{Start: protocol.Position{Line: uint32(b.start)}, End: protocol.Position{Line: uint32(b.end)}}}
		body := convertFtBody(src, sym)
		if len(body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, ln := range body {
				out.WriteString(ln)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
	return []byte(out.String()), nil
}

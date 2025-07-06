package any2mochi

import (
	"fmt"
	"os"
	"strings"
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
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != SymbolKindFunction {
			continue
		}
		var params []erlParam
		var ret string
		if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
			if mc, ok := hov.Contents.(MarkupContent); ok {
				params, ret = parseErlangHover(mc.Value)
			}
		}
		if s.Name == "" {
			s.Name = "fun"
		}
		out.WriteString("fun ")
		out.WriteString(s.Name)
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
		if ret != "" && ret != "ok" {
			out.WriteString(": ")
			out.WriteString(ret)
		}
		body := parseErlangBody(src, s.Range)
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
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func parseErlangParams(paramStr string) []erlParam {
	paramStr = strings.TrimSpace(paramStr)
	if paramStr == "" {
		return nil
	}
	parts := strings.Split(paramStr, ",")
	out := make([]erlParam, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		// remove pattern matches like X = Y
		if idx := strings.Index(p, "="); idx >= 0 {
			p = strings.TrimSpace(p[:idx])
		}
		typ := ""
		if idx := strings.Index(p, "::"); idx >= 0 {
			typ = mapErlangType(strings.TrimSpace(p[idx+2:]))
			p = strings.TrimSpace(p[:idx])
		}
		fields := strings.Fields(p)
		name := ""
		if len(fields) > 0 {
			name = fields[len(fields)-1]
		}
		out = append(out, erlParam{name: name, typ: typ})
	}
	return out
}

func parseErlangHover(sig string) ([]erlParam, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	open := strings.Index(sig, "(")
	close := strings.Index(sig, ")")
	arrow := strings.Index(sig, "->")
	if open == -1 || close == -1 || arrow == -1 || close < open || arrow < close {
		return nil, ""
	}
	params := parseErlangParams(sig[open+1 : close])
	ret := strings.TrimSpace(sig[arrow+2:])
	if idx := strings.IndexAny(ret, ". "); idx >= 0 {
		ret = strings.TrimSpace(ret[:idx])
	}
	ret = mapErlangType(ret)
	return params, ret
}

func mapErlangType(t string) string {
	switch strings.TrimSpace(t) {
	case "integer()":
		return "int"
	case "float()", "number()":
		return "float"
	case "binary()", "string()":
		return "string"
	case "boolean()", "bool()":
		return "bool"
	default:
		return strings.TrimSpace(t)
	}
}

func offsetFromPosition(src string, pos Position) int {
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

func parseErlangBody(src string, rng Range) []string {
	start := offsetFromPosition(src, rng.Start)
	end := offsetFromPosition(src, rng.End)
	if start >= end || start < 0 || end > len(src) {
		return nil
	}
	body := strings.TrimSpace(src[start:end])
	if idx := strings.Index(body, "->"); idx >= 0 {
		body = body[idx+2:]
	}
	body = strings.TrimSpace(body)
	if strings.HasSuffix(body, ".") {
		body = strings.TrimSuffix(body, ".")
	}
	stmts := strings.Split(body, ",")
	var out []string
	for i, s := range stmts {
		s = strings.TrimSpace(s)
		if s == "" {
			continue
		}
		if eq := strings.Index(s, "="); eq >= 0 && !strings.Contains(s[:eq], " ") {
			name := strings.TrimSpace(s[:eq])
			expr := strings.TrimSpace(s[eq+1:])
			out = append(out, "  var "+name+" = "+expr)
			continue
		}
		if i == len(stmts)-1 {
			out = append(out, "  return "+s)
		} else {
			out = append(out, "  "+s)
		}
	}
	return out
}

type erlParam struct {
	name string
	typ  string
}

// ConvertErlangFile reads the erlang file and converts it to Mochi.
func ConvertErlangFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertErlang(string(data))
}

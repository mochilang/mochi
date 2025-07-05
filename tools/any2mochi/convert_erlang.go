package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// ConvertErlang converts Erlang source code to Mochi using simple regular
// expression parsing. Type information is not extracted.
func ConvertErlang(src string) ([]byte, error) {
	funcs := parseErlangFuncs(src)
	var out strings.Builder
	for _, fn := range funcs {
		out.WriteString("fun ")
		if fn.name == "" {
			out.WriteString("fun")
		} else {
			out.WriteString(fn.name)
		}
		out.WriteByte('(')
		for i, p := range fn.params {
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
		if len(fn.body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, line := range fn.body {
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

// parseErlangBodyText converts a comma separated list of Erlang expressions
// into a slice of Mochi statements.
func parseErlangBodyText(body string) []string {
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

type erlFunc struct {
	name   string
	params []erlParam
	body   []string
}

var erlFuncRE = regexp.MustCompile(`(?ms)^([a-zA-Z0-9_]+)\s*\(([^)]*)\)\s*->\s*(.*?)\.`)

// parseErlangFuncs finds simple top-level function definitions using regular
// expressions. It ignores multiple clauses and only converts the first match for
// each function name.
func parseErlangFuncs(src string) []erlFunc {
	matches := erlFuncRE.FindAllStringSubmatch(src, -1)
	funcs := make([]erlFunc, 0, len(matches))
	for _, m := range matches {
		params := parseErlangParams(m[2])
		body := parseErlangBodyText(m[3])
		funcs = append(funcs, erlFunc{name: m[1], params: params, body: body})
	}
	return funcs
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

package any2mochi

import (
	"regexp"
	"strings"
)

// ErlParam represents a function parameter.
type ErlParam struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// ErlFunc represents a top-level Erlang function.
type ErlFunc struct {
	Name   string     `json:"name"`
	Params []ErlParam `json:"params"`
	Body   []string   `json:"body"`
}

// MapErlangType converts common Erlang types to Mochi types.
func MapErlangType(t string) string {
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

// ParseErlangParams parses a function parameter list.
func ParseErlangParams(paramStr string) []ErlParam {
	paramStr = strings.TrimSpace(paramStr)
	if paramStr == "" {
		return nil
	}
	parts := strings.Split(paramStr, ",")
	out := make([]ErlParam, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if idx := strings.Index(p, "="); idx >= 0 {
			p = strings.TrimSpace(p[:idx])
		}
		typ := ""
		if idx := strings.Index(p, "::"); idx >= 0 {
			typ = MapErlangType(strings.TrimSpace(p[idx+2:]))
			p = strings.TrimSpace(p[:idx])
		}
		fields := strings.Fields(p)
		name := ""
		if len(fields) > 0 {
			name = fields[len(fields)-1]
		}
		out = append(out, ErlParam{Name: name, Type: typ})
	}
	return out
}

// parseErlangBodyText converts a comma separated list of Erlang expressions into Mochi statements.
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

var erlFuncRE = regexp.MustCompile(`(?ms)^([a-zA-Z0-9_]+)\s*\(([^)]*)\)\s*->\s*(.*?)\.`)

// ParseErlangFuncs finds simple top-level function definitions using regular expressions.
func ParseErlangFuncs(src string) []ErlFunc {
	matches := erlFuncRE.FindAllStringSubmatch(src, -1)
	funcs := make([]ErlFunc, 0, len(matches))
	for _, m := range matches {
		params := ParseErlangParams(m[2])
		body := parseErlangBodyText(m[3])
		funcs = append(funcs, ErlFunc{Name: m[1], Params: params, Body: body})
	}
	return funcs
}

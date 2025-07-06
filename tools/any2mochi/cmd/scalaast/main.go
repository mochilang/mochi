package main

import (
	"encoding/json"
	"io"
	"os"
	"regexp"
	"strings"
)

type scalaFunc struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   []string `json:"body"`
}

func parseParams(s string) []string {
	var out []string
	for _, p := range strings.Split(s, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if idx := strings.Index(p, ":"); idx != -1 {
			p = p[:idx]
		}
		p = strings.TrimSpace(p)
		if p != "" {
			out = append(out, p)
		}
	}
	return out
}

func parseScala(src string) []scalaFunc {
	lines := strings.Split(src, "\n")
	reDef := regexp.MustCompile(`^\s*def\s+([a-zA-Z0-9_]+)\s*\(([^)]*)\)`)
	var funcs []scalaFunc
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		m := reDef.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		name := m[1]
		params := parseParams(m[2])
		braceDepth := strings.Count(line, "{") - strings.Count(line, "}")
		var body []string
		// capture any code after the first '{'
		if idx := strings.Index(line, "{"); idx != -1 {
			rest := strings.TrimSpace(line[idx+1:])
			if rest != "" {
				body = append(body, rest)
			}
		}
		for i++; i < len(lines); i++ {
			l := lines[i]
			braceDepth += strings.Count(l, "{")
			braceDepth -= strings.Count(l, "}")
			trimmed := strings.TrimSpace(l)
			if trimmed != "" && trimmed != "}" {
				body = append(body, trimmed)
			}
			if braceDepth <= 0 {
				break
			}
		}
		funcs = append(funcs, scalaFunc{Name: name, Params: params, Body: body})
	}
	return funcs
}

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	funcs := parseScala(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.Encode(funcs)
}

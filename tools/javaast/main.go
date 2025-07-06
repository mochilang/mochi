package main

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

type Program struct {
	Structs []JStruct `json:"structs"`
	Funcs   []JFunc   `json:"funcs"`
}

type JStruct struct {
	Name   string   `json:"name"`
	Fields []string `json:"fields"`
}

type JFunc struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Lines  []string `json:"lines"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: javaast <file>")
		os.Exit(1)
	}
	data, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	ast := parse(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(ast); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func parse(src string) Program {
	lines := strings.Split(src, "\n")
	var prog Program
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" {
			continue
		}
		if strings.HasPrefix(line, "static class ") {
			name := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(line, "static class "), "{"))
			var fields []string
			i++
			for i < len(lines) {
				l := strings.TrimSpace(lines[i])
				if strings.HasPrefix(l, "}") {
					break
				}
				if strings.HasSuffix(l, ";") && !strings.Contains(l, "(") {
					_, field := parseField(l)
					if field != "" {
						fields = append(fields, field)
					}
				}
				i++
			}
			prog.Structs = append(prog.Structs, JStruct{Name: name, Fields: fields})
		} else if (strings.HasPrefix(line, "public static") || strings.HasPrefix(line, "static ")) && strings.Contains(line, "(") && strings.HasSuffix(line, "{") {
			l := line
			l = strings.Replace(l, "public ", "", 1)
			l = strings.TrimPrefix(l, "static ")
			l = strings.TrimSuffix(l, "{")
			l = strings.TrimSpace(l)
			parts := strings.SplitN(l, " ", 2)
			if len(parts) != 2 {
				continue
			}
			ret := parts[0]
			rest := parts[1]
			open := strings.Index(rest, "(")
			close := strings.LastIndex(rest, ")")
			if open == -1 || close == -1 {
				continue
			}
			name := strings.TrimSpace(rest[:open])
			paramsPart := rest[open+1 : close]
			params := parseParams(paramsPart)
			var body []string
			depth := 1
			i++
			for i < len(lines) {
				ln := strings.TrimSpace(lines[i])
				depth += strings.Count(ln, "{")
				depth -= strings.Count(ln, "}")
				if depth <= 0 {
					break
				}
				body = append(body, ln)
				i++
			}
			_ = ret
			prog.Funcs = append(prog.Funcs, JFunc{Name: name, Params: params, Lines: body})
		}
	}
	return prog
}

func parseParams(s string) []string {
	var out []string
	for _, p := range strings.Split(s, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if i := strings.LastIndex(p, " "); i != -1 {
			out = append(out, strings.TrimSpace(p[i+1:]))
		} else {
			out = append(out, p)
		}
	}
	return out
}

func parseField(line string) (string, string) {
	line = strings.TrimSuffix(line, ";")
	idx := strings.LastIndex(line, " ")
	if idx == -1 {
		return "", line
	}
	return strings.TrimSpace(line[:idx]), strings.TrimSpace(line[idx+1:])
}

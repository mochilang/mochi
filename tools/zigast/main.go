package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"
)

type AST struct {
	Vars      []Var  `json:"vars"`
	Functions []Func `json:"functions"`
}

type Var struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type Func struct {
	Name   string   `json:"name"`
	Params string   `json:"params"`
	Ret    string   `json:"ret"`
	Lines  []string `json:"lines"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: zigast <file>")
		os.Exit(1)
	}
	var data []byte
	var err error
	if os.Args[1] == "-" {
		data, err = io.ReadAll(os.Stdin)
	} else {
		data, err = os.ReadFile(os.Args[1])
	}
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

var funRE = regexp.MustCompile(`^(?:pub\s+)?fn\s+([A-Za-z0-9_]+)\s*\(([^)]*)\)\s*([^\s{]+)?\s*{`)

func parse(src string) AST {
	sc := bufio.NewScanner(strings.NewReader(src))
	var ast AST
	var cur *Func
	depth := 0
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if cur == nil {
			if funRE.MatchString(line) {
				m := funRE.FindStringSubmatch(line)
				f := Func{Name: m[1], Params: strings.TrimSpace(m[2]), Ret: strings.TrimSpace(m[3])}
				ast.Functions = append(ast.Functions, f)
				cur = &ast.Functions[len(ast.Functions)-1]
				depth = 1
				if idx := strings.Index(line, "{"); idx != -1 && idx < len(line)-1 {
					rest := strings.TrimSpace(line[idx+1:])
					if rest != "" {
						cur.Lines = append(cur.Lines, rest)
					}
				}
				continue
			}
			if strings.HasPrefix(line, "const ") || strings.HasPrefix(line, "var ") {
				fields := strings.Fields(line)
				if len(fields) >= 2 {
					name := fields[1]
					typ := ""
					if i := strings.Index(name, ":"); i != -1 {
						typ = strings.TrimSpace(name[i+1:])
						name = name[:i]
					}
					ast.Vars = append(ast.Vars, Var{Name: name, Type: typ})
				}
			}
			continue
		}
		depth += strings.Count(line, "{")
		depth -= strings.Count(line, "}")
		if depth <= 0 {
			// handle possible code before closing brace
			if idx := strings.Index(line, "}"); idx != -1 {
				l := strings.TrimSpace(line[:idx])
				if l != "" {
					cur.Lines = append(cur.Lines, l)
				}
			}
			cur = nil
			depth = 0
			continue
		}
		cur.Lines = append(cur.Lines, line)
	}
	return ast
}

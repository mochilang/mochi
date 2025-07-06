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

type Program struct {
	Classes   []Class    `json:"classes,omitempty"`
	Functions []Function `json:"functions,omitempty"`
	Vars      []VarDecl  `json:"vars,omitempty"`
}

type Class struct {
	Name    string     `json:"name"`
	Fields  []Field    `json:"fields"`
	Methods []Function `json:"methods,omitempty"`
}

type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type VarDecl struct {
	Name  string `json:"name"`
	Type  string `json:"type,omitempty"`
	Value string `json:"value"`
}

type Function struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Lines  []string `json:"lines"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: ktast <file>")
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

func parse(src string) Program {
	sc := bufio.NewScanner(strings.NewReader(src))
	funRE := regexp.MustCompile(`^fun\s+([A-Za-z0-9_]+)\s*\(([^)]*)\)`)
	classRE := regexp.MustCompile(`^(?:data\s+)?class\s+([A-Za-z0-9_]+)\s*(\(([^)]*)\))?`)
	varRE := regexp.MustCompile(`^(val|var)\s+([A-Za-z0-9_]+)(?:\s*:\s*([^=]+))?\s*=\s*(.+)`)

	var prog Program
	var curFn *Function
	var depth int
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		switch {
		case curFn != nil:
			depth += strings.Count(line, "{")
			depth -= strings.Count(line, "}")
			if depth <= 0 && line == "}" {
				curFn = nil
				depth = 0
				continue
			}
			curFn.Lines = append(curFn.Lines, line)
			if depth <= 0 {
				curFn = nil
				depth = 0
			}
		default:
			if funRE.MatchString(line) {
				m := funRE.FindStringSubmatch(line)
				fn := Function{Name: m[1], Params: parseParams(m[2])}
				prog.Functions = append(prog.Functions, fn)
				curFn = &prog.Functions[len(prog.Functions)-1]
				depth = strings.Count(line, "{") - strings.Count(line, "}")
				continue
			}
			if classRE.MatchString(line) {
				m := classRE.FindStringSubmatch(line)
				cls := Class{Name: m[1], Fields: parseFields(m[3])}
				prog.Classes = append(prog.Classes, cls)
				continue
			}
			if varRE.MatchString(line) {
				m := varRE.FindStringSubmatch(line)
				v := VarDecl{Name: m[2], Type: strings.TrimSpace(m[3]), Value: strings.TrimSpace(m[4])}
				prog.Vars = append(prog.Vars, v)
				continue
			}
		}
	}
	return prog
}

func parseFields(s string) []Field {
	var out []Field
	for _, p := range strings.Split(s, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		parts := strings.SplitN(p, ":", 2)
		if len(parts) == 2 {
			name := strings.TrimSpace(strings.TrimPrefix(parts[0], "val"))
			name = strings.TrimSpace(strings.TrimPrefix(name, "var"))
			out = append(out, Field{Name: strings.TrimSpace(name), Type: strings.TrimSpace(parts[1])})
		}
	}
	return out
}

func parseParams(s string) []string {
	var out []string
	for _, p := range strings.Split(s, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if i := strings.Index(p, ":"); i != -1 {
			p = p[:i]
		}
		out = append(out, strings.TrimSpace(p))
	}
	return out
}

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
	Name      string     `json:"name"`
	Kind      string     `json:"kind,omitempty"`
	Data      bool       `json:"data,omitempty"`
	Sealed    bool       `json:"sealed,omitempty"`
	Extends   string     `json:"extends,omitempty"`
	Fields    []Field    `json:"fields"`
	Methods   []Function `json:"methods,omitempty"`
	StartLine int        `json:"start,omitempty"`
	StartCol  int        `json:"startCol,omitempty"`
	EndLine   int        `json:"end,omitempty"`
	EndCol    int        `json:"endCol,omitempty"`
	Snippet   string     `json:"snippet,omitempty"`
}

type Field struct {
	Name      string `json:"name"`
	Type      string `json:"type"`
	StartLine int    `json:"start,omitempty"`
	StartCol  int    `json:"startCol,omitempty"`
	EndLine   int    `json:"end,omitempty"`
	EndCol    int    `json:"endCol,omitempty"`
	Snippet   string `json:"snippet,omitempty"`
}

type Param struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

type VarDecl struct {
	Name      string `json:"name"`
	Type      string `json:"type,omitempty"`
	Value     string `json:"value"`
	StartLine int    `json:"start,omitempty"`
	StartCol  int    `json:"startCol,omitempty"`
	EndLine   int    `json:"end,omitempty"`
	EndCol    int    `json:"endCol,omitempty"`
	Snippet   string `json:"snippet,omitempty"`
}

type Function struct {
	Name      string   `json:"name"`
	Params    []Param  `json:"params"`
	Ret       string   `json:"ret,omitempty"`
	Lines     []string `json:"lines"`
	StartLine int      `json:"start,omitempty"`
	StartCol  int      `json:"startCol,omitempty"`
	EndLine   int      `json:"end,omitempty"`
	EndCol    int      `json:"endCol,omitempty"`
	Snippet   string   `json:"snippet,omitempty"`
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
	funRE := regexp.MustCompile(`^fun\s+([A-Za-z0-9_]+)\s*\(([^)]*)\)(?:\s*:\s*([^\s{]+))?`)
	classRE := regexp.MustCompile(`^(?:(sealed)\s+)?(?:(data)\s+)?(class|interface)\s+([A-Za-z0-9_]+)(?:\s*\(([^)]*)\))?(?:\s*:\s*([^\s{]+))?`)
	varRE := regexp.MustCompile(`^(val|var)\s+([A-Za-z0-9_]+)(?:\s*:\s*([^=]+))?\s*=\s*(.+)`)

	var prog Program
	var curFn *Function
	var curClass *Class
	var depth int
	var classDepth int
	lineNum := 0
	for sc.Scan() {
		orig := sc.Text()
		line := strings.TrimSpace(orig)
		lineNum++
		startCol := strings.Index(orig, line)
		if startCol < 0 {
			startCol = 0
		}
		startCol++
		endCol := startCol + len(line)
		switch {
		case curFn != nil:
			depth += strings.Count(line, "{")
			depth -= strings.Count(line, "}")
			if depth <= 0 && line == "}" {
				curFn.EndLine = lineNum
				curFn.EndCol = endCol
				curFn = nil
				depth = 0
				continue
			}
			curFn.Lines = append(curFn.Lines, line)
			if depth <= 0 {
				curFn.EndLine = lineNum
				curFn.EndCol = endCol
				curFn = nil
				depth = 0
			}
		case curClass != nil:
			classDepth += strings.Count(line, "{")
			classDepth -= strings.Count(line, "}")
			if funRE.MatchString(line) {
				m := funRE.FindStringSubmatch(line)
				fn := Function{Name: m[1], Params: parseParams(m[2]), Ret: strings.TrimSpace(m[3]), StartLine: lineNum, StartCol: startCol, Snippet: orig}
				curClass.Methods = append(curClass.Methods, fn)
				curFn = &curClass.Methods[len(curClass.Methods)-1]
				depth = strings.Count(line, "{") - strings.Count(line, "}")
				continue
			}
			if classDepth <= 0 && line == "}" {
				curClass.EndLine = lineNum
				curClass.EndCol = endCol
				curClass = nil
				classDepth = 0
				continue
			}
		default:
			if funRE.MatchString(line) {
				m := funRE.FindStringSubmatch(line)
				fn := Function{Name: m[1], Params: parseParams(m[2]), Ret: strings.TrimSpace(m[3]), StartLine: lineNum, StartCol: startCol, Snippet: orig}
				prog.Functions = append(prog.Functions, fn)
				curFn = &prog.Functions[len(prog.Functions)-1]
				depth = strings.Count(line, "{") - strings.Count(line, "}")
				continue
			}
			if classRE.MatchString(line) {
				m := classRE.FindStringSubmatch(line)
				cls := Class{
					Name:      m[4],
					Kind:      m[3],
					Data:      m[2] != "",
					Sealed:    m[1] != "",
					Extends:   strings.TrimSpace(m[6]),
					Fields:    parseFields(m[5]),
					StartLine: lineNum,
					StartCol:  startCol,
					Snippet:   orig,
				}
				prog.Classes = append(prog.Classes, cls)
				classDepth = strings.Count(line, "{") - strings.Count(line, "}")
				if classDepth > 0 {
					curClass = &prog.Classes[len(prog.Classes)-1]
				}
				continue
			}
			if varRE.MatchString(line) {
				m := varRE.FindStringSubmatch(line)
				v := VarDecl{Name: m[2], Type: strings.TrimSpace(m[3]), Value: strings.TrimSpace(m[4]), StartLine: lineNum, StartCol: startCol, Snippet: orig}
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
			out = append(out, Field{Name: strings.TrimSpace(name), Type: strings.TrimSpace(parts[1]), Snippet: p})
		}
	}
	return out
}

func parseParams(s string) []Param {
	var out []Param
	for _, p := range strings.Split(s, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		name := p
		typ := ""
		if i := strings.Index(p, ":"); i != -1 {
			name = strings.TrimSpace(p[:i])
			typ = strings.TrimSpace(p[i+1:])
		}
		out = append(out, Param{Name: name, Type: typ})
	}
	return out
}

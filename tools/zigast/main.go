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
	Vars      []Var    `json:"vars"`
	Structs   []Struct `json:"structs"`
	Functions []Func   `json:"functions"`
}

type Var struct {
	Name string `json:"name"`
	Type string `json:"type"`
	Line int    `json:"line"`
}

type Func struct {
	Name   string   `json:"name"`
	Params string   `json:"params"`
	Ret    string   `json:"ret"`
	Line   int      `json:"line"`
	Lines  []string `json:"lines"`
}

type Struct struct {
	Name   string  `json:"name"`
	Line   int     `json:"line"`
	Fields []Field `json:"fields"`
}

type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
	Line int    `json:"line"`
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
var structStartRE = regexp.MustCompile(`^(?:pub\s+)?const\s+([A-Za-z0-9_]+)\s*=\s*struct\s*{`)

func parse(src string) AST {
	sc := bufio.NewScanner(strings.NewReader(src))
	var ast AST
	var curFunc *Func
	var curStruct *Struct
	depth := 0
	lineNum := 0
	for sc.Scan() {
		raw := sc.Text()
		line := strings.TrimSpace(raw)

		if curFunc == nil && curStruct == nil {
			if funRE.MatchString(line) {
				m := funRE.FindStringSubmatch(line)
				f := Func{Name: m[1], Params: strings.TrimSpace(m[2]), Ret: strings.TrimSpace(m[3]), Line: lineNum + 1}
				ast.Functions = append(ast.Functions, f)
				curFunc = &ast.Functions[len(ast.Functions)-1]
				depth = 1
				if idx := strings.Index(line, "{"); idx != -1 && idx < len(line)-1 {
					rest := strings.TrimSpace(line[idx+1:])
					if rest != "" {
						curFunc.Lines = append(curFunc.Lines, rest)
					}
				}
				lineNum++
				continue
			}
			if structStartRE.MatchString(line) {
				m := structStartRE.FindStringSubmatch(line)
				s := Struct{Name: m[1], Line: lineNum + 1}
				ast.Structs = append(ast.Structs, s)
				curStruct = &ast.Structs[len(ast.Structs)-1]
				depth = 1
				lineNum++
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
					ast.Vars = append(ast.Vars, Var{Name: name, Type: typ, Line: lineNum + 1})
				}
			}
			lineNum++
			continue
		}

		if curFunc != nil {
			depth += strings.Count(line, "{")
			depth -= strings.Count(line, "}")
			if depth <= 0 {
				if idx := strings.Index(line, "}"); idx != -1 {
					l := strings.TrimSpace(line[:idx])
					if l != "" {
						curFunc.Lines = append(curFunc.Lines, l)
					}
				}
				curFunc = nil
				depth = 0
				lineNum++
				continue
			}
			curFunc.Lines = append(curFunc.Lines, line)
			lineNum++
			continue
		}

		if curStruct != nil {
			if strings.Contains(line, "};") || line == "}" || line == "};" {
				if idx := strings.Index(line, "};"); idx != -1 {
					field := strings.TrimSpace(line[:idx])
					if field != "" && !strings.HasPrefix(field, "/") {
						if parts := strings.SplitN(strings.TrimSuffix(field, ","), ":", 2); len(parts) == 2 {
							name := strings.TrimSpace(parts[0])
							typ := strings.TrimSpace(parts[1])
							curStruct.Fields = append(curStruct.Fields, Field{Name: name, Type: typ, Line: lineNum + 1})
						}
					}
				}
				curStruct = nil
				depth = 0
				lineNum++
				continue
			}
			if line != "" && !strings.HasPrefix(line, "/") {
				if parts := strings.SplitN(strings.TrimSuffix(line, ","), ":", 2); len(parts) == 2 {
					name := strings.TrimSpace(parts[0])
					typ := strings.TrimSpace(parts[1])
					curStruct.Fields = append(curStruct.Fields, Field{Name: name, Type: typ, Line: lineNum + 1})
				}
			}
			lineNum++
			continue
		}

		lineNum++
	}
	return ast
}

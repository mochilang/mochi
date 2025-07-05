package any2mochi

import (
	"regexp"
	"strings"
)

// DartProgram represents a very small subset of Dart parsed into a generic AST.
type DartProgram struct {
	Decls []DartDecl `json:"decls"`
}

type DartDecl struct {
	Kind   string      `json:"kind"`
	Name   string      `json:"name,omitempty"`
	Params []string    `json:"params,omitempty"`
	Fields []DartField `json:"fields,omitempty"`
	Body   []string    `json:"body,omitempty"`
	Stmt   string      `json:"stmt,omitempty"`
}

type DartField struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

// ParseDartToAST parses the given Dart source using simple regex rules.
// It supports only a tiny subset of the language and is intended as a
// best-effort fallback when no real parser is available.
func ParseDartToAST(src string) *DartProgram {
	stmts := dartSplitStatements(src)
	funcRE := regexp.MustCompile(`^(?:[A-Za-z0-9_<>,\[\]\?]+\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)$`)
	fieldRE := regexp.MustCompile(`^(?:final\s+|var\s+)?([A-Za-z0-9_<>,\[\]\?]+)\s+(\w+)`)
	parseParams := func(s string) []string {
		var params []string
		for _, p := range strings.Split(s, ",") {
			p = strings.TrimSpace(p)
			p = strings.TrimPrefix(p, "required ")
			if eq := strings.Index(p, "="); eq != -1 {
				p = p[:eq]
			}
			fields := strings.Fields(p)
			if len(fields) == 0 {
				continue
			}
			name := fields[len(fields)-1]
			if strings.HasPrefix(name, "this.") {
				name = strings.TrimPrefix(name, "this.")
			}
			params = append(params, name)
		}
		return params
	}

	var prog DartProgram
	for i := 0; i < len(stmts); i++ {
		s := strings.TrimSpace(stmts[i])
		if s == "" {
			continue
		}
		nextBlock := i+1 < len(stmts) && stmts[i+1] == "{"
		if nextBlock && strings.HasPrefix(s, "class ") {
			name := strings.TrimSpace(strings.TrimPrefix(s, "class "))
			decl := DartDecl{Kind: "class", Name: name}
			i += 2 // skip "{" token
			for i < len(stmts) {
				line := strings.TrimSpace(stmts[i])
				if line == "}" {
					break
				}
				if m := fieldRE.FindStringSubmatch(line); m != nil {
					decl.Fields = append(decl.Fields, DartField{Name: m[2], Type: dartToMochiType(m[1])})
				}
				i++
			}
			prog.Decls = append(prog.Decls, decl)
			continue
		}
		if nextBlock && funcRE.MatchString(s) {
			m := funcRE.FindStringSubmatch(s)
			decl := DartDecl{Kind: "func", Name: m[1], Params: parseParams(m[2])}
			i += 2 // skip "{" token
			for i < len(stmts) {
				line := strings.TrimSpace(stmts[i])
				if line == "}" {
					break
				}
				decl.Body = append(decl.Body, line)
				i++
			}
			prog.Decls = append(prog.Decls, decl)
			continue
		}
		prog.Decls = append(prog.Decls, DartDecl{Kind: "stmt", Stmt: s})
	}
	return &prog
}

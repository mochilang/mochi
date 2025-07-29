//go:build slow

package fs

// Package fs implements a small F# parser for the any2mochi tool. It relies on
// the official F# compiler service via a small helper script to obtain a JSON
// representation of the source.

import (
	"encoding/json"
	"os"
	"os/exec"
	"regexp"
	"strings"

	_ "embed"
)

//go:embed parse.fsx
var parseScript string

type Var struct {
	Name    string `json:"name"`
	Expr    string `json:"expr"`
	Mutable bool   `json:"mutable"`
	Type    string `json:"type"`
	Line    int    `json:"line"`
	Raw     string `json:"raw"`
}

type Assign struct {
	Name  string `json:"name"`
	Index string `json:"index"`
	Expr  string `json:"expr"`
	Line  int    `json:"line"`
	Raw   string `json:"raw"`
}

type ForRange struct {
	Var   string `json:"var"`
	Start string `json:"start"`
	End   string `json:"end"`
	Body  []Stmt `json:"body"`
	Line  int    `json:"line"`
	Raw   string `json:"raw"`
}

type ForIn struct {
	Var  string `json:"var"`
	Expr string `json:"expr"`
	Body []Stmt `json:"body"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type While struct {
	Cond string `json:"cond"`
	Body []Stmt `json:"body"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type PrintStmt struct {
	Expr string `json:"expr"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type Stmt interface{}

// Fun represents a simple function declaration parsed from the generated F# code.
type Fun struct {
	Name   string  `json:"name"`
	Params []Field `json:"params"`
	Ret    string  `json:"ret"`
	Body   []Stmt  `json:"body"`
	Line   int     `json:"line"`
	Raw    string  `json:"raw"`
}

// Return models a return statement which originates from a raise expression.
type Return struct {
	Expr string `json:"expr"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

// Break represents a loop break statement.
type Break struct {
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

// Continue represents a loop continue statement.
type Continue struct {
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

// If represents a simple if-else statement.
type If struct {
	Cond string `json:"cond"`
	Then []Stmt `json:"then"`
	Else []Stmt `json:"else"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type Variant struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
}

type TypeDecl struct {
	Name     string    `json:"name"`
	Fields   []Field   `json:"fields"`
	Variants []Variant `json:"variants"`
	Line     int       `json:"line"`
	Raw      string    `json:"raw"`
}

type Expect struct {
	Cond string `json:"cond"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type Program struct {
	// Source holds the original F# input. It is preserved so the
	// converter can emit the original program as a block comment.
	Source string   `json:"source"`
	Vars   []Var    `json:"vars"`
	Prints []string `json:"prints"`
	Stmts  []Stmt   `json:"stmts"`
}

// Parse invokes a small F# script that uses the official compiler service to
// parse the provided source and return a JSON representation compatible with
// the Program structure. The script is executed via `dotnet fsi`. If the
// command fails the error is forwarded to the caller.
func Parse(src string) (*Program, error) {
	if _, err := exec.LookPath("dotnet"); err == nil {
		tmp, err := os.CreateTemp("", "parse-*.fsx")
		if err != nil {
			return nil, err
		}
		if _, err := tmp.WriteString(parseScript); err != nil {
			tmp.Close()
			os.Remove(tmp.Name())
			return nil, err
		}
		tmp.Close()
		defer os.Remove(tmp.Name())

		cmd := exec.Command("dotnet", "fsi", tmp.Name())
		cmd.Stdin = strings.NewReader(src)
		out, err := cmd.Output()
		if err == nil {
			var p Program
			if err := json.Unmarshal(out, &p); err == nil {
				p.Source = src
				return &p, nil
			}
		}
	}
	return parseFallback(src)
}

// ParseFile reads the F# source at path and returns the parsed Program.
func ParseFile(path string) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}

var (
	reLet    = regexp.MustCompile(`^let\s+(mutable\s+)?([a-zA-Z_][\w]*)?(?::\s*([^=]+))?\s*=\s*(.+)$`)
	rePrint  = regexp.MustCompile(`^printfn\s+"[^"]*"\s*(.*)$`)
	reAssign = regexp.MustCompile(`^([a-zA-Z_][\w]*(?:\.\[[^\]]+\])?)\s*<-\s*(.+)$`)
	reFor    = regexp.MustCompile(`^for\s+([a-zA-Z_][\w]*)\s+in\s+(.+)\s+\.\.\s+(.+)\s+do$`)
	reWhile  = regexp.MustCompile(`^while\s+(.+)\s+do$`)
	reIf     = regexp.MustCompile(`^if\s+(.+)\s+then$`)
	reElse   = regexp.MustCompile(`^else$`)
)

func parseFallback(src string) (*Program, error) {
	lines := strings.Split(strings.TrimSpace(src), "\n")
	p := &Program{Source: src}
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" || strings.HasPrefix(line, "//") {
			continue
		}
		if m := reLet.FindStringSubmatch(line); m != nil {
			p.Vars = append(p.Vars, Var{
				Name:    m[2],
				Mutable: m[1] != "",
				Type:    strings.TrimSpace(m[3]),
				Expr:    strings.TrimSpace(m[4]),
			})
			continue
		}
		if m := rePrint.FindStringSubmatch(line); m != nil {
			expr := strings.TrimSpace(m[1])
			if len(p.Stmts) > 0 {
				p.Stmts = append(p.Stmts, PrintStmt{Expr: expr})
			} else {
				p.Prints = append(p.Prints, expr)
			}
			continue
		}
		if m := reAssign.FindStringSubmatch(line); m != nil {
			name := m[1]
			idx := ""
			if j := strings.Index(name, ".["); j != -1 && strings.HasSuffix(name, "]") {
				idx = name[j+2 : len(name)-1]
				name = name[:j]
			}
			p.Stmts = append(p.Stmts, Assign{Name: name, Index: idx, Expr: strings.TrimSpace(m[2])})
			continue
		}
		if m := reFor.FindStringSubmatch(line); m != nil {
			var body []Stmt
			if i+1 < len(lines) {
				i++
				if st := parseLine(strings.TrimSpace(lines[i])); st != nil {
					body = append(body, st)
				}
			}
			p.Stmts = append(p.Stmts, ForRange{Var: m[1], Start: strings.TrimSpace(m[2]), End: strings.TrimSpace(m[3]), Body: body})
			continue
		}
		if m := reWhile.FindStringSubmatch(line); m != nil {
			var body []Stmt
			for i+1 < len(lines) {
				next := strings.TrimSpace(lines[i+1])
				if next == "" || reIf.MatchString(next) || reFor.MatchString(next) || reWhile.MatchString(next) {
					break
				}
				if st := parseLine(next); st != nil {
					body = append(body, st)
				}
				i++
			}
			p.Stmts = append(p.Stmts, While{Cond: strings.TrimSpace(m[1]), Body: body})
			continue
		}
		if m := reIf.FindStringSubmatch(line); m != nil {
			var thenBody, elseBody []Stmt
			if i+1 < len(lines) {
				i++
				if st := parseLine(strings.TrimSpace(lines[i])); st != nil {
					thenBody = append(thenBody, st)
				}
			}
			if i+1 < len(lines) && reElse.MatchString(strings.TrimSpace(lines[i+1])) {
				i += 2
				if i < len(lines) {
					if st := parseLine(strings.TrimSpace(lines[i])); st != nil {
						elseBody = append(elseBody, st)
					}
				}
			}
			p.Stmts = append(p.Stmts, If{Cond: strings.TrimSpace(m[1]), Then: thenBody, Else: elseBody})
			continue
		}
	}
	return p, nil
}

func parseLine(line string) Stmt {
	if line == "" {
		return nil
	}
	if m := rePrint.FindStringSubmatch(line); m != nil {
		return PrintStmt{Expr: strings.TrimSpace(m[1])}
	}
	if m := reAssign.FindStringSubmatch(line); m != nil {
		name := m[1]
		idx := ""
		if j := strings.Index(name, ".["); j != -1 && strings.HasSuffix(name, "]") {
			idx = name[j+2 : len(name)-1]
			name = name[:j]
		}
		return Assign{Name: name, Index: idx, Expr: strings.TrimSpace(m[2])}
	}
	if m := reLet.FindStringSubmatch(line); m != nil {
		return Var{Name: m[2], Mutable: m[1] != "", Type: strings.TrimSpace(m[3]), Expr: strings.TrimSpace(m[4])}
	}
	return nil
}

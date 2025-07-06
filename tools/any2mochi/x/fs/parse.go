package fs

import (
	"bufio"
	"regexp"
	"strings"
)

// Var represents a simple variable declaration extracted from the F# source.
type Var struct {
	Name string `json:"name"`
	Expr string `json:"expr"`
}

// Program is the parsed representation used by the converter.
type Program struct {
	Vars   []Var    `json:"vars"`
	Prints []string `json:"prints"`
}

var (
	printRe = regexp.MustCompile(`ignore\s*\(printfn\s*"%A"\s*\((.*)\)\)`)
	letRe   = regexp.MustCompile(`^let\s+(\w+)\s*=\s*(.*)$`)
)

func parseAST(src string) (*Program, error) {
	scanner := bufio.NewScanner(strings.NewReader(src))
	prog := &Program{}
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if m := printRe.FindStringSubmatch(line); m != nil {
			expr := strings.TrimSpace(m[1])
			prog.Prints = append(prog.Prints, expr)
			continue
		}
		if m := letRe.FindStringSubmatch(line); m != nil {
			prog.Vars = append(prog.Vars, Var{Name: m[1], Expr: strings.TrimSpace(m[2])})
		}
	}
	return prog, nil
}

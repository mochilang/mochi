package fs

// Package fs provides a tiny F# parser used by the any2mochi tool. It mirrors
// the behaviour of the previous fsparse command but avoids spawning an
// external process.

import (
	"bufio"
	"regexp"
	"strings"
)

type Var struct {
	Name string `json:"name"`
	Expr string `json:"expr"`
}

type Program struct {
	Vars   []Var    `json:"vars"`
	Prints []string `json:"prints"`
}

var (
	printRe = regexp.MustCompile(`ignore\s*\(printfn\s*"%A"\s*\((.*)\)\)`)
	letRe   = regexp.MustCompile(`^let\s+(\w+)\s*=\s*(.*)$`)
)

// Parse performs a very small subset of F# parsing using regular expressions.
// It mirrors the functionality of the removed fsparse command.
func Parse(src string) (*Program, error) {
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
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return prog, nil
}

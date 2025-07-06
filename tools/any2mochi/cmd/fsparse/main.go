package main

import (
	"bufio"
	"encoding/json"
	"os"
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

var printRe = regexp.MustCompile(`ignore\s*\(printfn\s*"%A"\s*\((.*)\)\)`)
var letRe = regexp.MustCompile(`^let\s+(\w+)\s*=\s*(.*)$`)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	prog := Program{}
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
	enc := json.NewEncoder(os.Stdout)
	_ = enc.Encode(prog)
}

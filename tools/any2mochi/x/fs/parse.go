package fs

// Package fs provides a tiny F# parser used by the any2mochi tool. It mirrors
// the behaviour of the previous fsparse command but avoids spawning an
// external process.

import (
	"bufio"
	"fmt"
	"regexp"
	"strings"
)

type Var struct {
	Name    string `json:"name"`
	Expr    string `json:"expr"`
	Mutable bool   `json:"mutable"`
}

type Assign struct {
	Name string `json:"name"`
	Expr string `json:"expr"`
}

type ForRange struct {
	Var   string `json:"var"`
	Start string `json:"start"`
	End   string `json:"end"`
	Body  []Stmt `json:"body"`
}

type ForIn struct {
	Var  string `json:"var"`
	Expr string `json:"expr"`
	Body []Stmt `json:"body"`
}

type While struct {
	Cond string `json:"cond"`
	Body []Stmt `json:"body"`
}

type Print struct {
	Expr string `json:"expr"`
}

type Stmt interface{}

type Program struct {
	Vars   []Var    `json:"vars"`
	Prints []string `json:"prints"`
	Stmts  []Stmt   `json:"stmts"`
}

var (
	printRe      = regexp.MustCompile(`ignore\s*\(printfn\s*"%A"\s*\((.*)\)\)`)
	letRe        = regexp.MustCompile(`^let\s+(\w+)\s*=\s*(.*)$`)
	mutableLetRe = regexp.MustCompile(`^let\s+mutable\s+(\w+)\s*=\s*(.*)$`)
	assignRe     = regexp.MustCompile(`^(\w+)\s*<-\s*(.*)$`)
	forRangeRe   = regexp.MustCompile(`^for\s+(\w+)\s*=\s*(.+)\s+to\s+(.+)\s+do$`)
	forInRe      = regexp.MustCompile(`^for\s+(\w+)\s+in\s+(.+)\s+do$`)
	whileRe      = regexp.MustCompile(`^while\s*\((.+)\)\s*do$`)
)

// Parse performs a very small subset of F# parsing using regular expressions.
// It mirrors the functionality of the removed fsparse command.
func Parse(src string) (*Program, error) {
	scanner := bufio.NewScanner(strings.NewReader(src))
	type line struct {
		raw    string
		text   string
		indent int
	}
	var lines []line
	for scanner.Scan() {
		raw := scanner.Text()
		trimmed := strings.TrimLeft(raw, " ")
		indent := len(raw) - len(trimmed)
		lines = append(lines, line{raw: raw, text: strings.TrimSpace(raw), indent: indent})
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}

	var idx int
	var parseErr error
	var parseBlock func(int) []Stmt
	parseBlock = func(ind int) []Stmt {
		var stmts []Stmt
		for idx < len(lines) {
			l := lines[idx]
			if l.indent < ind {
				break
			}
			if l.indent > ind {
				idx++
				continue
			}
			lineNum := idx + 1
			idx++
			t := l.text
			if strings.HasPrefix(t, "open ") {
				continue
			}
			switch {
			case printRe.MatchString(t):
				m := printRe.FindStringSubmatch(t)
				stmts = append(stmts, Print{Expr: strings.TrimSpace(m[1])})
			case mutableLetRe.MatchString(t):
				m := mutableLetRe.FindStringSubmatch(t)
				stmts = append(stmts, Var{Name: m[1], Expr: strings.TrimSpace(m[2]), Mutable: true})
			case letRe.MatchString(t):
				m := letRe.FindStringSubmatch(t)
				stmts = append(stmts, Var{Name: m[1], Expr: strings.TrimSpace(m[2])})
			case assignRe.MatchString(t):
				m := assignRe.FindStringSubmatch(t)
				stmts = append(stmts, Assign{Name: m[1], Expr: strings.TrimSpace(m[2])})
			case forRangeRe.MatchString(t):
				m := forRangeRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, ForRange{Var: m[1], Start: strings.TrimSpace(m[2]), End: strings.TrimSpace(m[3]), Body: body})
			case forInRe.MatchString(t):
				m := forInRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, ForIn{Var: m[1], Expr: strings.TrimSpace(m[2]), Body: body})
			case whileRe.MatchString(t):
				m := whileRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, While{Cond: strings.TrimSpace(m[1]), Body: body})
			default:
				if parseErr == nil {
					snippet := l.raw
					parseErr = fmt.Errorf("unsupported syntax at line %d: %s", lineNum, strings.TrimSpace(snippet))
				}
			}
		}
		return stmts
	}

	prog := &Program{}
	idx = 0
	prog.Stmts = parseBlock(0)
	return prog, parseErr
}

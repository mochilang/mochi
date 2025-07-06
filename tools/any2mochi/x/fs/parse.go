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
	Type    string `json:"type"`
	Line    int    `json:"line"`
}

type Assign struct {
	Name  string `json:"name"`
	Index string `json:"index"`
	Expr  string `json:"expr"`
	Line  int    `json:"line"`
}

type ForRange struct {
	Var   string `json:"var"`
	Start string `json:"start"`
	End   string `json:"end"`
	Body  []Stmt `json:"body"`
	Line  int    `json:"line"`
}

type ForIn struct {
	Var  string `json:"var"`
	Expr string `json:"expr"`
	Body []Stmt `json:"body"`
	Line int    `json:"line"`
}

type While struct {
	Cond string `json:"cond"`
	Body []Stmt `json:"body"`
	Line int    `json:"line"`
}

type Print struct {
	Expr string `json:"expr"`
	Line int    `json:"line"`
}

type Stmt interface{}

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
}

type Expect struct {
	Cond string `json:"cond"`
	Line int    `json:"line"`
}

type Program struct {
	Vars   []Var    `json:"vars"`
	Prints []string `json:"prints"`
	Stmts  []Stmt   `json:"stmts"`
}

var (
	printRe       = regexp.MustCompile(`ignore\s*\(printfn\s*"%A"\s*\((.*)\)\)`)
	typedLetRe    = regexp.MustCompile(`^let\s+(mutable\s+)?(\w+)\s*:\s*([^=]+)\s*=\s*(.*)$`)
	letRe         = regexp.MustCompile(`^let\s+(\w+)\s*=\s*(.*)$`)
	mutableLetRe  = regexp.MustCompile(`^let\s+mutable\s+(\w+)\s*=\s*(.*)$`)
	assignIndexRe = regexp.MustCompile(`^(\w+)\.\[(.+)\]\s*<-\s*(.*)$`)
	assignRe      = regexp.MustCompile(`^(\w+)\s*<-\s*(.*)$`)
	expectRe      = regexp.MustCompile(`^if\s+not\s+\((.+)\)\s+then\s+failwith\s+"expect failed"$`)
	forRangeRe    = regexp.MustCompile(`^for\s+(\w+)\s*=\s*(.+)\s+to\s+(.+)\s+do$`)
	forInRe       = regexp.MustCompile(`^for\s+(\w+)\s+in\s+(.+)\s+do$`)
	whileRe       = regexp.MustCompile(`^while\s*\((.+)\)\s*do$`)
	typeRe        = regexp.MustCompile(`^type\s+(\w+)\s*=\s*$`)
	variantRe     = regexp.MustCompile(`^\|\s*(\w+)(.*)$`)
	fieldRe       = regexp.MustCompile(`^(\w+)\s*:\s*([^;]+);?$`)
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
	var errLine int
	var linesCopy []line
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
			if strings.TrimSpace(t) == "" {
				continue
			}
			if strings.HasPrefix(t, "open ") {
				continue
			}
			if strings.HasPrefix(strings.TrimSpace(t), "|]") {
				continue
			}
			switch {
			case printRe.MatchString(t):
				m := printRe.FindStringSubmatch(t)
				stmts = append(stmts, Print{Expr: strings.TrimSpace(m[1]), Line: lineNum})
			case typedLetRe.MatchString(t):
				m := typedLetRe.FindStringSubmatch(t)
				stmts = append(stmts, Var{Name: m[2], Expr: strings.TrimSpace(m[4]), Mutable: strings.TrimSpace(m[1]) != "", Type: strings.TrimSpace(m[3]), Line: lineNum})
			case mutableLetRe.MatchString(t):
				m := mutableLetRe.FindStringSubmatch(t)
				stmts = append(stmts, Var{Name: m[1], Expr: strings.TrimSpace(m[2]), Mutable: true, Line: lineNum})
			case letRe.MatchString(t):
				m := letRe.FindStringSubmatch(t)
				stmts = append(stmts, Var{Name: m[1], Expr: strings.TrimSpace(m[2]), Line: lineNum})
			case assignIndexRe.MatchString(t):
				m := assignIndexRe.FindStringSubmatch(t)
				stmts = append(stmts, Assign{Name: m[1], Index: strings.TrimSpace(m[2]), Expr: strings.TrimSpace(m[3]), Line: lineNum})
			case assignRe.MatchString(t):
				m := assignRe.FindStringSubmatch(t)
				stmts = append(stmts, Assign{Name: m[1], Expr: strings.TrimSpace(m[2]), Line: lineNum})
			case expectRe.MatchString(t):
				m := expectRe.FindStringSubmatch(t)
				stmts = append(stmts, Expect{Cond: strings.TrimSpace(m[1]), Line: lineNum})
			case typeRe.MatchString(t):
				m := typeRe.FindStringSubmatch(t)
				name := m[1]
				if idx < len(lines) && strings.TrimSpace(lines[idx].text) == "{" {
					idx++
					var fields []Field
					for idx < len(lines) {
						lt := strings.TrimSpace(lines[idx].text)
						if lt == "}" {
							idx++
							break
						}
						if fm := fieldRe.FindStringSubmatch(lt); fm != nil {
							fields = append(fields, Field{Name: fm[1], Type: strings.TrimSpace(fm[2])})
						}
						idx++
					}
					stmts = append(stmts, TypeDecl{Name: name, Fields: fields, Line: lineNum})
				} else if idx < len(lines) && strings.HasPrefix(strings.TrimSpace(lines[idx].text), "|") {
					var vars []Variant
					for idx < len(lines) {
						lt := strings.TrimSpace(lines[idx].text)
						if !strings.HasPrefix(lt, "|") {
							break
						}
						vm := variantRe.FindStringSubmatch(lt)
						if vm != nil {
							var fl []Field
							rest := strings.TrimSpace(vm[2])
							if rest != "" {
								parts := strings.Split(rest, "*")
								for _, p := range parts {
									p = strings.TrimSpace(p)
									if fm := fieldRe.FindStringSubmatch(p); fm != nil {
										fl = append(fl, Field{Name: fm[1], Type: strings.TrimSpace(fm[2])})
									}
								}
							}
							vars = append(vars, Variant{Name: vm[1], Fields: fl})
						}
						idx++
					}
					stmts = append(stmts, TypeDecl{Name: name, Variants: vars, Line: lineNum})
				} else {
					stmts = append(stmts, TypeDecl{Name: name, Line: lineNum})
				}
			case forRangeRe.MatchString(t):
				m := forRangeRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, ForRange{Var: m[1], Start: strings.TrimSpace(m[2]), End: strings.TrimSpace(m[3]), Body: body, Line: lineNum})
			case forInRe.MatchString(t):
				m := forInRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, ForIn{Var: m[1], Expr: strings.TrimSpace(m[2]), Body: body, Line: lineNum})
			case whileRe.MatchString(t):
				m := whileRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, While{Cond: strings.TrimSpace(m[1]), Body: body, Line: lineNum})
			default:
				if parseErr == nil {
					errLine = lineNum
					linesCopy = append([]line(nil), lines...)
					snippet := l.raw
					parseErr = fmt.Errorf("unsupported syntax at line %d: %s", lineNum, strings.TrimSpace(snippet))
				}
			}
		}
		return stmts
	}

	prog := &Program{}
	idx = 0
	snippetAround := func(lines []line, ln int) string {
		start := ln - 2
		if start < 0 {
			start = 0
		}
		end := ln + 2
		if end > len(lines) {
			end = len(lines)
		}
		var out strings.Builder
		for i := start; i < end; i++ {
			out.WriteString(fmt.Sprintf("%3d: %s\n", i+1, lines[i].raw))
		}
		return strings.TrimRight(out.String(), "\n")
	}

	prog.Stmts = parseBlock(0)
	if parseErr != nil {
		if linesCopy == nil {
			linesCopy = lines
		}
		msg := parseErr.Error() + "\n" + snippetAround(linesCopy, errLine)
		return prog, fmt.Errorf("%s", msg)
	}
	return prog, nil
}

//go:build slow

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

type Print struct {
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
	funRe         = regexp.MustCompile(`^let\s+(?:inline\s+)?(?:rec\s+)?(\w+)(.*)?:\s*([^=]+)=\s*$`)
	exceptionRe   = regexp.MustCompile(`^exception\s+Return_(\w+)\s+of\s+(.+)$`)
	returnRe      = regexp.MustCompile(`^raise\s*\(Return_(\w+)\s*\((.*)\)\)$`)
	ifRe          = regexp.MustCompile(`^if\s+(.+)\s+then$`)
	elifRe        = regexp.MustCompile(`^elif\s+(.+)\s+then$`)
	elseRe        = regexp.MustCompile(`^else$`)
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
	retMap := map[string]string{}
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
			if strings.HasPrefix(t, "member ") {
				continue
			}
			if strings.HasPrefix(t, "exception BreakException") || strings.HasPrefix(t, "exception ContinueException") {
				// Loop control helpers inserted by the F# backend
				// are irrelevant for roundtripping back to Mochi.
				continue
			}
			switch {
			case strings.Contains(t, "failwith \"unreachable\""):
				// Ignore code paths marked unreachable in the
				// generated F# output. Treating these as errors
				// prevented successful roundtripping of many
				// valid programs.
				continue
			case printRe.MatchString(t):
				m := printRe.FindStringSubmatch(t)
				stmts = append(stmts, Print{Expr: strings.TrimSpace(m[1]), Line: lineNum, Raw: l.raw})
			case typedLetRe.MatchString(t):
				m := typedLetRe.FindStringSubmatch(t)
				stmts = append(stmts, Var{Name: m[2], Expr: strings.TrimSpace(m[4]), Mutable: strings.TrimSpace(m[1]) != "", Type: strings.TrimSpace(m[3]), Line: lineNum, Raw: l.raw})
			case mutableLetRe.MatchString(t):
				m := mutableLetRe.FindStringSubmatch(t)
				stmts = append(stmts, Var{Name: m[1], Expr: strings.TrimSpace(m[2]), Mutable: true, Line: lineNum, Raw: l.raw})
			case letRe.MatchString(t):
				m := letRe.FindStringSubmatch(t)
				stmts = append(stmts, Var{Name: m[1], Expr: strings.TrimSpace(m[2]), Line: lineNum, Raw: l.raw})
			case assignIndexRe.MatchString(t):
				m := assignIndexRe.FindStringSubmatch(t)
				stmts = append(stmts, Assign{Name: m[1], Index: strings.TrimSpace(m[2]), Expr: strings.TrimSpace(m[3]), Line: lineNum, Raw: l.raw})
			case assignRe.MatchString(t):
				m := assignRe.FindStringSubmatch(t)
				stmts = append(stmts, Assign{Name: m[1], Expr: strings.TrimSpace(m[2]), Line: lineNum, Raw: l.raw})
			case expectRe.MatchString(t):
				m := expectRe.FindStringSubmatch(t)
				stmts = append(stmts, Expect{Cond: strings.TrimSpace(m[1]), Line: lineNum, Raw: l.raw})
			case returnRe.MatchString(t):
				m := returnRe.FindStringSubmatch(t)
				stmts = append(stmts, Return{Expr: strings.TrimSpace(m[2]), Line: lineNum, Raw: l.raw})
			case ifRe.MatchString(t):
				m := ifRe.FindStringSubmatch(t)
				thenBlock := parseBlock(ind + 4)
				var elseBlock []Stmt
				if idx < len(lines) && lines[idx].indent == ind {
					if elifRe.MatchString(lines[idx].text) {
						em := elifRe.FindStringSubmatch(lines[idx].text)
						ln := idx + 1
						idx++
						elseBlock = []Stmt{If{Cond: strings.TrimSpace(em[1]), Then: parseBlock(ind + 4), Line: ln, Raw: lines[ln-1].raw}}
					} else if elseRe.MatchString(lines[idx].text) {
						idx++
						elseBlock = parseBlock(ind + 4)
					}
				}
				stmts = append(stmts, If{Cond: strings.TrimSpace(m[1]), Then: thenBlock, Else: elseBlock, Line: lineNum, Raw: l.raw})
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
					stmts = append(stmts, TypeDecl{Name: name, Fields: fields, Line: lineNum, Raw: l.raw})
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
					stmts = append(stmts, TypeDecl{Name: name, Variants: vars, Line: lineNum, Raw: l.raw})
				} else {
					stmts = append(stmts, TypeDecl{Name: name, Line: lineNum, Raw: l.raw})
				}
			case forRangeRe.MatchString(t):
				m := forRangeRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, ForRange{Var: m[1], Start: strings.TrimSpace(m[2]), End: strings.TrimSpace(m[3]), Body: body, Line: lineNum, Raw: l.raw})
			case forInRe.MatchString(t):
				m := forInRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, ForIn{Var: m[1], Expr: strings.TrimSpace(m[2]), Body: body, Line: lineNum, Raw: l.raw})
			case whileRe.MatchString(t):
				m := whileRe.FindStringSubmatch(t)
				body := parseBlock(ind + 4)
				stmts = append(stmts, While{Cond: strings.TrimSpace(m[1]), Body: body, Line: lineNum, Raw: l.raw})
			case exceptionRe.MatchString(t):
				m := exceptionRe.FindStringSubmatch(t)
				retMap[m[1]] = strings.TrimSpace(m[2])
			case funRe.MatchString(t):
				m := funRe.FindStringSubmatch(t)
				name := m[1]
				params := parseParams(m[2])
				ret := strings.TrimSpace(m[3])
				if r, ok := retMap[name]; ok {
					ret = r
				}
				if idx < len(lines) && strings.TrimSpace(lines[idx].text) == "try" {
					idx++
				}
				body := parseBlock(ind + 8)
				if idx < len(lines) && strings.Contains(lines[idx].text, "failwith") {
					idx++
				}
				if idx < len(lines) && strings.HasPrefix(strings.TrimSpace(lines[idx].text), "with Return_"+name) {
					idx++
				}
				stmts = append(stmts, Fun{Name: name, Params: params, Ret: ret, Body: body, Line: lineNum, Raw: l.raw})
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

func parseParams(sig string) []Field {
	sig = strings.TrimSpace(sig)
	if sig == "" {
		return nil
	}
	parts := strings.Fields(sig)
	var fields []Field
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if !strings.HasPrefix(part, "(") || !strings.HasSuffix(part, ")") {
			continue
		}
		p := strings.Trim(part, "()")
		if idx := strings.Index(p, ":"); idx != -1 {
			name := strings.TrimSpace(p[:idx])
			typ := strings.TrimSpace(p[idx+1:])
			fields = append(fields, Field{Name: name, Type: typ})
		} else {
			fields = append(fields, Field{Name: p})
		}
	}
	return fields
}

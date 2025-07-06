package cobol

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// Convert converts COBOL source code to Mochi.
func Convert(src string) ([]byte, error) {
	ast, err := Parse(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, fn := range ast.Functions {
		name := fn.Name
		if name == "" {
			name = "main"
		}
		out.WriteString("fun ")
		out.WriteString(name)
		out.WriteString("()")
		vars := map[string]bool{}
		for _, v := range ast.Vars {
			vars[v.Name] = false
		}
		lines := make([]string, len(fn.Lines))
		for i, ln := range fn.Lines {
			lines[i] = ln.Text
		}
		stmts, err := parseStatements(lines, vars, fn.StartLine)
		if err != nil {
			return nil, err
		}
		if len(stmts) == 0 {
			out.WriteString(" {}\n")
			continue
		}
		out.WriteString(" {\n")
		for _, st := range stmts {
			out.WriteString("  ")
			out.WriteString(st)
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible functions found")
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the COBOL file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func parseStatements(lines []string, vars map[string]bool, lineBase int) ([]string, error) {
	var out []string
	for i := 0; i < len(lines); i++ {
		ll := strings.TrimSpace(strings.TrimSuffix(lines[i], "."))
		switch {
		case strings.HasPrefix(ll, "DISPLAY "):
			expr := strings.TrimSpace(strings.TrimPrefix(ll, "DISPLAY "))
			expr = normalizeExpr(expr, vars)
			out = append(out, "print("+expr+")")

		case strings.HasPrefix(ll, "MOVE "):
			rest := strings.TrimSpace(strings.TrimPrefix(ll, "MOVE "))
			parts := strings.Split(rest, " TO ")
			if len(parts) == 2 {
				expr := normalizeExpr(strings.TrimSpace(parts[0]), vars)
               dest := strings.ToLower(strings.TrimSpace(parts[1]))
               dest = strings.ReplaceAll(dest, "(", "[")
               dest = strings.ReplaceAll(dest, ")", "]")
               baseVar := strings.Split(dest, "[")[0]
				if _, ok := vars[baseVar]; !ok {
					vars[baseVar] = true
				}
				if defined, ok := vars[dest]; ok && !defined {
					out = append(out, "var "+dest+" = "+expr)
					vars[dest] = true
				} else {
					if !ok {
						vars[dest] = true
					}
					out = append(out, dest+" = "+expr)
				}
			}

		case strings.HasPrefix(ll, "COMPUTE "):
			expr := strings.TrimSpace(strings.TrimPrefix(ll, "COMPUTE "))
			if strings.Contains(expr, "=") {
				parts := strings.SplitN(expr, "=", 2)
               dest := strings.ToLower(strings.TrimSpace(parts[0]))
               dest = strings.ReplaceAll(dest, "(", "[")
               dest = strings.ReplaceAll(dest, ")", "]")
               rhs := normalizeExpr(strings.TrimSpace(parts[1]), vars)
               baseVar := strings.Split(dest, "[")[0]
				if _, ok := vars[baseVar]; !ok {
					vars[baseVar] = true
				}
				if defined, ok := vars[dest]; ok && !defined {
					out = append(out, "var "+dest+" = "+rhs)
					vars[dest] = true
				} else {
					if !ok {
						vars[dest] = true
					}
					out = append(out, dest+" = "+rhs)
				}
			} else {
				out = append(out, expr)
			}

		case strings.HasPrefix(ll, "ADD "):
			rest := strings.TrimSpace(strings.TrimPrefix(ll, "ADD "))
			parts := strings.Split(rest, " TO ")
			if len(parts) == 2 {
				val := normalizeExpr(strings.TrimSpace(parts[0]), vars)
               dest := strings.ToLower(strings.TrimSpace(parts[1]))
               dest = strings.ReplaceAll(dest, "(", "[")
               dest = strings.ReplaceAll(dest, ")", "]")
               baseVar := strings.Split(dest, "[")[0]
				if _, ok := vars[baseVar]; !ok {
					vars[baseVar] = true
				}
				if _, ok := vars[dest]; !ok {
					vars[dest] = true
					out = append(out, "var "+dest+" = "+dest+" + "+val)
				} else {
					out = append(out, dest+" = "+dest+" + "+val)
				}
			}

		case strings.HasPrefix(ll, "PERFORM UNTIL "):
			cond := normalizeExpr(strings.TrimSpace(strings.TrimPrefix(ll, "PERFORM UNTIL ")), vars)
			body := []string{}
			for j := i + 1; j < len(lines); j++ {
				n := strings.TrimSpace(lines[j])
				if n == "END-PERFORM" {
					i = j
					break
				}
				body = append(body, lines[j])
			}
			isNot := strings.HasPrefix(cond, "NOT ")
			if isNot {
				cond = strings.TrimSpace(strings.TrimPrefix(cond, "NOT "))
				out = append(out, "while "+cond+" {")
			} else {
				out = append(out, "while !("+cond+") {")
			}
			inner, err := parseStatements(body, vars, lineBase+i+1)
			if err != nil {
				return nil, err
			}
			for _, st := range inner {
				out = append(out, "  "+st)
			}
			out = append(out, "}")

		case strings.HasPrefix(ll, "PERFORM VARYING "):
			rest := strings.TrimSpace(strings.TrimPrefix(ll, "PERFORM VARYING "))
			parts := strings.Split(rest, " FROM ")
			if len(parts) != 2 {
				break
			}
			varName := strings.TrimSpace(parts[0])
			vn := strings.ToLower(varName)
			if _, ok := vars[vn]; !ok {
				vars[vn] = true
			}
			rest = parts[1]
			parts = strings.Split(rest, " BY ")
			if len(parts) != 2 {
				break
			}
			startExpr := normalizeExpr(strings.TrimSpace(parts[0]), vars)
			rest = parts[1]
			parts = strings.Split(rest, " UNTIL ")
			if len(parts) != 2 {
				break
			}
			step := normalizeExpr(strings.TrimSpace(parts[0]), vars)
			cond := normalizeExpr(strings.TrimSpace(parts[1]), vars)
			body := []string{}
			for j := i + 1; j < len(lines); j++ {
				n := strings.TrimSpace(lines[j])
				if n == "END-PERFORM" {
					i = j
					break
				}
				body = append(body, lines[j])
			}
			end := ""
			if strings.Contains(cond, ">=") {
				end = normalizeExpr(strings.TrimSpace(strings.Split(cond, ">=")[1]), vars)
			} else if strings.Contains(cond, "<=") {
				end = normalizeExpr(strings.TrimSpace(strings.Split(cond, "<=")[1]), vars)
			}
			loop := "for " + strings.ToLower(varName) + " in range(" + startExpr + ", " + end
			if step != "1" && step != "-1" {
				loop += ", " + step
			}
			loop += ") {"
			out = append(out, loop)
			inner, err := parseStatements(body, vars, lineBase+i+1)
			if err != nil {
				return nil, err
			}
			for _, st := range inner {
				out = append(out, "  "+st)
			}
			out = append(out, "}")

		case strings.HasPrefix(ll, "IF "):
			cond := normalizeExpr(strings.TrimSpace(strings.TrimPrefix(ll, "IF ")), vars)
			thenLines := []string{}
			elseLines := []string{}
			j := i + 1
			inElse := false
			depth := 0
			for ; j < len(lines); j++ {
				n := strings.TrimSpace(lines[j])
				upper := strings.ToUpper(n)
				if strings.HasPrefix(upper, "IF ") {
					depth++
				}
				if upper == "END-IF" {
					if depth == 0 {
						break
					}
					depth--
					if inElse {
						elseLines = append(elseLines, lines[j])
					} else {
						thenLines = append(thenLines, lines[j])
					}
					continue
				}
				if depth == 0 && upper == "ELSE" {
					inElse = true
					continue
				}
				if inElse {
					elseLines = append(elseLines, lines[j])
				} else {
					thenLines = append(thenLines, lines[j])
				}
			}
			i = j
			out = append(out, "if "+cond+" {")
			inner, err := parseStatements(thenLines, vars, lineBase+i+1)
			if err != nil {
				return nil, err
			}
			for _, st := range inner {
				out = append(out, "  "+st)
			}
			if len(elseLines) > 0 {
				out = append(out, "} else {")
				inner2, err := parseStatements(elseLines, vars, lineBase+i+1)
				if err != nil {
					return nil, err
				}
				for _, st := range inner2 {
					out = append(out, "  "+st)
				}
			}
			out = append(out, "}")
		case ll == "" || ll == "EXIT" || ll == "EXIT PERFORM":
			// ignore
		case strings.HasPrefix(ll, "*>"):
			// comment
		case strings.HasPrefix(ll, "PERFORM "):
			name := strings.TrimSpace(strings.TrimPrefix(ll, "PERFORM "))
			name = strings.ToLower(strings.TrimSuffix(name, "."))
			out = append(out, name+"()")
		default:
			// skip unsupported statements but include context in error if needed
			continue
		}
	}
	return out, nil
}

func normalizeExpr(expr string, vars map[string]bool) string {
       for name := range vars {
               upper := strings.ToUpper(name)
               // convert array access like FOO(1) to foo[1]
               reIdx := regexp.MustCompile(`\b` + regexp.QuoteMeta(upper) + `\(([^)]+)\)`)
               expr = reIdx.ReplaceAllString(expr, name+`[$1]`)
       }
       for name := range vars {
               upper := strings.ToUpper(name)
               re := regexp.MustCompile(`\b` + regexp.QuoteMeta(upper) + `\b`)
               expr = re.ReplaceAllString(expr, name)
       }
       return expr
}

func snippet(lines []string, idx, start int) string {
	from := idx - 1
	if from < 0 {
		from = 0
	}
	to := idx + 1
	if to >= len(lines) {
		to = len(lines) - 1
	}
	var out []string
	for i := from; i <= to; i++ {
		out = append(out, fmt.Sprintf("%4d | %s", start+i, lines[i]))
	}
	return strings.Join(out, "\n")
}

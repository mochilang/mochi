package cobol

import (
	"fmt"
	"os"
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
		stmts := parseStatements(fn.Lines)
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

func parseStatements(lines []string) []string {
	var out []string
	for i := 0; i < len(lines); i++ {
		ll := strings.TrimSpace(strings.TrimSuffix(lines[i], "."))
		switch {
		case strings.HasPrefix(ll, "DISPLAY "):
			expr := strings.TrimSpace(strings.TrimPrefix(ll, "DISPLAY "))
			out = append(out, "print("+expr+")")

		case strings.HasPrefix(ll, "MOVE "):
			rest := strings.TrimSpace(strings.TrimPrefix(ll, "MOVE "))
			parts := strings.Split(rest, " TO ")
			if len(parts) == 2 {
				expr := strings.TrimSpace(parts[0])
				dest := strings.TrimSpace(parts[1])
				out = append(out, dest+" = "+expr)
			}

		case strings.HasPrefix(ll, "COMPUTE "):
			expr := strings.TrimSpace(strings.TrimPrefix(ll, "COMPUTE "))
			out = append(out, expr)

		case strings.HasPrefix(ll, "ADD "):
			rest := strings.TrimSpace(strings.TrimPrefix(ll, "ADD "))
			parts := strings.Split(rest, " TO ")
			if len(parts) == 2 {
				val := strings.TrimSpace(parts[0])
				dest := strings.TrimSpace(parts[1])
				out = append(out, dest+" = "+dest+" + "+val)
			}

		case strings.HasPrefix(ll, "PERFORM UNTIL "):
			cond := strings.TrimSpace(strings.TrimPrefix(ll, "PERFORM UNTIL "))
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
			inner := parseStatements(body)
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
			rest = parts[1]
			parts = strings.Split(rest, " BY ")
			if len(parts) != 2 {
				break
			}
			start := strings.TrimSpace(parts[0])
			rest = parts[1]
			parts = strings.Split(rest, " UNTIL ")
			if len(parts) != 2 {
				break
			}
			step := strings.TrimSpace(parts[0])
			cond := strings.TrimSpace(parts[1])
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
				end = strings.TrimSpace(strings.Split(cond, ">=")[1])
			} else if strings.Contains(cond, "<=") {
				end = strings.TrimSpace(strings.Split(cond, "<=")[1])
			}
			loop := "for " + strings.ToLower(varName) + " in range(" + start + ", " + end
			if step != "1" && step != "-1" {
				loop += ", " + step
			}
			loop += ") {"
			out = append(out, loop)
			inner := parseStatements(body)
			for _, st := range inner {
				out = append(out, "  "+st)
			}
			out = append(out, "}")

		case strings.HasPrefix(ll, "IF "):
			cond := strings.TrimSpace(strings.TrimPrefix(ll, "IF "))
			thenLines := []string{}
			elseLines := []string{}
			j := i + 1
			inElse := false
			for ; j < len(lines); j++ {
				n := strings.TrimSpace(lines[j])
				if n == "ELSE" {
					inElse = true
					continue
				}
				if n == "END-IF" {
					break
				}
				if inElse {
					elseLines = append(elseLines, lines[j])
				} else {
					thenLines = append(thenLines, lines[j])
				}
			}
			i = j
			out = append(out, "if "+cond+" {")
			inner := parseStatements(thenLines)
			for _, st := range inner {
				out = append(out, "  "+st)
			}
			if len(elseLines) > 0 {
				out = append(out, "} else {")
				inner2 := parseStatements(elseLines)
				for _, st := range inner2 {
					out = append(out, "  "+st)
				}
			}
			out = append(out, "}")
		}
	}
	return out
}

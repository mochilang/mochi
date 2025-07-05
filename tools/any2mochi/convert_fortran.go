package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// ConvertFortran converts fortran source code to Mochi using the language server.
func ConvertFortran(src string) ([]byte, error) {
	ls := Servers["fortran"]
	_, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	lines := strings.Split(src, "\n")
	indent := 0
	stack := []string{}

	reProg := regexp.MustCompile(`(?i)^program\s+(\w+)`)
	reFn := regexp.MustCompile(`(?i)^(function|subroutine)\s+(\w+)`)
	reIf := regexp.MustCompile(`(?i)^if\s*\((.*)\)\s*then`)
	reElseIf := regexp.MustCompile(`(?i)^else\s*if\s*\((.*)\)\s*then`)
	reElse := regexp.MustCompile(`(?i)^else\b`)
	reEndIf := regexp.MustCompile(`(?i)^end\s+if`)
	reDo := regexp.MustCompile(`(?i)^do\s+(\w+)\s*=\s*(.*?),\s*(.*?)(,\s*(.*))?$`)
	reDoWhile := regexp.MustCompile(`(?i)^do\s+while\s*\((.*)\)`)
	reEndDo := regexp.MustCompile(`(?i)^end\s+do`)
	reSelect := regexp.MustCompile(`(?i)^select\s+case\s*\((.*)\)`)
	reCase := regexp.MustCompile(`(?i)^case\s*(.*)`)
	reEndSelect := regexp.MustCompile(`(?i)^end\s+select`)
	rePrint := regexp.MustCompile(`(?i)^print\s*\*,\s*(.*)`)
	reCycle := regexp.MustCompile(`(?i)^cycle$`)
	reExit := regexp.MustCompile(`(?i)^exit$`)
	reReturn := regexp.MustCompile(`(?i)^return$`)
	reAssign := regexp.MustCompile(`(?i)^([a-zA-Z0-9_%]+)\s*=\s*(.*)$`)
	reCall := regexp.MustCompile(`(?i)^call\s+(\w+)\s*(\(.*\))?`)

	indentStr := func() string { return strings.Repeat("  ", indent) }

	for _, raw := range lines {
		t := strings.TrimSpace(raw)
		if t == "" || strings.HasPrefix(t, "!") {
			continue
		}
		lower := strings.ToLower(t)
		switch {
		case reProg.MatchString(t):
			name := reProg.FindStringSubmatch(t)[1]
			fmt.Fprintf(&out, "fun %s() {\n", name)
			indent++
			stack = append(stack, "program")
		case reFn.MatchString(t):
			name := reFn.FindStringSubmatch(t)[2]
			fmt.Fprintf(&out, "fun %s() {\n", name)
			indent++
			stack = append(stack, "fn")
		case strings.HasPrefix(lower, "contains"):
			continue
		case reIf.MatchString(t):
			cond := sanitizeExpr(reIf.FindStringSubmatch(t)[1])
			fmt.Fprintf(&out, "%sif %s {\n", indentStr(), cond)
			indent++
			stack = append(stack, "if")
		case reElseIf.MatchString(t):
			cond := sanitizeExpr(reElseIf.FindStringSubmatch(t)[1])
			if len(stack) > 0 && stack[len(stack)-1] == "if" {
				indent--
				fmt.Fprintf(&out, "%s} else if %s {\n", indentStr(), cond)
				indent++
			}
		case reElse.MatchString(t):
			if len(stack) > 0 && stack[len(stack)-1] == "if" {
				indent--
				fmt.Fprintf(&out, "%s} else {\n", indentStr())
				indent++
			}
		case reEndIf.MatchString(t):
			if len(stack) > 0 && stack[len(stack)-1] == "if" {
				stack = stack[:len(stack)-1]
				indent--
				fmt.Fprintf(&out, "%s}\n", indentStr())
			}
		case reDoWhile.MatchString(t):
			cond := sanitizeExpr(reDoWhile.FindStringSubmatch(t)[1])
			fmt.Fprintf(&out, "%swhile %s {\n", indentStr(), cond)
			indent++
			stack = append(stack, "do")
		case reDo.MatchString(t):
			m := reDo.FindStringSubmatch(t)
			varName := m[1]
			start := sanitizeExpr(m[2])
			end := sanitizeExpr(m[3])
			step := "1"
			if m[5] != "" {
				step = sanitizeExpr(m[5])
			}
			end = strings.TrimSuffix(end, "- 1")
			if step == "1" {
				fmt.Fprintf(&out, "%sfor %s in %s..%s {\n", indentStr(), varName, start, end)
			} else {
				fmt.Fprintf(&out, "%sfor %s in %s..%s {\n", indentStr(), varName, start, end)
				indent++
				fmt.Fprintf(&out, "%sif ((%s - %s) %% %s != 0) { continue }\n", indentStr(), varName, start, step)
				indent--
			}
			indent++
			stack = append(stack, "do")
		case reEndDo.MatchString(t):
			if len(stack) > 0 && stack[len(stack)-1] == "do" {
				stack = stack[:len(stack)-1]
				indent--
				fmt.Fprintf(&out, "%s}\n", indentStr())
			}
		case rePrint.MatchString(t):
			expr := sanitizeExpr(rePrint.FindStringSubmatch(t)[1])
			fmt.Fprintf(&out, "%sprint(%s)\n", indentStr(), expr)
		case reCycle.MatchString(t):
			fmt.Fprintf(&out, "%scontinue\n", indentStr())
		case reExit.MatchString(t):
			fmt.Fprintf(&out, "%sbreak\n", indentStr())
		case reReturn.MatchString(t):
			fmt.Fprintf(&out, "%sreturn\n", indentStr())
		case reSelect.MatchString(t):
			expr := sanitizeExpr(reSelect.FindStringSubmatch(t)[1])
			fmt.Fprintf(&out, "%sswitch %s {\n", indentStr(), expr)
			indent++
			stack = append(stack, "select")
		case reCase.MatchString(t):
			if len(stack) > 0 && stack[len(stack)-1] == "case" {
				stack = stack[:len(stack)-1]
				indent--
				fmt.Fprintf(&out, "%s}\n", indentStr())
			}
			val := strings.TrimSpace(reCase.FindStringSubmatch(t)[1])
			if strings.HasPrefix(strings.ToLower(val), "default") {
				fmt.Fprintf(&out, "%selse {\n", indentStr())
			} else {
				val = strings.Trim(val, "()")
				val = sanitizeExpr(val)
				fmt.Fprintf(&out, "%scase %s {\n", indentStr(), val)
			}
			indent++
			stack = append(stack, "case")
		case reEndSelect.MatchString(t):
			if len(stack) > 0 && stack[len(stack)-1] == "case" {
				stack = stack[:len(stack)-1]
				indent--
				fmt.Fprintf(&out, "%s}\n", indentStr())
			}
			if len(stack) > 0 && stack[len(stack)-1] == "select" {
				stack = stack[:len(stack)-1]
				indent--
				fmt.Fprintf(&out, "%s}\n", indentStr())
			}
		case reCall.MatchString(t):
			name := reCall.FindStringSubmatch(t)[1]
			args := strings.Trim(reCall.FindStringSubmatch(t)[2], "()")
			args = sanitizeExpr(args)
			fmt.Fprintf(&out, "%s%s(%s)\n", indentStr(), name, strings.TrimSpace(args))
		case reAssign.MatchString(t):
			if strings.Contains(t, "::") {
				continue
			}
			m := reAssign.FindStringSubmatch(t)
			fmt.Fprintf(&out, "%s%s = %s\n", indentStr(), sanitizeExpr(m[1]), sanitizeExpr(m[2]))
		case strings.HasPrefix(lower, "end program"), strings.HasPrefix(lower, "end function"), strings.HasPrefix(lower, "end subroutine"):
			if len(stack) > 0 {
				stack = stack[:len(stack)-1]
				indent--
				fmt.Fprintf(&out, "%s}\n", indentStr())
			}
		}
	}

	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFortranFile reads the fortran file and converts it to Mochi.
func ConvertFortranFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertFortran(string(data))
}

func sanitizeExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	expr = strings.ReplaceAll(expr, "_8", "")
	expr = strings.ReplaceAll(expr, "_4", "")
	expr = strings.ReplaceAll(expr, "_2", "")
	expr = strings.ReplaceAll(expr, "_1", "")
	expr = strings.ReplaceAll(expr, ".true.", "true")
	expr = strings.ReplaceAll(expr, ".false.", "false")
	expr = strings.ReplaceAll(expr, ".and.", "&&")
	expr = strings.ReplaceAll(expr, ".or.", "||")
	expr = strings.ReplaceAll(expr, ".not.", "!")
	expr = strings.ReplaceAll(expr, "%", ".")
	expr = strings.ReplaceAll(expr, " // ", " + ")
	expr = strings.ReplaceAll(expr, ".eq.", "==")
	expr = strings.ReplaceAll(expr, ".ne.", "!=")
	expr = strings.ReplaceAll(expr, ".lt.", "<")
	expr = strings.ReplaceAll(expr, ".gt.", ">")
	expr = strings.ReplaceAll(expr, ".le.", "<=")
	expr = strings.ReplaceAll(expr, ".ge.", ">=")
	expr = strings.ReplaceAll(expr, ".eqv.", "==")
	expr = strings.ReplaceAll(expr, ".neqv.", "!=")
	expr = strings.ReplaceAll(expr, "(/", "[")
	expr = strings.ReplaceAll(expr, "/)", "]")
	powRe := regexp.MustCompile(`([A-Za-z0-9_.]+)\s*\*\*\s*([A-Za-z0-9_.]+)`)
	expr = powRe.ReplaceAllString(expr, "pow($1, $2)")
	return expr
}

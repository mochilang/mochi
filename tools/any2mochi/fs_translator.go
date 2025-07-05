package any2mochi

import (
	"fmt"
	"regexp"
	"strings"
)

var (
	rePrintf       = regexp.MustCompile(`^printfn "([^"]*)"`)
	reIgnorePrintf = regexp.MustCompile(`^ignore \(printfn "%A" \((.+)\)\)`)
	reLetArray     = regexp.MustCompile(`^let (\w+) = \[\|(.+)\|\]`)
	reLet          = regexp.MustCompile(`^let (\w+) = (.+)`)
	reForInArray   = regexp.MustCompile(`^for (\w+) in \[\|(.+)\|\] do`)
	reForRange     = regexp.MustCompile(`^for (\w+) = (.+) to (.+) do`)
	reIf           = regexp.MustCompile(`^if (.+) then`)
)

// translateFs converts a small subset of F# syntax into Mochi.
func translateFs(src string) string {
	lines := strings.Split(src, "\n")
	var out []string
	var stack []int
	for _, ln := range lines {
		if strings.TrimSpace(ln) == "" {
			continue
		}
		indent := len(ln) - len(strings.TrimLeft(ln, " \t"))
		line := strings.TrimSpace(ln)
		for len(stack) > 0 && indent <= stack[len(stack)-1] {
			out = append(out, "}")
			stack = stack[:len(stack)-1]
		}
		switch {
		case strings.HasPrefix(line, "open "):
			continue
		case rePrintf.MatchString(line):
			m := rePrintf.FindStringSubmatch(line)
			out = append(out, fmt.Sprintf("print(\"%s\")", m[1]))
			continue
		case reIgnorePrintf.MatchString(line):
			m := reIgnorePrintf.FindStringSubmatch(line)
			expr := normalizeExpr(m[1])
			out = append(out, fmt.Sprintf("print(%s)", expr))
			continue
		case reLetArray.MatchString(line):
			m := reLetArray.FindStringSubmatch(line)
			arr := strings.ReplaceAll(m[2], ";", ",")
			out = append(out, fmt.Sprintf("let %s = [%s]", m[1], arr))
			continue
		case reLet.MatchString(line):
			m := reLet.FindStringSubmatch(line)
			expr := normalizeExpr(m[2])
			out = append(out, fmt.Sprintf("let %s = %s", m[1], expr))
			continue
		case reForInArray.MatchString(line):
			m := reForInArray.FindStringSubmatch(line)
			arr := strings.ReplaceAll(m[2], ";", ",")
			out = append(out, fmt.Sprintf("for %s in [%s] {", m[1], arr))
			stack = append(stack, indent)
			continue
		case reForRange.MatchString(line):
			m := reForRange.FindStringSubmatch(line)
			out = append(out, fmt.Sprintf("for %s in range(%s, %s) {", m[1], m[2], m[3]))
			stack = append(stack, indent)
			continue
		case reIf.MatchString(line):
			m := reIf.FindStringSubmatch(line)
			cond := m[1]
			if !strings.Contains(cond, "==") {
				cond = strings.Replace(cond, "=", "==", 1)
			}
			out = append(out, fmt.Sprintf("if %s {", cond))
			stack = append(stack, indent)
			continue
		default:
			out = append(out, "// "+line)
		}
	}
	for len(stack) > 0 {
		stack = stack[:len(stack)-1]
		out = append(out, "}")
	}
	if len(out) > 0 {
		out = append(out, "")
	}
	return strings.Join(out, "\n")
}

func normalizeExpr(expr string) string {
	expr = strings.ReplaceAll(expr, "[|", "[")
	expr = strings.ReplaceAll(expr, "|]", "]")
	expr = strings.ReplaceAll(expr, ";", ",")
	return expr
}

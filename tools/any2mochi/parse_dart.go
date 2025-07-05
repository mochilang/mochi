package any2mochi

import (
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

var (
	dartForInRE   = regexp.MustCompile(`^for\s*\(\s*var\s+(\w+)\s+in\s+([^\)]+)\)$`)
	dartVarDeclRE = regexp.MustCompile(`^(?:var|dynamic|int|double|num|String|bool|List<[^>]+>|Map<[^>]+>|[A-Z][A-Za-z0-9_<>, ]*)\s+(\w+)\s*=\s*(.+)$`)
	dartLengthRE  = regexp.MustCompile(`(\w+)\.length`)
)

func dartExpr(e string) string {
	e = strings.TrimSpace(e)
	e = dartLengthRE.ReplaceAllString(e, "len($1)")
	e = strings.ReplaceAll(e, ".toString()", "")
	if strings.HasPrefix(e, "(") && strings.HasSuffix(e, ")") {
		inner := strings.TrimSpace(e[1 : len(e)-1])
		if inner != "" {
			e = inner
		}
	}
	return e
}

func convertDartStmt(s string) string {
	s = strings.TrimSpace(s)
	if s == "" {
		return ""
	}
	if s == "{" || s == "}" {
		return s
	}
	s = strings.TrimSuffix(s, ";")
	if strings.HasPrefix(s, "for ") {
		inner := strings.TrimPrefix(s, "for")
		inner = strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(inner, "("), ")"))
		parts := strings.Split(inner, ";")
		if len(parts) == 3 {
			// pattern var i = start; i < end; i++
			assign := strings.Fields(parts[0])
			if len(assign) >= 3 && assign[0] == "var" {
				name := assign[1]
				startExpr := strings.Join(assign[2:], " ")
				cond := strings.TrimSpace(parts[1])
				inc := strings.TrimSpace(parts[2])
				if strings.HasPrefix(cond, name) && strings.Contains(cond, "<") && strings.HasPrefix(inc, name+"++") {
					endExpr := strings.TrimSpace(strings.TrimPrefix(cond[len(name):], "<"))
					return "for " + name + " in " + dartExpr(startExpr) + ".." + dartExpr(endExpr) + " {"
				}
			}
		}
		if m := dartForInRE.FindStringSubmatch("for(" + inner + ")"); m != nil {
			return "for " + m[1] + " in " + dartExpr(m[2]) + " {"
		}
	}
	if m := dartForInRE.FindStringSubmatch(s); m != nil {
		return "for " + m[1] + " in " + dartExpr(m[2]) + " {"
	}
	if strings.HasPrefix(s, "if ") || strings.HasPrefix(s, "if(") {
		cond := strings.TrimPrefix(s, "if")
		cond = strings.TrimSpace(cond)
		cond = strings.TrimPrefix(cond, "(")
		cond = strings.TrimSuffix(cond, ")")
		if strings.HasSuffix(cond, "{") {
			cond = strings.TrimSuffix(cond, "{")
			return "if " + dartExpr(strings.TrimSpace(cond)) + " {"
		}
		return "if " + dartExpr(cond)
	}
	if strings.HasPrefix(s, "else {") {
		return "else {"
	}
	if strings.HasPrefix(s, "return ") {
		return "return " + dartExpr(strings.TrimSpace(strings.TrimPrefix(s, "return ")))
	}
	if strings.HasPrefix(s, "print(") {
		inner := strings.TrimSuffix(strings.TrimPrefix(s, "print("), ")")
		return "print(" + dartExpr(inner) + ")"
	}
	if m := dartVarDeclRE.FindStringSubmatch(s); m != nil {
		return "let " + m[1] + " = " + dartExpr(m[2])
	}
	if idx := strings.Index(s, "="); idx != -1 {
		left := strings.TrimSpace(s[:idx])
		right := dartExpr(strings.TrimSpace(s[idx+1:]))
		return left + " = " + right
	}
	return dartExpr(s)
}

func dartSplitStatements(body string) []string {
	var stmts []string
	var cur strings.Builder
	depth := 0
	for i := 0; i < len(body); i++ {
		c := body[i]
		switch c {
		case '{', '}':
			if cur.Len() > 0 {
				t := strings.TrimSpace(cur.String())
				if t != "" {
					stmts = append(stmts, t)
				}
				cur.Reset()
			}
			stmts = append(stmts, string(c))
		case ';':
			if depth == 0 {
				t := strings.TrimSpace(cur.String())
				if t != "" {
					stmts = append(stmts, t)
				}
				cur.Reset()
			} else {
				cur.WriteByte(c)
			}
		case '(':
			depth++
			cur.WriteByte(c)
		case ')':
			if depth > 0 {
				depth--
			}
			cur.WriteByte(c)
		case '\n', '\r':
			cur.WriteByte(' ')
		default:
			cur.WriteByte(c)
		}
	}
	if cur.Len() > 0 {
		t := strings.TrimSpace(cur.String())
		if t != "" {
			stmts = append(stmts, t)
		}
	}
	return stmts
}

func parseDartFunctionBody(src string, sym protocol.DocumentSymbol) []string {
	code := extractRangeGeneric(src, sym.Range)
	start := strings.Index(code, "{")
	end := strings.LastIndex(code, "}")
	if start == -1 || end == -1 || end <= start {
		return nil
	}
	body := code[start+1 : end]
	stmts := dartSplitStatements(body)
	var out []string
	indent := 1
	for _, st := range stmts {
		if st == "{" {
			out = append(out, strings.Repeat("  ", indent)+"{")
			indent++
			continue
		}
		if st == "}" {
			indent--
			out = append(out, strings.Repeat("  ", indent)+"}")
			continue
		}
		line := convertDartStmt(st)
		if line == "" {
			continue
		}
		out = append(out, strings.Repeat("  ", indent)+line)
	}
	return out
}

func extractRangeGeneric(src string, r protocol.Range) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line) && i < len(lines); i++ {
		line := lines[i]
		if i == int(r.Start.Line) && int(r.Start.Character) < len(line) {
			line = line[int(r.Start.Character):]
		}
		if i == int(r.End.Line) && int(r.End.Character) <= len(line) {
			line = line[:int(r.End.Character)]
		}
		out.WriteString(line)
		if i != int(r.End.Line) {
			out.WriteByte('\n')
		}
	}
	return out.String()
}

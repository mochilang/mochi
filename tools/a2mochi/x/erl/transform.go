//go:build slow

package erl

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

var assignRe = regexp.MustCompile(`^([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+)$`)
var appendRe = regexp.MustCompile(`lists:append\(([^,]+),\s*\[([^\]]+)\]\)`)
var mapsGetRe = regexp.MustCompile(`maps:get\(([^,]+),\s*([^\)]+)\)`)
var mapsPutRe = regexp.MustCompile(`maps:put\(([^,]+),\s*([^,]+),\s*([^\)]+)\)`)
var mapsIsKeyRe = regexp.MustCompile(`maps:is_key\(([^,]+),\s*([^\)]+)\)`)
var listsMemberRe = regexp.MustCompile(`lists:member\(([^,]+),\s*([^\)]+)\)`)
var notListsMemberRe = regexp.MustCompile(`not\s+lists:member\(([^,]+),\s*([^\)]+)\)`)
var stringStrNotZeroRe = regexp.MustCompile(`string:str\(([^,]+),\s*([^\)]+)\)\s*(=/=|/=)\s*0`)
var stringStrZeroRe = regexp.MustCompile(`string:str\(([^,]+),\s*([^\)]+)\)\s*(=|=:=)\s*0`)
var substrRe = regexp.MustCompile(`string:substr\(([^,]+),\s*([^,]+?)\s*\+\s*1,\s*([^\)]+)\)`)
var listsNthRe = regexp.MustCompile(`lists:nth\(([^,]+),\s*([^\)]+)\)`)
var caseIfRe = regexp.MustCompile(`case\s+(.+?)\s+of\s+true\s*->\s*(.+?);\s*(?:_|false)\s*->\s*(.+?)\s*end`)
var printCallRe = regexp.MustCompile(`io:(?:format|fwrite)\("~[sp]~n",\s*\[(.+?)\]\)`)
var foreachRe = regexp.MustCompile(`^lists:foreach\(fun\(([^)]*)\)\s*->\s*(.*?)\s*end,\s*(.+)\)$`)
var lambdaRe = regexp.MustCompile(`^fun\(([^)]*)\)\s*->\s*(.+)\s*end$`)
var seqRangeRe = regexp.MustCompile(`lists:seq\(([^,]+),\s*([^\)]+)\s*-\s*1\)`)
var throwReturnRe = regexp.MustCompile(`^throw\(\{return,\s*(.+)\}\)$`)
var listAssignRe = regexp.MustCompile(`^lists:sublist\(([^,]+),\s*([^,\)]+)\)\s*\+\+\s*\[([^\]]+)\]\s*\+\+\s*lists:nthtail\(([^,]+),\s*([^\)]+)\)$`)
var listsAnyRe = regexp.MustCompile(`lists:any\(fun\(([^)]*)\)\s*->\s*(.+?)\s*end,\s*([^\)]+)\)`)
var listsSublistCall = "lists:sublist("
var divRe = regexp.MustCompile(`([0-9A-Za-z_()+\-*/]+)\s+div\s+([0-9A-Za-z_()+\-*/]+)`)
var remRe = regexp.MustCompile(`([0-9A-Za-z_()+\-*/]+)\s+rem\s+([0-9A-Za-z_()+\-*/]+)`)

func node(kind string, value any, children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: kind, Value: value, Children: children}
}

func fromParsedProgram(p *parser.Program) *ast.Node {
	root := node("program", p.Package)
	for _, s := range p.Statements {
		root.Children = append(root.Children, ast.FromStatement(s))
	}
	return root
}

// Transform converts a Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	src, err := formatProgram(p)
	if err != nil {
		return nil, err
	}
	if os.Getenv("ERL_DEBUG") != "" {
		fmt.Println("SOURCE:\n" + src)
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return fromParsedProgram(prog), nil
}

func formatProgram(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var parts []string
	if p.Module != "" {
		parts = append(parts, "package "+p.Module, "")
	}
	for _, r := range p.Records {
		parts = append(parts, "// line "+fmt.Sprint(r.Line))
		if r.EndLine > 0 && r.EndLine != r.Line {
			parts[len(parts)-1] += "-" + fmt.Sprint(r.EndLine)
		}
		parts = append(parts, "type "+strings.Title(r.Name)+" {")
		for _, f := range r.Fields {
			parts = append(parts, "  "+f+": any")
		}
		parts = append(parts, "}")
	}
	hasMain := false
	for _, f := range p.Functions {
		if f.Name == "main" {
			hasMain = true
		}
		if strings.HasPrefix(f.Name, "mochi_") && f.Name != "main" {
			continue
		}
		header := "// line " + fmt.Sprint(f.Line)
		if f.EndLine > 0 && f.EndLine != f.Line {
			header += "-" + fmt.Sprint(f.EndLine)
		}
		if f.Exported {
			header += " (exported)"
		}
		parts = append(parts, header)
		funLine := "fun "
		if f.Name == "" {
			funLine += "fun"
		} else {
			funLine += f.Name
		}
		funLine += "("
		params := f.Params
		if f.Name == "main" && len(params) == 1 && params[0] == "_" {
			params = nil
		}
		boolReturn := false
		returnType := ""
		paramType := make(map[string]string)
		for _, line := range f.Body {
			if strings.Contains(line, "lists:nth(") {
				for _, p := range f.Params {
					if strings.Contains(line, ", "+p+")") {
						paramType[p] = "list<int>"
					}
				}
			}
			if strings.Contains(line, "maps:get(") || strings.Contains(line, "maps:put(") || strings.Contains(line, "maps:is_key(") {
				for _, p := range f.Params {
					if strings.Contains(line, p) {
						if _, ok := paramType[p]; !ok {
							paramType[p] = "map<string, int>"
						}
					}
				}
			}
		}
		for _, p := range f.Params {
			if p == "Target" {
				paramType[p] = "int"
			}
		}
		if len(f.Body) > 0 {
			last := strings.TrimSpace(f.Body[len(f.Body)-1])
			lines := rewriteLine(strings.ReplaceAll(last, "\n", " "), p.Records)
			rewritten := ""
			if len(lines) > 0 {
				rewritten = lines[len(lines)-1]
			}
			if rewritten == "true" || rewritten == "false" {
				boolReturn = true
			} else if strings.HasPrefix(rewritten, "return fun(") {
				returnType = "fun(any): any"
			} else if strings.HasPrefix(rewritten, "return [") {
				returnType = "list<int>"
			} else if strings.HasPrefix(rewritten, "[") && strings.HasSuffix(rewritten, "]") {
				returnType = "list<int>"
			} else if strings.HasPrefix(rewritten, "return ") {
				boolReturn = true
			}
		}
		for i, p := range params {
			if i > 0 {
				funLine += ", "
			}
			funLine += p
			if p != "_" {
				if t, ok := paramType[p]; ok {
					funLine += ": " + t
				} else {
					funLine += ": any"
				}
			}
		}
		funLine += ")"
		if returnType != "" {
			funLine += ": " + returnType
		} else if boolReturn {
			funLine += ": bool"
		} else if len(params) > 0 {
			funLine += ": any"
		}
		if len(f.Body) == 0 {
			parts = append(parts, funLine+" {}")
		} else {
			parts = append(parts, funLine+" {")
			for i, line := range f.Body {
				ln := strings.ReplaceAll(line, "\n", " ")
				lines := rewriteLine(ln, p.Records)
				for j, l := range lines {
					tln := strings.TrimSpace(l)
					if i == len(f.Body)-1 && j == len(lines)-1 && !strings.HasPrefix(tln, "return ") &&
						!assignRe.MatchString(strings.TrimSpace(line)) &&
						!strings.HasPrefix(tln, "print(") &&
						!strings.HasPrefix(tln, "for ") &&
						!strings.HasPrefix(tln, "if ") {
						tln = "return " + tln
					}
					parts = append(parts, "  "+tln)
				}
			}
			parts = append(parts, "}")
		}
	}
	if hasMain {
		parts = append(parts, "main()")
	}
	if len(parts) == 0 {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return strings.Join(parts, "\n") + "\n", nil
}

func rewriteLine(ln string, recs []Record) []string {
	ln = strings.TrimSpace(ln)
	ln = strings.TrimSuffix(ln, ",")
	ln = strings.TrimSuffix(ln, ";")
	ln = strings.TrimSuffix(ln, ".")

	if strings.HasPrefix(ln, "try ") && strings.HasSuffix(ln, " end") && strings.Contains(ln, " catch ") {
		inner := strings.TrimSuffix(strings.TrimPrefix(ln, "try "), " end")
		parts := strings.SplitN(inner, " catch ", 2)
		body := strings.TrimSpace(parts[0])
		exprs := splitTop(body)
		var out []string
		for _, e := range exprs {
			out = append(out, rewriteLine(e, recs)...)
		}
		return out
	}

	exprs := splitTop(ln)
	if len(exprs) > 1 {
		var out []string
		for _, e := range exprs {
			out = append(out, rewriteLine(e, recs)...)
		}
		return out
	}
	ln = strings.TrimSpace(ln)
	prefix := ""
	if m := assignRe.FindStringSubmatch(ln); m != nil {
		expr := strings.TrimSpace(m[2])
		if m2 := listAssignRe.FindStringSubmatch(expr); m2 != nil {
			list := strings.TrimSpace(m2[1])
			idx := strings.TrimSpace(m2[2])
			val := strings.TrimSpace(m2[3])
			return []string{
				fmt.Sprintf("var %s = %s", m[1], list),
				fmt.Sprintf("%s[%s] = %s", m[1], idx, val),
			}
		}
		if k, v, mp, ok := parseMapsPut(expr); ok {
			val := rewriteMapsGet(v)
			return []string{
				fmt.Sprintf("var %s = %s", m[1], mp),
				fmt.Sprintf("%s[%s] = %s", m[1], k, val),
			}
		}
		if expr == "nil" || expr == "undefined" {
			return []string{fmt.Sprintf("var %s: int", m[1])}
		}
		prefix = "let " + m[1] + " = "
		ln = expr
	}
	if ln == "ok" {
		return nil
	}
	if ln == "nil" {
		return []string{"null"}
	}
	if m := throwReturnRe.FindStringSubmatch(ln); m != nil {
		return []string{"return " + strings.TrimSpace(m[1])}
	}
	if strings.HasPrefix(ln, "io:format(") {
		ln = strings.TrimPrefix(ln, "io:format(")
		ln = rewritePrint(ln)
	} else if strings.HasPrefix(ln, "io:fwrite(") {
		ln = strings.TrimPrefix(ln, "io:fwrite(")
		ln = rewritePrint(ln)
	} else if strings.HasPrefix(ln, "mochi_print([") && strings.HasSuffix(ln, "])") {
		ln = "print(" + strings.TrimSuffix(strings.TrimPrefix(ln, "mochi_print(["), "])") + ")"
	}
	if appendRe.MatchString(ln) {
		ln = appendRe.ReplaceAllString(ln, "append($1, $2)")
	}
	if strings.Contains(ln, "lists:sum(") {
		ln = strings.ReplaceAll(ln, "lists:sum(", "sum(")
	}
	if strings.Contains(ln, "lists:min(") {
		ln = strings.ReplaceAll(ln, "lists:min(", "min(")
	}
	if strings.Contains(ln, "lists:max(") {
		ln = strings.ReplaceAll(ln, "lists:max(", "max(")
	}
	if strings.Contains(ln, "list_to_integer(") {
		ln = strings.ReplaceAll(ln, "list_to_integer(", "int(")
	}
	if strings.Contains(ln, "integer_to_list(") {
		ln = strings.ReplaceAll(ln, "integer_to_list(", "str(")
	}
	if strings.Contains(ln, "length(") {
		ln = strings.ReplaceAll(ln, "length(", "len(")
	}
	if strings.Contains(ln, "maps:size(") {
		ln = strings.ReplaceAll(ln, "maps:size(", "len(")
	}
	if strings.Contains(ln, "maps:values(") {
		ln = strings.ReplaceAll(ln, "maps:values(", "values(")
	}
	if strings.Contains(ln, "++") {
		ln = strings.ReplaceAll(ln, "++", "+")
	}
	ln = fixPlusNeg(ln)
	for divRe.MatchString(ln) {
		ln = divRe.ReplaceAllString(ln, "int($1 / $2)")
	}
	if strings.Contains(ln, "int(print(") {
		ln = strings.ReplaceAll(ln, "int(print(", "print(int(")
	}
	for remRe.MatchString(ln) {
		ln = remRe.ReplaceAllString(ln, "$1 % $2")
	}
	if notListsMemberRe.MatchString(ln) {
		ln = notListsMemberRe.ReplaceAllString(ln, "!($1 in $2)")
	}
	if listsMemberRe.MatchString(ln) {
		ln = listsMemberRe.ReplaceAllString(ln, "$1 in $2")
	}
	if stringStrNotZeroRe.MatchString(ln) {
		ln = stringStrNotZeroRe.ReplaceAllString(ln, "$1.contains($2)")
	}
	if stringStrZeroRe.MatchString(ln) {
		ln = stringStrZeroRe.ReplaceAllString(ln, "!$1.contains($2)")
	}
	ln = strings.ReplaceAll(ln, "=<", "<=")
	ln = strings.ReplaceAll(ln, "=:=", "==")
	ln = strings.ReplaceAll(ln, "=/=", "!=")
	if substrRe.MatchString(ln) {
		ln = substrRe.ReplaceAllStringFunc(ln, func(s string) string {
			m := substrRe.FindStringSubmatch(s)
			start := strings.TrimSpace(m[2])
			length := strings.TrimSpace(m[3])
			target := strings.TrimSpace(m[1])
			if length == "1" {
				return target + "[" + start + "]"
			}
			return "substring(" + target + ", " + start + ", " + start + " + " + length + ")"
		})
	}
	if strings.Contains(ln, "lists:nth(") {
		ln = rewriteListsNth(ln)
	}
	if strings.Contains(ln, listsSublistCall) {
		ln = rewriteSublist(ln)
	}
	if printCallRe.MatchString(ln) {
		ln = printCallRe.ReplaceAllString(ln, "print($1)")
	}
	if p, b, iter, ok := parseForeach(ln); ok {
		blines := rewriteLine(b, recs)
		body := ""
		if len(blines) > 0 {
			body = blines[len(blines)-1]
		}
		if strings.HasPrefix(iter, "maps:keys(") && strings.HasSuffix(iter, ")") {
			iter = strings.TrimSuffix(strings.TrimPrefix(iter, "maps:keys("), ")")
		}
		if seqRangeRe.MatchString(iter) {
			im := seqRangeRe.FindStringSubmatch(iter)
			start := strings.TrimSpace(im[1])
			end := strings.TrimSpace(im[2])
			iter = start + ".." + end
		}
		ln = fmt.Sprintf("for %s in %s { %s }", strings.TrimSpace(p), iter, body)
		if m := throwReturnRe.FindStringSubmatch(ln); m != nil {
			return []string{"return " + strings.TrimSpace(m[1])}
		}
	} else if foreachRe.MatchString(ln) {
		m := foreachRe.FindStringSubmatch(ln)
		body := strings.TrimSpace(m[2])
		blines := rewriteLine(body, recs)
		if len(blines) > 0 {
			body = blines[len(blines)-1]
		}
		iter := strings.TrimSpace(m[3])
		if strings.HasPrefix(iter, "maps:keys(") && strings.HasSuffix(iter, ")") {
			iter = strings.TrimSuffix(strings.TrimPrefix(iter, "maps:keys("), ")")
		}
		if seqRangeRe.MatchString(iter) {
			im := seqRangeRe.FindStringSubmatch(iter)
			start := strings.TrimSpace(im[1])
			end := strings.TrimSpace(im[2])
			iter = start + ".." + end
		}
		ln = fmt.Sprintf("for %s in %s { %s }", strings.TrimSpace(m[1]), iter, body)
		if m := throwReturnRe.FindStringSubmatch(ln); m != nil {
			return []string{"return " + strings.TrimSpace(m[1])}
		}
	}
	if listsAnyRe.MatchString(ln) {
		m := listsAnyRe.FindStringSubmatch(ln)
		param := strings.TrimSpace(m[1])
		if param != "_" && !strings.Contains(param, ":") {
			param += ": any"
		}
		blines := rewriteLine(strings.TrimSpace(m[2]), recs)
		body := ""
		if len(blines) > 0 {
			body = blines[len(blines)-1]
		}
		iter := strings.TrimSpace(m[3])
		if seqRangeRe.MatchString(iter) {
			im := seqRangeRe.FindStringSubmatch(iter)
			start := strings.TrimSpace(im[1])
			end := strings.TrimSpace(im[2])
			iter = start + ".." + end
		}
		ln = fmt.Sprintf("any(%s, fun(%s): any => %s)", iter, param, body)
	}
	if lambdaRe.MatchString(ln) {
		m := lambdaRe.FindStringSubmatch(ln)
		param := strings.TrimSpace(m[1])
		if param != "_" && !strings.Contains(param, ":") {
			param += ": any"
		}
		blines := rewriteLine(strings.TrimSpace(m[2]), recs)
		body := ""
		if len(blines) > 0 {
			body = blines[len(blines)-1]
		}
		ln = fmt.Sprintf("return fun(%s): any => %s", param, body)
	}
	for caseIfRe.MatchString(ln) {
		ln = caseIfRe.ReplaceAllStringFunc(ln, func(s string) string {
			m := caseIfRe.FindStringSubmatch(s)
			if len(m) == 4 {
				cond := strings.TrimSpace(m[1])
				tbranch := strings.TrimSpace(m[2])
				fbranch := strings.TrimSpace(m[3])
				if tt := throwReturnRe.FindStringSubmatch(tbranch); tt != nil {
					tbranch = "return " + strings.TrimSpace(tt[1])
				}
				if ff := throwReturnRe.FindStringSubmatch(fbranch); ff != nil {
					fbranch = "return " + strings.TrimSpace(ff[1])
				}
				if fbranch == "ok" {
					return fmt.Sprintf("if %s { %s }", cond, tbranch)
				}
				return fmt.Sprintf("if %s { %s } else { %s }", cond, tbranch, fbranch)
			}
			return s
		})
	}
	if strings.Contains(ln, "andalso") {
		ln = strings.ReplaceAll(ln, "andalso", "&&")
	}
	if strings.Contains(ln, "orelse") {
		ln = strings.ReplaceAll(ln, "orelse", "||")
	}
	if strings.Contains(ln, "#{") {
		ln = strings.ReplaceAll(ln, "#{", "{")
		ln = strings.ReplaceAll(ln, "=>", ":")
	}
	if mapsGetRe.MatchString(ln) {
		ln = rewriteMapsGet(ln)
	}
	if mapsPutRe.MatchString(ln) {
		ln = mapsPutRe.ReplaceAllString(ln, "$3[$1] = $2")
	}
	if mapsIsKeyRe.MatchString(ln) {
		ln = mapsIsKeyRe.ReplaceAllString(ln, "$1 in $2")
	}
	for _, r := range recs {
		t := strings.Title(r.Name)
		if strings.Contains(ln, "#"+r.Name+"{") {
			ln = strings.ReplaceAll(ln, "#"+r.Name+"{", t+" {")
			if i := strings.Index(ln, t+" {"); i != -1 {
				after := ln[i+len(t)+2:]
				after = strings.ReplaceAll(after, "=", ":")
				ln = ln[:i+len(t)+2] + after
			}
		}
		ln = strings.ReplaceAll(ln, "#"+r.Name+".", ".")
	}
	if ln == "true" || ln == "false" {
		ln = "return " + ln
	}
	return []string{prefix + ln}
}

var printfRe = regexp.MustCompile(`^"(?:~(?:[sp]|ts)~n|~p ~p~n)",\s*\[(.+)\]\)$`)

func rewritePrint(args string) string {
	trimmed := strings.TrimSpace(args)
	if m := printfRe.FindStringSubmatch(trimmed); m != nil {
		return "print(" + m[1] + ")"
	}
	return "print(" + trimmed
}

func fixPlusNeg(s string) string {
	for i := strings.Index(s, "+ -"); i != -1; i = strings.Index(s, "+ -") {
		j := i + 3
		k := j
		for k < len(s) && s[k] >= '0' && s[k] <= '9' {
			k++
		}
		if k > j {
			s = s[:i+2] + "(-" + s[j:k] + ")" + s[k:]
			i = i + 4
		} else {
			break
		}
	}
	return s
}

func rewriteMapsGet(s string) string {
	for {
		idx := strings.LastIndex(s, "maps:get(")
		if idx == -1 {
			break
		}
		start := idx + len("maps:get(")
		depth := 1
		j := start
		for j < len(s) && depth > 0 {
			switch s[j] {
			case '(':
				depth++
			case ')':
				depth--
			}
			j++
		}
		if depth != 0 {
			break
		}
		content := s[start : j-1]
		// split first comma outside nested parens
		partDepth := 0
		split := -1
		for i := 0; i < len(content); i++ {
			switch content[i] {
			case '(':
				partDepth++
			case ')':
				if partDepth > 0 {
					partDepth--
				}
			case ',':
				if partDepth == 0 {
					split = i
					i = len(content)
				}
			}
		}
		if split == -1 {
			break
		}
		key := strings.TrimSpace(content[:split])
		mp := strings.TrimSpace(content[split+1:])
		replacement := mp + "[" + key + "]"
		s = s[:idx] + replacement + s[j:]
	}
	return s
}

func rewriteListsNth(s string) string {
	for {
		idx := strings.LastIndex(s, "lists:nth(")
		if idx == -1 {
			break
		}
		start := idx + len("lists:nth(")
		depth := 1
		j := start
		for j < len(s) && depth > 0 {
			switch s[j] {
			case '(':
				depth++
			case ')':
				depth--
			}
			j++
		}
		if depth != 0 {
			break
		}
		content := s[start : j-1]
		args := splitArgs(content, 2)
		if len(args) != 2 {
			break
		}
		idxExpr := strings.TrimSpace(args[0])
		listExpr := strings.TrimSpace(args[1])
		replacement := fmt.Sprintf("%s[(%s) - 1]", listExpr, idxExpr)
		s = s[:idx] + replacement + s[j:]
	}
	return s
}

func rewriteSublist(s string) string {
	for {
		idx := strings.LastIndex(s, listsSublistCall)
		if idx == -1 {
			break
		}
		start := idx + len(listsSublistCall)
		depth := 1
		j := start
		for j < len(s) && depth > 0 {
			switch s[j] {
			case '(':
				depth++
			case ')':
				depth--
			}
			j++
		}
		if depth != 0 {
			break
		}
		content := s[start : j-1]
		args := splitArgs(content, 3)
		if len(args) != 3 {
			break
		}
		list := strings.TrimSpace(args[0])
		startExpr := strings.TrimSpace(args[1])
		length := strings.TrimSpace(args[2])
		replacement := fmt.Sprintf("%s[(%s) - 1:(%s) - 1 + (%s)]", list, startExpr, startExpr, length)
		s = s[:idx] + replacement + s[j:]
	}
	return s
}

func parseMapsPut(s string) (key, val, mp string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "maps:put(") || !strings.HasSuffix(s, ")") {
		return
	}
	content := s[len("maps:put(") : len(s)-1]
	args := splitArgs(content, 3)
	if len(args) != 3 {
		return
	}
	key = strings.TrimSpace(args[0])
	val = strings.TrimSpace(args[1])
	mp = strings.TrimSpace(args[2])
	ok = true
	return
}

func splitArgs(s string, n int) []string {
	parts := make([]string, 0, n)
	depth := 0
	last := 0
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '(', '[':
			depth++
		case ')', ']':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, s[last:i])
				last = i + 1
				if len(parts) == n-1 {
					i = len(s)
					break
				}
			}
		}
	}
	if last < len(s) {
		parts = append(parts, s[last:])
	}
	return parts
}

func splitTop(s string) []string {
	parts := []string{}
	depth := 0
	last := 0
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[last:i]))
				last = i + 1
			}
		}
	}
	if last < len(s) {
		parts = append(parts, strings.TrimSpace(s[last:]))
	}
	return parts
}

func parseForeach(s string) (param, body, iter string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "lists:foreach(") || !strings.HasSuffix(s, ")") {
		return
	}
	inner := s[len("lists:foreach(") : len(s)-1]
	args := splitArgs(inner, 2)
	if len(args) != 2 {
		return
	}
	funPart := strings.TrimSpace(args[0])
	if !strings.HasPrefix(funPart, "fun(") || !strings.HasSuffix(funPart, "end") {
		return
	}
	funPart = strings.TrimSuffix(funPart, "end")
	funPart = strings.TrimSpace(funPart[len("fun("):])
	idx := strings.Index(funPart, ")")
	if idx == -1 {
		return
	}
	param = strings.TrimSpace(funPart[:idx])
	body = strings.TrimSpace(funPart[idx+1:])
	if strings.HasPrefix(body, "->") {
		body = strings.TrimSpace(body[2:])
	}
	iter = strings.TrimSpace(args[1])
	ok = true
	return
}

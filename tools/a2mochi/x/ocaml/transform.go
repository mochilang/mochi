//go:build slow

package ocaml

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Transform converts the parsed OCaml program into a Mochi AST. The
// conversion is intentionally tiny and handles just the small subset of
// constructs present in the example sources used by the tests.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	code := buildMochi(p)
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func buildMochi(p *Program) string {
	var out strings.Builder
	for _, fn := range p.Funcs {
		if strings.HasPrefix(fn.Name, "__") {
			continue
		}
		params := make([]string, 0, len(fn.Params))
		for _, p := range fn.Params {
			if p == "()" || p == "" {
				continue
			}
			params = append(params, fmt.Sprintf("%s: any", p))
		}
		lines := convertExpr(fn.Body)
		rtype := "any"
		ptype := "any"
		if len(lines) > 0 {
			last := lines[len(lines)-1]
			if last == "true" || last == "false" || strings.ContainsAny(last, "<>=!&|") {
				rtype = "bool"
			} else if strings.ContainsAny(last, "+-*/%") {
				rtype = "int"
				ptype = "int"
			}
		}
		for i, p := range params {
			params[i] = strings.Replace(p, ": any", ": "+ptype, 1)
		}
		fmt.Fprintf(&out, "fun %s(%s): %s {\n", fn.Name, strings.Join(params, ", "), rtype)
		for i, line := range lines {
			if i == len(lines)-1 && !strings.Contains(line, "=") && !strings.HasPrefix(line, "print(") {
				fmt.Fprintf(&out, "  return %s\n", line)
			} else {
				fmt.Fprintf(&out, "  %s\n", line)
			}
		}
		out.WriteString("}\n")
	}
	for _, pr := range p.Prints {
		for _, line := range convertExpr(pr.Expr) {
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	for _, v := range p.Vars {
		expr := strings.TrimSpace(v.Expr)
		if strings.HasPrefix(expr, "fun") {
			rest := strings.TrimPrefix(expr, "fun")
			rest = strings.TrimSpace(rest)
			parts := strings.SplitN(rest, "->", 2)
			if len(parts) == 2 {
				params := strings.Fields(parts[0])
				pList := make([]string, 0, len(params))
				for _, p := range params {
					if p == "()" || p == "" {
						continue
					}
					pList = append(pList, fmt.Sprintf("%s: any", p))
				}
				body := strings.TrimSpace(parts[1])
				lines := convertExpr(body)
				rtype := "any"
				if len(lines) > 0 {
					last := lines[len(lines)-1]
					if last == "true" || last == "false" || strings.ContainsAny(last, "<>=!&|") {
						rtype = "bool"
					} else if regexp.MustCompile(`^[-0-9+*/% ]+$`).MatchString(last) {
						rtype = "int"
					}
				}
				fmt.Fprintf(&out, "fun %s(%s): %s {\n", v.Name, strings.Join(pList, ", "), rtype)
				for i, line := range lines {
					if i == len(lines)-1 && !strings.Contains(line, "=") && !strings.HasPrefix(line, "print(") {
						fmt.Fprintf(&out, "  return %s\n", line)
					} else {
						fmt.Fprintf(&out, "  %s\n", line)
					}
				}
				out.WriteString("}\n")
				continue
			}
		}
		expr = simplify(expr)
		if v.Mutable {
			fmt.Fprintf(&out, "var %s = %s\n", v.Name, expr)
		} else {
			fmt.Fprintf(&out, "let %s = %s\n", v.Name, expr)
		}
	}
	return out.String()
}

// DebugSimplify exposes simplify for debugging.
func DebugSimplify(s string) string { return simplify(s) }

// DebugBuildMochi exposes buildMochi for debugging.
func DebugBuildMochi(p *Program) string { return buildMochi(p) }

// convertExpr converts a single OCaml expression that may contain nested let
// bindings and multiple statements separated by semicolons. The returned slice
// contains Mochi statements.
func convertExpr(expr string) []string {
	expr = strings.ReplaceAll(expr, "\r", "")
	expr = strings.TrimSpace(expr)
	expr = fixListSemicolons(expr)
	var lines []string

	// handle leading let-bindings: let x = ... in ...
	reLet := regexp.MustCompile(`(?s)^let\s+([A-Za-z0-9_']+)\s*=\s*(.*?)\s+in\s*(.*)$`)
	for {
		m := reLet.FindStringSubmatch(expr)
		if m == nil {
			break
		}
		name := m[1]
		raw := strings.TrimSpace(m[2])
		if strings.HasPrefix(raw, "fun") || strings.HasPrefix(raw, "(fun") {
			rest := strings.TrimPrefix(raw, "(")
			rest = strings.TrimPrefix(rest, "fun")
			rest = strings.TrimSpace(rest)
			parts2 := strings.SplitN(rest, "->", 2)
			if len(parts2) == 2 {
				params := strings.Fields(parts2[0])
				pList := make([]string, 0, len(params))
				for _, p := range params {
					if p == "()" || p == "" {
						continue
					}
					pList = append(pList, fmt.Sprintf("%s: any", p))
				}
				body := strings.TrimSpace(parts2[1])
				body = strings.TrimSuffix(body, ")")
				fnLines := convertExpr(body)
				rtype := "any"
				ptype := "any"
				if len(fnLines) > 0 {
					last := fnLines[len(fnLines)-1]
					if last == "true" || last == "false" || strings.ContainsAny(last, "<>=!&|") {
						rtype = "bool"
					} else if strings.ContainsAny(last, "+-*/%") {
						rtype = "int"
						ptype = "int"
					}
				}
				var b strings.Builder
				for i, p := range pList {
					pList[i] = strings.Replace(p, ": any", ": "+ptype, 1)
				}
				fmt.Fprintf(&b, "fun %s(%s): %s {\n", name, strings.Join(pList, ", "), rtype)
				for i, line := range fnLines {
					if i == len(fnLines)-1 && !strings.Contains(line, "=") && !strings.HasPrefix(line, "print(") {
						fmt.Fprintf(&b, "  return %s\n", line)
					} else {
						fmt.Fprintf(&b, "  %s\n", line)
					}
				}
				b.WriteString("}")
				lines = append(lines, b.String())
			}
		} else {
			val := simplify(raw)
			if strings.Contains(m[3], name+":=") || strings.Contains(m[3], name+" :=") {
				lines = append(lines, fmt.Sprintf("var %s = %s", name, val))
			} else {
				lines = append(lines, fmt.Sprintf("let %s = %s", name, val))
			}
		}
		expr = strings.TrimSpace(m[3])
	}

	parts := strings.Split(expr, ";")
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if strings.Contains(p, ":=") {
			fs := strings.SplitN(p, ":=", 2)
			left := strings.TrimSpace(fs[0])
			right := simplify(strings.TrimSpace(fs[1]))
			lines = append(lines, fmt.Sprintf("%s = %s", left, right))
			continue
		}
		if strings.HasPrefix(p, "print_endline") {
			arg := unwrapPrint(p)
			lines = append(lines, fmt.Sprintf("print(%s)", simplify(arg)))
			continue
		}
		lines = append(lines, simplify(p))
	}
	return lines
}

func fixListSemicolons(s string) string {
	var out strings.Builder
	depth := 0
	for _, r := range s {
		switch r {
		case '[':
			depth++
			out.WriteRune(r)
		case ']':
			if depth > 0 {
				depth--
			}
			out.WriteRune(r)
		case ';':
			if depth > 0 {
				out.WriteByte(',')
			} else {
				out.WriteRune(r)
			}
		default:
			out.WriteRune(r)
		}
	}
	return out.String()
}

func unwrapPrint(s string) string {
	s = strings.TrimSpace(s)
	if strings.HasPrefix(s, "print_endline") {
		s = strings.TrimPrefix(s, "print_endline")
		s = strings.TrimSpace(s)
		if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
			s = strings.TrimSuffix(strings.TrimPrefix(s, "("), ")")
		}
	}
	s = strings.TrimSpace(s)
	if strings.HasPrefix(s, "String.concat \" \"") {
		s = strings.TrimPrefix(s, "String.concat \" \"")
		s = strings.TrimSpace(s)
		if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
			s = strings.TrimSuffix(strings.TrimPrefix(s, "("), ")")
		}
		if strings.HasPrefix(s, "List.filter (fun s -> s <> \"\") [") && strings.HasSuffix(s, "]") {
			s = strings.TrimSuffix(strings.TrimPrefix(s, "List.filter (fun s -> s <> \"\") ["), "]")
		} else if strings.HasPrefix(s, "[") && strings.HasSuffix(s, "]") {
			s = strings.TrimSuffix(strings.TrimPrefix(s, "["), "]")
		}
		reAppend := regexp.MustCompile(`List\.append\s+([A-Za-z0-9_]+)\s+\[([0-9]+)\]`)
		if m := reAppend.FindStringSubmatch(s); m != nil {
			s = fmt.Sprintf("append(%s, %s)", m[1], m[2])
		}
	}
	return strings.TrimSpace(s)
}

func simplify(e string) string {
	e = strings.TrimSpace(e)
	e = strings.ReplaceAll(e, "string_of_int", "")
	e = strings.ReplaceAll(e, "string_of_float", "")
	e = strings.ReplaceAll(e, "string_of_bool", "")
	if strings.HasPrefix(e, "(") && strings.HasSuffix(e, ")") {
		e = strings.TrimSuffix(strings.TrimPrefix(e, "("), ")")
	}
	e = strings.ReplaceAll(e, "not ", "!")
	e = strings.ReplaceAll(e, "ref ", "")
	reRef := regexp.MustCompile(`!([A-Za-z0-9_]+)`)
	e = reRef.ReplaceAllString(e, `$1`)

	// List and string length
	e = strings.ReplaceAll(e, "List.length", "len")
	e = strings.ReplaceAll(e, "String.length", "len")
	e = strings.ReplaceAll(e, "List.fold_left (fun acc x -> acc + x) 0", "sum")
	reCall := regexp.MustCompile(`\b(len|sum)\s+(\[[^\]]*\]|"[^"]+"|[A-Za-z0-9_]+)`)
	e = reCall.ReplaceAllString(e, `$1($2)`)

	reFold := regexp.MustCompile(`List\.fold_left\s+min\s+max_int\s+([A-Za-z0-9_]+)`)
	e = reFold.ReplaceAllString(e, `min($1)`)
	reFold = regexp.MustCompile(`List\.fold_left\s+max\s+min_int\s+([A-Za-z0-9_]+)`)
	e = reFold.ReplaceAllString(e, `max($1)`)

	re := regexp.MustCompile(`List\.append\s+(\[[^\]]*\]|[A-Za-z0-9_]+)\s+(\[[^\]]*\]|[A-Za-z0-9_]+)`)
	e = re.ReplaceAllString(e, `append($1, $2)`)

	mapListRe := regexp.MustCompile(`\[(\(("[^"]+"|[0-9]+),[^\)]+\)\s*,?\s*)+\]`)
	e = mapListRe.ReplaceAllStringFunc(e, func(m string) string {
		pairRe := regexp.MustCompile(`\(("[^"]+"|[0-9]+),\s*([^\)]+)\)`)
		pairs := pairRe.FindAllStringSubmatch(m, -1)
		var b strings.Builder
		b.WriteString("{")
		for i, p := range pairs {
			if i > 0 {
				b.WriteString(", ")
			}
			key := strings.TrimSpace(p[1])
			if strings.HasPrefix(key, "\"") {
				fmt.Fprintf(&b, "%s: %s", key, strings.TrimSpace(p[2]))
			} else {
				fmt.Fprintf(&b, "%s: %s", key, strings.TrimSpace(p[2]))
			}
		}
		b.WriteString("}")
		return b.String()
	})

	re = regexp.MustCompile(`List\.mem\s+([^\s]+)\s+([^\s]+)`)
	e = re.ReplaceAllString(e, `$1 in $2`)

	// map membership using mem_assoc
	re = regexp.MustCompile(`List\.mem_assoc\s+([^\s]+)\s+([^\s]+)`)
	e = re.ReplaceAllString(e, `$1 in $2`)

	// string contains/in operator pattern
	if strings.Contains(e, "String.sub") && strings.Contains(e, "aux 0") {
		re := regexp.MustCompile(`len ([A-Za-z0-9_]+) and len_sub = len "([^"]+)"`)
		if m := re.FindStringSubmatch(e); m != nil {
			e = fmt.Sprintf("%s.contains(\"%s\")", m[1], m[2])
		}
	}
	re = regexp.MustCompile(`if \(([^\)]*\.contains\("[^"\)]+"\))\) then true else false`)
	e = re.ReplaceAllString(e, `$1`)

	// list syntax
	e = strings.ReplaceAll(e, ";", ",")
	e = strings.ReplaceAll(e, "^", "+")
	e = strings.ReplaceAll(e, " mod ", " % ")
	reDiv := regexp.MustCompile(`\b(\d+)\s*/\s*(\d+)\b`)
	e = reDiv.ReplaceAllString(e, `int($1 / $2)`)

	// list indexing
	re = regexp.MustCompile(`List\.nth\s+(\([^)]+\)|[A-Za-z0-9_]+)\s+(\d+)`)
	e = re.ReplaceAllStringFunc(e, func(m string) string {
		parts := re.FindStringSubmatch(m)
		if parts == nil {
			return m
		}
		expr := strings.TrimSpace(parts[1])
		if strings.HasPrefix(expr, "(") && strings.HasSuffix(expr, ")") {
			expr = strings.TrimSuffix(strings.TrimPrefix(expr, "("), ")")
		}
		return fmt.Sprintf("%s[%s]", expr, parts[2])
	})
	re = regexp.MustCompile(`String\.get\s+([A-Za-z0-9_]+)\s+(\d+)`)
	e = re.ReplaceAllString(e, `$1[$2]`)

	re = regexp.MustCompile(`String\.make\s+1\s+\(([^\)]+)\)`)
	e = re.ReplaceAllString(e, `$1`)

	re = regexp.MustCompile(`String\.sub\s+("[^"]+"|[A-Za-z0-9_]+)\s+(\d+)\s+\((\d+)\s*-\s*(\d+)\)`)
	if m := re.FindStringSubmatch(e); m != nil {
		start, _ := strconv.Atoi(m[2])
		end, _ := strconv.Atoi(m[3])
		e = re.ReplaceAllString(e, fmt.Sprintf("substring(%s, %d, %d)", m[1], start, end))
	} else {
		re = regexp.MustCompile(`String\.sub\s+("[^"]+"|[A-Za-z0-9_]+)\s+(\d+)\s+(\d+)`)
		if m := re.FindStringSubmatch(e); m != nil {
			start, _ := strconv.Atoi(m[2])
			ln, _ := strconv.Atoi(m[3])
			end := start + ln
			e = re.ReplaceAllString(e, fmt.Sprintf("substring(%s, %d, %d)", m[1], start, end))
		} else {
			re = regexp.MustCompile(`String\.sub\s+([A-Za-z0-9_]+)\s+(\d+)\s+\(String\.length\s+([A-Za-z0-9_]+)\s*-\s*(\d+)\)`)
			if m := re.FindStringSubmatch(e); m != nil {
				start, _ := strconv.Atoi(m[2])
				off, _ := strconv.Atoi(m[4])
				endExpr := fmt.Sprintf("len(%s)", m[3])
				if off != 0 {
					endExpr = fmt.Sprintf("len(%s)-%d", m[3], off)
				}
				e = re.ReplaceAllString(e, fmt.Sprintf("substring(%s, %d, %s)", m[1], start, endExpr))
			}
		}
	}

	e = strings.TrimSpace(e)
	for hasOuterParens(e) {
		e = strings.TrimSpace(e[1 : len(e)-1])
	}

	if parts := strings.Fields(e); len(parts) > 1 {
		name := parts[0]
		if regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_']*$`).MatchString(name) {
			ok := true
			var args []string
			for _, a := range parts[1:] {
				if a == "()" || a == "" {
					continue
				}
				if strings.ContainsAny(a, "+-*/%<>=!&|") {
					ok = false
					break
				}
				args = append(args, a)
			}
			if ok {
				e = fmt.Sprintf("%s(%s)", name, strings.Join(args, ", "))
			}
		}
	}

	e = strings.TrimSpace(e)
	return e
}

func hasOuterParens(s string) bool {
	if len(s) < 2 || s[0] != '(' || s[len(s)-1] != ')' {
		return false
	}
	depth := 0
	for i, r := range s {
		switch r {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 && i != len(s)-1 {
				return false
			}
		}
	}
	return depth == 0
}

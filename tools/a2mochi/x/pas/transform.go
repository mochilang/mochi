//go:build slow

package pas

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

var (
	stringVars map[string]bool
)

// Transform converts a parsed Pascal program into a Mochi AST node.
func Transform(n *Node) (*ast.Node, error) {
	if n == nil {
		return nil, fmt.Errorf("nil node")
	}
	src := strings.Join(n.Lines, "\n")
	out, err := convertFallback(src)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(string(out))
	if err != nil {
		return nil, fmt.Errorf("parse error: %v\n%s", err, out)
	}
	node := &ast.Node{Kind: "program"}
	if prog.Package != "" {
		node.Value = prog.Package
	}
	for _, s := range prog.Statements {
		node.Children = append(node.Children, ast.FromStatement(s))
	}
	return node, nil
}

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func convertStringLits(s string) string {
	in := false
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c == '\'' {
			if in {
				in = false
				b.WriteByte('"')
			} else {
				in = true
				b.WriteByte('"')
			}
			continue
		}
		b.WriteByte(c)
	}
	return b.String()
}

var (
	andRe = regexp.MustCompile(`\band\b`)
	orRe  = regexp.MustCompile(`\bor\b`)
	notRe = regexp.MustCompile(`\bnot\b`)
	modRe = regexp.MustCompile(`\bmod\b`)
	divRe = regexp.MustCompile(`(?i)([^\s]+)\s+div\s+([^\s]+)`)
)

func convertOps(s string) string {
	s = strings.ReplaceAll(s, "<>", "!=")
	s = strings.ReplaceAll(s, " = ", " == ")
	s = andRe.ReplaceAllString(s, "&&")
	s = orRe.ReplaceAllString(s, "||")
	s = notRe.ReplaceAllString(s, "!")
	s = modRe.ReplaceAllString(s, "%")
	s = divRe.ReplaceAllString(s, "(($1 / $2) as int)")
	return s
}

func convertExpr(s string) string {
	s = convertBuiltins(s)
	s = convertOps(s)
	s = convertStringLits(s)
	s = convertStringIndex(s)
	s = convertRecordLit(s)
	s = stripFormatSpec(s)
	return s
}

var (
	highRe       = regexp.MustCompile(`High\(([^\)]+)\)`)
	copyRe       = regexp.MustCompile(`copy\(([^,]+),\s*([^,]+),\s*([^\)]+)\)`)
	posRe        = regexp.MustCompile(`Pos\(([^,]+),\s*([^\)]+)\)`)
	posCmpRe     = regexp.MustCompile(`Pos\(([^,]+),\s*([^\)]+)\)\s*(<>|=)\s*0`)
	strToIntRe   = regexp.MustCompile(`StrToInt\(([^\)]+)\)`)
	recordLitRe  = regexp.MustCompile(`\(([^()]+:[^()]+)\)`)
	formatSpecRe = regexp.MustCompile(`:[0-9]+(?::[0-9]+)?`)
)

func convertBuiltins(s string) string {
	s = strings.ReplaceAll(s, "Length(", "len(")
	s = strings.ReplaceAll(s, "length(", "len(")
	s = strings.ReplaceAll(s, "IntToStr(", "str(")
	if strings.HasPrefix(strings.ToLower(s), "booltostr(") && strings.HasSuffix(strings.ToLower(s), ", true)") {
		inner := s[len("BoolToStr(") : len(s)-len(", True)")]
		s = inner
	}
	s = highRe.ReplaceAllString(s, "len($1) - 1")
	s = strToIntRe.ReplaceAllString(s, "($1 as int)")
	s = posCmpRe.ReplaceAllStringFunc(s, func(m string) string {
		parts := posCmpRe.FindStringSubmatch(m)
		if len(parts) < 4 {
			return m
		}
		sub := strings.TrimSpace(parts[1])
		str := strings.TrimSpace(parts[2])
		op := parts[3]
		if op == "<>" {
			return fmt.Sprintf("%s.contains(%s)", str, sub)
		}
		return fmt.Sprintf("!%s.contains(%s)", str, sub)
	})
	s = copyRe.ReplaceAllStringFunc(s, func(m string) string {
		parts := copyRe.FindStringSubmatch(m)
		if len(parts) < 4 {
			return m
		}
		src := strings.TrimSpace(parts[1])
		start := strings.TrimSpace(parts[2])
		length := strings.TrimSpace(parts[3])
		return fmt.Sprintf("substring(%s, (%s)-1, (%s)-1 + %s)", src, start, start, length)
	})
	s = posRe.ReplaceAllStringFunc(s, func(m string) string {
		parts := posRe.FindStringSubmatch(m)
		if len(parts) < 3 {
			return m
		}
		sub := strings.TrimSpace(parts[1])
		str := strings.TrimSpace(parts[2])
		return fmt.Sprintf("%s.contains(%s)", str, sub)
	})
	return s
}

var unaryRe = regexp.MustCompile(`(^|[=(+\-*/])\s*-(\d+)`)

func fixUnary(s string) string {
	return unaryRe.ReplaceAllString(s, `${1}(-${2})`)
}

var indexLitRe = regexp.MustCompile(`^\d+$`)
var stringIdxRe = regexp.MustCompile(`([a-zA-Z_][a-zA-Z0-9_]*)\s*\[(.+?)\]`)

func convertRecordLit(s string) string {
	for {
		if !recordLitRe.MatchString(s) {
			break
		}
		s = recordLitRe.ReplaceAllStringFunc(s, func(m string) string {
			inner := strings.TrimSpace(recordLitRe.FindStringSubmatch(m)[1])
			inner = strings.ReplaceAll(inner, ";", ",")
			return "{" + inner + "}"
		})
	}
	return s
}

func convertStringIndex(s string) string {
	return stringIdxRe.ReplaceAllStringFunc(s, func(m string) string {
		parts := stringIdxRe.FindStringSubmatch(m)
		if len(parts) < 3 {
			return m
		}
		name := parts[1]
		inner := strings.TrimSpace(parts[2])
		if !stringVars[name] {
			return m
		}
		if indexLitRe.MatchString(inner) {
			v, _ := strconv.Atoi(inner)
			return fmt.Sprintf("%s[%d]", name, v-1)
		}
		return fmt.Sprintf("%s[%s - 1]", name, inner)
	})
}

func stripFormatSpec(s string) string {
	return formatSpecRe.ReplaceAllString(s, "")
}

func toMochiType(t string) string {
	orig := strings.TrimSpace(t)
	lower := strings.ToLower(orig)
	switch lower {
	case "", "void":
		return ""
	case "integer", "longint", "shortint", "byte", "smallint":
		return "int"
	case "double", "single", "real", "real64":
		return "float"
	case "boolean", "bool":
		return "bool"
	case "string", "ansistring", "widestring", "shortstring", "pchar", "char":
		return "string"
	}
	if strings.HasPrefix(lower, "specialize tarray<") && strings.HasSuffix(lower, ">") {
		inner := toMochiType(lower[len("specialize tarray<") : len(lower)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(lower, "array of ") {
		inner := toMochiType(strings.TrimPrefix(lower, "array of "))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return orig
}

func zeroValue(t string) string {
	switch t {
	case "int":
		return "0"
	case "float":
		return "0.0"
	case "bool":
		return "false"
	case "string":
		return "\"\""
	}
	if strings.HasPrefix(t, "list") {
		return "[]"
	}
	if strings.HasPrefix(t, "map") || strings.HasPrefix(t, "{") {
		return "{}"
	}
	if t != "" {
		return "nil"
	}
	return ""
}

func parseFuncHeader(l string) (string, string, string) {
	l = strings.TrimSpace(l)
	l = strings.TrimSuffix(l, ";")
	lower := strings.ToLower(l)
	if strings.HasPrefix(lower, "function") {
		l = strings.TrimSpace(l[len("function"):])
	} else if strings.HasPrefix(lower, "procedure") {
		l = strings.TrimSpace(l[len("procedure"):])
	} else {
		return "", "", ""
	}
	rest := l
	open := strings.Index(rest, "(")
	close := strings.LastIndex(rest, ")")
	if open == -1 || close == -1 || close <= open {
		return "", "", ""
	}
	name := strings.TrimSpace(rest[:open])
	paramsPart := rest[open+1 : close]
	ret := ""
	after := strings.TrimSpace(rest[close+1:])
	if strings.HasPrefix(after, ":") {
		ret = toMochiType(strings.TrimSpace(after[1:]))
	}
	var params []string
	for _, p := range strings.Split(paramsPart, ";") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if idx := strings.Index(p, ":"); idx != -1 {
			namePart := strings.TrimSpace(p[:idx])
			typPart := toMochiType(strings.TrimSpace(p[idx+1:]))
			if typPart != "" {
				params = append(params, namePart+": "+typPart)
			} else {
				params = append(params, namePart)
			}
		}
	}
	return name, strings.Join(params, ", "), ret
}

// convertFallback converts a limited subset of Pascal syntax to Mochi.
// It recognises simple type declarations, variable definitions and a
// handful of statements inside the main program block.
func convertFallback(src string) ([]byte, error) {
	lines := strings.Split(src, "\n")
	var out []string
	inBody := false
	inFunc := false
	funcName := ""
	loopDepth := 0
	ifDepth := 0
	inRepeat := false
	waitingBegin := false
	stringVars = map[string]bool{}

	addError := func(i int, msg string) ([]byte, error) {
		snippet := func(idx int) string {
			start := idx - 1
			if start < 0 {
				start = 0
			}
			end := idx + 1
			if end >= len(lines) {
				end = len(lines) - 1
			}
			var b strings.Builder
			for j := start; j <= end; j++ {
				prefix := "  "
				if j == idx {
					prefix = "->"
				}
				b.WriteString(fmt.Sprintf("%s %3d| %s\n", prefix, j+1, strings.TrimSpace(lines[j])))
			}
			return b.String()
		}
		return nil, fmt.Errorf("line %d: %s\n%s", i+1, msg, snippet(i))
	}

	appendLine := func(s string) {
		s = convertStringLits(s)
		s = fixUnary(s)
		out = append(out, s)
	}

	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		lower := strings.ToLower(l)
		if !inBody {
			switch {
			case strings.HasPrefix(lower, "type") && strings.Contains(lower, "record"):
				name := ""
				if idx := strings.Index(strings.ToLower(l), "type"); idx != -1 {
					rest := strings.TrimSpace(l[idx+len("type"):])
					eq := strings.Index(rest, "=")
					if eq != -1 {
						name = strings.TrimSpace(rest[:eq])
						rest = strings.TrimSpace(rest[eq+1:])
					}
					if !strings.Contains(strings.ToLower(rest), "record") && i+1 < len(lines) {
						i++
						rest = strings.TrimSpace(lines[i])
					}
				}
				var fields []string
				for j := i + 1; j < len(lines); j++ {
					ln := strings.TrimSpace(lines[j])
					if strings.ToLower(ln) == "end;" {
						i = j
						break
					}
					if idx := strings.Index(ln, ":"); idx != -1 {
						fname := strings.TrimSpace(ln[:idx])
						ftype := toMochiType(strings.TrimSpace(strings.TrimSuffix(ln[idx+1:], ";")))
						if fname != "" {
							if ftype != "" {
								fields = append(fields, fmt.Sprintf("  %s: %s", fname, ftype))
							} else {
								fields = append(fields, "  "+fname)
							}
						}
					}
				}
				if name != "" && len(fields) > 0 {
					appendLine("type " + name + " {")
					for _, f := range fields {
						appendLine(f)
					}
					appendLine("}")
				}

			case strings.HasPrefix(lower, "type") && strings.Contains(l, "("):
				name := ""
				if idx := strings.Index(strings.ToLower(l), "type"); idx != -1 {
					rest := strings.TrimSpace(l[idx+len("type"):])
					eq := strings.Index(rest, "=")
					if eq != -1 {
						name = strings.TrimSpace(rest[:eq])
						rest = rest[eq+1:]
					}
					vals := rest
					for !strings.Contains(vals, ")") && i+1 < len(lines) {
						i++
						vals += strings.TrimSpace(lines[i])
					}
					if cIdx := strings.Index(vals, "("); cIdx != -1 {
						vals = vals[cIdx+1:]
					}
					if end := strings.Index(vals, ")"); end != -1 {
						vals = vals[:end]
					}
					var members []string
					for _, part := range strings.Split(vals, ",") {
						v := strings.TrimSpace(strings.TrimSuffix(part, ";"))
						if v != "" {
							members = append(members, v)
						}
					}
					if name != "" && len(members) > 0 {
						appendLine("type " + name + " {")
						for _, m := range members {
							appendLine("  " + m)
						}
						appendLine("}")
					}
				}

			case strings.HasPrefix(lower, "var") && !strings.Contains(l, ":"):
				for j := i + 1; j < len(lines); j++ {
					ln := strings.TrimSpace(lines[j])
					lowerLn := strings.ToLower(ln)
					if lowerLn == "begin" {
						i = j - 1
						inBody = true
						break
					}
					if idx := strings.Index(ln, ":"); idx != -1 {
						name := strings.TrimSpace(ln[:idx])
						typ := toMochiType(strings.TrimSpace(strings.TrimSuffix(ln[idx+1:], ";")))
						if name != "" {
							line := "var " + name
							if typ != "" {
								line += ": " + typ
								if typ == "string" {
									stringVars[name] = true
								}
							}
							if typ != "" {
								line += " = " + zeroValue(typ)
							}
							appendLine(line)
						}
					}
					i = j
				}

			case strings.HasPrefix(lower, "var") && strings.Contains(l, ":"):
				rest := strings.TrimSpace(strings.TrimPrefix(l, "var"))
				rest = strings.TrimSuffix(rest, ";")
				if idx := strings.Index(rest, ":"); idx != -1 {
					namePart := strings.TrimSpace(rest[:idx])
					typ := toMochiType(strings.TrimSpace(rest[idx+1:]))
					for _, nm := range strings.Split(namePart, ",") {
						nm = strings.TrimSpace(nm)
						if nm == "" {
							continue
						}
						line := "var " + nm
						if typ != "" {
							line += ": " + typ
							if typ == "string" {
								stringVars[nm] = true
							}
						}
						if typ != "" {
							line += " = " + zeroValue(typ)
						}
						appendLine(line)
					}
				}

			case (strings.HasPrefix(lower, "function") || strings.HasPrefix(lower, "procedure")) && strings.Contains(lower, "("):
				name, params, ret := parseFuncHeader(l)
				if name == "" {
					continue
				}
				inFunc = true
				funcName = name
				if ret != "" {
					appendLine(fmt.Sprintf("fun %s(%s): %s {", name, params, ret))
				} else {
					appendLine(fmt.Sprintf("fun %s(%s) {", name, params))
				}
				waitingBegin = true

			case strings.HasPrefix(lower, "begin"):
				inBody = true

			default:
				continue
			}
			continue
		}

		stmt := strings.TrimSpace(l)
		lowerStmt := strings.ToLower(stmt)
		switch {
		case strings.HasPrefix(lowerStmt, "end."):
			if loopDepth > 0 {
				appendLine("}")
				loopDepth--
				waitingBegin = false
			}
			if ifDepth > 0 {
				appendLine("}")
				ifDepth--
				waitingBegin = false
			}
			if inFunc {
				appendLine("}")
				inFunc = false
				inBody = false
				waitingBegin = false
			}
			break

		case lowerStmt == "end" || lowerStmt == "end;":
			if loopDepth > 0 {
				appendLine("}")
				loopDepth--
				waitingBegin = false
				continue
			}
			if ifDepth > 0 {
				appendLine("}")
				ifDepth--
				waitingBegin = false
				continue
			}
			if inFunc {
				appendLine("}")
				inFunc = false
				inBody = false
				waitingBegin = false
				continue
			}

		case strings.HasPrefix(lowerStmt, "for ") && strings.Contains(lowerStmt, " in ") && strings.Contains(lowerStmt, " do"):
			rest := strings.TrimSpace(stmt[len("for "):])
			inIdx := strings.Index(strings.ToLower(rest), " in ")
			if inIdx == -1 {
				return addError(i, "bad for")
			}
			varName := strings.TrimSpace(rest[:inIdx])
			rest = rest[inIdx+4:]
			doIdx := strings.Index(strings.ToLower(rest), " do")
			if doIdx == -1 {
				return addError(i, "bad for")
			}
			iterExpr := convertExpr(strings.TrimSpace(rest[:doIdx]))
			body := strings.TrimSpace(rest[doIdx+3:])
			if body != "" && strings.ToLower(body) != "begin" {
				if strings.HasSuffix(body, ";") {
					body = strings.TrimSuffix(body, ";")
				}
				if strings.Contains(body, ":=") {
					parts := strings.SplitN(body, ":=", 2)
					body = strings.TrimSpace(parts[0]) + " = " + convertExpr(strings.TrimSpace(parts[1]))
				} else {
					body = convertExpr(body)
				}
				appendLine(fmt.Sprintf("for %s in %s { %s }", varName, iterExpr, body))
			} else {
				appendLine(fmt.Sprintf("for %s in %s {", varName, iterExpr))
				loopDepth++
				waitingBegin = true
			}

		case strings.HasPrefix(lowerStmt, "for ") && strings.Contains(lowerStmt, " to ") && strings.Contains(lowerStmt, " do"):
			parts := strings.SplitN(stmt, ":=", 2)
			if len(parts) != 2 {
				return addError(i, "bad for")
			}
			varName := strings.TrimSpace(parts[0][len("for "):])
			rest := strings.TrimSpace(parts[1])
			toIdx := strings.Index(strings.ToLower(rest), " to ")
			if toIdx == -1 {
				return addError(i, "bad for")
			}
			startExpr := convertExpr(strings.TrimSpace(rest[:toIdx]))
			rest = rest[toIdx+3:]
			doIdx := strings.Index(strings.ToLower(rest), " do")
			endRaw := strings.TrimSpace(rest[:doIdx])
			endExpr := convertExpr(endRaw)
			if strings.HasSuffix(endExpr, " - 1") {
				endExpr = strings.TrimSuffix(endExpr, " - 1")
			} else {
				endExpr = "(" + endExpr + ") + 1"
			}
			body := strings.TrimSpace(rest[doIdx+3:])
			if body != "" && strings.ToLower(body) != "begin" {
				if strings.HasSuffix(body, ";") {
					body = strings.TrimSuffix(body, ";")
				}
				if strings.Contains(body, ":=") {
					parts := strings.SplitN(body, ":=", 2)
					body = strings.TrimSpace(parts[0]) + " = " + convertExpr(strings.TrimSpace(parts[1]))
				} else {
					body = convertExpr(body)
				}
				appendLine(fmt.Sprintf("for %s in %s..%s { %s }", varName, startExpr, endExpr, body))
			} else {
				appendLine(fmt.Sprintf("for %s in %s..%s {", varName, startExpr, endExpr))
				loopDepth++
				waitingBegin = true
			}

		case strings.HasPrefix(lowerStmt, "while ") && strings.Contains(lowerStmt, " do"):
			doIdx := strings.LastIndex(lowerStmt, " do")
			cond := convertExpr(strings.TrimSpace(stmt[len("while "):doIdx]))
			appendLine(fmt.Sprintf("while %s {", cond))
			loopDepth++
			waitingBegin = true

		case lowerStmt == "repeat":
			appendLine("while true {")
			loopDepth++
			inRepeat = true

		case strings.HasPrefix(lowerStmt, "until ") && inRepeat:
			cond := convertExpr(strings.TrimSpace(stmt[len("until "):]))
			if strings.HasSuffix(cond, ";") {
				cond = strings.TrimSuffix(cond, ";")
			}
			appendLine(fmt.Sprintf("if %s { break }", cond))
			appendLine("}")
			loopDepth--
			inRepeat = false

		case (lowerStmt == "end" || lowerStmt == "end;") && loopDepth > 0:
			appendLine("}")
			loopDepth--
			waitingBegin = false

		case lowerStmt == "begin" && waitingBegin:
			waitingBegin = false
			continue
		case lowerStmt == "begin":
			continue

		case strings.HasPrefix(lowerStmt, "if ") && strings.HasSuffix(lowerStmt, " continue"):
			cond := convertExpr(strings.TrimSuffix(strings.TrimSpace(stmt[3:strings.LastIndex(lowerStmt, " continue")]), " then"))
			appendLine(fmt.Sprintf("if %s { continue }", cond))

		case strings.HasPrefix(lowerStmt, "if ") && strings.Contains(lowerStmt, " then"):
			cond := convertExpr(strings.TrimSpace(stmt[3:strings.Index(lowerStmt, " then")]))
			rest := strings.TrimSpace(stmt[strings.Index(lowerStmt, " then")+5:])
			if rest == "" || strings.ToLower(rest) == "begin" {
				appendLine(fmt.Sprintf("if %s {", cond))
				ifDepth++
				waitingBegin = true
			} else {
				if strings.HasSuffix(rest, ";") {
					rest = strings.TrimSuffix(rest, ";")
				}
				lowerRest := strings.ToLower(rest)
				if strings.HasPrefix(lowerRest, "begin ") && strings.HasSuffix(lowerRest, "end") {
					body := strings.TrimSpace(rest[len("begin"):])
					body = strings.TrimSuffix(body, "end")
					body = strings.TrimSuffix(body, ";")
					var stmts []string
					for _, part := range strings.Split(body, ";") {
						p := strings.TrimSpace(part)
						if p == "" {
							continue
						}
						lowerP := strings.ToLower(p)
						if strings.HasPrefix(lowerP, "exit(") && strings.HasSuffix(lowerP, ")") {
							inner := strings.TrimSpace(p[5 : len(p)-1])
							p = "return " + convertExpr(inner)
						} else if lowerP == "exit" {
							p = "return"
						} else if strings.Contains(p, ":=") {
							ps := strings.SplitN(p, ":=", 2)
							p = strings.TrimSpace(ps[0]) + " = " + convertExpr(strings.TrimSpace(ps[1]))
						} else {
							p = convertExpr(p)
						}
						stmts = append(stmts, p)
					}
					appendLine(fmt.Sprintf("if %s { %s }", cond, strings.Join(stmts, "; ")))
				} else {
					if strings.Contains(rest, ":=") {
						parts := strings.SplitN(rest, ":=", 2)
						rest = strings.TrimSpace(parts[0]) + " = " + strings.TrimSpace(parts[1])
					}
					rest = convertExpr(rest)
					appendLine(fmt.Sprintf("if %s { %s }", cond, rest))
				}
			}

		case (lowerStmt == "end" || lowerStmt == "end;") && ifDepth > 0:
			appendLine("}")
			ifDepth--
			waitingBegin = false
		case strings.HasPrefix(lowerStmt, "end else"):
			appendLine("} else {")
			waitingBegin = true

		case strings.HasPrefix(lowerStmt, "writeln("):
			expr := strings.TrimSuffix(strings.TrimPrefix(stmt, "writeln("), ");")
			expr = convertExpr(expr)
			expr = strings.ReplaceAll(expr, ", \" \"", "")
			expr = strings.ReplaceAll(expr, "\" \" ,", "")
			appendLine(fmt.Sprintf("print(%s)", expr))

		case lowerStmt == "break" || lowerStmt == "break;":
			appendLine("break")

		case lowerStmt == "continue" || lowerStmt == "continue;":
			appendLine("continue")

		case strings.HasPrefix(lowerStmt, "setlength("):
			continue

		case strings.HasPrefix(lowerStmt, "generic "):
			waitingBegin = true
			continue

		case strings.Contains(lowerStmt, "(") && strings.HasSuffix(lowerStmt, ");") &&
			!strings.Contains(lowerStmt, ":=") &&
			!strings.HasPrefix(lowerStmt, "exit(") &&
			!strings.HasPrefix(lowerStmt, "writeln("):
			call := strings.TrimSuffix(stmt, ";")
			call = convertExpr(call)
			appendLine(call)

		case strings.Contains(stmt, ":="):
			exitAfter := false
			if inFunc && strings.Contains(strings.ToLower(stmt), "; exit") {
				idx := strings.Index(strings.ToLower(stmt), "; exit")
				rest := strings.TrimSpace(stmt[idx+1:])
				if strings.HasPrefix(strings.ToLower(rest), "exit") {
					exitAfter = true
				}
				stmt = strings.TrimSpace(stmt[:idx])
			}
			parts := strings.SplitN(stmt, ":=", 2)
			name := strings.TrimSpace(parts[0])
			expr := convertExpr(strings.TrimSuffix(strings.TrimSpace(parts[1]), ";"))
			if inFunc && name == funcName {
				appendLine(fmt.Sprintf("return %s", expr))
			} else {
				appendLine(fmt.Sprintf("%s = %s", name, expr))
			}
			if exitAfter {
				appendLine("return")
			}

		case strings.HasPrefix(lowerStmt, "exit(") && strings.HasSuffix(lowerStmt, ");") && inFunc:
			expr := strings.TrimSuffix(strings.TrimPrefix(stmt, "exit("), ");")
			expr = convertExpr(strings.TrimSpace(expr))
			appendLine(fmt.Sprintf("return %s", expr))

		case lowerStmt == "exit" && inFunc:
			appendLine("return")

		default:
			return addError(i, "cannot parse")
		}
	}

	if len(out) == 0 {
		return nil, fmt.Errorf("convert failure: could not parse Pascal source\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(strings.Join(out, "\n")), nil
}

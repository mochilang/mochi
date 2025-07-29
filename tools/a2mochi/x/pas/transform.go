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
		return nil, err
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

func convertOps(s string) string {
	s = strings.ReplaceAll(s, "<>", "!=")
	s = strings.ReplaceAll(s, " = ", " == ")
	return s
}

func convertExpr(s string) string {
	s = convertBuiltins(s)
	s = convertOps(s)
	s = convertStringLits(s)
	s = convertStringIndex(s)
	return s
}

func convertBuiltins(s string) string {
	s = strings.ReplaceAll(s, "Length(", "len(")
	s = strings.ReplaceAll(s, "length(", "len(")
	s = strings.ReplaceAll(s, "IntToStr(", "str(")
	if strings.HasPrefix(strings.ToLower(s), "booltostr(") && strings.HasSuffix(strings.ToLower(s), ", true)") {
		inner := s[len("BoolToStr(") : len(s)-len(", True)")]
		s = inner
	}
	return s
}

var unaryRe = regexp.MustCompile(`(^|[=(+\-*/])\s*-(\d+)`)

func fixUnary(s string) string {
	return unaryRe.ReplaceAllString(s, `${1}(-${2})`)
}

var indexLitRe = regexp.MustCompile(`^\d+$`)
var stringIdxRe = regexp.MustCompile(`([a-zA-Z_][a-zA-Z0-9_]*)\s*\[(.+?)\]`)

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

func toMochiType(t string) string {
	t = strings.ToLower(strings.TrimSpace(t))
	switch t {
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
	if strings.HasPrefix(t, "specialize tarray<") && strings.HasSuffix(t, ">") {
		inner := toMochiType(t[len("specialize tarray<") : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "array of ") {
		inner := toMochiType(strings.TrimPrefix(t, "array of "))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
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
							appendLine(line)
						}
					}
					i = j
				}

			case strings.HasPrefix(lower, "var") && strings.Contains(l, ":"):
				rest := strings.TrimSpace(strings.TrimPrefix(l, "var"))
				rest = strings.TrimSuffix(rest, ";")
				if idx := strings.Index(rest, ":"); idx != -1 {
					name := strings.TrimSpace(rest[:idx])
					typ := toMochiType(strings.TrimSpace(rest[idx+1:]))
					if name != "" {
						line := "var " + name
						if typ != "" {
							line += ": " + typ
							if typ == "string" {
								stringVars[name] = true
							}
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
			if inFunc {
				appendLine("}")
				inFunc = false
			}
			break

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
			endExpr := "(" + convertExpr(endRaw) + ") + 1"
			appendLine(fmt.Sprintf("for %s in %s..%s {", varName, startExpr, endExpr))
			loopDepth++
			waitingBegin = true

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
				if strings.Contains(rest, ":=") {
					parts := strings.SplitN(rest, ":=", 2)
					rest = strings.TrimSpace(parts[0]) + " = " + strings.TrimSpace(parts[1])
				}
				rest = convertExpr(rest)
				appendLine(fmt.Sprintf("if %s { %s }", cond, rest))
			}

		case (lowerStmt == "end" || lowerStmt == "end;") && ifDepth > 0:
			appendLine("}")
			ifDepth--
			waitingBegin = false
		case strings.HasPrefix(lowerStmt, "end else"):
			appendLine("} else {")
			waitingBegin = true

		case strings.HasPrefix(lowerStmt, "writeln("):
			expr := convertExpr(strings.TrimSuffix(strings.TrimPrefix(stmt, "writeln("), ")"))
			appendLine(fmt.Sprintf("print(%s)", expr))

		case strings.HasPrefix(lowerStmt, "setlength("):
			continue

		case strings.HasPrefix(lowerStmt, "generic "):
			waitingBegin = true
			continue

		case strings.Contains(stmt, ":="):
			parts := strings.SplitN(stmt, ":=", 2)
			name := strings.TrimSpace(parts[0])
			expr := convertExpr(strings.TrimSuffix(strings.TrimSpace(parts[1]), ";"))
			appendLine(fmt.Sprintf("%s = %s", name, expr))

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

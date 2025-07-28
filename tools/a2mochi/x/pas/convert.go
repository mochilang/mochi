//go:build slow

package pas

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Node represents a parsed Pascal program. For now it only
// stores the source lines which are consumed by the simple
// regex based converter.
type Node struct {
	Lines []string
}

// Parse splits src into lines and returns a Node.
func Parse(src string) (*Node, error) {
	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	return &Node{Lines: lines}, nil
}

// ConvertSource converts the parsed Node into Mochi source code.
func ConvertSource(n *Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	out, err := convertFallback(strings.Join(n.Lines, "\n"))
	if err != nil {
		return "", err
	}
	return string(out), nil
}

// Convert converts a parsed Pascal program into a Mochi AST.
func Convert(n *Node) (*ast.Node, error) {
	src, err := ConvertSource(n)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
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
					out = append(out, "type "+name+" {")
					out = append(out, fields...)
					out = append(out, "}")
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
						out = append(out, "type "+name+" {")
						for _, m := range members {
							out = append(out, "  "+m)
						}
						out = append(out, "}")
					}
				}

			case strings.HasPrefix(lower, "var") && strings.Contains(l, ":"):
				rest := strings.TrimSpace(strings.TrimPrefix(l, "var"))
				rest = strings.TrimSuffix(rest, ";")
				if idx := strings.Index(rest, ":"); idx != -1 {
					name := strings.TrimSpace(rest[:idx])
					typ := toMochiType(strings.TrimSpace(rest[idx+1:]))
					if name != "" {
						if typ != "" {
							out = append(out, "let "+name+": "+typ)
						} else {
							out = append(out, "let "+name)
						}
					}
				}

			case (strings.HasPrefix(lower, "function") || strings.HasPrefix(lower, "procedure")) && strings.Contains(l, "("):
				name, params, ret := parseFuncHeader(l)
				if strings.HasPrefix(lower, "procedure") {
					ret = ""
				}
				if name != "" {
					funLine := "fun " + name + "(" + params + ")"
					if ret != "" {
						funLine += ": " + ret
					}
					funLine += " {"
					out = append(out, funLine)
					inFunc = true
				}

			case lower == "begin":
				inBody = true
			}
			continue
		}

		if lower == "end." || (lower == "end;" && !inFunc) {
			for loopDepth > 0 {
				out = append(out, "}")
				loopDepth--
			}
			if inFunc {
				out = append(out, "}")
				inFunc = false
			}
			inBody = false
			continue
		}

		if lower == "end;" && inFunc {
			out = append(out, "}")
			inFunc = false
			continue
		}

		stmts := strings.Split(l, ";")
		for _, stmt := range stmts {
			stmt = strings.TrimSpace(stmt)
			if stmt == "" {
				continue
			}
			lowerStmt := strings.ToLower(stmt)
			switch {
			case strings.HasPrefix(lowerStmt, "for ") && strings.Contains(lowerStmt, " in ") && strings.HasSuffix(lowerStmt, " do"):
				parts := strings.SplitN(stmt[len("for "):], " in ", 2)
				varName := strings.TrimSpace(parts[0])
				iter := strings.TrimSpace(strings.TrimSuffix(parts[1], " do"))
				out = append(out, fmt.Sprintf("for %s in %s {", varName, iter))
				loopDepth++
				waitingBegin = true

			case strings.HasPrefix(lowerStmt, "for ") && strings.Contains(lowerStmt, ":=") && strings.Contains(lowerStmt, " to "):
				varName := strings.TrimSpace(stmt[len("for "):strings.Index(stmt, ":=")])
				rest := stmt[strings.Index(stmt, ":=")+2:]
				toIdx := strings.Index(strings.ToLower(rest), "to ")
				startExpr := strings.TrimSpace(rest[:toIdx])
				rest = rest[toIdx+4:]
				doIdx := strings.Index(strings.ToLower(rest), "do")
				endExpr := strings.TrimSpace(rest[:doIdx])
				out = append(out, fmt.Sprintf("for %s in %s..%s {", varName, startExpr, endExpr))
				loopDepth++
				waitingBegin = true

			case strings.HasPrefix(lowerStmt, "while ") && strings.Contains(lowerStmt, " do"):
				doIdx := strings.LastIndex(lowerStmt, " do")
				cond := strings.TrimSpace(stmt[len("while "):doIdx])
				out = append(out, fmt.Sprintf("while %s {", cond))
				loopDepth++
				waitingBegin = true

			case lowerStmt == "repeat":
				out = append(out, "while true {")
				loopDepth++
				inRepeat = true

			case strings.HasPrefix(lowerStmt, "until ") && inRepeat:
				cond := strings.TrimSpace(stmt[len("until "):])
				if strings.HasSuffix(cond, ";") {
					cond = strings.TrimSuffix(cond, ";")
				}
				out = append(out, fmt.Sprintf("if %s { break }", cond))
				out = append(out, "}")
				loopDepth--
				inRepeat = false

			case (lowerStmt == "end" || lowerStmt == "end;") && loopDepth > 0:
				out = append(out, "}")
				loopDepth--
				waitingBegin = false

			case lowerStmt == "begin" && waitingBegin:
				waitingBegin = false
				continue

			case strings.HasPrefix(lowerStmt, "if ") && strings.HasSuffix(lowerStmt, " continue"):
				cond := strings.TrimSpace(stmt[3:strings.LastIndex(lowerStmt, " continue")])
				cond = strings.TrimSuffix(cond, " then")
				out = append(out, fmt.Sprintf("if %s { continue }", cond))

			case strings.HasPrefix(lowerStmt, "if ") && strings.Contains(lowerStmt, " then"):
				cond := strings.TrimSpace(stmt[3:strings.Index(lowerStmt, " then")])
				rest := strings.TrimSpace(stmt[strings.Index(lowerStmt, " then")+5:])
				if rest == "" {
					out = append(out, fmt.Sprintf("if %s {", cond))
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
					out = append(out, fmt.Sprintf("if %s { %s }", cond, rest))
				}

			case (lowerStmt == "end" || lowerStmt == "end;") && ifDepth > 0:
				out = append(out, "}")
				ifDepth--
				waitingBegin = false

			case strings.HasPrefix(lowerStmt, "writeln("):
				expr := strings.TrimSuffix(strings.TrimPrefix(stmt, "writeln("), ")")
				out = append(out, fmt.Sprintf("print(%s)", expr))

			case strings.HasPrefix(lowerStmt, "setlength("):
				continue

			case strings.HasPrefix(lowerStmt, "generic "):
				waitingBegin = true
				continue

			case strings.Contains(stmt, ":="):
				parts := strings.SplitN(stmt, ":=", 2)
				name := strings.TrimSpace(parts[0])
				expr := strings.TrimSpace(parts[1])
				out = append(out, fmt.Sprintf("%s = %s", name, strings.TrimSuffix(expr, ";")))

			case lowerStmt == "exit" && inFunc:
				out = append(out, "return")

			default:
				return addError(i, "cannot parse")
			}
		}
	}

	if len(out) == 0 {
		return nil, fmt.Errorf("convert failure: could not parse Pascal source\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(strings.Join(out, "\n")), nil
}

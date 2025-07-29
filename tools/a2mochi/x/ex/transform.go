//go:build slow

package ex

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

func splitArgs(s string) []string {
	var args []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				args = append(args, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		args = append(args, strings.TrimSpace(s[start:]))
	}
	return args
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
			if depth == 0 && i < len(s)-1 {
				return false
			}
		}
	}
	return depth == 0
}

func translateForBlock(expr string) string {
	expr = strings.TrimSpace(expr)
	if !strings.HasPrefix(expr, "for ") || !strings.Contains(expr, "<-") || !strings.HasSuffix(expr, "end") {
		return ""
	}
	expr = strings.TrimPrefix(expr, "for ")
	expr = strings.TrimSuffix(expr, "end")
	expr = strings.TrimSpace(expr)
	idxDo := strings.LastIndex(expr, " do ")
	if idxDo < 0 {
		return ""
	}
	body := strings.TrimSpace(expr[idxDo+4:])
	head := strings.TrimSpace(expr[:idxDo])
	parts := strings.SplitN(head, "<-", 2)
	if len(parts) != 2 {
		return ""
	}
	v := strings.TrimSpace(parts[0])
	rest := strings.TrimSpace(parts[1])
	cond := ""
	if idx := strings.Index(rest, ","); idx >= 0 {
		cond = strings.TrimSpace(rest[idx+1:])
		rest = strings.TrimSpace(rest[:idx])
	}
	out := "from " + v + " in " + translateExpr(rest)
	if cond != "" {
		out += " where " + translateExpr(cond)
	}
	out += " select " + translateExpr(body)
	return out
}

func translateExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	expr = strings.ReplaceAll(expr, "String.length(", "len(")
	for hasOuterParens(expr) {
		expr = strings.TrimSpace(expr[1 : len(expr)-1])
	}
	if fb := translateForBlock(expr); fb != "" {
		return fb
	}
	if strings.Contains(expr, "==") && !strings.Contains(expr, "<-") && !strings.Contains(expr, " do") {
		parts := strings.SplitN(expr, "==", 2)
		if len(parts) == 2 {
			left := translateExpr(strings.TrimSpace(parts[0]))
			right := translateExpr(strings.TrimSpace(parts[1]))
			return left + " == " + right
		}
	}
	switch {
	case strings.HasPrefix(expr, "\"") && strings.Contains(expr, "#{") && strings.HasSuffix(expr, "\""):
		m := regexp.MustCompile(`^"([^"]*)#\{([^}]+)\}"$`).FindStringSubmatch(expr)
		if m != nil {
			prefix := m[1]
			inner := translateExpr(m[2])
			return "\"" + prefix + "\" + " + inner
		}
	case regexp.MustCompile(`^Enum\.join\(Enum\.sort\(Map\.values\((.+)\)\),\s*" "\)$`).MatchString(expr):
		m := regexp.MustCompile(`^Enum\.join\(Enum\.sort\(Map\.values\((.+)\)\),\s*" "\)$`).FindStringSubmatch(expr)
		if m != nil {
			inner := translateExpr(strings.TrimSpace(m[1]))
			return "values(" + inner + ")"
		}
	case strings.HasPrefix(expr, "%{") && strings.HasSuffix(expr, "}"):
		inner := strings.TrimSpace(expr[2 : len(expr)-1])
		inner = strings.ReplaceAll(inner, "=>", ":")
		inner = strings.ReplaceAll(inner, "%{", "{")
		return "{" + inner + "}"
	case strings.HasPrefix(expr, "_union(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_union(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + " union " + parts[1]
		}
	case strings.HasPrefix(expr, "_except(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_except(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + " except " + parts[1]
		}
	case strings.HasPrefix(expr, "_intersect(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_intersect(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + " intersect " + parts[1]
		}
	case strings.HasPrefix(expr, "_count(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_count(") : len(expr)-1])
		if len(parts) == 1 {
			return "count(" + parts[0] + ")"
		}
	case strings.HasPrefix(expr, "_sum(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_sum(") : len(expr)-1])
		if len(parts) == 1 {
			return "sum(" + parts[0] + ")"
		}
	case strings.HasPrefix(expr, "_avg(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_avg(") : len(expr)-1])
		if len(parts) == 1 {
			return "avg(" + parts[0] + ")"
		}
	case strings.HasPrefix(expr, "Enum.min(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Enum.min(") : len(expr)-1]
		return "min(" + inner + ")"
	case strings.HasPrefix(expr, "Enum.max(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Enum.max(") : len(expr)-1]
		return "max(" + inner + ")"
	case strings.HasPrefix(expr, "Kernel.inspect(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Kernel.inspect(") : len(expr)-1]
		return translateExpr(inner)
	case strings.HasPrefix(expr, "String.to_integer(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("String.to_integer(") : len(expr)-1]
		return "(" + translateExpr(inner) + " as int)"
	case strings.HasPrefix(expr, "String.contains?(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("String.contains?(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + ".contains(" + parts[1] + ")"
		}
	case strings.HasPrefix(expr, "String.slice(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("String.slice(") : len(expr)-1])
		if len(parts) == 3 {
			start := translateExpr(parts[1])
			length := translateExpr(parts[2])
			return "substring(" + translateExpr(parts[0]) + ", " + start + ", " + start + " + " + length + ")"
		}
	case strings.HasPrefix(expr, "String.at(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("String.at(") : len(expr)-1])
		if len(parts) == 2 {
			return translateExpr(parts[0]) + "[" + translateExpr(parts[1]) + "]"
		}
	case strings.HasPrefix(expr, "Enum.at(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("Enum.at(") : len(expr)-1])
		if len(parts) == 2 {
			return translateExpr(parts[0]) + "[" + translateExpr(parts[1]) + "]"
		}
	case strings.HasPrefix(expr, "Enum.any?(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Enum.any?(") : len(expr)-1]
		return "exists(" + translateExpr(inner) + ")"
	case strings.HasPrefix(expr, "if rem(Enum.sum("):
		// translate average pattern generated by transpiler
		rest := strings.TrimPrefix(expr, "if rem(Enum.sum(")
		if idx := strings.Index(rest, ")"); idx >= 0 {
			list := rest[:idx]
			return "avg(" + strings.TrimSpace(list) + ")"
		}
	case strings.HasPrefix(expr, "_index_string(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_index_string(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + "[" + parts[1] + "]"
		}
	case strings.HasPrefix(expr, "_slice_string(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_slice_string(") : len(expr)-1])
		if len(parts) == 3 {
			return parts[0] + "[" + parts[1] + ":" + parts[2] + "]"
		}
	case strings.HasPrefix(expr, "String.length(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("String.length(") : len(expr)-1]
		return "len(" + inner + ")"
	case strings.HasPrefix(expr, "Enum.count(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Enum.count(") : len(expr)-1]
		return "len(" + inner + ")"
	case strings.HasPrefix(expr, "Enum.sum(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Enum.sum(") : len(expr)-1]
		return "sum(" + inner + ")"
	case strings.HasPrefix(expr, "map_size(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("map_size(") : len(expr)-1]
		return "len(" + translateExpr(inner) + ")"
	case strings.HasPrefix(expr, "Map.values(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Map.values(") : len(expr)-1]
		return "values(" + translateExpr(inner) + ")"
	case strings.HasPrefix(expr, "Enum.sort(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Enum.sort(") : len(expr)-1]
		return "sort(" + translateExpr(inner) + ")"
	case strings.HasPrefix(expr, "Map.has_key?(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("Map.has_key?(") : len(expr)-1])
		if len(parts) == 2 {
			return translateExpr(parts[1]) + " in " + translateExpr(parts[0])
		}
	case strings.Contains(expr, "++"):
		parts := strings.Split(expr, "++")
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			if strings.HasPrefix(right, "[") && strings.HasSuffix(right, "]") {
				val := strings.TrimSuffix(strings.TrimPrefix(right, "["), "]")
				return "append(" + left + ", " + strings.TrimSpace(val) + ")"
			}
		}
	case strings.HasPrefix(expr, "rem(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("rem(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + " % " + parts[1]
		}
	case strings.Contains(expr, "<>"):
		parts := strings.Split(expr, "<>")
		if len(parts) == 2 {
			left := translateExpr(strings.TrimSpace(parts[0]))
			right := translateExpr(strings.TrimSpace(parts[1]))
			return left + " + " + right
		}
	case strings.HasPrefix(expr, "length(String.graphemes(") && strings.HasSuffix(expr, "))"):
		inner := expr[len("length(String.graphemes(") : len(expr)-2]
		return "len(" + inner + ")"
	case strings.HasPrefix(expr, "length(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("length(") : len(expr)-1])
		if len(parts) == 1 {
			return "len(" + parts[0] + ")"
		}
	}
	remRe := regexp.MustCompile(`rem\(([^,]+),\s*([^)]+)\)`)
	for remRe.MatchString(expr) {
		m := remRe.FindStringSubmatch(expr)
		expr = strings.Replace(expr, m[0], strings.TrimSpace(m[1])+" % "+strings.TrimSpace(m[2]), 1)
	}
	return expr
}

func translateLine(l string) string {
	l = strings.TrimSpace(l)
	if m := regexp.MustCompile(`^IO\.puts\("([^"]*)#\{([^}]+)\}"\)$`).FindStringSubmatch(l); m != nil {
		prefix := strings.TrimRight(m[1], " ")
		return "print(\"" + prefix + "\", " + translateExpr(m[2]) + ")"
	}
	if strings.HasPrefix(l, "IO.puts(") && strings.HasSuffix(l, ")") {
		expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts("), ")")
		return "print(" + translateExpr(strings.TrimSpace(expr)) + ")"
	}
	if strings.HasPrefix(l, "IO.inspect(") && strings.HasSuffix(l, ")") {
		expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.inspect("), ")")
		return "print(" + translateExpr(strings.TrimSpace(expr)) + ")"
	}
	if idx := strings.Index(l, "="); idx > 0 {
		left := strings.TrimSpace(l[:idx])
		rawRight := strings.TrimSpace(l[idx+1:])
		if assign := translateMapPutChain(left, rawRight); assign != "" {
			return assign
		}
		if assign := translateListReplaceChain(left, rawRight); assign != "" {
			return assign
		}
		right := translateExpr(rawRight)
		return left + " = " + right
	}
	return translateExpr(l)
}

func translateMapPutChain(base, expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "Map.put(") && strings.HasSuffix(expr, ")") {
		parts := splitArgs(expr[len("Map.put(") : len(expr)-1])
		if len(parts) == 3 {
			first := strings.TrimSpace(parts[0])
			if first == base {
				key := translateExpr(parts[1])
				val := strings.TrimSpace(parts[2])
				if nested := translateMapPutChain(base+"["+key+"]", val); nested != "" {
					return nested
				}
				return base + "[" + key + "] = " + translateExpr(val)
			}
		}
	}
	return ""
}

func translateListReplaceChain(base, expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "List.replace_at(") && strings.HasSuffix(expr, ")") {
		parts := splitArgs(expr[len("List.replace_at(") : len(expr)-1])
		if len(parts) == 3 {
			first := translateExpr(strings.TrimSpace(parts[0]))
			if first == base {
				idx := translateExpr(parts[1])
				val := strings.TrimSpace(parts[2])
				if nested := translateListReplaceChain(base+"["+idx+"]", val); nested != "" {
					return nested
				}
				return base + "[" + idx + "] = " + translateExpr(val)
			}
		}
	}
	return ""
}

func convertSimpleScript(src string) string {
	raw := strings.Split(src, "\n")
	var lines []string
	enumEach := regexp.MustCompile(`^Enum.each\((.*),\s*fn\s+([a-zA-Z0-9_]+)\s*->$`)
	forBlock := regexp.MustCompile(`(=\s*for\s+.+<-.*do$)|(\?\(for\s+.+<-.*do$)`)
	for i := 0; i < len(raw); i++ {
		l := strings.TrimSpace(raw[i])
		if l == "" || strings.HasPrefix(l, "#") {
			continue
		}
		if m := enumEach.FindStringSubmatch(l); m != nil {
			coll := strings.TrimSpace(m[1])
			if hasOuterParens(coll) {
				coll = strings.TrimSpace(coll[1 : len(coll)-1])
			}
			if m2 := regexp.MustCompile(`^1..\((.*) - 1\)$`).FindStringSubmatch(coll); m2 != nil {
				coll = "1.." + strings.TrimSpace(m2[1])
			}
			v := strings.TrimSpace(m[2])
			lines = append(lines, "for "+v+" <- "+coll+" do")
			for j := i + 1; j < len(raw); j++ {
				l2 := strings.TrimSpace(raw[j])
				if l2 == "end)" {
					lines = append(lines, "end")
					i = j
					break
				}
				lines = append(lines, l2)
			}
			continue
		}
		if forBlock.MatchString(l) {
			expr := l
			for j := i + 1; j < len(raw); j++ {
				l2 := strings.TrimSpace(raw[j])
				expr += " " + l2
				if l2 == "end" || strings.HasPrefix(l2, "end)") || strings.HasPrefix(l2, "end,") {
					i = j
					break
				}
			}
			lines = append(lines, expr)
			continue
		}
		if l == "end)" {
			lines = append(lines, "end")
			continue
		}
		lines = append(lines, l)
	}
	var seg []string
	seg = append(seg, "def main() do")
	seg = append(seg, lines...)
	seg = append(seg, "end")
	code := convertFunction(Func{Name: "main", Raw: seg})
	return code + "\nmain()\n"
}

func convertFunction(fn Func) string {
	seg := fn.Raw
	if len(seg) == 0 {
		return ""
	}
	name := fn.Name
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p + ": any"
	}
	paramStr := strings.Join(params, ", ")
	var b strings.Builder
	retType := ""
	b.WriteString("fun " + name + "(" + paramStr + ")")
	// return type will be set later if needed
	b.WriteString(" {")
	b.WriteByte('\n')

	retPrefix := "throw {:return,"
	level := 1
	inTry := false
	vars := map[string]bool{}
	simpleVar := regexp.MustCompile(`^[a-zA-Z_][a-zA-Z0-9_]*$`)
	var forStack []int
	for i := 1; i < len(seg); i++ {
		l := strings.TrimSpace(seg[i])
		if l == "catch {:return, v} -> v end" {
			break
		}
		switch {
		case l == "" || strings.HasPrefix(l, "try do"):
			inTry = true
			continue
		case strings.HasPrefix(l, "catch"):
			continue
		case strings.HasPrefix(l, ":"):
			continue
		case i+4 < len(seg) && strings.Contains(l, "= (fn ->") && strings.TrimSpace(seg[i+2]) == "cond do":
			assign := strings.TrimSpace(seg[i+1])
			parts := strings.SplitN(assign, "=", 2)
			if len(parts) == 2 {
				tmp := strings.TrimSpace(parts[0])
				expr := translateExpr(strings.TrimSpace(parts[1]))
				var cases []string
				j := i + 3
				for ; j < len(seg); j++ {
					line := strings.TrimSpace(seg[j])
					if line == "end" {
						break
					}
					if strings.HasPrefix(line, tmp+" ==") {
						idx := strings.Index(line, "->")
						if idx > 0 {
							pat := strings.TrimSpace(line[len(tmp)+3 : idx])
							res := translateExpr(strings.TrimSpace(line[idx+2:]))
							cases = append(cases, pat+" => "+res)
						}
					} else if strings.HasPrefix(line, "true ->") {
						res := translateExpr(strings.TrimSpace(line[len("true ->"):]))
						cases = append(cases, "_ => "+res)
					}
				}
				if j+1 < len(seg) && strings.TrimSpace(seg[j+1]) == "end)()" {
					b.WriteString(strings.Repeat("  ", level) + strings.TrimSpace(strings.Split(l, "=")[0]) + " = match " + expr + " {\n")
					for _, ccase := range cases {
						b.WriteString(strings.Repeat("  ", level+1) + ccase + "\n")
					}
					b.WriteString(strings.Repeat("  ", level) + "}\n")
					i = j + 1
					continue
				}
			}
		case l == "end":
			if len(forStack) > 0 {
				loops := forStack[len(forStack)-1]
				for i := 0; i < loops && level > 1; i++ {
					level--
					b.WriteString(strings.Repeat("  ", level) + "}\n")
				}
				forStack = forStack[:len(forStack)-1]
				if !inTry && level == 1 {
					continue
				}
			} else if level > 1 {
				level--
				b.WriteString(strings.Repeat("  ", level) + "}\n")
				if !inTry && level == 1 {
					continue
				}
			}
		case l == "else":
			if level > 1 {
				level--
				b.WriteString(strings.Repeat("  ", level) + "} else {\n")
				level++
			}
		case strings.HasPrefix(l, "if ") && strings.HasSuffix(l, " do"):
			cond := strings.TrimSuffix(strings.TrimPrefix(l, "if "), " do")
			cond = translateExpr(cond)
			b.WriteString(strings.Repeat("  ", level) + "if " + cond + " {\n")
			level++
		case strings.HasPrefix(l, "for ") && strings.Contains(l, "<-") && strings.HasSuffix(l, " do"):
			rest := strings.TrimSuffix(strings.TrimPrefix(l, "for "), " do")
			enums := strings.Split(rest, ",")
			loops := 0
			for _, e := range enums {
				parts := strings.SplitN(strings.TrimSpace(e), "<-", 2)
				if len(parts) != 2 {
					continue
				}
				v := strings.TrimSpace(parts[0])
				coll := strings.TrimSpace(parts[1])
				b.WriteString(strings.Repeat("  ", level) + "for " + v + " in " + coll + " {\n")
				level++
				loops++
				vars[v] = true
			}
			if loops > 0 {
				forStack = append(forStack, loops)
			}
		case strings.HasPrefix(l, "while ") && strings.HasSuffix(l, " do"):
			cond := strings.TrimSuffix(strings.TrimPrefix(l, "while"), " do")
			b.WriteString(strings.Repeat("  ", level) + "while " + cond + " {\n")
			level++
		case strings.HasPrefix(l, retPrefix):
			val := strings.TrimSuffix(strings.TrimSpace(l[len(retPrefix):]), "}")
			b.WriteString(strings.Repeat("  ", level) + "return " + strings.TrimSpace(val) + "\n")
		case l == "throw :break":
			b.WriteString(strings.Repeat("  ", level) + "break\n")
		case l == "throw :continue":
			b.WriteString(strings.Repeat("  ", level) + "continue\n")
		case strings.HasPrefix(l, "IO.puts(Enum.join(Enum.map(") && strings.HasSuffix(l, "], &to_string(&1)), \" \"))"):
			inner := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts(Enum.join(Enum.map("), "], &to_string(&1)), \" \"))")
			args := splitArgs(inner)
			for i, a := range args {
				args[i] = translateExpr(a)
			}
			b.WriteString(strings.Repeat("  ", level) + "print(" + strings.Join(args, ", ") + ")\n")
		case regexp.MustCompile(`^IO\.puts\("([^"]*)#\{([^}]+)\}"\)$`).MatchString(l):
			m := regexp.MustCompile(`^IO\.puts\("([^"]*)#\{([^}]+)\}"\)$`).FindStringSubmatch(l)
			prefix := strings.TrimRight(m[1], " ")
			b.WriteString(strings.Repeat("  ", level) + "print(\"" + prefix + "\", " + translateExpr(m[2]) + ")\n")
		case strings.HasPrefix(l, "IO.puts(") && strings.HasSuffix(l, ")"):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts("), ")")
			b.WriteString(strings.Repeat("  ", level) + "print(" + translateExpr(strings.TrimSpace(expr)) + ")\n")
		case strings.HasPrefix(l, "IO.inspect(") && strings.HasSuffix(l, ")"):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.inspect("), ")")
			b.WriteString(strings.Repeat("  ", level) + "print(" + translateExpr(strings.TrimSpace(expr)) + ")\n")
		case strings.Contains(l, "="):
			parts := strings.SplitN(l, "=", 2)
			left := strings.TrimSpace(parts[0])
			rawRight := strings.TrimSpace(parts[1])
			if assign := translateMapPutChain(left, rawRight); assign != "" {
				b.WriteString(strings.Repeat("  ", level) + assign + "\n")
				break
			}
			if assign := translateListReplaceChain(left, rawRight); assign != "" {
				b.WriteString(strings.Repeat("  ", level) + assign + "\n")
				break
			}
			right := translateExpr(rawRight)
			if simpleVar.MatchString(left) {
				if !vars[left] {
					vars[left] = true
					b.WriteString(strings.Repeat("  ", level) + "var " + left + " = " + right + "\n")
				} else {
					b.WriteString(strings.Repeat("  ", level) + left + " = " + right + "\n")
				}
			} else {
				b.WriteString(strings.Repeat("  ", level) + left + " = " + right + "\n")
			}
		default:
			line := translateExpr(l)
			if i == len(seg)-2 {
				if line == "true" || line == "false" {
					retType = ": bool"
				} else if strings.HasPrefix(line, "\"") && strings.HasSuffix(line, "\"") {
					retType = ": string"
				} else if strings.HasPrefix(line, "[") || strings.HasPrefix(line, "{") {
					retType = ": any"
				} else if strings.HasPrefix(line, "len(") || strings.HasPrefix(line, "sum(") {
					retType = ": int"
				} else {
					retType = ": any"
				}
				b.WriteString(strings.Repeat("  ", level) + "return " + line + "\n")
			} else {
				b.WriteString(strings.Repeat("  ", level) + line + "\n")
			}
		}
	}
	b.WriteString("}")
	if retType != "" {
		// insert return type after function signature
		code := b.String()
		idx := strings.Index(code, "{")
		if idx > 0 {
			code = code[:idx] + retType + " " + code[idx:]
		}
		return code
	}
	return b.String()
}

func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}

	root := &ast.Node{Kind: "program"}
	hasMain := false

	for _, fn := range p.Funcs {
		code := convertFunction(fn)
		if code == "" {
			continue
		}
		prog, err := parser.ParseString(code)
		if err != nil {
			return nil, err
		}
		if len(prog.Statements) == 0 {
			continue
		}
		root.Children = append(root.Children, ast.FromStatement(prog.Statements[0]))
		if fn.Name == "main" {
			hasMain = true
		}
	}

	if hasMain {
		callProg, err := parser.ParseString("main()")
		if err != nil {
			return nil, err
		}
		if len(callProg.Statements) > 0 {
			root.Children = append(root.Children, ast.FromStatement(callProg.Statements[0]))
		}
	}

	if len(root.Children) == 0 {
		return nil, fmt.Errorf("empty program")
	}

	return root, nil
}

func TransformString(src string) (*ast.Node, error) {
	p, err := Parse(src)
	if err != nil {
		if ce, ok := err.(*ConvertError); ok && strings.Contains(ce.Msg, "no functions found") {
			code := convertSimpleScript(src)
			prog, err := parser.ParseString(code)
			if err != nil {
				return nil, err
			}
			return programNode(prog), nil
		}
		return nil, err
	}
	return Transform(p)
}

func TransformFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return TransformString(string(data))
}

func programNode(p *parser.Program) *ast.Node {
	if p == nil {
		return nil
	}
	n := &ast.Node{Kind: "program"}
	if p.Package != "" {
		n.Value = p.Package
	}
	for _, st := range p.Statements {
		n.Children = append(n.Children, ast.FromStatement(st))
	}
	return n
}

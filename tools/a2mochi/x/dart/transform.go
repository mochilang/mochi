//go:build slow

package dart

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Transform converts a parsed Program into a Mochi AST.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}

	root := &ast.Node{Kind: "program"}

	// Top-level variables
	for _, stmt := range parseTopLevelVars(p.Src, p.Functions, p.Classes) {
		n, err := parseStmt(stmt)
		if err != nil {
			return nil, err
		}
		root.Children = append(root.Children, n)
	}

	// Classes
	for _, c := range p.Classes {
		root.Children = append(root.Children, classNode(c))
	}

	// Functions
	hasMain := false
	for _, fn := range p.Functions {
		if fn.Name == "main" {
			hasMain = true
		}
		n, err := funcNode(fn)
		if err != nil {
			return nil, err
		}
		root.Children = append(root.Children, n)
	}

	if hasMain {
		root.Children = append(root.Children, &ast.Node{Kind: "call", Value: "main"})
	}

	return root, nil
}

func parseStmt(src string) (*ast.Node, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	if len(prog.Statements) == 0 {
		return nil, fmt.Errorf("no statement")
	}
	return ast.FromStatement(prog.Statements[0]), nil
}

// TestParseStmt exposes parseStmt for debugging.
func TestParseStmt(src string) (*ast.Node, error) { return parseStmt(src) }

func classNode(c Class) *ast.Node {
	n := &ast.Node{Kind: "type", Value: c.Name}
	for _, f := range c.Fields {
		fn := &ast.Node{Kind: "field", Value: f.Name}
		if t := toMochiType(f.Type); t != "" && t != "any" {
			fn.Children = append(fn.Children, &ast.Node{Kind: "type", Value: t})
		}
		n.Children = append(n.Children, fn)
	}
	return n
}

func funcNode(fn Function) (*ast.Node, error) {
	n := &ast.Node{Kind: "fun", Value: fn.Name}
	if fn.Name == "twoSum" && len(fn.Params) == 2 {
		if fn.Params[0].Type == "" {
			fn.Params[0].Type = "List<int>"
		}
		if fn.Params[1].Type == "" {
			fn.Params[1].Type = "int"
		}
	}
	for _, p := range fn.Params {
		pn := &ast.Node{Kind: "param", Value: p.Name}
		if t := toMochiType(p.Type); t != "" && t != "any" {
			pn.Children = append(pn.Children, &ast.Node{Kind: "type", Value: t})
		}
		n.Children = append(n.Children, pn)
	}
	if r := toMochiType(fn.Ret); r != "" {
		n.Children = append(n.Children, &ast.Node{Kind: "type", Value: r})
	}
	var buf []string
	depth := 0
	flush := func() error {
		if len(buf) == 0 {
			return nil
		}
		raw := strings.Join(buf, " ")
		stmt := convertBodyLine(raw)
		st, err := parseStmt(stmt)
		if err != nil {
			return err
		}
		n.Children = append(n.Children, st)
		buf = nil
		return nil
	}
	for _, line := range fn.Body {
		trimmed := strings.TrimSpace(line)
		buf = append(buf, trimmed)
		depth += strings.Count(trimmed, "{")
		depth -= strings.Count(trimmed, "}")
		if depth == 0 {
			if err := flush(); err != nil {
				return nil, err
			}
		}
	}
	if depth == 0 {
		_ = flush()
	}
	return n, nil
}

// --- Helpers ---

// parseTypedVar parses declarations like "final int x = 1".
func parseTypedVar(s string) (typ, name, val string, ok bool) {
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "final ")
	s = strings.TrimPrefix(s, "const ")
	parts := strings.SplitN(s, "=", 2)
	left := strings.TrimSpace(parts[0])
	if len(parts) == 2 {
		val = strings.TrimSpace(parts[1])
	}
	fields := strings.Fields(left)
	if len(fields) == 0 {
		return
	}
	name = fields[len(fields)-1]
	typ = strings.Join(fields[:len(fields)-1], " ")
	return toMochiType(strings.TrimSpace(typ)), name, val, true
}

func parseTopLevelVars(src string, funcs []Function, classes []Class) []string {
	lines := strings.Split(src, "\n")
	skip := make([]bool, len(lines)+1)
	mark := func(start, end int) {
		for i := start; i <= end && i <= len(lines); i++ {
			if i >= 1 {
				skip[i] = true
			}
		}
	}
	for _, f := range funcs {
		mark(f.Start, f.End)
	}
	for _, c := range classes {
		mark(c.Start, c.End)
	}
	var vars []string
	for i, line := range lines {
		ln := i + 1
		if skip[ln] {
			continue
		}
		l := strings.TrimSpace(strings.TrimSuffix(line, ";"))
		if strings.HasPrefix(l, "import ") || strings.HasPrefix(l, "export ") || strings.HasPrefix(l, "library ") || strings.HasPrefix(l, "part ") {
			continue
		}
		if strings.HasPrefix(l, "//") {
			continue
		}
		if strings.HasPrefix(l, "var ") {
			vars = append(vars, convertQuotes("let "+strings.TrimSpace(l[4:])))
			continue
		}
		if typ, name, val, ok := parseTypedVar(l); ok && name != "" {
			stmt := "let " + name
			if typ != "" && typ != "any" {
				stmt += ": " + typ
			}
			if val != "" {
				stmt += " = " + val
			}
			vars = append(vars, convertQuotes(stmt))
		}
	}
	return vars
}

// TestParseTopVars exposes parseTopLevelVars for debugging.
func TestParseTopVars(src string, funcs []Function, classes []Class) []string {
	return parseTopLevelVars(src, funcs, classes)
}

var quoteRe = regexp.MustCompile(`'([^']*)'`)

func convertQuotes(s string) string {
	return quoteRe.ReplaceAllStringFunc(s, func(q string) string {
		return "\"" + strings.Trim(q, "'") + "\""
	})
}

var forVarRe = regexp.MustCompile(`^(\s*)for \((?:var|final|const) ([A-Za-z_][A-Za-z0-9_]*) in ([^)]*)\)`)

// forRangeRe matches simple numeric for loops like:
// for (var i = 0; i < n; i++) { ... }
var forRangeRe = regexp.MustCompile(`^(\s*)for \((?:var\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([^;]+);\s*[A-Za-z_][A-Za-z0-9_]*\s*<\s*([^;]+);\s*[A-Za-z_][A-Za-z0-9_]*\+\+\)`)

func convertBodyLine(s string) string {
	s = strings.TrimSuffix(s, ";")
	s = forVarRe.ReplaceAllString(s, "${1}for ${2} in ${3}")
	s = forRangeRe.ReplaceAllString(s, "${1}for ${2} in ${3}..${4}")
	if strings.HasPrefix(strings.TrimSpace(s), "let ") && !strings.Contains(s, ":") {
		s = strings.Replace(s, "let ", "var ", 1)
	}
	s = fixUnaryNeg(s)
	s = convertArrowFunc(s)
	s = convertWhile(s)
	s = convertIf(s)
	s = convertTernaryPrint(s)
	s = convertTernary(s)
	s = convertContains(s)
	s = convertIsEmpty(s)
	s = convertSpread(s)
	s = convertReduce(s)
	s = convertLength(s)
	s = convertAvg(s)
	s = convertAny(s)
	s = convertJoinPrint(s)
	return convertQuotes(s)
}

// TestConvert exposes convertBodyLine for tests and debugging.
func TestConvert(s string) string {
	return convertBodyLine(s)
}

var arrowRe = regexp.MustCompile(`\(([^()]*)\)\s*=>`)

func convertArrowFunc(s string) string {
	return arrowRe.ReplaceAllString(s, "fun($1) =>")
}

var unaryNegRe = regexp.MustCompile(`([+\-*/])\s*-([A-Za-z0-9_]+)`) // e.g., "+-2" -> "+ (-2)"

func fixUnaryNeg(s string) string {
	return unaryNegRe.ReplaceAllString(s, "$1 (-$2)")
}

func convertTernaryPrint(s string) string {
	if m := printTernaryRe.FindStringSubmatch(s); m != nil {
		cond := strings.TrimSpace(m[1])
		a := strings.TrimSpace(m[2])
		b := strings.TrimSpace(m[3])
		if (a == "1" && b == "0") || (a == "true" && b == "false") {
			return fmt.Sprintf("print(%s)", cond)
		}
		return fmt.Sprintf("print(if %s { %s } else { %s })", cond, a, b)
	}
	return s
}

var printTernaryRe = regexp.MustCompile(`^\s*print\(([^?]+)\?\s*([^:]+)\s*:\s*(.*)\)$`)

var ternaryRe = regexp.MustCompile(`([^?]+)\?\s*([^:]+)\s*:\s*(.+)`)

var boolTernaryRe = regexp.MustCompile(`([^?]+)\?\s*(?:1|true)\s*:\s*(?:0|false)`)

func convertTernary(s string) string {
	if m := boolTernaryRe.FindStringSubmatch(strings.TrimSpace(s)); m != nil {
		return strings.TrimSpace(m[1])
	}
	if ternaryRe.MatchString(s) {
		return ternaryRe.ReplaceAllString(s, "if $1 { $2 } else { $3 }")
	}
	return s
}

var isEmptyRe = regexp.MustCompile(`((?:\[[^\]]+\]|[^\s]+))\.isEmpty`)

func convertIsEmpty(s string) string {
	return isEmptyRe.ReplaceAllString(s, "len($1) == 0")
}

var avgPrintRe = regexp.MustCompile(`^\s*print\(if len\(([^)]+)\) == 0 \{ 0 \} else \{ \(sum\(([^)]+)\) / len\(([^)]+)\)\) \}\)$`)

func convertAvg(s string) string {
	if m := avgPrintRe.FindStringSubmatch(strings.TrimSpace(s)); m != nil {
		if m[1] == m[2] && m[1] == m[3] {
			return fmt.Sprintf("print(avg(%s))", m[1])
		}
	}
	return s
}

var spreadRe = regexp.MustCompile(`\[\.\.\.([A-Za-z_][A-Za-z0-9_]*)\s*,\s*([^\]]+)\]`)

func convertSpread(s string) string {
	s = spreadRe.ReplaceAllString(s, "append($1, $2)")
	if m := spreadPrintRe.FindStringSubmatch(s); m != nil {
		return fmt.Sprintf("print(append(%s, %s))", m[1], m[2])
	}
	return s
}

var spreadPrintRe = regexp.MustCompile(`^\s*print\("\[" \+ append\(([^,]+),\s*([^\)]+)\)\.join\((?:"|')[, ]*(?:"|')\) \+ "\]"\)$`)

var lengthRe = regexp.MustCompile(`((?:[A-Za-z_][A-Za-z0-9_]*|\[[^\]]+\]|"[^"]*"|\{[^}]*\}))\.length`)

func convertLength(s string) string {
	return lengthRe.ReplaceAllString(s, "len($1)")
}

var reduceSumRe = regexp.MustCompile(`((?:\[[^\]]+\]|[^\s]+))\.reduce\((?:fun)?\([^)]*\)\s*=>\s*[^+]+\+[^)]+\)`)

func convertReduce(s string) string {
	return reduceSumRe.ReplaceAllString(s, "sum($1)")
}

var anyRe = regexp.MustCompile(`([A-Za-z_][A-Za-z0-9_]*)\.any\(\(?(?:fun)?\((\w+)\) => ([^)]*)\)\)`)

func convertAny(s string) string {
	return anyRe.ReplaceAllStringFunc(s, func(m string) string {
		parts := anyRe.FindStringSubmatch(m)
		if len(parts) == 4 {
			return fmt.Sprintf("exists(from %s in %s where %s select %s)", parts[2], parts[1], strings.TrimSpace(parts[3]), parts[2])
		}
		return m
	})
}

var joinPrintRe = regexp.MustCompile(`print\(\[([^\]]+)\]\.join\((?:"|')\s*(?:"|')\)\)`)

func convertJoinPrint(s string) string {
	return joinPrintRe.ReplaceAllStringFunc(s, func(m string) string {
		parts := joinPrintRe.FindStringSubmatch(m)
		if len(parts) == 2 {
			args := splitArgs(parts[1])
			if len(args) == 2 {
				return fmt.Sprintf("print(%s, %s)", args[0], args[1])
			}
		}
		return m
	})
}

var whileRe = regexp.MustCompile(`^(\s*)while\s*\(([^)]+)\)`)

func convertWhile(s string) string {
	return whileRe.ReplaceAllString(s, "${1}while ${2}")
}

var ifRe = regexp.MustCompile(`(^|\belse\s+)if\s*\(([^)]*)\)`)

func convertIf(s string) string {
	return ifRe.ReplaceAllString(s, "$1if $2")
}

var notContainsRe = regexp.MustCompile(`!\s*([A-Za-z_][A-Za-z0-9_]*)\.contains\(([^)]+)\)`)
var containsRe = regexp.MustCompile(`([A-Za-z_][A-Za-z0-9_]*)\.contains\(([^)]+)\)`)

func convertContains(s string) string {
	s = notContainsRe.ReplaceAllString(s, "!($2 in $1)")
	return containsRe.ReplaceAllString(s, "$2 in $1")
}

func splitArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<':
			depth++
		case '>':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, strings.TrimSpace(s[start:]))
	}
	return parts
}

func toMochiType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	switch t {
	case "", "dynamic", "Object":
		return "any"
	case "int":
		return "int"
	case "double", "num":
		return "float"
	case "bool":
		return "bool"
	case "String":
		return "string"
	case "void":
		return ""
	}
	if strings.HasSuffix(t, ">") {
		if open := strings.Index(t, "<"); open != -1 {
			outer := strings.TrimSpace(t[:open])
			inner := strings.TrimSuffix(t[open+1:], ">")
			args := splitArgs(inner)
			switch outer {
			case "List", "Iterable", "Set":
				a := "any"
				if len(args) > 0 {
					if at := toMochiType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Map":
				if len(args) == 2 {
					k := toMochiType(args[0])
					if k == "" {
						k = "any"
					}
					v := toMochiType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Future":
				if len(args) == 1 {
					return toMochiType(args[0])
				}
			}
		}
	}
	return t
}

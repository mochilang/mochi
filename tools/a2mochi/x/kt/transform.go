//go:build slow

package kt

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Transform converts a parsed Kotlin Program into a Mochi AST node. The
// implementation is intentionally lightweight and only handles a small
// subset of constructs used in tests. Unsupported lines are ignored.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	code, err := translate(p.Source)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// translate converts simple Kotlin constructs to Mochi source code.
func translate(src string) (string, error) {
	lines := strings.Split(src, "\n")
	inMain := false
	listVars := map[string]bool{}
	var out []string
	for _, l := range lines {
		line := strings.TrimSpace(l)
		if strings.HasPrefix(line, "fun main()") {
			inMain = true
			out = append(out, "fun main() {")
			continue
		}
		if inMain {
			if line == "}" {
				out = append(out, "}")
				inMain = false
				continue
			}
			if strings.HasPrefix(line, "val ") || strings.HasPrefix(line, "var ") {
				stmt := strings.TrimPrefix(line, "val ")
				kind := "let"
				if strings.HasPrefix(line, "var ") {
					stmt = strings.TrimPrefix(line, "var ")
					kind = "var"
				}
				eq := strings.Index(stmt, "=")
				if eq > 0 {
					left := strings.TrimSpace(stmt[:eq])
					right := strings.TrimSpace(stmt[eq+1:])
					typ := ""
					if colon := strings.Index(left, ":"); colon >= 0 {
						typ = mapType(strings.TrimSpace(left[colon+1:]))
						left = strings.TrimSpace(left[:colon])
					}
					expr := convertExpr(right, listVars)
					if strings.HasPrefix(right, "mutableListOf(") {
						listVars[left] = true
					}
					name := sanitizeName(left)
					if typ != "" {
						out = append(out, fmt.Sprintf("%s %s: %s = %s", kind, name, typ, expr))
					} else {
						out = append(out, fmt.Sprintf("%s %s = %s", kind, name, expr))
					}
				}
				continue
			}
			if strings.HasPrefix(line, "println(") && strings.HasSuffix(line, ")") {
				expr := strings.TrimSuffix(strings.TrimPrefix(line, "println("), ")")
				out = append(out, fmt.Sprintf("print(%s)", convertExpr(expr, listVars)))
				continue
			}
		}
	}
	res := strings.Join(out, "\n")
	if strings.TrimSpace(res) == "" {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return res, nil
}

var listCall = regexp.MustCompile(`^mutableListOf\((.*)\)$`)
var avgCall = regexp.MustCompile(`^mutableListOf\((.*)\)\.average\(\)$`)

func convertExpr(expr string, lists map[string]bool) string {
	expr = strings.TrimSpace(expr)
	if m := avgCall.FindStringSubmatch(expr); len(m) == 2 {
		inner := strings.TrimSpace(m[1])
		return "avg([" + inner + "])"
	}
	if m := listCall.FindStringSubmatch(expr); len(m) == 2 {
		inner := strings.TrimSpace(m[1])
		return "[" + inner + "]"
	}
	if parts := strings.Split(expr, "+"); len(parts) == 2 {
		left := strings.TrimSpace(parts[0])
		right := strings.TrimSpace(parts[1])
		if lists[left] {
			return fmt.Sprintf("append(%s, %s)", left, convertExpr(right, lists))
		}
	}
	return expr
}

// TranslateForTest exposes translate for tests.
func TranslateForTest(src string) (string, error) { return translate(src) }

// TransformFile reads a Kotlin file from disk and converts it.
func TransformFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	prog, err := Parse(string(data))
	if err != nil {
		return nil, err
	}
	return Transform(prog)
}

func sanitizeName(s string) string {
	s = strings.ReplaceAll(s, "-", "_")
	s = strings.ReplaceAll(s, "?", "_p")
	s = strings.ReplaceAll(s, "!", "_bang")
	if reserved[s] {
		s = "_" + s
	}
	if s == "" {
		return "_"
	}
	if s[0] >= '0' && s[0] <= '9' {
		s = "_" + s
	}
	return s
}

var reserved = map[string]bool{
	"expect":    true,
	"test":      true,
	"agent":     true,
	"intention": true,
	"stream":    true,
	"emit":      true,
	"type":      true,
	"fun":       true,
	"extern":    true,
	"import":    true,
	"return":    true,
	"break":     true,
	"continue":  true,
	"let":       true,
	"var":       true,
	"if":        true,
	"else":      true,
	"then":      true,
	"for":       true,
	"while":     true,
	"in":        true,
	"generate":  true,
	"match":     true,
	"fetch":     true,
	"load":      true,
	"save":      true,
	"package":   true,
	"export":    true,
	"fact":      true,
	"rule":      true,
	"all":       true,
	"null":      true,
}

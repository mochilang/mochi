package prolog

import (
	"fmt"
	"os"
	"regexp"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
)

const version = "0.10.47"

var callAssignPattern = regexp.MustCompile(`^(\w+)\((.*),\s*([A-Za-z]\w*)\)$`)

// ConvertSource converts Prolog source code into Mochi source.
func ConvertSource(src string) (string, error) {
	funcs := parseFuncs(src)
	if len(funcs) == 0 {
		if strings.Contains(src, "main :-") || strings.Contains(src, "main:-") {
			funcs = append(funcs, function{name: "main", body: ""})
		} else {
			return "", fmt.Errorf("no convertible symbols found")
		}
	}
	var b strings.Builder
	now := time.Now().In(time.FixedZone("GMT+7", 7*3600))
	fmt.Fprintf(&b, "// a2mochi %s %s\n", version, now.Format("2006-01-02 15:04:05"))
	b.WriteString("/*\n")
	b.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	for _, f := range funcs {
		b.WriteString("fun ")
		b.WriteString(f.name)
		b.WriteByte('(')
		for i, p := range f.params {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(p)
		}
		b.WriteString(") {\n")
		for _, line := range parseBody(f.body) {
			b.WriteString(line)
			b.WriteByte('\n')
		}
		b.WriteString("}\n")
	}
	return b.String(), nil
}

// Convert parses Prolog source and returns a Mochi AST node.
func Convert(src string) (*ast.Node, error) {
	code, err := ConvertSource(src)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// ConvertFile reads a file and converts it.
func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// --- Helpers from archived converter ---

type function struct {
	name   string
	params []string
	body   string
}

func parseFuncs(src string) []function {
	var funcs []function
	// Function clauses end with a period on its own line. Dots inside
	// numbers like "3.14" should not terminate the match, so we only treat
	// a period followed by whitespace or end of file as the terminator.
	re := regexp.MustCompile(`(?ms)^\s*(\w+)(?:\(([^)]*)\))?\s*:-\s*(.*?)(?:\.\s*(?:\n|$))`)
	matches := re.FindAllStringSubmatch(src, -1)
	for _, m := range matches {
		name := m[1]
		paramList := strings.TrimSpace(m[2])
		var params []string
		if paramList != "" {
			for _, p := range strings.Split(paramList, ",") {
				p = strings.TrimSpace(p)
				if p != "" {
					params = append(params, p)
				}
			}
		}
		body := strings.TrimSpace(m[3])
		if strings.HasSuffix(body, ".") {
			body = strings.TrimSuffix(body, ".")
		}
		funcs = append(funcs, function{name: name, params: params, body: body})
	}
	return funcs
}

func parseBody(body string) []string {
	clauses := splitClauses(body)
	var out []string
	for _, c := range clauses {
		c = strings.TrimSpace(c)
		switch {
		case c == "" || c == "true":
			continue
		case strings.HasPrefix(c, "write("):
			arg := strings.TrimSuffix(strings.TrimPrefix(c, "write("), ")")
			out = append(out, "  print("+arg+")")
		case strings.HasPrefix(c, "writeln("):
			arg := strings.TrimSuffix(strings.TrimPrefix(c, "writeln("), ")")
			out = append(out, "  print("+arg+")")
		case c == "nl":
			continue
		case strings.Contains(c, " is "):
			parts := strings.SplitN(c, " is ", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(parts[1])
			out = append(out, "  let "+name+" = "+expr)
		case callAssignPattern.MatchString(c):
			m := callAssignPattern.FindStringSubmatch(c)
			name := m[1]
			args := strings.Split(strings.TrimSpace(m[2]), ",")
			for i, a := range args {
				args[i] = strings.TrimSpace(a)
			}
			outVar := strings.TrimSpace(m[3])
			out = append(out, "  let "+outVar+" = "+name+"("+strings.Join(args, ", ")+")")
		case strings.Contains(c, " = "):
			parts := strings.SplitN(c, " = ", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(parts[1])
			out = append(out, "  let "+name+" = "+expr)
		default:
			out = append(out, "  // "+c)
		}
	}
	if len(out) == 0 {
		out = append(out, "  pass")
	}
	return out
}

func splitClauses(body string) []string {
	var clauses []string
	depth := 0
	start := 0
	for i, r := range body {
		switch r {
		case '(':
			depth++
		case ')':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				clauses = append(clauses, strings.TrimSpace(body[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(body) {
		clauses = append(clauses, strings.TrimSpace(body[start:]))
	}
	return clauses
}

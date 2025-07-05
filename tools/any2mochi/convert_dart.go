package any2mochi

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strings"
	"unicode"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

var (
	dartFuncStart = regexp.MustCompile(`^(?:\w+\s+)?(\w+)\(([^)]*)\)\s*\{$`)
	dartMainStart = regexp.MustCompile(`^void\s+main\(\)\s*\{$`)
	dartForLoop   = regexp.MustCompile(`^for\s*\(\s*var\s+(\w+)\s*=\s*([^;]+);\s*([^;]+);\s*([^\)]+)\)\s*\{$`)
	dartVarDecl   = regexp.MustCompile(`^(?:var|dynamic)\s+(\w+)\s*=\s*(.+);$`)
	dartAssign    = regexp.MustCompile(`^(\w+)\s*=\s*(.+);$`)
	dartIf        = regexp.MustCompile(`^if\s*\((.+)\)\s*\{$`)
	dartElseIf    = regexp.MustCompile(`^else\s+if\s*\((.+)\)\s*\{$`)
	dartReturn    = regexp.MustCompile(`^return\s+(.+);$`)
	dartPrint     = regexp.MustCompile(`^print\((.+)\);$`)
)

// ConvertDart converts dart source code to Mochi.
func ConvertDart(src string) ([]byte, error) {
	ls := Servers["dart"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		// fall back to simple parser on failure
		if out, err2 := convertDartSimple(src); err2 == nil {
			return out, nil
		}
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		snippet := extractRange(src, s.Range)
		if snippet == "" {
			continue
		}
		fnOut, err := convertDartSimple(snippet)
		if err != nil {
			// best effort using just the symbol information
			out.WriteString("fun ")
			out.WriteString(s.Name)
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseDartDetail(detail)
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteByte(')')
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
			continue
		}
		out.Write(fnOut)
	}
	if out.Len() == 0 {
		if fallback, err2 := convertDartSimple(src); err2 == nil {
			return fallback, nil
		}
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertDartFile reads the dart file and converts it to Mochi.
func ConvertDartFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertDart(string(data))
}

func extractRange(src string, r protocol.Range) string {
	lines := strings.Split(src, "\n")
	if int(r.Start.Line) >= len(lines) || int(r.End.Line) >= len(lines) {
		return ""
	}
	var out strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line); i++ {
		line := lines[i]
		if i == int(r.Start.Line) && int(r.Start.Character) < len(line) {
			line = line[r.Start.Character:]
		}
		if i == int(r.End.Line) && int(r.End.Character) < len(line) {
			line = line[:r.End.Character]
		}
		out.WriteString(line)
		if i < int(r.End.Line) {
			out.WriteByte('\n')
		}
	}
	return out.String()
}

func convertDartSimple(src string) ([]byte, error) {
	scanner := bufio.NewScanner(strings.NewReader(src))
	var out strings.Builder
	indent := 0
	found := false
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "//") || strings.HasPrefix(line, "import ") {
			continue
		}
		if m := dartFuncStart.FindStringSubmatch(line); m != nil {
			found = true
			name := m[1]
			params := parseParams(m[2])
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("fun " + name + "(")
			out.WriteString(strings.Join(params, ", "))
			out.WriteString(") {\n")
			indent++
			continue
		}
		if dartMainStart.MatchString(line) {
			found = true
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("fun main() {\n")
			indent++
			continue
		}
		if line == "}" {
			indent--
			if indent < 0 {
				indent = 0
			}
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("}\n")
			continue
		}
		if m := dartForLoop.FindStringSubmatch(line); m != nil {
			found = true
			name := m[1]
			start := strings.TrimSpace(m[2])
			cond := strings.TrimSpace(m[3])
			if strings.HasPrefix(cond, name) {
				cond = strings.TrimSpace(cond[len(name):])
			}
			end := strings.TrimSpace(strings.TrimLeft(strings.TrimLeft(cond, "<"), "="))
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("for " + name + " in " + start + ".." + end + " {\n")
			indent++
			continue
		}
		if m := dartVarDecl.FindStringSubmatch(line); m != nil {
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("var " + m[1] + " = " + m[2] + "\n")
			continue
		}
		if m := dartAssign.FindStringSubmatch(line); m != nil {
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString(m[1] + " = " + m[2] + "\n")
			continue
		}
		if m := dartIf.FindStringSubmatch(line); m != nil {
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("if " + m[1] + " {\n")
			indent++
			continue
		}
		if m := dartElseIf.FindStringSubmatch(line); m != nil {
			indent--
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("} else if " + m[1] + " {\n")
			indent++
			continue
		}
		if line == "else {" {
			indent--
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("} else {\n")
			indent++
			continue
		}
		if m := dartReturn.FindStringSubmatch(line); m != nil {
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("return " + m[1] + "\n")
			continue
		}
		if m := dartPrint.FindStringSubmatch(line); m != nil {
			out.WriteString(strings.Repeat("  ", indent))
			out.WriteString("print(" + m[1] + ")\n")
			continue
		}
		// unrecognized line, comment it
		out.WriteString(strings.Repeat("  ", indent))
		out.WriteString("// " + line + "\n")
	}
	if !found {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func parseDartDetail(detail string) ([]string, string) {
	open := strings.Index(detail, "(")
	close := strings.LastIndex(detail, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := detail[open+1 : close]
	retPart := strings.TrimSpace(detail[close+1:])
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.FieldsFunc(p, func(r rune) bool { return unicode.IsSpace(r) || r == '=' })
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		params = append(params, name)
	}
	return params, retPart
}

func parseParams(list string) []string {
	if strings.TrimSpace(list) == "" {
		return nil
	}
	parts := strings.Split(list, ",")
	params := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.FieldsFunc(p, func(r rune) bool { return unicode.IsSpace(r) || r == '=' })
		if len(fields) == 0 {
			continue
		}
		params = append(params, fields[len(fields)-1])
	}
	return params
}

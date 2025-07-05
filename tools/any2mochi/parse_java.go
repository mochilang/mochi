package any2mochi

import (
	"fmt"
	"regexp"
	"strings"
)

var (
	javaPackageRE      = regexp.MustCompile(`^package\s+`)
	javaStructRE       = regexp.MustCompile(`^(?:public|private|protected)?\s*static\s+class\s+(\w+)\s*\{`)
	javaFieldRE        = regexp.MustCompile(`^(?:public|private|protected)?\s*([A-Za-z0-9_<>]+)\s+(\w+);`)
	javaFuncRE         = regexp.MustCompile(`^(?:public|private|protected)?\s*static\s+[A-Za-z0-9_<>]+\s+(\w+)\(([^)]*)\)\s*\{`)
	javaMainRE         = regexp.MustCompile(`^public\s+static\s+void\s+main\s*\(`)
	javaIfRE           = regexp.MustCompile(`^if\s*\((.+)\)\s*\{`)
	javaElseIfRE       = regexp.MustCompile(`^else\s+if\s*\((.+)\)\s*\{`)
	javaElseRE         = regexp.MustCompile(`^else\s*\{`)
	javaWhileRE        = regexp.MustCompile(`^while\s*\((.+)\)\s*\{`)
	javaForEachRE      = regexp.MustCompile(`^for\s*\((?:var\s+)?(\w+)\s*:\s*(.+)\)\s*\{`)
	javaForRangeRE     = regexp.MustCompile(`^for\s*\(int\s+(\w+)\s*=\s*([^;]+);\s*\1\s*([<>]=?)\s*([^;]+);\s*\1\+\+\)\s*\{`)
	javaPrintRE        = regexp.MustCompile(`^System\.out\.println\((.+)\);`)
	javaReturnRE       = regexp.MustCompile(`^return\s+(.+);`)
	javaVarAssignRE    = regexp.MustCompile(`^(?:var|int|double|float|String|boolean|Map<[^>]+>|List<[^>]+>|[A-Za-z_][A-Za-z0-9_<>]*)\s+(\w+)\s*=\s*(.+);`)
	javaSimpleAssignRE = regexp.MustCompile(`^(\w+(?:\[[^\]]+\])?)\s*=\s*(.+);`)
	javaMapPutRE       = regexp.MustCompile(`^(\w+)\.put\(([^,]+),\s*(.+)\);`)
)

// parseJava converts a limited subset of Java source code to Mochi statements.
// It is not a full parser but is sufficient for the compiler generated output.
func parseJava(src string) ([]string, error) {
	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	var out []string
	indent := 0
	structFields := map[string][]string{}
	currentStruct := ""

	write := func(s string) {
		if indent < 0 {
			indent = 0
		}
		out = append(out, strings.Repeat("  ", indent)+s)
	}

	for _, l := range lines {
		line := strings.TrimSpace(l)
		if line == "" || strings.HasPrefix(line, "//") {
			continue
		}
		if javaPackageRE.MatchString(line) {
			continue
		}
		if m := javaStructRE.FindStringSubmatch(line); m != nil {
			currentStruct = m[1]
			structFields[currentStruct] = []string{}
			write("type " + currentStruct + " {")
			indent++
			continue
		}
		if currentStruct != "" && line == "}" {
			indent--
			write("}")
			currentStruct = ""
			continue
		}
		if currentStruct != "" {
			if m := javaFieldRE.FindStringSubmatch(line); m != nil {
				typ := mapJavaType(m[1])
				structFields[currentStruct] = append(structFields[currentStruct], m[2])
				write(m[2] + ": " + typ)
			}
			continue
		}
		if m := javaFuncRE.FindStringSubmatch(line); m != nil {
			name := m[1]
			params := strings.TrimSpace(m[2])
			var names []string
			if params != "" {
				for _, p := range strings.Split(params, ",") {
					fields := strings.Fields(strings.TrimSpace(p))
					if len(fields) > 0 {
						names = append(names, fields[len(fields)-1])
					}
				}
			}
			write("fun " + name + "(" + strings.Join(names, ", ") + ") {")
			indent++
			continue
		}
		if javaMainRE.MatchString(line) {
			write("fun main() {")
			indent++
			continue
		}
		if line == "}" || line == "};" {
			if indent > 0 {
				indent--
			}
			write("}")
			continue
		}
		if m := javaIfRE.FindStringSubmatch(line); m != nil {
			write("if " + convertJavaExpr(m[1], structFields) + " {")
			indent++
			continue
		}
		if m := javaElseIfRE.FindStringSubmatch(line); m != nil {
			if indent > 0 {
				indent--
			}
			write("else if " + convertJavaExpr(m[1], structFields) + " {")
			indent++
			continue
		}
		if javaElseRE.MatchString(line) {
			if indent > 0 {
				indent--
			}
			write("else {")
			indent++
			continue
		}
		if m := javaWhileRE.FindStringSubmatch(line); m != nil {
			write("while " + convertJavaExpr(m[1], structFields) + " {")
			indent++
			continue
		}
		if m := javaForEachRE.FindStringSubmatch(line); m != nil {
			write("for " + m[1] + " in " + convertJavaExpr(m[2], structFields) + " {")
			indent++
			continue
		}
		if m := javaForRangeRE.FindStringSubmatch(line); m != nil {
			start := convertJavaExpr(m[2], structFields)
			end := convertJavaExpr(m[4], structFields)
			if m[3] == "<=" {
				end = end + "+1"
			}
			write("for " + m[1] + " in " + start + ".." + end + " {")
			indent++
			continue
		}
		if m := javaPrintRE.FindStringSubmatch(line); m != nil {
			write("print(" + convertJavaExpr(m[1], structFields) + ")")
			continue
		}
		if m := javaReturnRE.FindStringSubmatch(line); m != nil {
			write("return " + convertJavaExpr(m[1], structFields))
			continue
		}
		if line == "break" || line == "break;" {
			write("break")
			continue
		}
		if line == "continue" || line == "continue;" {
			write("continue")
			continue
		}
		if m := javaVarAssignRE.FindStringSubmatch(line); m != nil {
			write("var " + m[1] + " = " + convertJavaExpr(m[2], structFields))
			continue
		}
		if m := javaMapPutRE.FindStringSubmatch(line); m != nil {
			write(m[1] + "[" + convertJavaExpr(m[2], structFields) + "] = " + convertJavaExpr(m[3], structFields))
			continue
		}
		if m := javaSimpleAssignRE.FindStringSubmatch(line); m != nil {
			write(m[1] + " = " + convertJavaExpr(m[2], structFields))
			continue
		}
		if strings.Contains(line, "new java.util.function.Supplier") {
			return nil, fmt.Errorf("unsupported line: %s", line)
		}
		return nil, fmt.Errorf("unsupported line: %s", line)
	}

	return out, nil
}

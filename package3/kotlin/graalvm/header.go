package graalvm

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// EntryPoint describes a single C entry point exposed by the GraalVM native image.
type EntryPoint struct {
	Name       string   // C function name (e.g. "okhttp_client_new")
	ReturnType string   // C return type (e.g. "long", "jobject", "void")
	Params     []string // C parameter types (excluding the isolate thread param)
}

// ParseHeader parses a GraalVM-generated C header (.h) file and returns the entry points.
// It parses lines of the form:
//
//	extern <rettype> <name>(graal_isolatethread_t*, <params...>);
func ParseHeader(headerPath string) ([]EntryPoint, error) {
	f, err := os.Open(headerPath)
	if err != nil {
		return nil, fmt.Errorf("graalvm: open header %q: %w", headerPath, err)
	}
	defer f.Close()

	var entries []EntryPoint
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		ep, ok := parseExternLine(line)
		if ok {
			entries = append(entries, ep)
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("graalvm: scan header %q: %w", headerPath, err)
	}
	return entries, nil
}

// parseExternLine attempts to parse a single line as an extern C function declaration.
// Returns (EntryPoint, true) on success, (zero, false) otherwise.
func parseExternLine(line string) (EntryPoint, bool) {
	// We expect: extern <rettype> <name>(<params>);
	if !strings.HasPrefix(line, "extern ") {
		return EntryPoint{}, false
	}
	// Strip trailing semicolon
	line = strings.TrimSuffix(line, ";")
	line = strings.TrimSpace(line)

	// Strip "extern "
	line = strings.TrimPrefix(line, "extern ")
	line = strings.TrimSpace(line)

	// Find opening paren
	parenIdx := strings.Index(line, "(")
	if parenIdx < 0 {
		return EntryPoint{}, false
	}

	// Everything before '(' is "<rettype> <name>"
	beforeParen := strings.TrimSpace(line[:parenIdx])
	// Find the last space in beforeParen to split rettype and name
	lastSpace := strings.LastIndex(beforeParen, " ")
	if lastSpace < 0 {
		return EntryPoint{}, false
	}
	retType := strings.TrimSpace(beforeParen[:lastSpace])
	name := strings.TrimSpace(beforeParen[lastSpace+1:])

	// Handle pointer types: e.g. "void*" or "char *"
	// The name could have a leading '*' if return type is a pointer
	if strings.HasSuffix(retType, "*") || strings.HasPrefix(name, "*") {
		// Already split correctly for "void *name" or "void* name"
		retType = strings.TrimSpace(retType + strings.TrimLeft(name, "* "))
		// This case is tricky; just use the raw split.
	}

	// Extract params from between parens
	closeIdx := strings.LastIndex(line, ")")
	if closeIdx < parenIdx {
		return EntryPoint{}, false
	}
	paramStr := strings.TrimSpace(line[parenIdx+1 : closeIdx])

	// Split params by comma
	var params []string
	if paramStr != "" {
		rawParams := splitParams(paramStr)
		// The first param should be graal_isolatethread_t* — skip it
		for i, p := range rawParams {
			p = strings.TrimSpace(p)
			if i == 0 && strings.Contains(p, "graal_isolatethread_t") {
				continue
			}
			// Extract just the type (strip parameter name if present)
			params = append(params, extractParamType(p))
		}
	}

	return EntryPoint{
		Name:       name,
		ReturnType: retType,
		Params:     params,
	}, true
}

// splitParams splits a parameter list by comma, respecting nested parentheses.
func splitParams(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, ch := range s {
		switch ch {
		case '(':
			depth++
		case ')':
			depth--
		case ',':
			if depth == 0 {
				parts = append(parts, s[start:i])
				start = i + 1
			}
		}
	}
	parts = append(parts, s[start:])
	return parts
}

// extractParamType extracts the type from a C parameter declaration like "int count" or "long*".
func extractParamType(param string) string {
	param = strings.TrimSpace(param)
	// If there's a space, the last token is the name, everything before is the type.
	// But we need to be careful about pointer types.
	lastSpace := strings.LastIndex(param, " ")
	if lastSpace < 0 {
		return param // just a type, no name
	}
	// Check if the "name" part starts with * — then it's part of the type
	possibleName := strings.TrimSpace(param[lastSpace+1:])
	if strings.HasPrefix(possibleName, "*") {
		return param // e.g. "void *p" -> return "void *p" as-is
	}
	// Check if possibleName is an identifier (doesn't contain *)
	if !strings.ContainsAny(possibleName, "*[]") {
		return strings.TrimSpace(param[:lastSpace])
	}
	return param
}

// Package swiftinterface parses Swift .swiftinterface files to extract the
// public API surface of a Swift module. It handles public func declarations,
// type declarations (struct, class, enum, protocol), extensions, async/throws
// modifiers, and @available attributes.
package swiftinterface

import (
	"fmt"
	"os"
	"strings"
)

// Param is one parameter in a function declaration.
type Param struct {
	// Label is the external argument label (may be "_" for unlabelled).
	Label string
	// Name is the internal parameter name.
	Name string
	// Type is the Swift type string.
	Type string
}

// FuncDecl is a parsed public function declaration.
type FuncDecl struct {
	Name        string
	Params      []Param
	ReturnType  string
	IsAsync     bool
	IsThrowing  bool
	IsPublic    bool
	Attributes  []string
}

// TypeDecl is a parsed public type declaration.
type TypeDecl struct {
	Name     string
	Kind     string // "struct" | "class" | "enum" | "protocol"
	IsPublic bool
}

// Extension is a parsed extension block.
type Extension struct {
	TypeName  string
	Functions []FuncDecl
}

// ApiSurface is the complete parsed API from a .swiftinterface file.
type ApiSurface struct {
	ModuleName   string
	SwiftVersion string
	Functions    []FuncDecl
	Types        []TypeDecl
	Extensions   []Extension
}

// ParseFile parses a .swiftinterface file at the given path.
func ParseFile(path string) (*ApiSurface, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("swiftinterface: read %s: %w", path, err)
	}
	return ParseString(string(data))
}

// ParseString parses .swiftinterface source text.
func ParseString(src string) (*ApiSurface, error) {
	p := &parser{lines: splitLines(src)}
	return p.parse()
}

// parser holds the parsing state.
type parser struct {
	lines      []string
	pos        int
	pendingAttrs []string
}

func (p *parser) peek() (string, bool) {
	if p.pos >= len(p.lines) {
		return "", false
	}
	return p.lines[p.pos], true
}

func (p *parser) next() (string, bool) {
	line, ok := p.peek()
	if ok {
		p.pos++
	}
	return line, ok
}

func (p *parser) parse() (*ApiSurface, error) {
	surf := &ApiSurface{}

	// Parse header comments for module name and swift version
	for p.pos < len(p.lines) {
		line := strings.TrimSpace(p.lines[p.pos])
		if !strings.HasPrefix(line, "//") {
			break
		}
		p.pos++
		if after, ok := strings.CutPrefix(line, "// swift-interface-format-version:"); ok {
			_ = after // format version not stored
			continue
		}
		if after, ok := strings.CutPrefix(line, "// swift-compiler-version:"); ok {
			_ = after
			continue
		}
		if after, ok := strings.CutPrefix(line, "// swift-module-flags:"); ok {
			// Extract -module-name flag
			parts := strings.Fields(after)
			for i, part := range parts {
				if part == "-module-name" && i+1 < len(parts) {
					surf.ModuleName = parts[i+1]
				}
			}
			continue
		}
	}

	// Parse declarations
	for p.pos < len(p.lines) {
		line, ok := p.next()
		if !ok {
			break
		}
		trimmed := strings.TrimSpace(line)
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}
		if strings.HasPrefix(trimmed, "import ") {
			continue
		}

		// @available and other attributes
		if strings.HasPrefix(trimmed, "@") {
			p.pendingAttrs = append(p.pendingAttrs, trimmed)
			continue
		}

		// Extension
		if strings.HasPrefix(trimmed, "extension ") {
			ext, err := p.parseExtension(trimmed)
			if err != nil {
				return nil, err
			}
			surf.Extensions = append(surf.Extensions, ext)
			p.pendingAttrs = nil
			continue
		}

		// Type declarations
		if kind, name, ok := parseTypeDecl(trimmed); ok {
			surf.Types = append(surf.Types, TypeDecl{
				Name:     name,
				Kind:     kind,
				IsPublic: strings.Contains(trimmed, "public "),
			})
			p.pendingAttrs = nil
			// Skip body
			p.skipBlock(trimmed)
			continue
		}

		// Function declarations
		if fn, ok := p.parseFuncDecl(trimmed); ok {
			surf.Functions = append(surf.Functions, fn)
			p.pendingAttrs = nil
			continue
		}

		p.pendingAttrs = nil
	}

	return surf, nil
}

// skipBlock skips over a { ... } block if the line opens one.
func (p *parser) skipBlock(openLine string) {
	if !strings.Contains(openLine, "{") {
		return
	}
	depth := strings.Count(openLine, "{") - strings.Count(openLine, "}")
	for depth > 0 && p.pos < len(p.lines) {
		line, ok := p.next()
		if !ok {
			break
		}
		depth += strings.Count(line, "{") - strings.Count(line, "}")
	}
}

// parseExtension parses an extension block and returns its functions.
func (p *parser) parseExtension(line string) (Extension, error) {
	// "extension TypeName { ... }" or "extension TypeName :"
	after := strings.TrimPrefix(line, "extension ")
	typeName := strings.Fields(after)[0]
	typeName = strings.TrimSuffix(typeName, "{")
	typeName = strings.TrimSuffix(typeName, ":")

	ext := Extension{TypeName: typeName}

	if !strings.Contains(line, "{") {
		return ext, nil
	}

	depth := strings.Count(line, "{") - strings.Count(line, "}")
	var attrs []string
	for depth > 0 && p.pos < len(p.lines) {
		inner, ok := p.next()
		if !ok {
			break
		}
		trimmed := strings.TrimSpace(inner)
		depth += strings.Count(inner, "{") - strings.Count(inner, "}")

		if strings.HasPrefix(trimmed, "@") {
			attrs = append(attrs, trimmed)
			continue
		}

		if fn, ok2 := p.parseFuncDeclWithAttrs(trimmed, attrs); ok2 {
			ext.Functions = append(ext.Functions, fn)
			attrs = nil
			continue
		}
		attrs = nil
	}
	return ext, nil
}

// parseTypeDecl checks if line is a type declaration. Returns (kind, name, true)
// or ("", "", false).
func parseTypeDecl(line string) (kind, name string, ok bool) {
	for _, kw := range []string{"public struct ", "public class ", "public enum ", "public protocol ",
		"struct ", "class ", "enum ", "protocol "} {
		if after, cut := strings.CutPrefix(line, kw); cut {
			name = strings.Fields(after)[0]
			name = strings.TrimSuffix(name, "{")
			name = strings.TrimSuffix(name, ":")
			kind = strings.TrimSuffix(strings.TrimPrefix(kw, "public "), " ")
			kind = strings.TrimSpace(kind)
			return kind, name, true
		}
	}
	return "", "", false
}

// parseFuncDecl attempts to parse a public function declaration from line.
func (p *parser) parseFuncDecl(line string) (FuncDecl, bool) {
	return p.parseFuncDeclWithAttrs(line, p.pendingAttrs)
}

func (p *parser) parseFuncDeclWithAttrs(line string, attrs []string) (FuncDecl, bool) {
	isPublic := false
	rest := line

	if after, ok := strings.CutPrefix(rest, "public "); ok {
		isPublic = true
		rest = after
	} else if after, ok := strings.CutPrefix(rest, "open "); ok {
		isPublic = true
		rest = after
	}

	isAsync := false
	isThrows := false

	if !strings.HasPrefix(rest, "func ") {
		// Check for static/class/mutating modifiers
		for _, mod := range []string{"static ", "class ", "mutating ", "override ", "final "} {
			if after, ok := strings.CutPrefix(rest, mod); ok {
				rest = after
			}
		}
		if !strings.HasPrefix(rest, "func ") {
			return FuncDecl{}, false
		}
	}

	rest = strings.TrimPrefix(rest, "func ")

	// Extract function name
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx < 0 {
		return FuncDecl{}, false
	}
	name := strings.TrimSpace(rest[:parenIdx])

	// Find matching closing paren
	paramsStr, afterParams := extractParens(rest[parenIdx:])
	if paramsStr == "" && afterParams == "" {
		return FuncDecl{}, false
	}

	// Parse modifiers after params: async throws -> RetType
	afterParams = strings.TrimSpace(afterParams)
	if after, ok := strings.CutPrefix(afterParams, "async"); ok {
		isAsync = true
		afterParams = strings.TrimSpace(after)
	}
	if after, ok := strings.CutPrefix(afterParams, "throws"); ok {
		isThrows = true
		afterParams = strings.TrimSpace(after)
	}
	// Skip rethrows too
	if after, ok := strings.CutPrefix(afterParams, "rethrows"); ok {
		isThrows = true
		afterParams = strings.TrimSpace(after)
	}

	returnType := ""
	if after, ok := strings.CutPrefix(afterParams, "->"); ok {
		// Return type is everything up to { or end of line
		rt := strings.TrimSpace(after)
		if brace := strings.IndexByte(rt, '{'); brace >= 0 {
			rt = strings.TrimSpace(rt[:brace])
		}
		returnType = rt
	}

	params := parseParams(paramsStr)

	attrsCopy := make([]string, len(attrs))
	copy(attrsCopy, attrs)

	return FuncDecl{
		Name:       name,
		Params:     params,
		ReturnType: returnType,
		IsAsync:    isAsync,
		IsThrowing: isThrows,
		IsPublic:   isPublic,
		Attributes: attrsCopy,
	}, true
}

// extractParens extracts the content inside the first set of matching parens
// starting at s (which must start with '('). Returns (inner, rest).
func extractParens(s string) (inner, rest string) {
	if len(s) == 0 || s[0] != '(' {
		return "", ""
	}
	depth := 0
	for i, c := range s {
		switch c {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				return s[1:i], s[i+1:]
			}
		}
	}
	return "", ""
}

// parseParams parses a comma-separated parameter list string.
// Handles "label name: Type" and "_ name: Type" forms.
func parseParams(s string) []Param {
	s = strings.TrimSpace(s)
	if s == "" {
		return nil
	}
	parts := splitParams(s)
	var params []Param
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		p := parseOneParam(part)
		params = append(params, p)
	}
	return params
}

// splitParams splits a param list on top-level commas.
func splitParams(s string) []string {
	var out []string
	depth := 0
	last := 0
	for i, c := range s {
		switch c {
		case '(', '[', '<':
			depth++
		case ')', ']', '>':
			depth--
		case ',':
			if depth == 0 {
				out = append(out, s[last:i])
				last = i + 1
			}
		}
	}
	out = append(out, s[last:])
	return out
}

// parseOneParam parses a single parameter "label name: Type" or "name: Type".
func parseOneParam(s string) Param {
	// Find the colon separating name from type
	colonIdx := strings.IndexByte(s, ':')
	if colonIdx < 0 {
		return Param{Name: s}
	}
	namesPart := strings.TrimSpace(s[:colonIdx])
	typePart := strings.TrimSpace(s[colonIdx+1:])

	words := strings.Fields(namesPart)
	switch len(words) {
	case 0:
		return Param{Type: typePart}
	case 1:
		return Param{Label: words[0], Name: words[0], Type: typePart}
	default:
		return Param{Label: words[0], Name: words[1], Type: typePart}
	}
}

// splitLines splits src into individual lines.
func splitLines(src string) []string {
	return strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
}

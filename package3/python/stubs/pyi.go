package stubs

import (
	"fmt"
	"strings"
	"unicode"
)

// ModuleSurface is the structured view of a .pyi file that phase 4 (type
// mapping) and phase 5 (wrapper synthesiser) read.
type ModuleSurface struct {
	// Imports is every top-level `import X` and `from X import Y` parsed
	// from the module.
	Imports []ImportDecl
	// Classes is every top-level `class X(...)` declaration.
	Classes []ClassDecl
	// Functions is every top-level `def X(...) -> T:` declaration. Methods
	// are stored inside the owning ClassDecl.Methods.
	Functions []FuncDecl
	// Aliases is every top-level `Name = Expr` where Expr is treated as a
	// type expression (Phase 4 may reclassify into TypeVar / value).
	Aliases []AliasDecl
	// Constants is every top-level `Name: Type` declaration with no body or
	// with a value body. The value is preserved as a raw string.
	Constants []ConstantDecl
}

// ImportDecl is `import X` or `from X import (Y, Z as W)`.
type ImportDecl struct {
	// Module is the source module ("typing" in `from typing import Any`).
	// Empty for `import X` (in that case Names has one entry holding X).
	Module string
	// Names is the imported names, possibly with aliases.
	Names []ImportedName
}

// ImportedName is `Y` or `Y as W` in an import list.
type ImportedName struct {
	Name  string
	Alias string
}

// ClassDecl is `class Name(Bases): <body>`.
type ClassDecl struct {
	Name      string
	Bases     []string // raw base expressions, e.g. "Protocol", "TypedDict", "Generic[T]".
	Methods   []FuncDecl
	Fields    []FieldDecl
	IsTypedDict bool
	IsProtocol  bool
	IsDataclass bool
	Decorators  []string
}

// FuncDecl is `def Name(Params) -> Return:`.
type FuncDecl struct {
	Name       string
	Params     []ParamDecl
	ReturnType string // raw expression, empty when omitted
	Decorators []string
	IsAsync    bool
}

// ParamDecl is one parameter in a FuncDecl. Kind tracks the four PEP 3102 /
// PEP 570 categories.
type ParamDecl struct {
	Name      string
	Type      string // raw expression, empty when no annotation
	Default   string // raw default expression, empty when no default
	Kind      ParamKind
}

// ParamKind is the parameter category.
type ParamKind int

const (
	ParamPositional ParamKind = iota // `x` (positional or keyword)
	ParamPositionalOnly                // before `/`
	ParamKeywordOnly                   // after `*` separator
	ParamVarArgs                       // `*args`
	ParamKwArgs                        // `**kwargs`
)

// FieldDecl is `name: Type` or `name: Type = default` inside a class body.
type FieldDecl struct {
	Name    string
	Type    string
	Default string
}

// AliasDecl is `Name = Expr` at module scope.
type AliasDecl struct {
	Name string
	Expr string
}

// ConstantDecl is `Name: Type` or `Name: Type = value` at module scope.
type ConstantDecl struct {
	Name    string
	Type    string
	Default string
}

// ParsePYI reads the surface from a .pyi document. The parser is intentionally
// scoped to the constructs phase 4 + 5 consume; anything else is silently
// skipped with no error so we degrade gracefully on exotic stubs.
func ParsePYI(src string) (*ModuleSurface, error) {
	lines, err := logicalLines(src)
	if err != nil {
		return nil, err
	}
	m := &ModuleSurface{}
	pendingDecorators := []string(nil)
	for i := 0; i < len(lines); i++ {
		l := lines[i]
		if l.indent != 0 {
			continue // body content; handled by class parser
		}
		text := strings.TrimSpace(l.text)
		switch {
		case text == "" || strings.HasPrefix(text, "#"):
			continue
		case strings.HasPrefix(text, "@"):
			pendingDecorators = append(pendingDecorators, strings.TrimSpace(text[1:]))
		case strings.HasPrefix(text, "import "):
			imp := parseImport(text)
			m.Imports = append(m.Imports, imp)
			pendingDecorators = nil
		case strings.HasPrefix(text, "from "):
			imp, err := parseFromImport(text)
			if err != nil {
				return nil, fmt.Errorf("pyi: line %d: %w", l.line, err)
			}
			m.Imports = append(m.Imports, imp)
			pendingDecorators = nil
		case strings.HasPrefix(text, "class "):
			cd, consumed, err := parseClass(lines, i, pendingDecorators)
			if err != nil {
				return nil, fmt.Errorf("pyi: line %d: %w", l.line, err)
			}
			m.Classes = append(m.Classes, cd)
			i += consumed
			pendingDecorators = nil
		case strings.HasPrefix(text, "def ") || strings.HasPrefix(text, "async def "):
			fd, err := parseFunction(text, pendingDecorators)
			if err != nil {
				return nil, fmt.Errorf("pyi: line %d: %w", l.line, err)
			}
			m.Functions = append(m.Functions, fd)
			pendingDecorators = nil
		default:
			// Module-level `Name: Type [= value]` or `Name = value` (alias).
			if name, typ, def, ok := splitAnnotation(text); ok {
				m.Constants = append(m.Constants, ConstantDecl{Name: name, Type: typ, Default: def})
				pendingDecorators = nil
				continue
			}
			if name, expr, ok := splitAlias(text); ok {
				m.Aliases = append(m.Aliases, AliasDecl{Name: name, Expr: expr})
				pendingDecorators = nil
				continue
			}
			// Unknown: silently skip per the doc.
			pendingDecorators = nil
		}
	}
	return m, nil
}

type pyiLine struct {
	indent int
	text   string
	line   int // 1-based source line for the start of the logical line
}

// logicalLines collapses Python's physical lines into logical lines, joining
// backslash continuations and parenthesised continuations.
func logicalLines(src string) ([]pyiLine, error) {
	var out []pyiLine
	physical := strings.Split(src, "\n")
	var buf strings.Builder
	var startLine int
	startIndent := -1
	depth := 0
	flush := func() {
		t := buf.String()
		buf.Reset()
		if strings.TrimSpace(t) == "" {
			startIndent = -1
			return
		}
		out = append(out, pyiLine{indent: startIndent, text: t, line: startLine})
		startIndent = -1
	}
	for i, raw := range physical {
		line := i + 1
		// Drop the BOM on the very first physical line.
		if i == 0 {
			raw = strings.TrimPrefix(raw, "\ufeff")
		}
		// Compute indent for the start of a new logical line.
		if startIndent == -1 {
			indent := 0
			for j := 0; j < len(raw); j++ {
				if raw[j] == ' ' {
					indent++
				} else if raw[j] == '\t' {
					indent += 8 - (indent % 8)
				} else {
					break
				}
			}
			startIndent = indent
			startLine = line
		}
		// Strip comments outside of strings.
		stripped := stripPyComment(raw)
		// Track bracket depth.
		depth += bracketDelta(stripped)
		if depth < 0 {
			return nil, fmt.Errorf("pyi: line %d: unbalanced brackets", line)
		}
		// Backslash continuation.
		trimRight := strings.TrimRight(stripped, " \t")
		cont := strings.HasSuffix(trimRight, "\\")
		if cont {
			trimRight = strings.TrimSuffix(trimRight, "\\")
		}
		buf.WriteString(trimRight)
		buf.WriteByte(' ')
		if cont || depth > 0 {
			continue
		}
		flush()
	}
	if buf.Len() > 0 {
		flush()
	}
	return out, nil
}

// stripPyComment removes `#`-comments outside of single / double / triple
// quoted strings. .pyi files routinely contain string literals (e.g. in
// `Literal["a", "b"]` and default values) so we cannot naively split on `#`.
func stripPyComment(s string) string {
	out := make([]byte, 0, len(s))
	i := 0
	for i < len(s) {
		c := s[i]
		if c == '#' {
			return string(out)
		}
		if c == '"' || c == '\'' {
			// Detect triple quote.
			triple := i+2 < len(s) && s[i+1] == c && s[i+2] == c
			out = append(out, c)
			i++
			if triple {
				out = append(out, c, c)
				i += 2
				for i < len(s) {
					if i+2 < len(s) && s[i] == c && s[i+1] == c && s[i+2] == c {
						out = append(out, c, c, c)
						i += 3
						break
					}
					out = append(out, s[i])
					i++
				}
				continue
			}
			for i < len(s) {
				if s[i] == '\\' && i+1 < len(s) {
					out = append(out, s[i], s[i+1])
					i += 2
					continue
				}
				if s[i] == c {
					out = append(out, s[i])
					i++
					break
				}
				out = append(out, s[i])
				i++
			}
			continue
		}
		out = append(out, c)
		i++
	}
	return string(out)
}

// bracketDelta returns the net change in open bracket depth contributed by s,
// ignoring brackets inside strings. Strings are skipped because stripPyComment
// preserves them and we need to count brackets again here.
func bracketDelta(s string) int {
	d := 0
	i := 0
	for i < len(s) {
		c := s[i]
		if c == '"' || c == '\'' {
			triple := i+2 < len(s) && s[i+1] == c && s[i+2] == c
			i++
			if triple {
				i += 2
				for i < len(s) {
					if i+2 < len(s) && s[i] == c && s[i+1] == c && s[i+2] == c {
						i += 3
						break
					}
					i++
				}
				continue
			}
			for i < len(s) {
				if s[i] == '\\' && i+1 < len(s) {
					i += 2
					continue
				}
				if s[i] == c {
					i++
					break
				}
				i++
			}
			continue
		}
		switch c {
		case '(', '[', '{':
			d++
		case ')', ']', '}':
			d--
		}
		i++
	}
	return d
}

func parseImport(text string) ImportDecl {
	rest := strings.TrimPrefix(text, "import ")
	rest = strings.TrimSpace(rest)
	names := splitTopLevel(rest, ',')
	var out []ImportedName
	for _, n := range names {
		n = strings.TrimSpace(n)
		if idx := strings.Index(n, " as "); idx >= 0 {
			out = append(out, ImportedName{Name: strings.TrimSpace(n[:idx]), Alias: strings.TrimSpace(n[idx+4:])})
		} else {
			out = append(out, ImportedName{Name: n})
		}
	}
	return ImportDecl{Names: out}
}

func parseFromImport(text string) (ImportDecl, error) {
	rest := strings.TrimPrefix(text, "from ")
	idx := strings.Index(rest, " import ")
	if idx < 0 {
		return ImportDecl{}, fmt.Errorf("malformed from-import: %q", text)
	}
	module := strings.TrimSpace(rest[:idx])
	names := strings.TrimSpace(rest[idx+8:])
	names = strings.TrimPrefix(names, "(")
	names = strings.TrimSuffix(names, ")")
	parts := splitTopLevel(names, ',')
	var out []ImportedName
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if alias := strings.Index(p, " as "); alias >= 0 {
			out = append(out, ImportedName{Name: strings.TrimSpace(p[:alias]), Alias: strings.TrimSpace(p[alias+4:])})
		} else {
			out = append(out, ImportedName{Name: p})
		}
	}
	return ImportDecl{Module: module, Names: out}, nil
}

func parseClass(lines []pyiLine, start int, decorators []string) (ClassDecl, int, error) {
	header := strings.TrimSpace(lines[start].text)
	// strip leading "class "
	rest := strings.TrimPrefix(header, "class ")
	var name, basesRaw string
	colon := strings.LastIndex(rest, ":")
	if colon < 0 {
		return ClassDecl{}, 0, fmt.Errorf("class header missing ':' : %q", header)
	}
	left := strings.TrimSpace(rest[:colon])
	if paren := strings.Index(left, "("); paren >= 0 {
		name = strings.TrimSpace(left[:paren])
		close := strings.LastIndex(left, ")")
		if close < paren {
			return ClassDecl{}, 0, fmt.Errorf("class header missing ')' : %q", header)
		}
		basesRaw = strings.TrimSpace(left[paren+1 : close])
	} else {
		name = left
	}
	cd := ClassDecl{Name: name, Decorators: append([]string(nil), decorators...)}
	for _, b := range splitTopLevel(basesRaw, ',') {
		b = strings.TrimSpace(b)
		if b == "" {
			continue
		}
		cd.Bases = append(cd.Bases, b)
		if b == "Protocol" || strings.HasPrefix(b, "Protocol[") {
			cd.IsProtocol = true
		}
		if b == "TypedDict" || strings.HasPrefix(b, "TypedDict[") || strings.Contains(b, "TypedDict,") {
			cd.IsTypedDict = true
		}
	}
	for _, d := range cd.Decorators {
		if d == "dataclass" || strings.HasPrefix(d, "dataclass(") || strings.HasSuffix(d, ".dataclass") {
			cd.IsDataclass = true
		}
	}
	// Walk the body: every subsequent line with indent > 0 belongs to the class.
	consumed := 0
	pendingDecorators := []string(nil)
	for j := start + 1; j < len(lines); j++ {
		body := lines[j]
		if body.indent == 0 {
			break
		}
		consumed++
		bt := strings.TrimSpace(body.text)
		switch {
		case bt == "" || bt == "..." || bt == "pass":
			continue
		case strings.HasPrefix(bt, "@"):
			pendingDecorators = append(pendingDecorators, strings.TrimSpace(bt[1:]))
		case strings.HasPrefix(bt, "def ") || strings.HasPrefix(bt, "async def "):
			fd, err := parseFunction(bt, pendingDecorators)
			if err != nil {
				return cd, 0, err
			}
			cd.Methods = append(cd.Methods, fd)
			pendingDecorators = nil
		default:
			if name, typ, def, ok := splitAnnotation(bt); ok {
				cd.Fields = append(cd.Fields, FieldDecl{Name: name, Type: typ, Default: def})
			}
			pendingDecorators = nil
		}
	}
	return cd, consumed, nil
}

func parseFunction(text string, decorators []string) (FuncDecl, error) {
	fd := FuncDecl{Decorators: append([]string(nil), decorators...)}
	rest := text
	if strings.HasPrefix(rest, "async def ") {
		fd.IsAsync = true
		rest = strings.TrimPrefix(rest, "async def ")
	} else {
		rest = strings.TrimPrefix(rest, "def ")
	}
	openParen := strings.Index(rest, "(")
	if openParen < 0 {
		return fd, fmt.Errorf("function missing '(' : %q", text)
	}
	fd.Name = strings.TrimSpace(rest[:openParen])
	closeParen := matchingClose(rest, openParen, '(', ')')
	if closeParen < 0 {
		return fd, fmt.Errorf("function missing ')' : %q", text)
	}
	paramsRaw := rest[openParen+1 : closeParen]
	// After ')', look for `-> Return:` (or just `:` if no return annotation).
	tail := strings.TrimSpace(rest[closeParen+1:])
	if strings.HasPrefix(tail, "->") {
		tail = strings.TrimSpace(tail[2:])
		if colon := strings.LastIndex(tail, ":"); colon >= 0 {
			fd.ReturnType = strings.TrimSpace(tail[:colon])
		} else {
			fd.ReturnType = tail
		}
	}
	fd.Params = parseParams(paramsRaw)
	return fd, nil
}

func parseParams(raw string) []ParamDecl {
	if strings.TrimSpace(raw) == "" {
		return nil
	}
	parts := splitTopLevel(raw, ',')
	var out []ParamDecl
	keywordOnly := false
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if p == "/" {
			// Convert all preceding to positional-only.
			for i := range out {
				if out[i].Kind == ParamPositional {
					out[i].Kind = ParamPositionalOnly
				}
			}
			continue
		}
		if p == "*" {
			keywordOnly = true
			continue
		}
		pd := ParamDecl{Kind: ParamPositional}
		if keywordOnly {
			pd.Kind = ParamKeywordOnly
		}
		if strings.HasPrefix(p, "**") {
			pd.Kind = ParamKwArgs
			p = strings.TrimPrefix(p, "**")
			keywordOnly = true
		} else if strings.HasPrefix(p, "*") {
			pd.Kind = ParamVarArgs
			p = strings.TrimPrefix(p, "*")
			keywordOnly = true
		}
		// Split name : type = default.
		if eq := findTopLevelEq(p); eq >= 0 {
			pd.Default = strings.TrimSpace(p[eq+1:])
			p = strings.TrimSpace(p[:eq])
		}
		if colon := strings.Index(p, ":"); colon >= 0 {
			pd.Name = strings.TrimSpace(p[:colon])
			pd.Type = strings.TrimSpace(p[colon+1:])
		} else {
			pd.Name = strings.TrimSpace(p)
		}
		out = append(out, pd)
	}
	return out
}

func splitAnnotation(text string) (name, typ, def string, ok bool) {
	// `Name: Type` or `Name: Type = value`. Reject if there's no colon at
	// the top level or no `=`-but-no-colon-but-looks-like-identifier.
	colon := findTopLevelColon(text)
	if colon < 0 {
		return "", "", "", false
	}
	name = strings.TrimSpace(text[:colon])
	if !isPlainIdent(name) {
		return "", "", "", false
	}
	rest := text[colon+1:]
	if eq := findTopLevelEq(rest); eq >= 0 {
		typ = strings.TrimSpace(rest[:eq])
		def = strings.TrimSpace(rest[eq+1:])
		return name, typ, def, true
	}
	return name, strings.TrimSpace(rest), "", true
}

func splitAlias(text string) (name, expr string, ok bool) {
	// `Name = Expr` where Name is a plain identifier.
	eq := findTopLevelEq(text)
	if eq < 0 {
		return "", "", false
	}
	name = strings.TrimSpace(text[:eq])
	if !isPlainIdent(name) {
		return "", "", false
	}
	expr = strings.TrimSpace(text[eq+1:])
	return name, expr, true
}

func isPlainIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		if i == 0 && !(unicode.IsLetter(r) || r == '_') {
			return false
		}
		if !(unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_') {
			return false
		}
	}
	return true
}

func findTopLevelColon(s string) int {
	depth := 0
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch c {
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			depth--
		case ':':
			if depth == 0 {
				return i
			}
		case '"', '\'':
			// Skip string content.
			q := c
			j := i + 1
			for j < len(s) && s[j] != q {
				if s[j] == '\\' && j+1 < len(s) {
					j += 2
					continue
				}
				j++
			}
			i = j
		}
	}
	return -1
}

func findTopLevelEq(s string) int {
	depth := 0
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch c {
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			depth--
		case '=':
			if depth == 0 {
				// Reject `==`, `>=`, `<=`, `!=`.
				if i+1 < len(s) && s[i+1] == '=' {
					i++
					continue
				}
				if i > 0 && (s[i-1] == '>' || s[i-1] == '<' || s[i-1] == '!' || s[i-1] == '=') {
					continue
				}
				return i
			}
		case '"', '\'':
			q := c
			j := i + 1
			for j < len(s) && s[j] != q {
				if s[j] == '\\' && j+1 < len(s) {
					j += 2
					continue
				}
				j++
			}
			i = j
		}
	}
	return -1
}

// splitTopLevel splits s on `sep` ignoring separators inside brackets / quotes.
func splitTopLevel(s string, sep byte) []string {
	var out []string
	depth := 0
	start := 0
	i := 0
	for i < len(s) {
		c := s[i]
		switch c {
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			depth--
		case '"', '\'':
			q := c
			j := i + 1
			for j < len(s) && s[j] != q {
				if s[j] == '\\' && j+1 < len(s) {
					j += 2
					continue
				}
				j++
			}
			i = j
		case sep:
			if depth == 0 {
				out = append(out, s[start:i])
				start = i + 1
			}
		}
		i++
	}
	if start <= len(s) {
		out = append(out, s[start:])
	}
	return out
}

// matchingClose returns the index of the closing bracket matching the opener
// at `open` (which is open / close pair).
func matchingClose(s string, openIdx int, openCh, closeCh byte) int {
	depth := 0
	for i := openIdx; i < len(s); i++ {
		c := s[i]
		if c == '"' || c == '\'' {
			q := c
			j := i + 1
			for j < len(s) && s[j] != q {
				if s[j] == '\\' && j+1 < len(s) {
					j += 2
					continue
				}
				j++
			}
			i = j
			continue
		}
		switch c {
		case openCh:
			depth++
		case closeCh:
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

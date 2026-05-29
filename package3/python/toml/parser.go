package toml

import (
	"fmt"
	"strconv"
	"strings"
)

// Parse decodes a TOML document into a map tree. See the package doc for the
// supported subset.
func Parse(src string) (map[string]any, error) {
	p := &parser{src: src, pos: 0, line: 1, col: 1}
	return p.parseDocument()
}

type parser struct {
	src  string
	pos  int
	line int
	col  int
}

func (p *parser) errf(format string, args ...any) error {
	return fmt.Errorf("toml: line %d col %d: "+format, append([]any{p.line, p.col}, args...)...)
}

func (p *parser) eof() bool {
	return p.pos >= len(p.src)
}

func (p *parser) peek() byte {
	if p.eof() {
		return 0
	}
	return p.src[p.pos]
}

func (p *parser) advance() byte {
	if p.eof() {
		return 0
	}
	c := p.src[p.pos]
	p.pos++
	if c == '\n' {
		p.line++
		p.col = 1
	} else {
		p.col++
	}
	return c
}

func (p *parser) skipInlineWS() {
	for !p.eof() {
		c := p.peek()
		if c == ' ' || c == '\t' {
			p.advance()
			continue
		}
		break
	}
}

// skipWSAndComments skips whitespace, newlines, and # comments.
func (p *parser) skipWSAndComments() {
	for !p.eof() {
		c := p.peek()
		switch {
		case c == ' ' || c == '\t' || c == '\n' || c == '\r':
			p.advance()
		case c == '#':
			for !p.eof() && p.peek() != '\n' {
				p.advance()
			}
		default:
			return
		}
	}
}

// skipLineEnd consumes trailing whitespace, an optional comment, and a newline
// (or EOF). Returns an error if non-whitespace garbage is found.
func (p *parser) skipLineEnd() error {
	p.skipInlineWS()
	if p.eof() {
		return nil
	}
	if p.peek() == '#' {
		for !p.eof() && p.peek() != '\n' {
			p.advance()
		}
	}
	if p.eof() {
		return nil
	}
	if p.peek() == '\r' {
		p.advance()
	}
	if p.eof() {
		return nil
	}
	if p.peek() != '\n' {
		return p.errf("trailing garbage: %q", string(p.peek()))
	}
	p.advance()
	return nil
}

func (p *parser) parseDocument() (map[string]any, error) {
	root := map[string]any{}
	// Tracks the current target table for subsequent key/value lines.
	current := root
	for {
		p.skipWSAndComments()
		if p.eof() {
			return root, nil
		}
		c := p.peek()
		switch c {
		case '[':
			p.advance()
			isArray := false
			if p.peek() == '[' {
				p.advance()
				isArray = true
			}
			path, err := p.parseKeyPath()
			if err != nil {
				return nil, err
			}
			if isArray {
				if p.peek() != ']' {
					return nil, p.errf("expected ']]' end of array-of-tables header, got %q", string(p.peek()))
				}
				p.advance()
			}
			if p.peek() != ']' {
				return nil, p.errf("expected ']' end of table header, got %q", string(p.peek()))
			}
			p.advance()
			if err := p.skipLineEnd(); err != nil {
				return nil, err
			}
			if isArray {
				tbl := map[string]any{}
				if err := appendArrayOfTables(root, path, tbl); err != nil {
					return nil, p.errf("%v", err)
				}
				current = tbl
			} else {
				tbl, err := descendTable(root, path, true)
				if err != nil {
					return nil, p.errf("%v", err)
				}
				current = tbl
			}
		default:
			if err := p.parseKeyValueLine(current); err != nil {
				return nil, err
			}
		}
	}
}

func (p *parser) parseKeyPath() ([]string, error) {
	var parts []string
	for {
		p.skipInlineWS()
		k, err := p.parseKey()
		if err != nil {
			return nil, err
		}
		parts = append(parts, k)
		p.skipInlineWS()
		if p.peek() != '.' {
			return parts, nil
		}
		p.advance()
	}
}

func (p *parser) parseKey() (string, error) {
	c := p.peek()
	if c == '"' || c == '\'' {
		return p.parseString()
	}
	start := p.pos
	for !p.eof() {
		c := p.peek()
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '-' {
			p.advance()
			continue
		}
		break
	}
	if start == p.pos {
		return "", p.errf("expected key, got %q", string(p.peek()))
	}
	return p.src[start:p.pos], nil
}

func (p *parser) parseKeyValueLine(target map[string]any) error {
	startLine := p.line
	k, err := p.parseKey()
	if err != nil {
		return err
	}
	p.skipInlineWS()
	// Disallow dotted keys on LHS.
	if p.peek() == '.' {
		return p.errf("dotted keys on LHS of key/value not supported (got %q.)", k)
	}
	if p.peek() != '=' {
		return p.errf("expected '=' after key %q, got %q", k, string(p.peek()))
	}
	p.advance()
	p.skipInlineWS()
	val, err := p.parseValue()
	if err != nil {
		return err
	}
	if _, exists := target[k]; exists {
		return p.errf("duplicate key %q on line %d", k, startLine)
	}
	target[k] = val
	return p.skipLineEnd()
}

func (p *parser) parseValue() (any, error) {
	if p.eof() {
		return nil, p.errf("unexpected EOF in value")
	}
	c := p.peek()
	switch {
	case c == '"' || c == '\'':
		return p.parseString()
	case c == '[':
		return p.parseArray()
	case c == '{':
		return p.parseInlineTable()
	case c == 't' || c == 'f':
		return p.parseBool()
	case c == '-' || c == '+' || (c >= '0' && c <= '9'):
		return p.parseNumber()
	default:
		return nil, p.errf("unexpected character %q starting value", string(c))
	}
}

func (p *parser) parseString() (string, error) {
	quote := p.advance()
	if (quote != '"' && quote != '\'') {
		return "", p.errf("internal: parseString called on non-quote")
	}
	// Reject triple-quoted multiline strings.
	if p.peek() == quote {
		p.advance()
		if p.peek() == quote {
			return "", p.errf("multiline strings (\"\"\" / ''') not supported")
		}
		// Two-quote empty string: backtrack one and treat as empty.
		return "", nil
	}
	var b strings.Builder
	for {
		if p.eof() {
			return "", p.errf("unterminated string")
		}
		c := p.advance()
		if c == quote {
			return b.String(), nil
		}
		if c == '\n' {
			return "", p.errf("newline in string literal")
		}
		if quote == '"' && c == '\\' {
			if p.eof() {
				return "", p.errf("unterminated escape")
			}
			esc := p.advance()
			switch esc {
			case '"':
				b.WriteByte('"')
			case '\\':
				b.WriteByte('\\')
			case 'n':
				b.WriteByte('\n')
			case 't':
				b.WriteByte('\t')
			case 'r':
				b.WriteByte('\r')
			case '/':
				b.WriteByte('/')
			case 'b':
				b.WriteByte('\b')
			case 'f':
				b.WriteByte('\f')
			case 'u':
				if p.pos+4 > len(p.src) {
					return "", p.errf("bad \\u escape (need 4 hex digits)")
				}
				hex := p.src[p.pos : p.pos+4]
				for i := 0; i < 4; i++ {
					p.advance()
				}
				n, err := strconv.ParseUint(hex, 16, 32)
				if err != nil {
					return "", p.errf("bad \\u escape %q", hex)
				}
				b.WriteRune(rune(n))
			default:
				return "", p.errf("unknown escape \\%s", string(esc))
			}
			continue
		}
		b.WriteByte(c)
	}
}

func (p *parser) parseArray() (any, error) {
	if p.advance() != '[' {
		return nil, p.errf("internal: parseArray entry")
	}
	var items []any
	for {
		p.skipWSAndComments()
		if p.eof() {
			return nil, p.errf("unterminated array")
		}
		if p.peek() == ']' {
			p.advance()
			break
		}
		val, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		items = append(items, val)
		p.skipWSAndComments()
		if p.peek() == ',' {
			p.advance()
			continue
		}
		p.skipWSAndComments()
		if p.peek() == ']' {
			p.advance()
			break
		}
		return nil, p.errf("expected ',' or ']' in array, got %q", string(p.peek()))
	}
	// If every element is a map, return []map[string]any for callers.
	allMaps := len(items) > 0
	for _, it := range items {
		if _, ok := it.(map[string]any); !ok {
			allMaps = false
			break
		}
	}
	if allMaps {
		out := make([]map[string]any, len(items))
		for i, it := range items {
			out[i] = it.(map[string]any)
		}
		return out, nil
	}
	if items == nil {
		return []any{}, nil
	}
	return items, nil
}

func (p *parser) parseInlineTable() (map[string]any, error) {
	if p.advance() != '{' {
		return nil, p.errf("internal: parseInlineTable entry")
	}
	t := map[string]any{}
	p.skipInlineWS()
	if p.peek() == '}' {
		p.advance()
		return t, nil
	}
	for {
		p.skipInlineWS()
		k, err := p.parseKey()
		if err != nil {
			return nil, err
		}
		p.skipInlineWS()
		if p.peek() != '=' {
			return nil, p.errf("expected '=' in inline table, got %q", string(p.peek()))
		}
		p.advance()
		p.skipInlineWS()
		val, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		if _, ok := t[k]; ok {
			return nil, p.errf("duplicate key %q in inline table", k)
		}
		t[k] = val
		p.skipInlineWS()
		if p.peek() == ',' {
			p.advance()
			continue
		}
		if p.peek() == '}' {
			p.advance()
			return t, nil
		}
		return nil, p.errf("expected ',' or '}' in inline table, got %q", string(p.peek()))
	}
}

func (p *parser) parseBool() (bool, error) {
	if strings.HasPrefix(p.src[p.pos:], "true") {
		for i := 0; i < 4; i++ {
			p.advance()
		}
		return true, nil
	}
	if strings.HasPrefix(p.src[p.pos:], "false") {
		for i := 0; i < 5; i++ {
			p.advance()
		}
		return false, nil
	}
	return false, p.errf("expected true/false")
}

func (p *parser) parseNumber() (any, error) {
	start := p.pos
	if p.peek() == '+' || p.peek() == '-' {
		p.advance()
	}
	digits := 0
	for !p.eof() {
		c := p.peek()
		if c >= '0' && c <= '9' {
			p.advance()
			digits++
			continue
		}
		if c == '_' && digits > 0 {
			p.advance()
			continue
		}
		break
	}
	if digits == 0 {
		return nil, p.errf("expected digit, got %q", string(p.peek()))
	}
	// Reject non-decimal bases (0x, 0o, 0b) by checking the literal so far.
	if p.peek() == 'x' || p.peek() == 'o' || p.peek() == 'b' || p.peek() == 'X' || p.peek() == 'O' || p.peek() == 'B' {
		return nil, p.errf("non-decimal integer literals not supported")
	}
	isFloat := false
	if p.peek() == '.' {
		isFloat = true
		p.advance()
		for !p.eof() && p.peek() >= '0' && p.peek() <= '9' {
			p.advance()
		}
	}
	if p.peek() == 'e' || p.peek() == 'E' {
		isFloat = true
		p.advance()
		if p.peek() == '+' || p.peek() == '-' {
			p.advance()
		}
		for !p.eof() && p.peek() >= '0' && p.peek() <= '9' {
			p.advance()
		}
	}
	raw := strings.ReplaceAll(p.src[start:p.pos], "_", "")
	if isFloat {
		f, err := strconv.ParseFloat(raw, 64)
		if err != nil {
			return nil, p.errf("bad float %q: %v", raw, err)
		}
		return f, nil
	}
	n, err := strconv.ParseInt(raw, 10, 64)
	if err != nil {
		return nil, p.errf("bad int %q: %v", raw, err)
	}
	return n, nil
}

// descendTable walks `target` along `path`, creating intermediate map[string]any
// entries when missing. If create is false, missing intermediates return nil.
func descendTable(target map[string]any, path []string, create bool) (map[string]any, error) {
	cur := target
	for i, key := range path {
		existing, ok := cur[key]
		if !ok {
			if !create {
				return nil, fmt.Errorf("missing key %q at path %v", key, path[:i+1])
			}
			nm := map[string]any{}
			cur[key] = nm
			cur = nm
			continue
		}
		switch v := existing.(type) {
		case map[string]any:
			cur = v
		case []map[string]any:
			if len(v) == 0 {
				return nil, fmt.Errorf("empty array-of-tables at %v", path[:i+1])
			}
			cur = v[len(v)-1]
		default:
			return nil, fmt.Errorf("key %q at %v is %T, not a table", key, path[:i+1], existing)
		}
	}
	return cur, nil
}

// appendArrayOfTables appends `tbl` to the array-of-tables at `path` in
// `target`, creating intermediate tables as needed.
func appendArrayOfTables(target map[string]any, path []string, tbl map[string]any) error {
	if len(path) == 0 {
		return fmt.Errorf("empty array-of-tables path")
	}
	parent := target
	if len(path) > 1 {
		var err error
		parent, err = descendTable(target, path[:len(path)-1], true)
		if err != nil {
			return err
		}
	}
	last := path[len(path)-1]
	existing, ok := parent[last]
	if !ok {
		parent[last] = []map[string]any{tbl}
		return nil
	}
	arr, ok := existing.([]map[string]any)
	if !ok {
		return fmt.Errorf("key %q is %T, not an array-of-tables", last, existing)
	}
	parent[last] = append(arr, tbl)
	return nil
}

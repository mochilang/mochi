//go:build slow

package lua

import (
	"bytes"
	"fmt"
	"strings"

	luaast "github.com/yuin/gopher-lua/ast"
	luaparse "github.com/yuin/gopher-lua/parse"
)

// Program holds parsed Lua statements.
type Program struct {
	Stmts []luaast.Stmt
}

// Parse parses Lua source code into a Program using the official parser.
func Parse(src string) (*Program, error) {
	pre := preprocessLuaSource(src)
	chunk, err := luaparse.Parse(bytes.NewReader([]byte(pre)), "src.lua")
	if err != nil {
		return nil, fmt.Errorf("%s", formatLuaParseError(err, src))
	}
	return &Program{Stmts: chunk}, nil
}

func preprocessLuaSource(src string) string {
	var out strings.Builder
	inStr := false
	strDelim := byte(0)
	inComment := false
	for i := 0; i < len(src); i++ {
		ch := src[i]
		if inComment {
			out.WriteByte(ch)
			if ch == '\n' {
				inComment = false
			}
			continue
		}
		if inStr {
			out.WriteByte(ch)
			if ch == strDelim && (i == 0 || src[i-1] != '\\') {
				inStr = false
			}
			continue
		}
		if ch == '-' && i+1 < len(src) && src[i+1] == '-' {
			out.WriteByte(ch)
			inComment = true
			continue
		}
		if ch == '\'' || ch == '"' {
			inStr = true
			strDelim = ch
			out.WriteByte(ch)
			continue
		}
		if ch == '/' && i+1 < len(src) && src[i+1] == '/' {
			out.WriteByte('/')
			out.WriteByte(' ')
			i++
			continue
		}
		out.WriteByte(ch)
	}
	res := out.String()
	lines := strings.Split(res, "\n")
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "goto __continue") || strings.HasPrefix(trimmed, "::__continue") {
			lines[i] = "--" + line
		}
	}
	return strings.Join(lines, "\n")
}

func formatLuaParseError(err error, src string) string {
	msg := err.Error()
	line := 0
	col := 0
	if idx := strings.Index(msg, "line:"); idx >= 0 {
		i := idx + len("line:")
		for i < len(msg) && msg[i] >= '0' && msg[i] <= '9' {
			line = line*10 + int(msg[i]-'0')
			i++
		}
		if strings.HasPrefix(msg[i:], "(column:") {
			i += len("(column:")
			for i < len(msg) && msg[i] >= '0' && msg[i] <= '9' {
				col = col*10 + int(msg[i]-'0')
				i++
			}
		}
	}
	lines := strings.Split(src, "\n")
	if line <= 0 || line-1 >= len(lines) {
		return msg
	}
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var b strings.Builder
	b.WriteString(fmt.Sprintf("line %d: %s\n", line, msg))
	for i := start; i <= end; i++ {
		b.WriteString(fmt.Sprintf("%4d| %s\n", i+1, lines[i]))
		if i == line-1 && col > 0 {
			b.WriteString("     " + strings.Repeat(" ", col-1) + "^\n")
		}
	}
	return strings.TrimSpace(b.String())
}

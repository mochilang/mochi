package any2mochi

import (
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

var phoFuncRE = regexp.MustCompile(`(?m)fun\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*{`)

// findMatch returns the index of the matching closing delimiter for the opening
// delimiter at openIdx. If no match is found it returns len(s).
func phoFindMatch(s string, openIdx int, open, close rune) int {
	depth := 0
	for i, r := range s[openIdx:] {
		if r == open {
			depth++
		} else if r == close {
			depth--
			if depth == 0 {
				return openIdx + i
			}
		}
	}
	return len(s)
}

func posFromOffsetPho(src string, off int) protocol.Position {
	line := 0
	col := 0
	for i := 0; i < off && i < len(src); i++ {
		if src[i] == '\n' {
			line++
			col = 0
		} else {
			col++
		}
	}
	return protocol.Position{Line: uint32(line), Character: uint32(col)}
}

// parsePho returns document symbols for top-level Pho functions.
func parsePho(src string) []protocol.DocumentSymbol {
	var syms []protocol.DocumentSymbol
	matches := phoFuncRE.FindAllStringSubmatchIndex(src, -1)
	for _, m := range matches {
		name := src[m[2]:m[3]]
		params := strings.TrimSpace(src[m[4]:m[5]])
		openBraceIdx := strings.Index(src[m[0]:], "{")
		if openBraceIdx == -1 {
			continue
		}
		openBraceIdx += m[0]
		closeIdx := phoFindMatch(src, openBraceIdx, '{', '}')
		start := posFromOffsetPho(src, m[0])
		end := posFromOffsetPho(src, closeIdx+1)
		detail := "(" + params + ")"
		syms = append(syms, protocol.DocumentSymbol{
			Name:           name,
			Kind:           protocol.SymbolKindFunction,
			Detail:         &detail,
			Range:          protocol.Range{Start: start, End: end},
			SelectionRange: protocol.Range{Start: start, End: start},
		})
	}
	return syms
}

func extractPhoBody(src string, sym protocol.DocumentSymbol) []string {
	start := indexForPosPho(src, sym.Range.Start)
	end := indexForPosPho(src, sym.Range.End)
	if start >= len(src) || end > len(src) || start >= end {
		return nil
	}
	code := src[start:end]
	open := strings.Index(code, "{")
	close := strings.LastIndex(code, "}")
	if open == -1 || close == -1 || close <= open {
		return nil
	}
	body := strings.TrimSpace(code[open+1 : close])
	if body == "" {
		return nil
	}
	parts := strings.Split(body, ";")
	var out []string
	for _, p := range parts {
		t := strings.TrimSpace(p)
		if t != "" {
			out = append(out, t)
		}
	}
	return out
}

// indexForPosition converts a protocol position to a byte offset in src.
func indexForPosPho(src string, pos protocol.Position) int {
	lines := strings.Split(src, "\n")
	if int(pos.Line) >= len(lines) {
		return len(src)
	}
	idx := 0
	for i := 0; i < int(pos.Line); i++ {
		idx += len(lines[i]) + 1
	}
	if int(pos.Character) > len(lines[int(pos.Line)]) {
		idx += len(lines[int(pos.Line)])
	} else {
		idx += int(pos.Character)
	}
	return idx
}

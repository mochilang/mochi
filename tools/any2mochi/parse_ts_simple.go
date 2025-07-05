package any2mochi

import (
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// tsFunctionBody parses a subset of TypeScript statements and returns
// corresponding Mochi statements. Unsupported statements are ignored.
func tsFunctionBody(src string) []string {
	var lines []string
	s := strings.TrimSpace(src)
	for len(s) > 0 {
		// trim leading whitespace and semicolons
		s = strings.TrimLeft(s, " \t\n\r;")
		if len(s) == 0 {
			break
		}
		switch {
		case strings.HasPrefix(s, "return "):
			end := strings.Index(s, ";")
			if end == -1 {
				end = len(s)
			}
			expr := strings.TrimSpace(s[len("return "):end])
			lines = append(lines, "return "+expr)
			if end < len(s) {
				s = s[end+1:]
			} else {
				s = ""
			}
			continue
		case strings.HasPrefix(s, "console.log("):
			end := strings.Index(s, ")")
			if end == -1 {
				end = len(s)
			}
			expr := strings.TrimSpace(s[len("console.log("):end])
			lines = append(lines, "print("+expr+")")
			sem := strings.Index(s[end:], ";")
			if sem != -1 {
				s = s[end+sem+1:]
			} else {
				s = s[end:]
			}
			continue
		default:
			if idx := strings.Index(s, ";"); idx != -1 {
				s = s[idx+1:]
			} else {
				s = ""
			}
		}
	}
	return lines
}

// indexForPosition converts a protocol position to a byte offset in src.
func indexForPosition(src string, pos protocol.Position) int {
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

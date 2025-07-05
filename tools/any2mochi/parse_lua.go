package any2mochi

import (
	"regexp"
	"strings"
)

var luaPrintRE = regexp.MustCompile(`^__print\((.*)\)`)

// ParseLuaString converts a small subset of Lua statements into Mochi statements.
func ParseLuaString(src string) []string {
	var out []string
	lines := strings.Split(src, "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "--") {
			continue
		}
		if m := luaPrintRE.FindStringSubmatch(line); m != nil {
			out = append(out, "print("+strings.TrimSpace(m[1])+")")
			continue
		}
	}
	return out
}

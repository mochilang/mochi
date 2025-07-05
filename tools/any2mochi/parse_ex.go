package any2mochi

import (
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

type exFuncInfo struct {
	sym    protocol.DocumentSymbol
	params []string
	ret    string
}

var exDefRE = regexp.MustCompile(`^\s*defp?\s+([A-Za-z_][A-Za-z0-9_!?]*)(?:\(([^)]*)\))?\s+do`)

func parseExFunctions(src string) []exFuncInfo {
	lines := strings.Split(src, "\n")
	var funcs []exFuncInfo
	for i := 0; i < len(lines); i++ {
		m := exDefRE.FindStringSubmatch(lines[i])
		if m == nil {
			continue
		}
		name := m[1]
		paramsStr := strings.TrimSpace(m[2])
		var params []string
		if paramsStr != "" {
			for _, p := range strings.Split(paramsStr, ",") {
				p = strings.TrimSpace(p)
				if p == "" {
					continue
				}
				if idx := strings.Index(p, "::"); idx != -1 {
					p = strings.TrimSpace(p[:idx])
				}
				fields := strings.Fields(p)
				if len(fields) > 0 {
					params = append(params, fields[0])
				}
			}
		}
		depth := 1
		end := i
		for j := i + 1; j < len(lines); j++ {
			l := strings.TrimSpace(lines[j])
			if strings.HasSuffix(l, " do") {
				depth++
			}
			if l == "end" {
				depth--
				if depth == 0 {
					end = j
					break
				}
			}
		}
		sym := protocol.DocumentSymbol{
			Name:           name,
			Kind:           protocol.SymbolKindFunction,
			Range:          protocol.Range{Start: protocol.Position{Line: uint32(i)}, End: protocol.Position{Line: uint32(end)}},
			SelectionRange: protocol.Range{Start: protocol.Position{Line: uint32(i)}, End: protocol.Position{Line: uint32(i)}},
		}
		funcs = append(funcs, exFuncInfo{sym: sym, params: params})
		i = end
	}
	return funcs
}

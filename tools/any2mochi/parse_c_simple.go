package any2mochi

import (
	"regexp"
	"strings"
)

// simple regex based function parser for C
var cFuncRE = regexp.MustCompile(`(?s)([A-Za-z_][A-Za-z0-9_\s\*]*?)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*\{`)

// cFunc represents a parsed C function.
type cFunc struct {
	name   string
	ret    string
	params []cParam
	body   []string
}

// parseCFileSimple extracts top-level functions from src using regular expressions.
func parseCFileSimple(src string) []cFunc {
	var funcs []cFunc
	idx := 0
	for {
		loc := cFuncRE.FindStringSubmatchIndex(src[idx:])
		if loc == nil {
			break
		}
		openIdx := idx + loc[1] - 1 // index of '{'
		closeIdx := findMatch(src, openIdx, '{', '}')
		if closeIdx <= openIdx {
			break
		}
		retPart := strings.TrimSpace(src[idx+loc[2] : idx+loc[3]])
		name := strings.TrimSpace(src[idx+loc[4] : idx+loc[5]])
		paramsPart := strings.TrimSpace(src[idx+loc[6] : idx+loc[7]])
		sig := retPart + " " + name + "(" + paramsPart + ")"
		ret, params := parseCSignature(&sig)
		body := parseCStatements(src[openIdx+1 : closeIdx])
		funcs = append(funcs, cFunc{name: name, ret: ret, params: params, body: body})
		idx = closeIdx + 1
	}
	return funcs
}

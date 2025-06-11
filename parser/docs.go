package parser

import "strings"

// attachDocs scans src for comment blocks immediately preceding declarations
// and attaches them to the parsed Program structure.
func attachDocs(src string, prog *Program) {
	lineDocs := extractLineDocs(src)
	if doc, ok := lineDocs[prog.Pos.Line]; ok {
		prog.PackageDoc = doc
	}
	for _, stmt := range prog.Statements {
		attachStmtDocs(stmt, lineDocs)
	}
}

// extractLineDocs returns a map from the first line of a declaration to the
// comment block that appears directly above it.
func extractLineDocs(src string) map[int]string {
	lines := strings.Split(src, "\n")
	docs := map[int]string{}
	i := 0
	for i < len(lines) {
		line := strings.TrimSpace(lines[i])
		if strings.HasPrefix(line, "//") {
			block := []string{strings.TrimSpace(strings.TrimPrefix(line, "//"))}
			j := i + 1
			for j < len(lines) {
				t := strings.TrimSpace(lines[j])
				if strings.HasPrefix(t, "//") {
					block = append(block, strings.TrimSpace(strings.TrimPrefix(t, "//")))
					j++
				} else {
					break
				}
			}
			if j < len(lines) && strings.TrimSpace(lines[j]) != "" {
				docs[j+1] = strings.Join(block, "\n")
			}
			i = j
		} else {
			i++
		}
	}
	return docs
}

func attachStmtDocs(s *Statement, docs map[int]string) {
	switch {
	case s.Fun != nil:
		if doc, ok := docs[s.Fun.Pos.Line]; ok {
			s.Fun.Doc = doc
		}
	case s.Agent != nil:
		if doc, ok := docs[s.Agent.Pos.Line]; ok {
			s.Agent.Doc = doc
		}
	case s.Type != nil:
		if doc, ok := docs[s.Type.Pos.Line]; ok {
			s.Type.Doc = doc
		}
	case s.Stream != nil:
		if doc, ok := docs[s.Stream.Pos.Line]; ok {
			s.Stream.Doc = doc
		}
	}
}

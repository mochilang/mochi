package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"time"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

var cobolParaRE = regexp.MustCompile(`^[A-Za-z0-9_-]+\.$`)

// parseCobol attempts to call an external CLI to parse the COBOL source and
// return document symbols encoded as JSON. If the CLI is not available or
// fails, a simple regex based fallback parser is used instead.
func parseCobol(src string) []protocol.DocumentSymbol {
	if syms, err := parseCobolCLI(src); err == nil && len(syms) > 0 {
		return syms
	}
	return fallbackParseCobol(src)
}

// parseCobolCLI runs the command specified by the COBOL_AST_CLI environment
// variable or the "cobol-json" default. The command should read source from
// stdin and output a JSON array of LSP DocumentSymbol values.
func parseCobolCLI(src string) ([]protocol.DocumentSymbol, error) {
	cmdName := os.Getenv("COBOL_AST_CLI")
	if cmdName == "" {
		cmdName = "cobol-json"
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	c := exec.CommandContext(ctx, cmdName)
	c.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	c.Stdout = &out
	if err := c.Run(); err != nil {
		return nil, err
	}
	var syms []protocol.DocumentSymbol
	if err := json.Unmarshal(out.Bytes(), &syms); err != nil {
		return nil, err
	}
	return syms, nil
}

// parseCobol parses a very small subset of COBOL and returns symbols for
// top-level variables and paragraphs. It is used as a fallback when no
// language server is available.
func fallbackParseCobol(src string) []protocol.DocumentSymbol {
	lines := strings.Split(src, "\n")
	var syms []protocol.DocumentSymbol

	// locate WORKING-STORAGE SECTION and PROCEDURE DIVISION
	ws := false
	procStart := -1
	for i, l := range lines {
		t := strings.TrimSpace(strings.ToUpper(l))
		if strings.HasPrefix(t, "WORKING-STORAGE SECTION") {
			ws = true
			continue
		}
		if strings.HasPrefix(t, "PROCEDURE DIVISION") {
			procStart = i + 1
			ws = false
			break
		}
		if ws && strings.HasPrefix(strings.TrimSpace(l), "01 ") {
			fields := strings.Fields(l)
			if len(fields) >= 2 {
				name := strings.TrimSuffix(fields[1], ".")
				detail := strings.Join(fields[2:], " ")
				start := protocol.Position{Line: protocol.UInteger(i), Character: 0}
				end := protocol.Position{Line: protocol.UInteger(i), Character: protocol.UInteger(len(l))}
				d := detail
				syms = append(syms, protocol.DocumentSymbol{
					Name:           name,
					Kind:           protocol.SymbolKindVariable,
					Range:          protocol.Range{Start: start, End: end},
					SelectionRange: protocol.Range{Start: start, End: end},
					Detail:         &d,
				})
			}
		}
	}

	if procStart == -1 {
		return syms
	}

	name := "main"
	start := procStart
	for i := procStart; i <= len(lines); i++ {
		if i == len(lines) || cobolParaRE.MatchString(strings.TrimSpace(lines[i])) {
			endLine := i - 1
			if endLine >= start {
				startPos := protocol.Position{Line: protocol.UInteger(start), Character: 0}
				endPos := protocol.Position{Line: protocol.UInteger(endLine), Character: protocol.UInteger(len(lines[endLine]))}
				syms = append(syms, protocol.DocumentSymbol{
					Name:           strings.ToLower(name),
					Kind:           protocol.SymbolKindFunction,
					Range:          protocol.Range{Start: startPos, End: endPos},
					SelectionRange: protocol.Range{Start: startPos, End: startPos},
				})
			}
			if i < len(lines) {
				name = strings.TrimSuffix(strings.TrimSpace(lines[i]), ".")
				start = i + 1
			}
		}
	}
	return syms
}

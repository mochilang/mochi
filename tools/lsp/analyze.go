package lsp

import (
	"errors"
	"regexp"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
	protocol "github.com/tliron/glsp/protocol_3_16"

	"mochi/diagnostic"
	"mochi/parser"
	"mochi/types"
)

// Analyze parses and type checks source and returns LSP diagnostics.
func Analyze(uri string, src string) []protocol.Diagnostic {
	var diags []protocol.Diagnostic

	prog, err := parser.ParseString(src)
	if err != nil {
		if d, ok := convertParseError(uri, err); ok {
			diags = append(diags, toLSPDiagnostic(d))
		} else {
			diags = append(diags, protocol.Diagnostic{
				Range:    protocol.Range{},
				Message:  err.Error(),
				Severity: severityPtr(protocol.DiagnosticSeverityError),
				Source:   strPtr("mochi"),
			})
		}
		return diags
	}

	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		for _, e := range errs {
			if d, ok := e.(diagnostic.Diagnostic); ok {
				diags = append(diags, toLSPDiagnostic(d))
			} else if e != nil {
				diags = append(diags, protocol.Diagnostic{
					Range:    protocol.Range{},
					Message:  e.Error(),
					Severity: severityPtr(protocol.DiagnosticSeverityError),
					Source:   strPtr("mochi"),
				})
			}
		}
	}
	return diags
}

func toLSPDiagnostic(d diagnostic.Diagnostic) protocol.Diagnostic {
	start := protocol.Position{Line: uint32(max(0, int(d.Pos.Line-1))), Character: uint32(max(0, int(d.Pos.Column-1)))}
	end := protocol.Position{Line: uint32(max(0, int(d.Pos.Line-1))), Character: uint32(max(0, int(d.Pos.Column)))}
	code := protocol.IntegerOrString{Value: d.Code}
	src := "mochi"
	return protocol.Diagnostic{
		Range:    protocol.Range{Start: start, End: end},
		Message:  d.Msg,
		Severity: severityPtr(protocol.DiagnosticSeverityError),
		Code:     &code,
		Source:   &src,
	}
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func severityPtr(s protocol.DiagnosticSeverity) *protocol.DiagnosticSeverity { return &s }

func strPtr(s string) *string { return &s }

func optStr(s string) *string {
	if s == "" {
		return nil
	}
	return &s
}

func convertParseError(filename string, err error) (diagnostic.Diagnostic, bool) {
	var posErr interface{ Position() lexer.Position }
	if !errors.As(err, &posErr) {
		return diagnostic.Diagnostic{}, false
	}
	pos := posErr.Position()
	code, help := suggestFix(err.Error())
	return diagnostic.New(code, pos, err.Error(), help), true
}

// suggestFix is copied from parser/errors.go to map parse messages to codes.
func suggestFix(msg string) (string, string) {
	msg = strings.ToLower(msg)

	switch {
	case strings.Contains(msg, "expected \"}\"") || strings.Contains(msg, "expected \"{\"") || strings.Contains(msg, "unexpected eof"):
		return "P001", "Check for a missing `{` or `}` to close the block."
	case strings.Contains(msg, "unexpected eof"):
		return "P002", "Expression or closing delimiter might be missing."
	case strings.Contains(msg, "expected \"{\" statement* \"}\""):
		return "P010", "Function bodies must be enclosed in `{ ... }`."
	case strings.Contains(msg, "expected") && strings.Contains(msg, "\"{\"") && strings.Contains(msg, "=>"):
		return "P011", "`=>` cannot replace a full `{}` block in function bodies."
	case strings.Contains(msg, "expected expression") || strings.Contains(msg, "expected primary"):
		return "P020", "An expression was expected here. Check syntax."
	case strings.Contains(msg, "expected identifier"):
		return "P030", "A variable or function name is required."
	case strings.Contains(msg, `unexpected token ":"`) && strings.Contains(msg, "let"):
		return "P031", "`let` must be followed by a variable name."
	case strings.Contains(msg, "unterminated string") || strings.Contains(msg, "invalid input text") || strings.Contains(msg, "\"unterminated"):
		return "P040", "String literals must be properly closed with a `\"`."
	case strings.Contains(msg, "unexpected token \"*\"") && strings.Contains(msg, "expected primary"):
		return "P050", "`*` is not allowed here. Did you mean to multiply? Use full expression."
	case strings.Contains(msg, "expected ,"):
		return "P051", "Separate function arguments or list elements with commas."
	case strings.Contains(msg, "expected \"(\"") || strings.Contains(msg, "expected \")\""):
		return "P052", "Unbalanced parentheses. Check for missing `(` or `)`."
	case strings.Contains(msg, "unexpected token \"}\"") && strings.Contains(msg, "expected expression"):
		return "P053", "`}` found unexpectedly. Check for incomplete expression."
	case strings.Contains(msg, `unexpected token ":"`) && strings.Contains(msg, "expected )") && strings.Contains(msg, "{"):
		return "P054", "Check for misused colon. Function parameters must use correct syntax: (name: type)."
	case strings.Contains(msg, "unexpected token \".\""):
		return "P055", "Stray `.` dot — expected a selector after it. Did you forget an identifier?"
	case strings.Contains(msg, "unexpected token \"let\""):
		return "P056", "Unexpected `let` — remove redundant declaration keyword."
	default:
		return "P999", "Parse error occurred. Check syntax near this location."
	}
}

// DocumentSymbols parses src and returns LSP document symbols for top-level declarations.
func DocumentSymbols(uri string, src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic) {
	var syms []protocol.DocumentSymbol
	var diags []protocol.Diagnostic

	prog, err := parser.Parser.ParseString(uri, src)
	if err != nil {
		if d, ok := convertParseError(uri, err); ok {
			diags = append(diags, toLSPDiagnostic(d))
		} else {
			diags = append(diags, protocol.Diagnostic{
				Range:    protocol.Range{},
				Message:  err.Error(),
				Severity: severityPtr(protocol.DiagnosticSeverityError),
				Source:   strPtr("mochi"),
			})
		}
		return syms, diags
	}

	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		for _, e := range errs {
			if d, ok := e.(diagnostic.Diagnostic); ok {
				diags = append(diags, toLSPDiagnostic(d))
			} else if e != nil {
				diags = append(diags, protocol.Diagnostic{
					Range:    protocol.Range{},
					Message:  e.Error(),
					Severity: severityPtr(protocol.DiagnosticSeverityError),
					Source:   strPtr("mochi"),
				})
			}
		}
	}

	for _, stmt := range prog.Statements {
		switch {
		case stmt.Fun != nil:
			syms = append(syms, funSymbol(stmt.Fun))
		case stmt.Let != nil:
			syms = append(syms, varSymbol(stmt.Let.Pos, stmt.Let.Name, stmt.Let.Doc))
		case stmt.Var != nil:
			syms = append(syms, varSymbol(stmt.Var.Pos, stmt.Var.Name, stmt.Var.Doc))
		case stmt.Type != nil:
			syms = append(syms, typeSymbol(stmt.Type.Pos, stmt.Type.Name, stmt.Type.Doc))
		}
	}

	return syms, diags
}

func funSymbol(f *parser.FunStmt) protocol.DocumentSymbol {
	rng := symbolRange(f.Pos, len(f.Name))
	kind := protocol.SymbolKindFunction
	return protocol.DocumentSymbol{
		Name:           f.Name,
		Detail:         optStr(f.Doc),
		Kind:           kind,
		Range:          rng,
		SelectionRange: rng,
	}
}

func varSymbol(pos lexer.Position, name, doc string) protocol.DocumentSymbol {
	rng := symbolRange(pos, len(name))
	kind := protocol.SymbolKindVariable
	return protocol.DocumentSymbol{
		Name:           name,
		Detail:         optStr(doc),
		Kind:           kind,
		Range:          rng,
		SelectionRange: rng,
	}
}

func typeSymbol(pos lexer.Position, name string, doc string) protocol.DocumentSymbol {
	rng := symbolRange(pos, len(name))
	kind := protocol.SymbolKindStruct
	return protocol.DocumentSymbol{
		Name:           name,
		Detail:         optStr(doc),
		Kind:           kind,
		Range:          rng,
		SelectionRange: rng,
	}
}

func symbolRange(pos lexer.Position, length int) protocol.Range {
	start := protocol.Position{Line: uint32(max(0, int(pos.Line-1))), Character: uint32(max(0, int(pos.Column-1)))}
	end := protocol.Position{Line: uint32(max(0, int(pos.Line-1))), Character: uint32(max(0, int(pos.Column-1+length)))}
	return protocol.Range{Start: start, End: end}
}

// CompletionItems returns static keyword completions.
func CompletionItems() []protocol.CompletionItem {
	keywords := []string{"fun", "let", "var", "type", "return", "if", "else"}
	kind := protocol.CompletionItemKindKeyword
	items := make([]protocol.CompletionItem, len(keywords))
	for i, kw := range keywords {
		items[i] = protocol.CompletionItem{Label: kw, Kind: &kind}
	}
	return items
}

// Hover returns simple hover information for symbols at the given position.
func Hover(uri, src string, line, character int) (protocol.Hover, []protocol.Diagnostic) {
	syms, diags := DocumentSymbols(uri, src)
	pos := protocol.Position{Line: uint32(line), Character: uint32(character)}
	for _, s := range syms {
		if contains(s.Range, pos) {
			var msg string
			if s.Detail != nil {
				msg = *s.Detail
			} else {
				msg = s.Name
			}
			return protocol.Hover{
				Contents: protocol.MarkupContent{Kind: protocol.MarkupKindPlainText, Value: msg},
				Range:    &s.Range,
			}, diags
		}
	}
	return protocol.Hover{}, diags
}

func contains(rng protocol.Range, pos protocol.Position) bool {
	if pos.Line < rng.Start.Line || pos.Line > rng.End.Line {
		return false
	}
	if pos.Line == rng.Start.Line && pos.Character < rng.Start.Character {
		return false
	}
	if pos.Line == rng.End.Line && pos.Character > rng.End.Character {
		return false
	}
	return true
}

// Definition returns locations of the symbol at the given position.
func Definition(uri, src string, line, character int) ([]protocol.Location, []protocol.Diagnostic) {
	syms, diags := DocumentSymbols(uri, src)
	pos := protocol.Position{Line: uint32(line), Character: uint32(character)}
	for _, s := range syms {
		if contains(s.Range, pos) {
			loc := protocol.Location{
				URI:   protocol.DocumentUri(uri),
				Range: s.Range,
			}
			return []protocol.Location{loc}, diags
		}
	}
	return nil, diags
}

// References returns locations of all occurrences of the symbol at the given position.
func References(uri, src string, line, character int) ([]protocol.Location, []protocol.Diagnostic) {
	syms, diags := DocumentSymbols(uri, src)
	pos := protocol.Position{Line: uint32(line), Character: uint32(character)}
	var name string
	for _, s := range syms {
		if contains(s.Range, pos) {
			name = s.Name
			break
		}
	}
	if name == "" {
		return nil, diags
	}
	word := regexp.MustCompile(`\b` + regexp.QuoteMeta(name) + `\b`)
	var locs []protocol.Location
	lines := strings.Split(src, "\n")
	for i, line := range lines {
		matches := word.FindAllStringIndex(line, -1)
		for _, m := range matches {
			start := protocol.Position{Line: uint32(i), Character: uint32(m[0])}
			end := protocol.Position{Line: uint32(i), Character: uint32(m[1])}
			locs = append(locs, protocol.Location{URI: protocol.DocumentUri(uri), Range: protocol.Range{Start: start, End: end}})
		}
	}
	return locs, diags
}

// WorkspaceSymbols returns symbols from all documents matching the given query.
func WorkspaceSymbols(docs map[string]string, query string) []protocol.SymbolInformation {
	var infos []protocol.SymbolInformation
	for uri, src := range docs {
		syms, _ := DocumentSymbols(uri, src)
		for _, s := range syms {
			if query == "" || strings.Contains(s.Name, query) {
				infos = append(infos, protocol.SymbolInformation{
					Name:          s.Name,
					Kind:          s.Kind,
					Location:      protocol.Location{URI: protocol.DocumentUri(uri), Range: s.Range},
					ContainerName: nil,
				})
			}
		}
	}
	return infos
}

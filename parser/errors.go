package parser

import (
	"errors"
	"os"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

// Parse loads and parses a Mochi source file.
func Parse(path string) (*Program, error) {
	src, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	prog, err := Parser.ParseString(path, string(src))
	if err != nil {
		return nil, wrapParseError(path, err)
	}
	return prog, nil
}

// wrapParseError upgrades raw participle errors into structured diagnostics.
func wrapParseError(filename string, err error) error {
	var posErr interface{ Position() lexer.Position }
	if !errors.As(err, &posErr) {
		return err
	}
	pos := posErr.Position()
	code, help := suggestFix(err.Error())
	return diagnostic.New(code, pos, err.Error(), help)
}

// suggestFix returns a (diagnosticCode, helpText) pair for known parsing errors.
func suggestFix(msg string) (string, string) {
	msg = strings.ToLower(msg)

	switch {
	// --- Block and EOF Errors ---
	case containsAny(msg, `expected "}"`, `expected "{"`, "unexpected eof"):
		return "P001", "Check for a missing `{` or `}` to close the block."

	case strings.Contains(msg, "unexpected eof"):
		return "P002", "Expression or closing delimiter might be missing."

	// --- Function Body Errors ---
	case strings.Contains(msg, `expected "{" statement* "}"`):
		return "P010", "Function bodies must be enclosed in `{ ... }`."

	case containsAll(msg, "expected", `"{"`, "=>"):
		return "P011", "`=>` cannot replace a full `{}` block in function bodies."

	// --- Expression Errors ---
	case containsAny(msg, "expected expression", "expected primary"):
		return "P020", "An expression was expected here. Check syntax."

	// --- Variable & Identifier Issues ---
	case strings.Contains(msg, "expected identifier"):
		return "P030", "A variable or function name is required."

	case containsAll(msg, `unexpected token ":"`, "let"):
		return "P031", "`let` must be followed by a variable name."

	// --- String and Literal Errors ---
	case containsAny(msg, "unterminated string", "invalid input text", `"unterminated`):
		return "P040", "String literals must be properly closed with a `\"`."

	// --- Punctuation Errors ---
	case containsAll(msg, `unexpected token "*"`, "expected primary"):
		return "P050", "`*` is not allowed here. Did you mean to multiply? Use full expression."

	case strings.Contains(msg, `expected ","`):
		return "P051", "Separate function arguments or list elements with commas."

	case containsAny(msg, `expected "("`, `expected ")"`):
		return "P052", "Unbalanced parentheses. Check for missing `(` or `)`."

	case containsAll(msg, `unexpected token "}"`, "expected expression"):
		return "P053", "`}` found unexpectedly. Check for incomplete expression."

	case containsAll(msg, `unexpected token ":"`, `expected ")"`, `"{"`):
		return "P054", "Check for misused colon. Function parameters must use correct syntax: (name: type)."

	case strings.Contains(msg, `unexpected token "."`):
		return "P055", "Stray `.` dot — expected a selector after it. Did you forget an identifier?"

	case strings.Contains(msg, `unexpected token "let"`):
		return "P056", "Unexpected `let` — remove redundant declaration keyword."

	// --- Unknown or Unclassified ---
	default:
		return "P999", "Parse error occurred. Check syntax near this location."
	}
}

// containsAll returns true if all substrings appear in s.
func containsAll(s string, parts ...string) bool {
	for _, p := range parts {
		if !strings.Contains(s, p) {
			return false
		}
	}
	return true
}

// containsAny returns true if any substring appears in s.
func containsAny(s string, parts ...string) bool {
	for _, p := range parts {
		if strings.Contains(s, p) {
			return true
		}
	}
	return false
}

/*
var reserved = map[string]struct{}{
	"let": {}, "fun": {}, "if": {}, "else": {}, "for": {}, "return": {},
	"true": {}, "false": {}, "stream": {}, "agent": {}, "test": {}, "expect": {},
	"on": {}, "intent": {},
}

func isReserved(name string) bool {
	_, found := reserved[name]
	return found
}
*/

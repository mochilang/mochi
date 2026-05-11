package parser

import (
	"io"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

// utf8BOM is the byte-order mark that some editors prepend to UTF-8 files.
// Mochi treats a leading BOM as insignificant whitespace.
const utf8BOM = "\xef\xbb\xbf"

// trimBOM removes a single leading UTF-8 byte-order mark from src, if present.
// Subsequent BOMs are left in place so they surface as ordinary lex errors.
func trimBOM(src string) string {
	return strings.TrimPrefix(src, utf8BOM)
}

// Lexer-level diagnostic templates. The codes are stable: see MEP 1.
var (
	errUnterminatedString = diagnostic.Template{
		Code:    "P040",
		Message: "unterminated string literal",
		Help:    "Close the string with a matching `\"` before end of file.",
	}
	errUnterminatedBlockComment = diagnostic.Template{
		Code:    "P042",
		Message: "unterminated block comment",
		Help:    "Block comments must be closed with `*/`; they do not nest.",
	}
	errAdjacentIdentAfterNumber = diagnostic.Template{
		Code:    "P043",
		Message: "invalid numeric literal: missing whitespace before %q",
		Help:    "Separate the numeric literal from the following identifier with whitespace.",
	}
	errIntegerOverflow = diagnostic.Template{
		Code:    "P045",
		Message: "integer literal %s is out of range for a 64-bit signed integer",
		Help:    "Use a value in the range [-9223372036854775808, 9223372036854775807].",
	}
	errIncompleteNumericPrefix = diagnostic.Template{
		Code:    "P046",
		Message: "incomplete numeric literal %q: missing digits after base prefix",
		Help:    "`0x`, `0b`, and `0o` must be followed by at least one digit of the chosen base.",
	}
)

// preLexScan validates the source for lexical-level errors that the
// regex-based simple lexer would otherwise mishandle silently. It must be
// cheap and side-effect free; it does not produce tokens.
//
// The scan reports the first error in source order. Successful scans
// guarantee that:
//   - every `/*` has a matching `*/`,
//   - every `"` has a matching `"` on the same or a later byte position,
//   - no numeric literal is immediately followed by an identifier-start
//     character (the classic `1e`, `0x`, or `1_000` paper cut),
//   - a `0x`, `0b`, or `0o` prefix is followed by at least one valid digit.
//
// preLexScan does not enforce escape-sequence validity inside strings;
// that is delegated to participle's Unquote pass.
func preLexScan(filename, src string) error {
	pos := lexer.Position{Filename: filename, Line: 1, Column: 1, Offset: 0}

	advance := func(n int) {
		end := min(pos.Offset+n, len(src))
		for pos.Offset < end {
			if src[pos.Offset] == '\n' {
				pos.Line++
				pos.Column = 1
			} else {
				pos.Column++
			}
			pos.Offset++
		}
	}

	for pos.Offset < len(src) {
		c := src[pos.Offset]

		// Block comment.
		if c == '/' && pos.Offset+1 < len(src) && src[pos.Offset+1] == '*' {
			start := pos
			advance(2)
			for pos.Offset+1 < len(src) && !(src[pos.Offset] == '*' && src[pos.Offset+1] == '/') {
				advance(1)
			}
			if pos.Offset+1 >= len(src) {
				return errUnterminatedBlockComment.New(start)
			}
			advance(2)
			continue
		}

		// Line comments.
		if c == '/' && pos.Offset+1 < len(src) && src[pos.Offset+1] == '/' {
			for pos.Offset < len(src) && src[pos.Offset] != '\n' {
				advance(1)
			}
			continue
		}
		if c == '#' {
			for pos.Offset < len(src) && src[pos.Offset] != '\n' {
				advance(1)
			}
			continue
		}

		// String literal.
		if c == '"' {
			start := pos
			advance(1)
			for pos.Offset < len(src) && src[pos.Offset] != '"' {
				if src[pos.Offset] == '\\' && pos.Offset+1 < len(src) {
					advance(2)
					continue
				}
				advance(1)
			}
			if pos.Offset >= len(src) {
				return errUnterminatedString.New(start)
			}
			advance(1)
			continue
		}

		// Numeric literal: scan the longest valid form, then verify the
		// following character cannot extend an identifier.
		if isAsciiDigit(c) {
			if err := scanNumber(src, &pos, advance); err != nil {
				return err
			}
			continue
		}

		advance(1)
	}
	return nil
}

func isAsciiDigit(b byte) bool { return b >= '0' && b <= '9' }

func isHexDigit(b byte) bool {
	return isAsciiDigit(b) || (b >= 'a' && b <= 'f') || (b >= 'A' && b <= 'F')
}

func isOctalDigit(b byte) bool { return b >= '0' && b <= '7' }

func isBinaryDigit(b byte) bool { return b == '0' || b == '1' }

// isIdentContinue reports whether r is a character that would extend an
// identifier under MEP 1: any Unicode letter, an "Other Symbol", a digit,
// or an underscore.
func isIdentContinue(r rune) bool {
	if r == '_' {
		return true
	}
	return unicode.IsLetter(r) || unicode.In(r, unicode.So) || unicode.IsDigit(r)
}

// scanNumber consumes the numeric literal at pos.Offset and reports the
// first lexical issue it finds. It mirrors the participle Int and Float
// rules; if the two diverge in the future they must be kept in sync.
func scanNumber(src string, pos *lexer.Position, advance func(int)) error {
	start := *pos
	end := pos.Offset

	prefixed := false
	if src[end] == '0' && end+1 < len(src) {
		switch src[end+1] {
		case 'x', 'X':
			prefixed = true
			end += 2
			digitStart := end
			for end < len(src) && isHexDigit(src[end]) {
				end++
			}
			if end == digitStart {
				return errIncompleteNumericPrefix.New(start, src[start.Offset:end])
			}
		case 'b', 'B':
			prefixed = true
			end += 2
			digitStart := end
			for end < len(src) && isBinaryDigit(src[end]) {
				end++
			}
			if end == digitStart {
				return errIncompleteNumericPrefix.New(start, src[start.Offset:end])
			}
		case 'o', 'O':
			prefixed = true
			end += 2
			digitStart := end
			for end < len(src) && isOctalDigit(src[end]) {
				end++
			}
			if end == digitStart {
				return errIncompleteNumericPrefix.New(start, src[start.Offset:end])
			}
		}
	}

	if !prefixed {
		for end < len(src) && isAsciiDigit(src[end]) {
			end++
		}
		// Fractional part: only recognised when the dot is followed by a
		// digit. `1.` is not a number here.
		if end+1 < len(src) && src[end] == '.' && isAsciiDigit(src[end+1]) {
			end++
			for end < len(src) && isAsciiDigit(src[end]) {
				end++
			}
		}
		// Exponent: same rule as participle's Float regex.
		if end < len(src) && (src[end] == 'e' || src[end] == 'E') {
			expEnd := end + 1
			if expEnd < len(src) && (src[expEnd] == '+' || src[expEnd] == '-') {
				expEnd++
			}
			if expEnd < len(src) && isAsciiDigit(src[expEnd]) {
				end = expEnd
				for end < len(src) && isAsciiDigit(src[end]) {
					end++
				}
			}
		}
	}

	// Adjacent identifier-start character is the classic silent-split bug.
	if end < len(src) {
		r, size := utf8.DecodeRuneInString(src[end:])
		if isIdentContinue(r) {
			next := string(r)
			_ = size
			return errAdjacentIdentAfterNumber.New(start, next)
		}
	}

	advance(end - pos.Offset)
	return nil
}

// wrapCaptureError upgrades opaque `failed to capture` errors. These
// surface as ugly `strconv.ParseInt: parsing "...": value out of range`
// strings; we translate them into stable diagnostics.
func wrapCaptureError(filename string, err error) error {
	msg := err.Error()
	if !strings.Contains(msg, "value out of range") {
		return err
	}
	pos := lexer.Position{Filename: filename}
	if p, ok := err.(interface{ Position() lexer.Position }); ok {
		pos = p.Position()
	}
	return errIntegerOverflow.New(pos, extractLiteralFromMsg(msg))
}

// extractLiteralFromMsg pulls the offending number out of a strconv error.
func extractLiteralFromMsg(msg string) string {
	const marker = `parsing "`
	if _, rest, ok := strings.Cut(msg, marker); ok {
		if lit, _, ok := strings.Cut(rest, `"`); ok {
			return lit
		}
	}
	return ""
}

// Token is a single lexical token produced by Tokenize. It is a thin
// wrapper around the participle token type and exists so that callers do
// not have to depend on participle directly.
type Token struct {
	Kind  string         `json:"kind"`
	Value string         `json:"value"`
	Pos   lexer.Position `json:"pos"`
}

// Tokenize runs the BOM strip, pre-lex validation, and underlying lexer
// over src, returning the resulting token stream with kind names resolved
// to the strings used in MEP 1 (`Int`, `Float`, `Bool`, `Keyword`,
// `Ident`, `String`, `Punct`, `Comment`, `Whitespace`).
//
// The function is intended for tooling (formatters, linters, editors) and
// for the conformance test suite. The parser uses the underlying
// participle lexer directly.
func Tokenize(filename, src string) ([]Token, error) {
	text := trimBOM(src)
	if err := preLexScan(filename, text); err != nil {
		return nil, err
	}
	lex, err := mochiLexer.Lex(filename, strings.NewReader(text))
	if err != nil {
		return nil, err
	}
	symbols := mochiLexer.Symbols()
	names := make(map[lexer.TokenType]string, len(symbols))
	for name, t := range symbols {
		names[t] = name
	}
	var out []Token
	for {
		tok, err := lex.Next()
		if err != nil {
			if err == io.EOF {
				return out, nil
			}
			return nil, err
		}
		if tok.EOF() {
			return out, nil
		}
		out = append(out, Token{
			Kind:  names[tok.Type],
			Value: tok.Value,
			Pos:   tok.Pos,
		})
	}
}

// reportUnknownEscape produces a friendlier message when participle's
// Unquote pass fails. Participle reports `invalid quoted string ...:
// invalid syntax`; we translate that to a positioned diagnostic.
func reportUnknownEscape(filename string, err error) error {
	msg := err.Error()
	if !strings.Contains(msg, "invalid quoted string") {
		return err
	}
	pos := lexer.Position{Filename: filename}
	if p, ok := err.(interface{ Position() lexer.Position }); ok {
		pos = p.Position()
	}
	return diagnostic.New(
		"P041",
		pos,
		"invalid escape sequence in string literal",
		"Mochi accepts the standard Go escapes: \\\\, \\\", \\n, \\r, \\t, \\b, \\f, \\v, \\a, \\xHH, \\uHHHH, \\UHHHHHHHH.",
	)
}

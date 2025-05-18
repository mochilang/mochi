package diagnostic

import (
	"fmt"
	"os"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
)

// --------------------
// 🔧 Global Color Flag
// --------------------

// NoColor controls whether color output is disabled (can be set manually or from env)
var NoColor = os.Getenv("NO_COLOR") != ""

// --------------------
// 🎨 Color Functions
// --------------------

func Red(s string) string {
	if NoColor {
		return s
	}
	return "\033[31m" + s + "\033[0m"
}

func Yellow(s string) string {
	if NoColor {
		return s
	}
	return "\033[33m" + s + "\033[0m"
}

func Gray(s string) string {
	if NoColor {
		return s
	}
	return "\033[90m" + s + "\033[0m"
}

// --------------------
// 🧾 Diagnostic Struct
// --------------------

// Diagnostic represents a structured, position-aware error with helpful context.
type Diagnostic struct {
	Code string         // Error code like "E1001"
	Pos  lexer.Position // File/line/col of error
	Msg  string         // Primary error message
	Help string         // Optional help message (guidance)
}

// Error implements error interface for Diagnostic.
func (d Diagnostic) Error() string {
	return d.Format()
}

// Format renders a multi-line Rust-style diagnostic.
func (d Diagnostic) Format() string {
	src, _ := os.ReadFile(d.Pos.Filename)
	lines := strings.Split(string(src), "\n")

	var line string
	if int(d.Pos.Line) <= len(lines) && d.Pos.Line > 0 {
		line = lines[d.Pos.Line-1]
	}

	marker := strings.Repeat(" ", int(d.Pos.Column)-1) + "^"

	out := []string{
		fmt.Sprintf("%s: %s", Red("error["+d.Code+"]"), d.Msg),
		fmt.Sprintf("  --> %s:%d:%d", d.Pos.Filename, d.Pos.Line, d.Pos.Column),
	}

	if line != "" {
		out = append(out, "",
			fmt.Sprintf("%s | %s", Gray(fmt.Sprintf("%3d", d.Pos.Line)), line),
			fmt.Sprintf("    | %s", Red(marker)),
		)
	}

	if d.Help != "" {
		out = append(out, "", Yellow("help:"), "  "+d.Help)
	}

	return strings.Join(out, "\n")
}

// --------------------
// ✨ Constructor Helpers
// --------------------

// New returns a new Diagnostic instance.
func New(code string, pos lexer.Position, msg, help string) Diagnostic {
	return Diagnostic{
		Code: code,
		Pos:  pos,
		Msg:  msg,
		Help: help,
	}
}

// Wrap creates an error interface from New (shorthand).
func Wrap(code string, pos lexer.Position, msg, help string) error {
	return New(code, pos, msg, help)
}

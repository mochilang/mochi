package diagnostic

import (
	"fmt"
	"os"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
	"github.com/fatih/color"
)

func init() {
	color.NoColor = true // Disable color by default
}

var (
	red    = color.New(color.FgRed, color.Bold).SprintFunc()
	yellow = color.New(color.FgYellow).SprintFunc()
	gray   = color.New(color.FgHiBlack).SprintFunc()
)

// Template represents a reusable diagnostic pattern with code, message, and help text.
type Template struct {
	Code    string // e.g. "T004"
	Message string // Format string, e.g. "expected %v but got %v"
	Help    string // Optional help message
}

// New instantiates a Diagnostic from a Template.
func (t Template) New(pos lexer.Position, args ...any) Diagnostic {
	return New(t.Code, pos, fmt.Sprintf(t.Message, args...), t.Help)
}

// Diagnostic represents a structured, position-aware compiler message.
type Diagnostic struct {
	Code string         // e.g. "E1001"
	Pos  lexer.Position // Source file, line, and column
	Msg  string         // Main error message
	Help string         // Optional help or suggestion
}

func (d Diagnostic) Error() string {
	return d.Format()
}

func (d Diagnostic) Format() string {
	src, _ := os.ReadFile(d.Pos.Filename)
	lines := strings.Split(string(src), "\n")

	var lineText, marker string
	if d.Pos.Line > 0 && int(d.Pos.Line) <= len(lines) {
		lineText = lines[d.Pos.Line-1]
		if d.Pos.Column > 0 && int(d.Pos.Column) <= len(lineText)+1 {
			marker = strings.Repeat(" ", int(d.Pos.Column)-1) + "^"
		}
	}

	out := []string{
		fmt.Sprintf("%s: %s", red("error["+d.Code+"]"), d.Msg),
		fmt.Sprintf("  --> %s:%d:%d", d.Pos.Filename, d.Pos.Line, d.Pos.Column),
	}

	if lineText != "" {
		lineno := fmt.Sprintf("%3d", d.Pos.Line)
		out = append(out, "",
			fmt.Sprintf("%s | %s", gray(lineno), lineText),
			fmt.Sprintf("    | %s", red(marker)),
		)
	}

	if d.Help != "" {
		out = append(out, "", yellow("help:"), "  "+d.Help)
	}

	return strings.Join(out, "\n")
}

// New creates a new Diagnostic.
func New(code string, pos lexer.Position, msg, help string) Diagnostic {
	return Diagnostic{
		Code: code,
		Pos:  pos,
		Msg:  msg,
		Help: help,
	}
}

/*
// Wrap returns a Diagnostic as an error.
func Wrap(code string, pos lexer.Position, msg, help string) error {
	return New(code, pos, msg, help)
}
*/

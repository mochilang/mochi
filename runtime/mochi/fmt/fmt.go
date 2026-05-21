// Package fmt is the Mochi-semantics print runtime for the Go target.
// Mochi's `print(a, b, c)` joins arguments with a single space and
// appends a newline (see VM op OpPrint in runtime/vm/vm.go), not
// Go's `fmt.Println` (which space-separates *and* newlines between
// successive Println calls but uses the default %v formatter). The
// difference matters for floats: Mochi formats `1.0` as `1`, not as
// `1` (Go prints `1` for floats with integral values too via %v, so
// this happens to align), and for booleans it prints `true`/`false`
// in lowercase (Go agrees).
//
// The behaviour Mochi diverges on is `nil`: Mochi prints `nil`, Go
// prints `<nil>`. The Format helper handles that one case explicitly.
package fmt

import (
	gofmt "fmt"
	"io"
	"os"
	"strings"
)

// Print writes the Mochi-formatted form of args to stdout, joined by
// a single space, with a trailing newline.
func Print(args ...any) {
	_ = Fprintln(os.Stdout, args...)
}

// Fprintln writes the Mochi-formatted form of args to w, joined by a
// single space, with a trailing newline.
func Fprintln(w io.Writer, args ...any) error {
	parts := make([]string, len(args))
	for i, a := range args {
		parts[i] = Format(a)
	}
	_, err := gofmt.Fprintln(w, strings.Join(parts, " "))
	return err
}

// Sprint returns the Mochi-formatted form of args joined by a single
// space. No trailing newline.
func Sprint(args ...any) string {
	parts := make([]string, len(args))
	for i, a := range args {
		parts[i] = Format(a)
	}
	return strings.Join(parts, " ")
}

// Format renders one value using Mochi's print conventions: `nil`
// rather than Go's `<nil>`, default %v for everything else.
func Format(v any) string {
	if v == nil {
		return "nil"
	}
	return gofmt.Sprintf("%v", v)
}

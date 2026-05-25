package build

import (
	"runtime"
	"testing"
)

// TestPhase6FmtStrings is the MEP-45 Phase 6.4 gate for format-string
// interpolation. It walks every fixture under
// tests/transpiler3/c/fixtures/fmt_strings.
//
// The fixtures probe:
//   - string variable interpolation: "Hello, {name}!"
//   - int variable interpolation: "You are {age} years old."
//   - float variable interpolation: "Pi is approximately {pi}."
//   - bool variable interpolation: "The check passed: {ok}"
//   - multiple holes in one string: "{name} is {age} years old and scored {score}."
//   - format strings inside a user function
//   - plain string with no holes (regression: must still work)
//   - adjacent holes with no literal between: "{x}{y}"
func TestPhase6FmtStrings(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "fmt_strings")
}

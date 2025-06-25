package mfmt

import (
	"mochi/golden"
	"testing"
)

func TestFormat(t *testing.T) {
	golden.Run(t, "tests/fmt", ".mochi", ".golden", func(src string) ([]byte, error) {
		return FormatFile(src)
	})
}

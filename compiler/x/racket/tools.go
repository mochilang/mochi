//go:build slow

package racket

import (
	"bytes"
	"fmt"
	"os/exec"

	meta "mochi/compiler/meta"
)

// EnsureRacket checks that the racket interpreter is available.
func EnsureRacket() error {
	if _, err := exec.LookPath("racket"); err == nil {
		return nil
	}
	return fmt.Errorf("racket not found")
}

// Format returns the source formatted using raco fmt if available.
func Format(src []byte) []byte {
	header := meta.Header(";")
	if _, err := exec.LookPath("raco"); err == nil {
		cmd := exec.Command("raco", "fmt", "--stdin")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &out
		if err := cmd.Run(); err == nil {
			outBytes := out.Bytes()
			if len(outBytes) > 0 && outBytes[len(outBytes)-1] != '\n' {
				outBytes = append(outBytes, '\n')
			}
			return append(header, outBytes...)
		}
	}
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return append(header, src...)
}

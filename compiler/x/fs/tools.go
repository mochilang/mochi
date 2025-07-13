//go:build slow

package fscode

import (
	"bytes"
	"os/exec"
)

// FormatFS attempts to pretty-print F# code using the `fantomas` formatter if
// available. If the formatter is missing or fails, the input is returned
// unchanged with a trailing newline ensured.
func FormatFS(src []byte) []byte {
	path, err := exec.LookPath("fantomas")
	if err == nil {
		cmd := exec.Command(path, "--stdin")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if e := cmd.Run(); e == nil {
			res := out.Bytes()
			if len(res) == 0 || res[len(res)-1] != '\n' {
				res = append(res, '\n')
			}
			return res
		}
	}
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

// EnsureFormatter checks that `fantomas` is available so generated code can be
// nicely formatted. It is safe to call from tests.
func EnsureFormatter() error {
	if _, err := exec.LookPath("fantomas"); err == nil {
		return nil
	}
	return exec.ErrNotFound
}

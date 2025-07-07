//go:build archived

package stcode

import (
	"bufio"
	"bytes"
	"os/exec"
	"strings"
)

// format takes generated Smalltalk code and adjusts spacing for readability.
func format(code []byte) []byte {
	if err := EnsureFormatter(); err == nil {
		if path, err := exec.LookPath("gst-format"); err == nil {
			cmd := exec.Command(path)
			cmd.Stdin = bytes.NewReader(code)
			var out bytes.Buffer
			cmd.Stdout = &out
			if err := cmd.Run(); err == nil {
				res := out.Bytes()
				if len(res) == 0 || res[len(res)-1] != '\n' {
					res = append(res, '\n')
				}
				return res
			}
		}
	}
	// convert tabs to four spaces and trim trailing whitespace
	s := strings.ReplaceAll(string(code), "\t", "    ")
	var buf bytes.Buffer
	scanner := bufio.NewScanner(strings.NewReader(s))
	for scanner.Scan() {
		line := strings.TrimRight(scanner.Text(), " \t")
		buf.WriteString(line)
		buf.WriteByte('\n')
	}
	return buf.Bytes()
}

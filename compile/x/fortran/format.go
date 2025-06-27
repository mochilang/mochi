package ftncode

import (
	"bytes"
	"os/exec"
	"strings"
)

// formatCode formats the generated Fortran source using `fprettify` or
// `findent`. If neither tool is available or formatting fails, the input is
// returned unchanged with the encountered error.
func formatCode(src []byte) ([]byte, error) {
	tool, err := EnsureFormatter()
	if err != nil {
		return src, err
	}
	args := []string{}
	if strings.Contains(tool, "fprettify") {
		args = []string{"--indent", "2"}
	} else if strings.Contains(tool, "findent") {
		args = []string{"-i2"}
	}
	cmd := exec.Command(tool, args...)
	cmd.Stdin = bytes.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return src, err
	}
	res := out.Bytes()
	if len(res) == 0 || res[len(res)-1] != '\n' {
		res = append(res, '\n')
	}
	return res, nil
}

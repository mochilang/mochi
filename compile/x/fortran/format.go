package ftncode

import (
	"bytes"
	"os/exec"
)

// formatCode runs `findent` on the generated Fortran source to improve
// readability. If findent is not available or fails, the input is returned
// unchanged along with the error.
func formatCode(src []byte) ([]byte, error) {
	if _, err := exec.LookPath("findent"); err != nil {
		return src, err
	}
	cmd := exec.Command("findent")
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

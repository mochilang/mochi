//go:build archived

package ftncode

import (
	"bytes"
	"fmt"
	"os/exec"
)

// formatCode formats the generated Fortran source using `fprettify` or
// `findent`. If neither tool is available or formatting fails, the input is
// returned unchanged with the encountered error.
func formatCode(src []byte) ([]byte, error) {
	tool, args := findFormatter()
	if tool == "" {
		return src, fmt.Errorf("formatter not found")
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

func findFormatter() (string, []string) {
	if path, err := exec.LookPath("fprettify"); err == nil {
		return path, []string{"--indent", "2"}
	}
	if path, err := exec.LookPath("findent"); err == nil {
		return path, []string{"-i2"}
	}
	return "", nil
}

// Format applies the best available Fortran formatter to src. If no
// formatter is found, tabs are expanded to two spaces and a trailing newline
// is ensured so the generated code remains readable.
func Format(src []byte) []byte {
	if out, err := formatCode(src); err == nil {
		return out
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

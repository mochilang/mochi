package ftncode

import (
	"bytes"
	"os"
	"os/exec"
)

// formatFortran runs fprettify on the given source if available.
func formatFortran(src []byte) ([]byte, error) {
	bin, err := EnsureFprettify()
	if err != nil {
		return src, err
	}
	cmd := exec.Command(bin)
	cmd.Stdin = bytes.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return src, err
	}
	return out.Bytes(), nil
}

package dart

import (
	"bytes"
	"os/exec"
)

// formatDart runs `dart format` if available, falling back to simple formatting.
func formatDart(src []byte) []byte {
	path, err := exec.LookPath("dart")
	if err == nil {
		cmd := exec.Command(path, "format", "--output", "show", "-")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if cmd.Run() == nil {
			res := out.Bytes()
			if len(res) == 0 || res[len(res)-1] != '\n' {
				res = append(res, '\n')
			}
			return res
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

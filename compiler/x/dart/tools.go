package dart

import (
	"bytes"
	"os"
	"os/exec"
)

// formatDart runs `dart format` if available, falling back to simple formatting.
func formatDart(src []byte) []byte {
	path, err := exec.LookPath("dart")
	if err == nil {
		tmp, err := os.CreateTemp("", "mochi_*.dart")
		if err == nil {
			tmp.Write(src)
			tmp.Close()
			defer os.Remove(tmp.Name())
			cmd := exec.Command(path, "format", "--output", "show", tmp.Name())
			var out bytes.Buffer
			cmd.Stdout = &out
			if cmd.Run() == nil {
				res := out.Bytes()
				lines := bytes.Split(res, []byte("\n"))
				if len(lines) > 0 && len(lines[len(lines)-1]) == 0 {
					lines = lines[:len(lines)-1]
				}
				if n := len(lines); n > 0 && bytes.HasPrefix(lines[n-1], []byte("Formatted")) {
					lines = lines[:n-1]
				}
				res = bytes.Join(lines, []byte("\n"))
				if len(res) == 0 || res[len(res)-1] != '\n' {
					res = append(res, '\n')
				}
				return res
			}
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

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
		tmp, err := os.CreateTemp("", "dart-src-*.dart")
		if err == nil {
			defer os.Remove(tmp.Name())
			tmp.Write(src)
			tmp.Close()
			cmd := exec.Command(path, "format", "--output", "write", tmp.Name())
			cmd.Stdout = nil
			cmd.Stderr = nil
			if cmd.Run() == nil {
				data, err := os.ReadFile(tmp.Name())
				if err == nil {
					if len(data) == 0 || data[len(data)-1] != '\n' {
						data = append(data, '\n')
					}
					return data
				}
			}
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

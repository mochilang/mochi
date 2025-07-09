package cpp

import (
	"bytes"
	"os/exec"
)

// FormatCPP runs clang-format on the given source code if available.
// If clang-format is missing or fails, the input is returned unchanged.
func FormatCPP(src []byte) []byte {
	path, err := exec.LookPath("clang-format")
	if err != nil {
		if len(src) > 0 && src[len(src)-1] != '\n' {
			src = append(src, '\n')
		}
		return src
	}
	cmd := exec.Command(path, "-style=LLVM")
	cmd.Stdin = bytes.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err == nil {
		res := out.Bytes()
		if len(res) == 0 || res[len(res)-1] != '\n' {
			res = append(res, '\n')
		}
		return res
	}
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

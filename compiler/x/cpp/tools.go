//go:build slow

package cpp

import (
	"bytes"
	"os/exec"

	meta "mochi/compiler/meta"
)

// FormatCPP runs clang-format on the given source code if available.
// If clang-format is missing or fails, the input is returned unchanged.
func FormatCPP(src []byte) []byte {
	header := meta.Header("//")
	src = append(header, src...)

	path, err := exec.LookPath("clang-format")
	if err != nil {
		if len(src) > 0 && src[len(src)-1] != '\n' {
			src = append(src, '\n')
		}
		return src
	}
	// Use LLVM style with 4-space indentation to better match the
	// human-crafted examples in tests/human/x/cpp.
	cmd := exec.Command(path, "-style={BasedOnStyle: LLVM, IndentWidth: 4}")
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

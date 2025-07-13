package erlang

import (
	"bufio"
	"bytes"
	"os/exec"
	"strings"

	"mochi/compiler/meta"
)

// Format formats Erlang source code using erlfmt if available and
// ensures a generation header and trailing newline.
func Format(src []byte) []byte {
	if path, err := exec.LookPath("erlfmt"); err == nil {
		cmd := exec.Command(path, "-")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			src = out.Bytes()
		}
	}

	s := strings.ReplaceAll(string(src), "\t", "    ")
	var buf bytes.Buffer
	scanner := bufio.NewScanner(strings.NewReader(s))
	for scanner.Scan() {
		line := strings.TrimRight(scanner.Text(), " \t")
		buf.WriteString(line)
		buf.WriteByte('\n')
	}
	src = buf.Bytes()
	header := meta.Header("%")
	if bytes.HasPrefix(src, []byte("#!")) {
		if i := bytes.IndexByte(src, '\n'); i != -1 {
			src = append(src[:i+1], append(header, src[i+1:]...)...)
		} else {
			src = append(src, '\n')
			src = append(src, header...)
		}
	} else {
		src = append(header, src...)
	}
	if len(src) == 0 || src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

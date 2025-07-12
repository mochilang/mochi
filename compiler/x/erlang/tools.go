package erlang

import (
	"bytes"
	"mochi/compiler/meta"
)

// FormatErlang adds a generation header and ensures a trailing newline.
// If the source begins with a shebang (#!), the header is inserted after it.
func FormatErlang(src []byte) []byte {
	header := meta.Header("%")
	var prefix []byte
	if bytes.HasPrefix(src, []byte("#!")) {
		if i := bytes.IndexByte(src, '\n'); i != -1 {
			prefix = append([]byte(nil), src[:i+1]...)
			src = src[i+1:]
		}
	}
	src = append(prefix, append(header, src...)...)
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

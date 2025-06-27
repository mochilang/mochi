package stcode

import (
	"bufio"
	"bytes"
	"strings"
)

// format takes generated Smalltalk code and adjusts spacing for readability.
func format(code []byte) []byte {
	// convert tabs to four spaces
	s := strings.ReplaceAll(string(code), "\t", "    ")
	var buf bytes.Buffer
	scanner := bufio.NewScanner(strings.NewReader(s))
	for scanner.Scan() {
		line := strings.TrimRight(scanner.Text(), " \t")
		buf.WriteString(line)
		buf.WriteByte('\n')
	}
	return buf.Bytes()
}

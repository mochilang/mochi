package any2mochi

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strings"
)

var (
	putsRe   = regexp.MustCompile(`^puts\((.*)\)$`)
	joinRe   = regexp.MustCompile(`^\[(.*)\]\.join\(" "\)$`)
	assignRe = regexp.MustCompile(`^(\w+)\s*=\s*(.+)$`)
)

// ConvertRb converts Ruby source code to Mochi. The source is first parsed using
// the configured Ruby language server to ensure it is valid. After successful
// parsing, a very small set of Ruby constructs are translated directly in Go.
func ConvertRb(src string) ([]byte, error) {
	ls := Servers["rb"]
	_, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	return translateRb(src), nil
}

// ConvertRbFile reads the Ruby file at path and converts it to Mochi.
func ConvertRbFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRb(string(data))
}

func translateRb(src string) []byte {
	var out strings.Builder
	sc := bufio.NewScanner(strings.NewReader(src))
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		if m := putsRe.FindStringSubmatch(line); m != nil {
			arg := m[1]
			if j := joinRe.FindStringSubmatch(arg); j != nil {
				arg = j[1]
			}
			out.WriteString("print(" + arg + ")\n")
			continue
		}
		if m := assignRe.FindStringSubmatch(line); m != nil {
			out.WriteString("let " + m[1] + " = " + m[2] + "\n")
			continue
		}
	}
	if out.Len() == 0 {
		return nil
	}
	return []byte(out.String())
}

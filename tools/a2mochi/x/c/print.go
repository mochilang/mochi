package c

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"mochi/ast"
)

var version = func() string {
	_, file, _, ok := runtime.Caller(0)
	if ok {
		data, err := os.ReadFile(filepath.Join(filepath.Dir(file), "../../..", "VERSION"))
		if err == nil {
			return strings.TrimSpace(string(data))
		}
	}
	return "dev"
}()

func header() string {
	tz := time.FixedZone("GMT+7", 7*3600)
	var b strings.Builder
	fmt.Fprintf(&b, "// a2mochi c v%s %s GMT+7", version, time.Now().In(tz).Format("2006-01-02 15:04:05"))
	b.WriteByte('\n')
	return b.String()
}

// Print returns Mochi source code for n with the standard header and original source.
func Print(n *ast.Node) (string, error) {
	if n == nil {
		return header(), nil
	}
	var b strings.Builder
	b.WriteString(header())
	if err := ast.Fprint(&b, n); err != nil {
		return "", err
	}
	if !strings.HasSuffix(b.String(), "\n") {
		b.WriteByte('\n')
	}
	return b.String(), nil
}

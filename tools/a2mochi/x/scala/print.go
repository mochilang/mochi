//go:build slow

package scala

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/ast"
)

// Print returns Mochi source code for the given AST node, prefixed with a
// generated header containing the current version and timestamp.
func Print(node *ast.Node) (string, error) {
	var b strings.Builder
	b.WriteString(header())
	if node != nil {
		if err := ast.Fprint(&b, node); err != nil {
			return "", err
		}
		if !strings.HasSuffix(b.String(), "\n") {
			b.WriteByte('\n')
		}
	}
	return b.String(), nil
}

func header() string {
	tz := time.FixedZone("GMT+7", 7*3600)
	var b strings.Builder
	fmt.Fprintf(&b, "// a2mochi scala v%s %s GMT+7\n", version(), time.Now().In(tz).Format("2006-01-02 15:04:05"))
	return b.String()
}

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

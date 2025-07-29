//go:build slow

package swift

import (
	"fmt"
	"os"
	"strings"
	"time"

	"mochi/ast"
)

// Print returns Mochi source code for the node with a version header.
func Print(n *ast.Node) (string, error) {
	var b strings.Builder
	b.WriteString(header())
	b.WriteString(n.Source())
	if !strings.HasSuffix(n.Source(), "\n") {
		b.WriteByte('\n')
	}
	return b.String(), nil
}

func header() string {
	zone := "GMT+7"
	if env := os.Getenv("MOCHI_TZ"); env != "" {
		zone = env
	}
	loc := time.FixedZone(zone, 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf("// a2mochi-swift v%s on %s\n", version(), t.Format("2006-01-02 15:04 MST"))
}

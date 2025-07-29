//go:build slow

package java

import (
	jparser "mochi/tools/a2mochi/x/java"
)

// Program represents a parsed Java source file in simplified form.
type Program struct {
	Body []jparser.Stmt `json:"body"`
}

// Inspect parses the given Java source code and returns its Program structure.
func Inspect(src string) (*Program, error) {
	node, err := jparser.Parse(src)
	if err != nil {
		return nil, err
	}
	return &Program{Body: node.Body}, nil
}

//go:build slow

package scheme_test

import "testing"

func TestGenerateReadme(t *testing.T) {
	updateReadme()
}

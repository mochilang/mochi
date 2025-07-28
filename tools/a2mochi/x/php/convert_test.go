//go:build slow

package php_test

import (
	"flag"
	"testing"
)

var update = flag.Bool("update", false, "update golden files")

func TestConvert_Golden(t *testing.T) {
	t.Skip("PHP converter not fully implemented")
}

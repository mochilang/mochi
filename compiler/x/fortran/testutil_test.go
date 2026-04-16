//go:build slow

package ftncode_test

import "flag"

var update = flag.Bool("update", false, "update golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

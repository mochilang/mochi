//go:build slow

package schemecode_test

import (
	"flag"
	"os"
)

func shouldUpdate() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

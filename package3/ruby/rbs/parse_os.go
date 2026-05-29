package rbs

import (
	"io"
	"os"
)

func init() {
	openFile = func(path string) io.ReadCloser {
		f, err := os.Open(path)
		if err != nil {
			panic(err)
		}
		return f
	}
}

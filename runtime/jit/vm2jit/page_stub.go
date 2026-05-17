//go:build !(darwin && arm64) && !(linux && amd64)

package vm2jit

import "errors"

var errNoPage = errors.New("vm2jit/page: unsupported OS/arch")

func pageAllocOS(int) ([]byte, error) { return nil, errNoPage }

func pageMakeExecOS([]byte, []byte) error { return errNoPage }

func pageFreeOS([]byte) error { return nil }

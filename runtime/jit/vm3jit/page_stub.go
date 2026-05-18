//go:build !(darwin && arm64) && !(linux && amd64)

package vm3jit

import "errors"

var errNoPageBackend = errors.New("vm3jit/page: no executable-page backend for this OS/arch")

func pageAllocOS(int) ([]byte, error)         { return nil, errNoPageBackend }
func pageMakeExecOS(page, src []byte) error   { return errNoPageBackend }
func pageFreeOS([]byte) error                 { return errNoPageBackend }

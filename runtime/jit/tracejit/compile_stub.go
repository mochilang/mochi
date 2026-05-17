//go:build !(darwin && arm64)

package tracejit

import (
	"errors"

	"mochi/runtime/jit/tmpljit"
)

// CompiledTrace on unsupported platforms is just a handle that
// reports its source trace and a no-op Free. Compile always
// returns an error so the engine permanently blacklists the loop
// and falls back to pure interpretation, matching the spec's
// fail-closed contract for unsupported targets.
type CompiledTrace struct {
	trace *Trace
}

func (c *CompiledTrace) Trace() *Trace { return c.trace }

func (c *CompiledTrace) CodeLen() int { return 0 }

func (c *CompiledTrace) Free() error { return nil }

func (c *CompiledTrace) run(*[tmpljit.NumRegs]int64) {}

func Compile(*Trace) (*CompiledTrace, error) {
	return nil, errors.New("tracejit: native codegen only on darwin/arm64")
}

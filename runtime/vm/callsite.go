package vm

// MEP-19 call inline cache. Each call instruction can carry a single
// observed-closure slot in a per-Function side table indexed by the
// instruction's IP. On hit we skip the generic closure resolution and
// the capture-merge construction; on miss we replace the slot.
//
// The cache is monomorphic: a polymorphic site (e.g. a list of four
// closures dispatched in a loop) churns the slot every iteration.
// After siteMaxMisses consecutive misses on the same slot we mark the
// site bypassed and stop touching the cache for the lifetime of the
// program.
//
// The cache is single-goroutine-safe by construction (the VM is
// single-goroutine today). A future concurrent VM revisits this.

const (
	siteMaxMisses = 8
)

type callSite struct {
	closurePtr *closure
	fn         *Function
	captureLen int
	misses     uint8
	bypassed   bool
}

// siteFor returns the call site slot for the given instruction pointer,
// allocating the per-Function slot table lazily.
func siteFor(fn *Function, ip int) *callSite {
	if fn.callSites == nil {
		fn.callSites = make([]*callSite, len(fn.Code))
	}
	cs := fn.callSites[ip]
	if cs == nil {
		cs = &callSite{}
		fn.callSites[ip] = cs
	}
	return cs
}

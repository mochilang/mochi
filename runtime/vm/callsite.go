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

// tryQuicken patches Instr.Quick to q at the given ip, unless the
// site has already been bypassed for polymorphism. Bypassed sites
// stay generic for the rest of the run.
func tryQuicken(fn *Function, ip int, q Op) {
	if fn.quickMisses == nil {
		fn.quickMisses = make([]uint8, len(fn.Code))
	}
	if fn.quickMisses[ip] >= siteMaxMisses {
		return
	}
	fn.Code[ip].Quick = uint8(q)
}

// deoptQuicken is called when a quickened handler observes a tag
// that does not match its specialization. It clears Quick so the
// next dispatch goes through the generic op, and bumps the miss
// counter. After siteMaxMisses misses tryQuicken will refuse to
// re-quicken the slot for the lifetime of the run.
func deoptQuicken(fn *Function, ip int) {
	if fn.quickMisses == nil {
		fn.quickMisses = make([]uint8, len(fn.Code))
	}
	fn.Code[ip].Quick = 0
	if fn.quickMisses[ip] < 255 {
		fn.quickMisses[ip]++
	}
}

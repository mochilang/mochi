package vm

// MEP-19 call inline cache. Each call instruction carries a small
// polymorphic cache in a per-Function side table indexed by the
// instruction's IP. On hit we skip the generic closure resolution
// and the capture-merge construction; on miss we either add the
// callee to a free slot or, once all slots are full and a new
// callee is observed, bump a megamorphism counter.
//
// The cache holds up to siteSlots distinct closures (Hölzle/
// Chambers/Ungar's classic polymorphic IC). siteSlots=4 catches
// the agent-intent dispatch shape (a small fixed list of closures
// rotated in a loop) without paying for an unbounded table.
//
// After siteMaxMisses consecutive megamorphic events the site is
// bypassed for the lifetime of the program.
//
// The cache is single-goroutine-safe by construction (the VM is
// single-goroutine today). A future concurrent VM revisits this.

const (
	siteMaxMisses = 8
	siteSlots     = 4
)

type callSiteEntry struct {
	closurePtr *closure
	captureLen int
}

type callSite struct {
	entries  [siteSlots]callSiteEntry
	nEntries uint8
	misses   uint8
	bypassed bool
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

// lookup returns the cached captureLen and true if cl is already in
// the cache. Linear scan over at most siteSlots entries.
func (cs *callSite) lookup(cl *closure) (int, bool) {
	for i := uint8(0); i < cs.nEntries; i++ {
		if cs.entries[i].closurePtr == cl {
			return cs.entries[i].captureLen, true
		}
	}
	return 0, false
}

// observe records cl in the cache. If a free slot exists it is
// filled. Otherwise a megamorphic event is recorded and, after
// siteMaxMisses such events, the site is permanently bypassed.
func (cs *callSite) observe(cl *closure) {
	if cs.bypassed {
		return
	}
	if cs.nEntries < siteSlots {
		cs.entries[cs.nEntries] = callSiteEntry{closurePtr: cl, captureLen: len(cl.args)}
		cs.nEntries++
		return
	}
	if cs.misses < siteMaxMisses {
		cs.misses++
		if cs.misses >= siteMaxMisses {
			cs.bypassed = true
		}
	}
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

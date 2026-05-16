package vm2

// MEP-19 polymorphic call inline cache. Same shape as runtime/vm:
// up to siteSlots entries per call site, linear-scan lookup, fill
// on miss until full, then bump a megamorphism counter and bypass
// after siteMaxMisses.

const (
	siteMaxMisses = 8
	siteSlots     = 4
)

type callSiteEntry struct {
	ClosurePtr *Closure
	CaptureLen int
}

type callSite struct {
	Entries  [siteSlots]callSiteEntry
	N        uint8
	Misses   uint8
	Bypassed bool
}

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

func (cs *callSite) lookup(cl *Closure) (int, bool) {
	for i := uint8(0); i < cs.N; i++ {
		if cs.Entries[i].ClosurePtr == cl {
			return cs.Entries[i].CaptureLen, true
		}
	}
	return 0, false
}

func (cs *callSite) observe(cl *Closure) {
	if cs.Bypassed {
		return
	}
	if cs.N < siteSlots {
		cs.Entries[cs.N] = callSiteEntry{ClosurePtr: cl, CaptureLen: len(cl.Args)}
		cs.N++
		return
	}
	if cs.Misses < siteMaxMisses {
		cs.Misses++
		if cs.Misses >= siteMaxMisses {
			cs.Bypassed = true
		}
	}
}

func tryQuicken(fn *Function, ip int, q Op) {
	if fn.quickMisses == nil {
		fn.quickMisses = make([]uint8, len(fn.Code))
	}
	if fn.quickMisses[ip] >= siteMaxMisses {
		return
	}
	fn.Code[ip].Quick = uint8(q)
}

func deoptQuicken(fn *Function, ip int) {
	if fn.quickMisses == nil {
		fn.quickMisses = make([]uint8, len(fn.Code))
	}
	fn.Code[ip].Quick = 0
	if fn.quickMisses[ip] < 255 {
		fn.quickMisses[ip]++
	}
}

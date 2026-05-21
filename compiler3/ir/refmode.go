package ir

// RefMode names one of the optional reference-mode annotations from
// MEP-41 §6.9. The default RefModeNone preserves the pre-MEP-41 semantics
// (full per-deref generation check, no exclusivity obligation). A
// non-default mode is recorded on Function.RefModes as a verifier
// obligation; rule class E in compiler3/verify enforces the obligation
// statically when it can, and the runtime enforces the residual
// dynamically by leaving the generation check enabled outside elision
// scopes.
//
// The four modes were chosen to match the vocabulary downstream
// Mochi authors will already know from Swift, Mojo, Hylo, and Vale:
//
//   - RefModeConsume: Swift `consuming`, Mojo `var ... ^`, Hylo `sink`.
//     The annotated binding is used exactly once on the consuming path,
//     after which the underlying handle is eligible for `gc.kill`-style
//     deterministic free. The verifier obligation is at-most-one
//     mutating-or-reading Dispatch use per consume binding.
//
//   - RefModeBorrow: Swift `borrowing`, Mojo `read`, Hylo `let`. The
//     binding has read-only access; no mutating Dispatch op may target it.
//     Generation checks on reads are elidable in JIT lowering because the
//     borrow scope's owner pins the value.
//
//   - RefModeInout: Swift `inout`, Mojo `mut`. Read-write access with
//     an exclusivity obligation (no other live alias). Mutating ops are
//     permitted, and generation checks are still elidable because the
//     binding still pins the value.
//
//   - RefModeWeak: Vale-inspired. Forces non-elision: even if escape
//     analysis would let the optimizer drop the gen check, rule E
//     refuses. The verifier requires every Dispatch op against a weak
//     binding to go through a check op (when the surface language wires
//     `try_deref`, MEP-16 §6.6); for now, rule E rejects a Dispatch
//     whose first argument is a weak-tagged Value, requiring an explicit
//     RefModeNone temporary to be introduced by the frontend.
//
// RefMode is a side table on Function rather than a field on Value so
// the IR ABI is unchanged for default-mode programs: nil RefModes means
// "no obligations." This matches the "opt-in surface" property MEP-41
// §6.9 promises.
type RefMode uint8

const (
	RefModeNone RefMode = iota
	RefModeConsume
	RefModeBorrow
	RefModeInout
	RefModeWeak
)

// String renders a RefMode for error messages and IR dumps.
func (m RefMode) String() string {
	switch m {
	case RefModeNone:
		return "none"
	case RefModeConsume:
		return "consume"
	case RefModeBorrow:
		return "borrow"
	case RefModeInout:
		return "inout"
	case RefModeWeak:
		return "weak"
	}
	return "?"
}

// SetRefMode tags Value id with mode. Idempotent: setting the same mode
// twice is a no-op. Setting a different mode panics, because reference
// modes are immutable for a Value's lifetime (see §6.9: "modes propagate
// through SSA"). The panic catches frontend bugs at the construction
// site rather than letting them silently last-write-wins.
//
// Passing RefModeNone removes the entry, which matches the default
// semantics. The map itself is lazily allocated; a Function whose
// frontend never calls SetRefMode keeps a nil RefModes (zero overhead).
func (fn *Function) SetRefMode(id uint32, mode RefMode) {
	if mode == RefModeNone {
		delete(fn.RefModes, id)
		return
	}
	if existing, ok := fn.RefModes[id]; ok && existing != mode {
		panic("ir: SetRefMode rewrites existing mode (v" + itoaU32(id) + " " + existing.String() + " -> " + mode.String() + "); reference modes are immutable")
	}
	if fn.RefModes == nil {
		fn.RefModes = make(map[uint32]RefMode)
	}
	fn.RefModes[id] = mode
}

// RefModeOf returns the mode tagged on Value id. The zero value
// RefModeNone is returned when the Value is untagged.
func (fn *Function) RefModeOf(id uint32) RefMode {
	if fn.RefModes == nil {
		return RefModeNone
	}
	return fn.RefModes[id]
}

// itoaU32 is a tiny formatter used only by the SetRefMode panic message.
// We avoid pulling strconv into the ir package so the dependency graph
// stays flat.
func itoaU32(v uint32) string {
	if v == 0 {
		return "0"
	}
	var buf [10]byte
	i := len(buf)
	for v > 0 {
		i--
		buf[i] = byte('0' + v%10)
		v /= 10
	}
	return string(buf[i:])
}

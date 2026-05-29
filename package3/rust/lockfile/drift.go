package lockfile

import (
	"fmt"
	"sort"
)

// DriftKind classifies what changed between a recorded lockfile
// entry (what mochi.lock said) and a freshly computed entry (what
// the bridge would write if it locked again right now). Each kind
// is a hard error at `mochi pkg lock --check` time per MEP-73 §6.
type DriftKind int

const (
	// DriftUnknown is the zero value; never returned by Check.
	DriftUnknown DriftKind = iota
	// DriftAdded means the fresh set contains a [[rust-package]]
	// the lockfile did not. Caller may treat this as "expected" on
	// `mochi pkg lock` (it's the result of a new dep) but as a
	// hard error on `mochi pkg lock --check`.
	DriftAdded
	// DriftRemoved means the lockfile contains a [[rust-package]]
	// the fresh set does not. Mirror of DriftAdded.
	DriftRemoved
	// DriftVersion means the resolved version moved.
	DriftVersion
	// DriftSource means the source kind / registry / url moved.
	DriftSource
	// DriftCrateBlake3 means the .crate tarball's BLAKE3 digest
	// changed (so the upstream tarball is no longer
	// byte-identical to what we locked).
	DriftCrateBlake3
	// DriftCrateSHA256 means the .crate SHA-256 changed.
	DriftCrateSHA256
	// DriftRustdocTypesVersion means the rustdoc-types schema
	// the bridge ingests now differs from the schema at lock time.
	DriftRustdocTypesVersion
	// DriftRustdocSHA256 means the rustdoc JSON the bridge would
	// ingest is no longer byte-identical to what we locked.
	DriftRustdocSHA256
	// DriftWrapperSHA256 means the wrapper crate src/lib.rs the
	// bridge would emit is no longer byte-identical.
	DriftWrapperSHA256
	// DriftCapabilityAdded means the fresh set declares a
	// capability the lockfile did not. Per MEP-57's monotonicity
	// rule, this is a hard error pending re-acknowledgement.
	DriftCapabilityAdded
	// DriftCapabilityRemoved means the fresh set drops a
	// capability the lockfile had. Not a hard error semantically,
	// but reported so a downstream check can decide policy.
	DriftCapabilityRemoved
	// DriftFeatures means the feature set changed.
	DriftFeatures
	// DriftDependencies means the transitive dep set changed.
	DriftDependencies
)

// String renders a DriftKind as a short token.
func (k DriftKind) String() string {
	switch k {
	case DriftAdded:
		return "added"
	case DriftRemoved:
		return "removed"
	case DriftVersion:
		return "version"
	case DriftSource:
		return "source"
	case DriftCrateBlake3:
		return "crate-blake3"
	case DriftCrateSHA256:
		return "crate-sha256"
	case DriftRustdocTypesVersion:
		return "rustdoc-types-version"
	case DriftRustdocSHA256:
		return "rustdoc-sha256"
	case DriftWrapperSHA256:
		return "wrapper-sha256"
	case DriftCapabilityAdded:
		return "capability-added"
	case DriftCapabilityRemoved:
		return "capability-removed"
	case DriftFeatures:
		return "features"
	case DriftDependencies:
		return "dependencies"
	default:
		return "unknown"
	}
}

// Drift is one observed difference between a `want` (lockfile) and
// `have` (freshly computed) entry. Crate is the [[rust-package]]
// name the drift applies to; for DriftAdded / DriftRemoved one of
// Want / Have is nil. Detail is a human-readable rendering of the
// difference suitable for inclusion in a `--check` diagnostic.
type Drift struct {
	Crate  string
	Kind   DriftKind
	Want   string
	Have   string
	Detail string
}

// String formats a Drift the way `mochi pkg lock --check` reports
// it, e.g. `hex: wrapper-sha256 drift: want abc... have def...`.
func (d Drift) String() string {
	switch d.Kind {
	case DriftAdded:
		return fmt.Sprintf("%s: package added (not in lockfile)", d.Crate)
	case DriftRemoved:
		return fmt.Sprintf("%s: package removed (in lockfile, not in fresh resolve)", d.Crate)
	default:
		if d.Detail != "" {
			return fmt.Sprintf("%s: %s drift: %s", d.Crate, d.Kind, d.Detail)
		}
		return fmt.Sprintf("%s: %s drift: want %s have %s", d.Crate, d.Kind, d.Want, d.Have)
	}
}

// Check compares a `want` set (the recorded lockfile entries) to a
// `have` set (the freshly resolved entries) and returns every
// observed Drift. The result is sorted by crate name then drift
// kind for stable diagnostic output.
//
// A clean check (no drifts) returns nil.
func Check(want, have []RustPackage) []Drift {
	wantByName := indexByName(want)
	haveByName := indexByName(have)
	var drifts []Drift
	for name, w := range wantByName {
		h, ok := haveByName[name]
		if !ok {
			drifts = append(drifts, Drift{Crate: name, Kind: DriftRemoved})
			continue
		}
		drifts = append(drifts, diffPair(name, w, h)...)
	}
	for name := range haveByName {
		if _, ok := wantByName[name]; !ok {
			drifts = append(drifts, Drift{Crate: name, Kind: DriftAdded})
		}
	}
	sort.Slice(drifts, func(i, j int) bool {
		if drifts[i].Crate != drifts[j].Crate {
			return drifts[i].Crate < drifts[j].Crate
		}
		return drifts[i].Kind < drifts[j].Kind
	})
	return drifts
}

func indexByName(ps []RustPackage) map[string]RustPackage {
	out := make(map[string]RustPackage, len(ps))
	for _, p := range ps {
		out[p.Name] = p
	}
	return out
}

func diffPair(name string, w, h RustPackage) []Drift {
	var out []Drift
	add := func(k DriftKind, want, have, detail string) {
		out = append(out, Drift{Crate: name, Kind: k, Want: want, Have: have, Detail: detail})
	}
	if w.Version != h.Version {
		add(DriftVersion, w.Version, h.Version, "")
	}
	if !sourceEqual(w.Source, h.Source) {
		add(DriftSource, sourceString(w.Source), sourceString(h.Source), "")
	}
	if w.CrateBlake3 != h.CrateBlake3 {
		add(DriftCrateBlake3, w.CrateBlake3, h.CrateBlake3, "")
	}
	if w.CrateSHA256 != h.CrateSHA256 {
		add(DriftCrateSHA256, w.CrateSHA256, h.CrateSHA256, "")
	}
	if w.RustdocTypesVersion != h.RustdocTypesVersion {
		add(DriftRustdocTypesVersion, w.RustdocTypesVersion, h.RustdocTypesVersion, "")
	}
	if w.RustdocSHA256 != h.RustdocSHA256 {
		add(DriftRustdocSHA256, w.RustdocSHA256, h.RustdocSHA256, "")
	}
	if w.WrapperSHA256 != h.WrapperSHA256 {
		add(DriftWrapperSHA256, w.WrapperSHA256, h.WrapperSHA256, "")
	}
	if added := stringSetDiff(h.CapabilitiesDeclared, w.CapabilitiesDeclared); len(added) > 0 {
		add(DriftCapabilityAdded, joinSorted(w.CapabilitiesDeclared), joinSorted(h.CapabilitiesDeclared),
			fmt.Sprintf("new capabilities: %v", added))
	}
	if removed := stringSetDiff(w.CapabilitiesDeclared, h.CapabilitiesDeclared); len(removed) > 0 {
		add(DriftCapabilityRemoved, joinSorted(w.CapabilitiesDeclared), joinSorted(h.CapabilitiesDeclared),
			fmt.Sprintf("dropped capabilities: %v", removed))
	}
	if !stringSetEqual(w.Features, h.Features) {
		add(DriftFeatures, joinSorted(w.Features), joinSorted(h.Features), "")
	}
	if !stringSetEqual(w.Dependencies, h.Dependencies) {
		add(DriftDependencies, joinSorted(w.Dependencies), joinSorted(h.Dependencies), "")
	}
	return out
}

func sourceEqual(a, b Source) bool {
	return a.Kind == b.Kind &&
		a.Registry == b.Registry &&
		a.URL == b.URL &&
		a.Rev == b.Rev &&
		a.Path == b.Path
}

func sourceString(s Source) string {
	switch s.Kind {
	case SourceRegistry:
		return fmt.Sprintf("registry %s", s.Registry)
	case SourceGit:
		if s.Rev != "" {
			return fmt.Sprintf("git %s#%s", s.URL, s.Rev)
		}
		return fmt.Sprintf("git %s", s.URL)
	case SourcePath:
		return fmt.Sprintf("path %s", s.Path)
	default:
		return fmt.Sprintf("%s", s.Kind)
	}
}

func stringSetDiff(a, b []string) []string {
	set := map[string]struct{}{}
	for _, s := range b {
		set[s] = struct{}{}
	}
	var out []string
	for _, s := range a {
		if _, ok := set[s]; !ok {
			out = append(out, s)
		}
	}
	sort.Strings(out)
	return out
}

func stringSetEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	am, bm := map[string]int{}, map[string]int{}
	for _, s := range a {
		am[s]++
	}
	for _, s := range b {
		bm[s]++
	}
	for k, v := range am {
		if bm[k] != v {
			return false
		}
	}
	return true
}

func joinSorted(vs []string) string {
	cp := append([]string{}, vs...)
	sort.Strings(cp)
	return fmt.Sprintf("%v", cp)
}

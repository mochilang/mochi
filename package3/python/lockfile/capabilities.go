package lockfile

import (
	"sort"

	"mochi/package3/python/typemap"
	"mochi/package3/python/wrapper"
)

// Capability tokens are the closed vocabulary the lockfile records and the
// Phase 15 attestation gate verifies against. The set is intentionally small
// so the lockfile diff stays human-readable; richer per-item capability data
// stays on the wrapper side.
const (
	CapAsync     = "async"
	CapDataclass = "dataclass"
	CapProtocol  = "protocol"
	CapCallable  = "callable"
	CapStream    = "stream"
	CapMap       = "map"
	CapSet       = "set"
	CapList      = "list"
	CapOptional  = "optional"
	CapSum       = "sum"
	CapTuple     = "tuple"
	CapConstant  = "constant"
	CapTypeVar   = "typevar"
)

// ExtractCapabilities walks a wrapper.Wrapper and returns the sorted, deduped
// capability list. The classification is intentionally conservative: only
// concrete Mochi type shapes contribute, so a wrapper whose surface fits
// inside scalars + records + functions emits an empty list (records produce
// the "dataclass" token, functions produce "callable").
func ExtractCapabilities(w *wrapper.Wrapper) []string {
	if w == nil {
		return nil
	}
	seen := map[string]struct{}{}
	for _, it := range w.Items {
		if it.IsAsync {
			seen[CapAsync] = struct{}{}
		}
		switch it.Kind {
		case wrapper.ItemRecord:
			seen[CapDataclass] = struct{}{}
		case wrapper.ItemInterface:
			seen[CapProtocol] = struct{}{}
		case wrapper.ItemConstant:
			seen[CapConstant] = struct{}{}
		}
		walkMochiType(it.Type, seen)
	}
	if len(seen) == 0 {
		return nil
	}
	out := make([]string, 0, len(seen))
	for k := range seen {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func walkMochiType(t typemap.MochiType, seen map[string]struct{}) {
	switch t.Kind {
	case typemap.KindList:
		seen[CapList] = struct{}{}
		walkParams(t, seen)
	case typemap.KindSet:
		seen[CapSet] = struct{}{}
		walkParams(t, seen)
	case typemap.KindMap:
		seen[CapMap] = struct{}{}
		walkParams(t, seen)
	case typemap.KindOptional:
		seen[CapOptional] = struct{}{}
		walkParams(t, seen)
	case typemap.KindSum:
		seen[CapSum] = struct{}{}
		walkParams(t, seen)
	case typemap.KindTuple:
		seen[CapTuple] = struct{}{}
		walkParams(t, seen)
	case typemap.KindFun:
		seen[CapCallable] = struct{}{}
		walkParams(t, seen)
	case typemap.KindAsync:
		seen[CapAsync] = struct{}{}
		walkParams(t, seen)
	case typemap.KindStream:
		seen[CapStream] = struct{}{}
		walkParams(t, seen)
	case typemap.KindRecord:
		seen[CapDataclass] = struct{}{}
		for _, f := range t.Fields {
			walkMochiType(f.Type, seen)
		}
	case typemap.KindInterface:
		seen[CapProtocol] = struct{}{}
		for _, m := range t.Methods {
			if m.IsAsync {
				seen[CapAsync] = struct{}{}
			}
			for _, p := range m.Params {
				walkMochiType(p, seen)
			}
			walkMochiType(m.Return, seen)
		}
	case typemap.KindTypeVar:
		seen[CapTypeVar] = struct{}{}
	}
}

func walkParams(t typemap.MochiType, seen map[string]struct{}) {
	for _, p := range t.Params {
		walkMochiType(p, seen)
	}
}

package wrapper

import (
	"mochi/package3/python/errors"
	"mochi/package3/python/typemap"
)

// EventLoopMode controls how async items are wrapped.
type EventLoopMode int

const (
	// EventLoopPerCall constructs a fresh asyncio loop for every call. Default.
	EventLoopPerCall EventLoopMode = iota
	// EventLoopPersistent reuses a process-global asyncio loop. Opt-in via
	// `[python] runtime = { event-loop = "persistent" }` in mochi.toml.
	EventLoopPersistent
)

func (m EventLoopMode) String() string {
	switch m {
	case EventLoopPerCall:
		return "per-call"
	case EventLoopPersistent:
		return "persistent"
	default:
		return "unknown"
	}
}

// ItemKind classifies a wrapper entry. The kinds match the MEP-71 §2 list:
// shape-coercing wrappers for functions, records, and interfaces.
type ItemKind int

const (
	// ItemFunc is a top-level function (sync or async).
	ItemFunc ItemKind = iota
	// ItemRecord is a wrapped record (TypedDict or frozen @dataclass).
	ItemRecord
	// ItemInterface is a wrapped Protocol surface.
	ItemInterface
	// ItemConstant is a re-exported module-level constant.
	ItemConstant
)

func (k ItemKind) String() string {
	switch k {
	case ItemFunc:
		return "fun"
	case ItemRecord:
		return "record"
	case ItemInterface:
		return "interface"
	case ItemConstant:
		return "const"
	default:
		return "unknown"
	}
}

// Item is one entry the wrapper exposes to the Mochi side. The Phase 6 extern
// emitter walks the Items list to produce `extern python fun` / `extern python
// type` declarations.
type Item struct {
	// Name is the wrapper-side identifier the Mochi extern binds to. For
	// async funcs, this is the synchronous shim's name (e.g. `get_sync`).
	Name string
	// SourceName is the dotted path of the item in the source package
	// (e.g. `httpx.get`).
	SourceName string
	// Kind classifies the item.
	Kind ItemKind
	// Type is the Mochi type signature. For functions, KindFun. For records,
	// KindRecord. For interfaces, KindInterface. For constants, the scalar.
	Type typemap.MochiType
	// IsAsync is true for async functions. The Python side emits a sync shim
	// that wraps `asyncio.run(...)`, and the Mochi-facing name uses the
	// `_sync` suffix.
	IsAsync bool
	// Loop records how an async item is wrapped. Ignored when IsAsync=false.
	Loop EventLoopMode
}

// Wrapper is the synthesised Python source set for one consumed PyPI package.
type Wrapper struct {
	// Package is the source PyPI distribution name (e.g. "httpx").
	Package string
	// Module is the wrapper module basename: `<Package>_externs`. The full
	// file path is `<Module>.py`.
	Module string
	// PySource is the contents of `<Module>.py`.
	PySource string
	// PYISource is the contents of `<Module>.pyi`.
	PYISource string
	// Items lists every successfully wrapped entry, in deterministic order
	// (sorted by SourceName).
	Items []Item
	// Skipped records every item Phase 4 or Phase 5 refused. The wrapper
	// emits an alongside `SKIPPED.txt` file that mirrors this slice.
	Skipped []errors.SkipReport
	// Loop is the EventLoopMode the wrapper was synthesised with.
	Loop EventLoopMode
}

// Options control the wrapper synthesiser.
type Options struct {
	// Loop selects the async event-loop mode for the whole wrapper.
	Loop EventLoopMode
	// AllowPartial mirrors the typemap.Mapper.AllowPartial flag: when true,
	// `Any` lowers to `ref<Any>` instead of being refused.
	AllowPartial bool
}

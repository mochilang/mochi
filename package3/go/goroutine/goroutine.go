// Package goroutine is the MEP-74 phase 14 goroutine bridge. It
// covers the cgo-handle pool that lets Go-side `chan T` and
// `func(...)` callback values survive a yield across the cgo
// boundary into the Mochi runtime.
//
// The package has two layers:
//
//  1. A real Go HandlePool that the bridge can use at runtime
//     to mint, resolve, and release opaque uint64 handle IDs
//     backed by `runtime/cgo.Handle`. The pool is concurrent-
//     safe, leak-free (release deletes the underlying cgo.Handle
//     so the GC can reclaim the value), and bounds-checked
//     against a caller-supplied max-handles soft cap (mirroring
//     the MEP-74 spec §`mochi.toml` `goroutine-bridge.max-handles`
//     setting; the underlying cgo.Handle is uint64 so the hard
//     limit is 2^64).
//
//  2. A pure renderer that emits the `mochi_rt.go` source file
//     a wrapper package needs to include when it exposes a
//     `chan T` or a `func(...)` callback in its public surface.
//     The renderer's output is the canonical handle-pool shape
//     described in MEP-74 §7; the runtime layer above is the
//     test-time stand-in for that generated code so the unit
//     tests can exercise the pool semantics without writing the
//     file to disk and shelling out to `go build`.
//
// A `NeedsRuntime` predicate lets the wrapper synthesiser
// (phase 6, the cgo wrapper) decide whether to include the
// runtime file in a given module's wrapper package: pure-sync
// modules (no chan, no callback param, no exported method whose
// signature mentions either) skip the runtime entirely and pay
// zero cgo-handle cost.
package goroutine

import (
	"errors"
	"fmt"
	"runtime/cgo"
	"strings"
	"sync"

	"mochi/package3/go/apisurface"
)

// ErrGoroutine is the package-wide error sentinel.
var ErrGoroutine = errors.New("goroutine")

// DefaultMaxHandles is the default soft cap on the number of
// live cgo handles. Mirrors the `mochi.toml`
// `[runtime] goroutine-bridge.max-handles = 4096` default per
// MEP-74 §`mochi.toml` reference. The cap is per-pool, not
// per-process; a wrapper package that hits the cap returns a
// pool-exhausted error rather than panicking, so the caller can
// drain channels and retry.
const DefaultMaxHandles = 4096

// HandlePool is a concurrent, bounded, leak-free pool of
// runtime/cgo handles keyed by opaque uint64 IDs. The pool is
// the runtime stand-in for the generated `mochi_rt.go` handle
// pool: the generated file embeds the same shape inline so the
// wrapper package owns its own pool instance per module (so a
// module-A leak cannot exhaust a module-B pool).
//
// Concurrency: every method is safe to call from multiple
// goroutines concurrently. The mutex is fine-grained over the
// map only; the underlying cgo.NewHandle / Handle.Delete calls
// (which acquire their own internal lock) happen outside the
// critical section so a `Release` cannot block an `Acquire` for
// a different ID.
type HandlePool struct {
	max     int
	mu      sync.Mutex
	nextID  uint64
	handles map[uint64]cgo.Handle
}

// NewHandlePool constructs a HandlePool with the given soft cap.
// A max of 0 picks DefaultMaxHandles; a negative max disables
// the cap (only the uint64 ID space limits the pool).
func NewHandlePool(max int) *HandlePool {
	if max == 0 {
		max = DefaultMaxHandles
	}
	return &HandlePool{
		max:     max,
		handles: map[uint64]cgo.Handle{},
	}
}

// Acquire mints a fresh opaque ID and binds it to v. The ID is
// the value the wrapper passes across the cgo boundary; resolve
// recovers v. Returns an error if the pool is at its soft cap.
func (p *HandlePool) Acquire(v any) (uint64, error) {
	p.mu.Lock()
	if p.max > 0 && len(p.handles) >= p.max {
		p.mu.Unlock()
		return 0, fmt.Errorf("%w: pool exhausted (max=%d, live=%d)", ErrGoroutine, p.max, len(p.handles))
	}
	p.nextID++
	id := p.nextID
	h := cgo.NewHandle(v)
	p.handles[id] = h
	p.mu.Unlock()
	return id, nil
}

// Resolve returns the value bound to id, or false if id is not
// live in this pool. A two-result return mirrors a map lookup;
// the alternative of a single-result `(v any, err error)` would
// force the caller to allocate an error on every miss, which
// matters in the hot channel-recv loop.
func (p *HandlePool) Resolve(id uint64) (any, bool) {
	p.mu.Lock()
	h, ok := p.handles[id]
	p.mu.Unlock()
	if !ok {
		return nil, false
	}
	return h.Value(), true
}

// Release deletes id from the pool and calls Delete on the
// underlying cgo.Handle so the GC can reclaim the referenced
// value. A double-release is a no-op (returns false on the
// second call); a release of a never-acquired id is also a
// no-op.
func (p *HandlePool) Release(id uint64) bool {
	p.mu.Lock()
	h, ok := p.handles[id]
	if ok {
		delete(p.handles, id)
	}
	p.mu.Unlock()
	if !ok {
		return false
	}
	h.Delete()
	return true
}

// Live reports the number of currently-held handles. Useful for
// leak detection at test-time; the wrapper itself does not call
// this on the hot path.
func (p *HandlePool) Live() int {
	p.mu.Lock()
	defer p.mu.Unlock()
	return len(p.handles)
}

// NeedsRuntime reports whether the wrapper package for pkg
// needs to include the `mochi_rt.go` handle-pool file. It
// returns true when any exported function in pkg has a
// parameter, result, or method that contains a `chan` type or
// a `func` (callback) type. Pure-sync surfaces skip the runtime
// entirely.
//
// The walk is conservative-positive: any reachable `chan T` or
// `func(...)` in an exported signature flips the answer to
// true. Unparseable type strings are treated as "needs runtime"
// to fail safe (an erroneous skip would leave a wrapper without
// the runtime file the cgo build expects).
func NeedsRuntime(pkg apisurface.Package) bool {
	for _, f := range pkg.Funcs {
		if funcNeedsRuntime(f) {
			return true
		}
	}
	for _, t := range pkg.Types {
		for _, m := range t.Methods {
			if funcNeedsRuntime(m) {
				return true
			}
		}
		for _, m := range t.InterfaceMethods {
			if funcNeedsRuntime(m) {
				return true
			}
		}
	}
	return false
}

func funcNeedsRuntime(f apisurface.Func) bool {
	for _, p := range f.Params {
		if typeStringNeedsRuntime(p.Type) {
			return true
		}
	}
	for _, r := range f.Results {
		if typeStringNeedsRuntime(r.Type) {
			return true
		}
	}
	return false
}

func typeStringNeedsRuntime(s string) bool {
	t, err := apisurface.ParseType(s)
	if err != nil {
		// Treat parse failures as "needs runtime" so the wrapper
		// fails safe; phase 4 (apisurface parser) is responsible
		// for catching the syntactic error separately.
		return true
	}
	return typeNeedsRuntime(t)
}

func typeNeedsRuntime(t apisurface.GoType) bool {
	switch v := t.(type) {
	case apisurface.ChanType:
		return true
	case apisurface.FuncType:
		return true
	case apisurface.PointerType:
		return typeNeedsRuntime(v.Elem)
	case apisurface.SliceType:
		return typeNeedsRuntime(v.Elem)
	case apisurface.ArrayType:
		return typeNeedsRuntime(v.Elem)
	case apisurface.MapType:
		return typeNeedsRuntime(v.Key) || typeNeedsRuntime(v.Value)
	case apisurface.EllipsisType:
		return typeNeedsRuntime(v.Elem)
	case apisurface.NamedType:
		for _, a := range v.TypeArgs {
			if typeNeedsRuntime(a) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

// RenderRuntime returns the source of the `mochi_rt.go` file the
// wrapper synthesiser should drop into the wrapper package for
// pkgName. The output is byte-deterministic: callers can sha256
// it directly for `wrapper-sha256` lockfile pinning per MEP-74
// phase 10.
//
// The generated file uses the `mochi_wrap` build tag so a
// downstream consumer that imports the wrapper outside of a cgo
// c-archive build (e.g. for go-vet on the source tree) does not
// accidentally pull in the runtime/cgo dependency. Phase 9
// (build orchestration) sets the build tag when invoking
// `go build -tags=mochi_wrap`.
func RenderRuntime(pkgName string) string {
	if pkgName == "" {
		pkgName = "gowrap"
	}
	var sb strings.Builder
	sb.WriteString("//go:build mochi_wrap\n")
	sb.WriteString("\n")
	sb.WriteString("// Code generated by mochi MEP-74 phase 14 (goroutine bridge). DO NOT EDIT.\n")
	sb.WriteString("\n")
	sb.WriteString("package ")
	sb.WriteString(pkgName)
	sb.WriteString("\n\n")
	sb.WriteString("import (\n")
	sb.WriteString("\t\"runtime/cgo\"\n")
	sb.WriteString("\t\"sync\"\n")
	sb.WriteString(")\n\n")
	sb.WriteString("var (\n")
	sb.WriteString("\tmochiHandlesMu sync.Mutex\n")
	sb.WriteString("\tmochiHandles   = map[uint64]cgo.Handle{}\n")
	sb.WriteString("\tmochiNextID    uint64\n")
	sb.WriteString(")\n\n")
	sb.WriteString("// mochiAcquireHandle binds v to a fresh uint64 ID and stores\n")
	sb.WriteString("// the underlying cgo.Handle in the package-local map. The\n")
	sb.WriteString("// caller passes the returned ID across the cgo boundary as\n")
	sb.WriteString("// the opaque channel-or-callback handle.\n")
	sb.WriteString("func mochiAcquireHandle(v any) uint64 {\n")
	sb.WriteString("\tmochiHandlesMu.Lock()\n")
	sb.WriteString("\tmochiNextID++\n")
	sb.WriteString("\tid := mochiNextID\n")
	sb.WriteString("\th := cgo.NewHandle(v)\n")
	sb.WriteString("\tmochiHandles[id] = h\n")
	sb.WriteString("\tmochiHandlesMu.Unlock()\n")
	sb.WriteString("\treturn id\n")
	sb.WriteString("}\n\n")
	sb.WriteString("// mochiResolveHandle returns the value bound to id, or nil\n")
	sb.WriteString("// if id is not live. A double-resolve returns the value\n")
	sb.WriteString("// (Resolve does not delete the handle).\n")
	sb.WriteString("func mochiResolveHandle(id uint64) any {\n")
	sb.WriteString("\tmochiHandlesMu.Lock()\n")
	sb.WriteString("\th, ok := mochiHandles[id]\n")
	sb.WriteString("\tmochiHandlesMu.Unlock()\n")
	sb.WriteString("\tif !ok {\n")
	sb.WriteString("\t\treturn nil\n")
	sb.WriteString("\t}\n")
	sb.WriteString("\treturn h.Value()\n")
	sb.WriteString("}\n\n")
	sb.WriteString("// mochiReleaseHandle deletes id from the map and calls\n")
	sb.WriteString("// Delete on the underlying cgo.Handle so the GC can\n")
	sb.WriteString("// reclaim the referenced value. A double-release is a\n")
	sb.WriteString("// no-op (returns false on the second call).\n")
	sb.WriteString("func mochiReleaseHandle(id uint64) bool {\n")
	sb.WriteString("\tmochiHandlesMu.Lock()\n")
	sb.WriteString("\th, ok := mochiHandles[id]\n")
	sb.WriteString("\tif ok {\n")
	sb.WriteString("\t\tdelete(mochiHandles, id)\n")
	sb.WriteString("\t}\n")
	sb.WriteString("\tmochiHandlesMu.Unlock()\n")
	sb.WriteString("\tif !ok {\n")
	sb.WriteString("\t\treturn false\n")
	sb.WriteString("\t}\n")
	sb.WriteString("\th.Delete()\n")
	sb.WriteString("\treturn true\n")
	sb.WriteString("}\n")
	return sb.String()
}

// ChannelShim describes one cgo-exported `chan T` shim the
// wrapper needs to emit. The shim has the canonical four-symbol
// surface (`_new`, `_send`, `_recv`, `_close`) so the Mochi-side
// extern fn emitter (phase 7) can bind to it from a closed set
// of name patterns.
type ChannelShim struct {
	// ModuleFlatName is the wrapper package's module-flat name
	// (e.g. "github_com_user_lib"); used as a prefix on the
	// emitted symbol names.
	ModuleFlatName string
	// ElemGoType is the element type as a Go source expression
	// (e.g. "int64", "string", "*github.com/foo/lib.Thing").
	ElemGoType string
	// ElemCType is the C-side representation per the phase 5
	// type-mapping table (e.g. "int64_t", "MochiString"). Used
	// in the //export comment doc strings.
	ElemCType string
	// SymbolBase is the base symbol name (e.g. "Counter" yields
	// `mochi_<module>_Counter_chan_new` and so on). When empty,
	// the shim is unnamed and uses the elem type as the base.
	SymbolBase string
	// BufferSize is the default channel buffer size; mirrors
	// `mochi.toml` `goroutine-bridge.default-buffer` (default 1
	// per MEP-74 spec).
	BufferSize int
}

// RenderChannelShim returns the Go source for one ChannelShim.
// The shim exports four functions over the cgo boundary:
//
//	mochi_<module>_<base>_chan_new(buf int64) uint64        - create the chan, return its handle ID
//	mochi_<module>_<base>_chan_send(id uint64, v <Elem>)    - blocking send
//	mochi_<module>_<base>_chan_recv(id uint64) (<Elem>, bool) - blocking recv; ok=false on closed+drained
//	mochi_<module>_<base>_chan_close(id uint64)             - close the chan and release the handle
//
// The `_send` / `_recv` overloads use the Go-side type directly;
// the cgo wrapper synthesiser (phase 6) is responsible for the
// C-side argument marshalling. Phase 14 emits only the Go side.
func RenderChannelShim(s ChannelShim) (string, error) {
	if s.ElemGoType == "" {
		return "", fmt.Errorf("%w: ChannelShim.ElemGoType is required", ErrGoroutine)
	}
	if s.ModuleFlatName == "" {
		return "", fmt.Errorf("%w: ChannelShim.ModuleFlatName is required", ErrGoroutine)
	}
	base := s.SymbolBase
	if base == "" {
		base = "value"
	}
	buf := s.BufferSize
	if buf <= 0 {
		buf = 1
	}
	prefix := "mochi_" + s.ModuleFlatName + "_" + base + "_chan"
	var sb strings.Builder
	sb.WriteString("// " + prefix + "_new constructs a buffered channel of\n")
	sb.WriteString("// " + s.ElemGoType + " with the given buffer size and returns\n")
	sb.WriteString("// the opaque handle ID. A buffer of 0 falls back to the\n")
	sb.WriteString("// per-package default.\n")
	sb.WriteString("//\n")
	sb.WriteString("//export " + prefix + "_new\n")
	sb.WriteString("func " + prefix + "_new(buf int64) uint64 {\n")
	sb.WriteString("\tn := int(buf)\n")
	sb.WriteString(fmt.Sprintf("\tif n <= 0 {\n\t\tn = %d\n\t}\n", buf))
	sb.WriteString("\tch := make(chan " + s.ElemGoType + ", n)\n")
	sb.WriteString("\treturn mochiAcquireHandle(ch)\n")
	sb.WriteString("}\n\n")

	sb.WriteString("// " + prefix + "_send is a blocking send of v on the\n")
	sb.WriteString("// channel identified by id.\n")
	sb.WriteString("//\n")
	sb.WriteString("//export " + prefix + "_send\n")
	sb.WriteString("func " + prefix + "_send(id uint64, v " + s.ElemGoType + ") {\n")
	sb.WriteString("\tch := mochiResolveHandle(id).(chan " + s.ElemGoType + ")\n")
	sb.WriteString("\tch <- v\n")
	sb.WriteString("}\n\n")

	sb.WriteString("// " + prefix + "_recv is a blocking receive on the\n")
	sb.WriteString("// channel identified by id. ok==false signals that the\n")
	sb.WriteString("// channel is closed and drained.\n")
	sb.WriteString("//\n")
	sb.WriteString("//export " + prefix + "_recv\n")
	sb.WriteString("func " + prefix + "_recv(id uint64) (" + s.ElemGoType + ", bool) {\n")
	sb.WriteString("\tch := mochiResolveHandle(id).(chan " + s.ElemGoType + ")\n")
	sb.WriteString("\tv, ok := <-ch\n")
	sb.WriteString("\treturn v, ok\n")
	sb.WriteString("}\n\n")

	sb.WriteString("// " + prefix + "_close closes the channel identified by\n")
	sb.WriteString("// id and releases its handle. After close, _send panics\n")
	sb.WriteString("// (matching Go semantics); _recv drains the buffer and\n")
	sb.WriteString("// then returns ok==false on every subsequent call.\n")
	sb.WriteString("//\n")
	sb.WriteString("//export " + prefix + "_close\n")
	sb.WriteString("func " + prefix + "_close(id uint64) {\n")
	sb.WriteString("\tv := mochiResolveHandle(id)\n")
	sb.WriteString("\tif v == nil {\n")
	sb.WriteString("\t\treturn\n")
	sb.WriteString("\t}\n")
	sb.WriteString("\tch := v.(chan " + s.ElemGoType + ")\n")
	sb.WriteString("\tclose(ch)\n")
	sb.WriteString("\tmochiReleaseHandle(id)\n")
	sb.WriteString("}\n")
	return sb.String(), nil
}

// CallbackShim describes one cgo-exported callback-as-handle
// shim. A Mochi-side function value is hoisted into a handle on
// the Go side; the wrapper invokes the function by handle ID
// through the `_call` symbol. The handle is released via the
// `_release` symbol when the callback is no longer needed.
type CallbackShim struct {
	// ModuleFlatName is the wrapper package's module-flat name.
	ModuleFlatName string
	// SymbolBase is the base symbol name (e.g. "OnEvent" yields
	// `mochi_<module>_OnEvent_cb_call`).
	SymbolBase string
	// Signature is the Go-source callback function signature
	// (e.g. "func(int64) string"). The shim _call symbol mirrors
	// the parameter and result list one-to-one.
	Signature string
}

// RenderCallbackShim returns the Go source for one CallbackShim.
// The shim exports two functions over the cgo boundary:
//
//	mochi_<module>_<base>_cb_call(id uint64, <params...>) <results...>
//	mochi_<module>_<base>_cb_release(id uint64)
//
// The caller is responsible for putting the actual function
// value into the handle pool via mochiAcquireHandle before
// passing the ID to the wrapper (the C side of the bridge owns
// the lifetime of the handle; the Go side just resolves it on
// each call).
func RenderCallbackShim(s CallbackShim) (string, error) {
	if s.ModuleFlatName == "" {
		return "", fmt.Errorf("%w: CallbackShim.ModuleFlatName is required", ErrGoroutine)
	}
	if s.SymbolBase == "" {
		return "", fmt.Errorf("%w: CallbackShim.SymbolBase is required", ErrGoroutine)
	}
	if s.Signature == "" {
		return "", fmt.Errorf("%w: CallbackShim.Signature is required", ErrGoroutine)
	}
	parsed, err := apisurface.ParseType(s.Signature)
	if err != nil {
		return "", fmt.Errorf("%w: bad signature %q: %v", ErrGoroutine, s.Signature, err)
	}
	ft, ok := parsed.(apisurface.FuncType)
	if !ok {
		return "", fmt.Errorf("%w: signature %q is not a func type", ErrGoroutine, s.Signature)
	}

	paramNames := make([]string, len(ft.Params))
	for i := range ft.Params {
		paramNames[i] = fmt.Sprintf("p%d", i)
	}
	paramDecls := make([]string, len(ft.Params))
	for i, p := range ft.Params {
		paramDecls[i] = paramNames[i] + " " + p.String()
	}
	resultDecls := make([]string, len(ft.Results))
	for i, r := range ft.Results {
		resultDecls[i] = r.String()
	}

	prefix := "mochi_" + s.ModuleFlatName + "_" + s.SymbolBase + "_cb"
	var sb strings.Builder
	sb.WriteString("// " + prefix + "_call invokes the callback identified by\n")
	sb.WriteString("// id with the supplied arguments. The handle must have\n")
	sb.WriteString("// been registered via mochiAcquireHandle by the cgo C\n")
	sb.WriteString("// side before this is called.\n")
	sb.WriteString("//\n")
	sb.WriteString("//export " + prefix + "_call\n")
	sb.WriteString("func " + prefix + "_call(id uint64")
	for _, d := range paramDecls {
		sb.WriteString(", ")
		sb.WriteString(d)
	}
	sb.WriteString(")")
	switch len(resultDecls) {
	case 0:
	case 1:
		sb.WriteString(" " + resultDecls[0])
	default:
		sb.WriteString(" (")
		sb.WriteString(strings.Join(resultDecls, ", "))
		sb.WriteString(")")
	}
	sb.WriteString(" {\n")
	sb.WriteString("\tfn := mochiResolveHandle(id).(" + s.Signature + ")\n")
	if len(ft.Results) == 0 {
		sb.WriteString("\tfn(")
		sb.WriteString(strings.Join(paramNames, ", "))
		sb.WriteString(")\n")
	} else {
		sb.WriteString("\treturn fn(")
		sb.WriteString(strings.Join(paramNames, ", "))
		sb.WriteString(")\n")
	}
	sb.WriteString("}\n\n")

	sb.WriteString("// " + prefix + "_release deletes the callback handle so\n")
	sb.WriteString("// the GC can reclaim the underlying function value.\n")
	sb.WriteString("//\n")
	sb.WriteString("//export " + prefix + "_release\n")
	sb.WriteString("func " + prefix + "_release(id uint64) {\n")
	sb.WriteString("\tmochiReleaseHandle(id)\n")
	sb.WriteString("}\n")
	return sb.String(), nil
}

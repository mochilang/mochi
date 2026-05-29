package goroutine

import (
	"errors"
	"strings"
	"sync"
	"testing"

	"mochi/package3/go/apisurface"
)

func TestHandlePoolAcquireResolveRelease(t *testing.T) {
	p := NewHandlePool(0)
	id, err := p.Acquire("hello")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	if id == 0 {
		t.Errorf("expected non-zero id")
	}
	v, ok := p.Resolve(id)
	if !ok {
		t.Fatalf("Resolve missed live id %d", id)
	}
	if v.(string) != "hello" {
		t.Errorf("Resolve = %v; want hello", v)
	}
	if !p.Release(id) {
		t.Errorf("Release should report true on first release")
	}
	if p.Release(id) {
		t.Errorf("Release should report false on second release")
	}
	if _, ok := p.Resolve(id); ok {
		t.Errorf("released id should not resolve")
	}
}

func TestHandlePoolAcquireRespectsMaxHandles(t *testing.T) {
	p := NewHandlePool(2)
	id1, err := p.Acquire("a")
	if err != nil {
		t.Fatalf("Acquire 1: %v", err)
	}
	id2, err := p.Acquire("b")
	if err != nil {
		t.Fatalf("Acquire 2: %v", err)
	}
	if _, err := p.Acquire("c"); err == nil || !errors.Is(err, ErrGoroutine) {
		t.Errorf("Acquire 3 should report ErrGoroutine; got %v", err)
	}
	_ = id2
	if p.Live() != 2 {
		t.Errorf("Live() = %d; want 2", p.Live())
	}
	// Releasing one should free the slot.
	p.Release(id1)
	if _, err := p.Acquire("c-retry"); err != nil {
		t.Errorf("Acquire after Release should succeed: %v", err)
	}
}

func TestHandlePoolAcquireUnboundedWhenMaxNegative(t *testing.T) {
	p := NewHandlePool(-1)
	for i := 0; i < 100; i++ {
		if _, err := p.Acquire(i); err != nil {
			t.Fatalf("Acquire %d (unbounded pool): %v", i, err)
		}
	}
	if p.Live() != 100 {
		t.Errorf("Live() = %d; want 100", p.Live())
	}
}

func TestHandlePoolResolveMissReturnsFalse(t *testing.T) {
	p := NewHandlePool(0)
	v, ok := p.Resolve(9999)
	if ok {
		t.Errorf("expected miss; got v=%v", v)
	}
}

func TestHandlePoolReleaseUnknownReturnsFalse(t *testing.T) {
	p := NewHandlePool(0)
	if p.Release(9999) {
		t.Errorf("Release of unknown id should return false")
	}
}

func TestHandlePoolConcurrentAcquireRelease(t *testing.T) {
	p := NewHandlePool(0)
	const n = 200
	var wg sync.WaitGroup
	wg.Add(n)
	ids := make([]uint64, n)
	for i := 0; i < n; i++ {
		go func(i int) {
			defer wg.Done()
			id, err := p.Acquire(i)
			if err != nil {
				t.Errorf("Acquire %d: %v", i, err)
				return
			}
			ids[i] = id
		}(i)
	}
	wg.Wait()
	// All ids must be unique.
	seen := map[uint64]bool{}
	for _, id := range ids {
		if id == 0 {
			t.Errorf("zero id in concurrent acquire")
			continue
		}
		if seen[id] {
			t.Errorf("duplicate id %d under concurrent acquire", id)
		}
		seen[id] = true
	}
	// Release everything.
	wg.Add(n)
	for _, id := range ids {
		go func(id uint64) {
			defer wg.Done()
			p.Release(id)
		}(id)
	}
	wg.Wait()
	if p.Live() != 0 {
		t.Errorf("Live() after Release sweep = %d; want 0", p.Live())
	}
}

func TestNeedsRuntimeDetectsChannelInFuncParam(t *testing.T) {
	pkg := apisurface.Package{
		Funcs: []apisurface.Func{
			{Name: "Pipe", Params: []apisurface.Param{{Type: "chan int"}}},
		},
	}
	if !NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should be true for chan param")
	}
}

func TestNeedsRuntimeDetectsChannelInFuncResult(t *testing.T) {
	pkg := apisurface.Package{
		Funcs: []apisurface.Func{
			{Name: "Take", Results: []apisurface.Param{{Type: "<-chan string"}}},
		},
	}
	if !NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should be true for chan result")
	}
}

func TestNeedsRuntimeDetectsCallback(t *testing.T) {
	pkg := apisurface.Package{
		Funcs: []apisurface.Func{
			{Name: "WithHook", Params: []apisurface.Param{{Type: "func(int64) string"}}},
		},
	}
	if !NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should be true for callback param")
	}
}

func TestNeedsRuntimeDetectsChannelNestedInSlice(t *testing.T) {
	pkg := apisurface.Package{
		Funcs: []apisurface.Func{
			{Name: "Fan", Results: []apisurface.Param{{Type: "[]chan int"}}},
		},
	}
	if !NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should be true for []chan T result")
	}
}

func TestNeedsRuntimeDetectsCallbackInMapValue(t *testing.T) {
	pkg := apisurface.Package{
		Funcs: []apisurface.Func{
			{Name: "Dispatch", Params: []apisurface.Param{{Type: "map[string]func()"}}},
		},
	}
	if !NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should be true for callback nested in map value")
	}
}

func TestNeedsRuntimeFalseForPureSync(t *testing.T) {
	pkg := apisurface.Package{
		Funcs: []apisurface.Func{
			{Name: "Add", Params: []apisurface.Param{{Type: "int64"}, {Type: "int64"}}, Results: []apisurface.Param{{Type: "int64"}}},
			{Name: "Hello", Results: []apisurface.Param{{Type: "string"}}},
		},
		Types: []apisurface.Type{
			{Name: "Counter", Kind: apisurface.KindStruct, Methods: []apisurface.Func{
				{Name: "Inc"},
				{Name: "Value", Results: []apisurface.Param{{Type: "int64"}}},
			}},
		},
	}
	if NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should be false for pure-sync API")
	}
}

func TestNeedsRuntimeTreatsParseErrorAsTrue(t *testing.T) {
	pkg := apisurface.Package{
		Funcs: []apisurface.Func{
			{Name: "Borked", Params: []apisurface.Param{{Type: "@@@not a type@@@"}}},
		},
	}
	if !NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should fail safe to true on parse error")
	}
}

func TestNeedsRuntimeDetectsChannelInMethod(t *testing.T) {
	pkg := apisurface.Package{
		Types: []apisurface.Type{
			{Name: "Worker", Kind: apisurface.KindStruct, Methods: []apisurface.Func{
				{Name: "Out", Results: []apisurface.Param{{Type: "chan int"}}},
			}},
		},
	}
	if !NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should be true for chan in method result")
	}
}

func TestNeedsRuntimeDetectsCallbackInInterfaceMethod(t *testing.T) {
	pkg := apisurface.Package{
		Types: []apisurface.Type{
			{Name: "Notifier", Kind: apisurface.KindInterface, InterfaceMethods: []apisurface.Func{
				{Name: "OnEvent", Params: []apisurface.Param{{Type: "func()"}}},
			}},
		},
	}
	if !NeedsRuntime(pkg) {
		t.Errorf("NeedsRuntime should be true for callback in interface method")
	}
}

func TestRenderRuntimeContainsCanonicalHeader(t *testing.T) {
	src := RenderRuntime("gowrap_demo")
	if !strings.HasPrefix(src, "//go:build mochi_wrap\n") {
		t.Errorf("RenderRuntime missing build tag")
	}
	if !strings.Contains(src, "package gowrap_demo\n") {
		t.Errorf("RenderRuntime missing package clause")
	}
	if !strings.Contains(src, "\"runtime/cgo\"") {
		t.Errorf("RenderRuntime missing runtime/cgo import")
	}
	if !strings.Contains(src, "func mochiAcquireHandle(v any) uint64") {
		t.Errorf("RenderRuntime missing mochiAcquireHandle")
	}
	if !strings.Contains(src, "func mochiResolveHandle(id uint64) any") {
		t.Errorf("RenderRuntime missing mochiResolveHandle")
	}
	if !strings.Contains(src, "func mochiReleaseHandle(id uint64) bool") {
		t.Errorf("RenderRuntime missing mochiReleaseHandle")
	}
}

func TestRenderRuntimeFallsBackToDefaultPackageName(t *testing.T) {
	src := RenderRuntime("")
	if !strings.Contains(src, "package gowrap\n") {
		t.Errorf("RenderRuntime should default to package gowrap; got:\n%s", src)
	}
}

func TestRenderRuntimeIsByteDeterministic(t *testing.T) {
	a := RenderRuntime("gowrap_x")
	b := RenderRuntime("gowrap_x")
	if a != b {
		t.Errorf("RenderRuntime drift between back-to-back calls")
	}
}

func TestRenderChannelShimRequiresElemGoType(t *testing.T) {
	_, err := RenderChannelShim(ChannelShim{ModuleFlatName: "m"})
	if err == nil || !errors.Is(err, ErrGoroutine) {
		t.Errorf("expected ElemGoType-required error, got %v", err)
	}
}

func TestRenderChannelShimRequiresModuleFlatName(t *testing.T) {
	_, err := RenderChannelShim(ChannelShim{ElemGoType: "int64"})
	if err == nil || !errors.Is(err, ErrGoroutine) {
		t.Errorf("expected ModuleFlatName-required error, got %v", err)
	}
}

func TestRenderChannelShimCoversTheFourSymbols(t *testing.T) {
	src, err := RenderChannelShim(ChannelShim{
		ModuleFlatName: "github_com_x_y",
		ElemGoType:     "int64",
		ElemCType:      "int64_t",
		SymbolBase:     "Counter",
		BufferSize:     8,
	})
	if err != nil {
		t.Fatalf("RenderChannelShim: %v", err)
	}
	wantSymbols := []string{
		"//export mochi_github_com_x_y_Counter_chan_new",
		"//export mochi_github_com_x_y_Counter_chan_send",
		"//export mochi_github_com_x_y_Counter_chan_recv",
		"//export mochi_github_com_x_y_Counter_chan_close",
	}
	for _, want := range wantSymbols {
		if !strings.Contains(src, want) {
			t.Errorf("rendered channel shim missing %s; got:\n%s", want, src)
		}
	}
	if !strings.Contains(src, "make(chan int64, n)") {
		t.Errorf("expected make(chan int64, n) in rendered shim")
	}
}

func TestRenderChannelShimDefaultsSymbolBaseAndBuffer(t *testing.T) {
	src, err := RenderChannelShim(ChannelShim{
		ModuleFlatName: "m",
		ElemGoType:     "string",
	})
	if err != nil {
		t.Fatalf("RenderChannelShim: %v", err)
	}
	if !strings.Contains(src, "mochi_m_value_chan_new") {
		t.Errorf("expected default symbol base 'value' when SymbolBase empty")
	}
	if !strings.Contains(src, "n = 1") {
		t.Errorf("expected default buffer size 1 when BufferSize 0")
	}
}

func TestRenderCallbackShimMatchesSignature(t *testing.T) {
	src, err := RenderCallbackShim(CallbackShim{
		ModuleFlatName: "lib",
		SymbolBase:     "OnEvent",
		Signature:      "func(int64, string) bool",
	})
	if err != nil {
		t.Fatalf("RenderCallbackShim: %v", err)
	}
	if !strings.Contains(src, "//export mochi_lib_OnEvent_cb_call") {
		t.Errorf("call export missing")
	}
	if !strings.Contains(src, "//export mochi_lib_OnEvent_cb_release") {
		t.Errorf("release export missing")
	}
	if !strings.Contains(src, "func mochi_lib_OnEvent_cb_call(id uint64, p0 int64, p1 string) bool {") {
		t.Errorf("call signature wrong; got:\n%s", src)
	}
	if !strings.Contains(src, "return fn(p0, p1)") {
		t.Errorf("expected return fn(p0, p1) body")
	}
}

func TestRenderCallbackShimNoResults(t *testing.T) {
	src, err := RenderCallbackShim(CallbackShim{
		ModuleFlatName: "lib",
		SymbolBase:     "Done",
		Signature:      "func()",
	})
	if err != nil {
		t.Fatalf("RenderCallbackShim: %v", err)
	}
	if !strings.Contains(src, "func mochi_lib_Done_cb_call(id uint64) {") {
		t.Errorf("zero-arg, zero-result call signature wrong; got:\n%s", src)
	}
	if strings.Contains(src, "return fn(") {
		t.Errorf("zero-result body must not start with 'return'")
	}
	if !strings.Contains(src, "\tfn()") {
		t.Errorf("expected bare fn() invocation; got:\n%s", src)
	}
}

func TestRenderCallbackShimMultipleResults(t *testing.T) {
	src, err := RenderCallbackShim(CallbackShim{
		ModuleFlatName: "lib",
		SymbolBase:     "Pair",
		Signature:      "func(int64) (string, error)",
	})
	if err != nil {
		t.Fatalf("RenderCallbackShim: %v", err)
	}
	if !strings.Contains(src, "func mochi_lib_Pair_cb_call(id uint64, p0 int64) (string, error) {") {
		t.Errorf("multi-result call signature wrong; got:\n%s", src)
	}
}

func TestRenderCallbackShimRejectsBadSignature(t *testing.T) {
	_, err := RenderCallbackShim(CallbackShim{
		ModuleFlatName: "lib",
		SymbolBase:     "Bad",
		Signature:      "not a func",
	})
	if err == nil {
		t.Errorf("expected error for non-func signature")
	}
}

func TestRenderCallbackShimRequiresFields(t *testing.T) {
	cases := []struct {
		name string
		cb   CallbackShim
	}{
		{"missing module", CallbackShim{SymbolBase: "X", Signature: "func()"}},
		{"missing symbol", CallbackShim{ModuleFlatName: "m", Signature: "func()"}},
		{"missing signature", CallbackShim{ModuleFlatName: "m", SymbolBase: "X"}},
	}
	for _, tc := range cases {
		if _, err := RenderCallbackShim(tc.cb); err == nil || !errors.Is(err, ErrGoroutine) {
			t.Errorf("%s: expected ErrGoroutine; got %v", tc.name, err)
		}
	}
}

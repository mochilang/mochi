package asyncbridge_test

import (
	"strings"
	"testing"

	"mochi/package3/dotnet/asyncbridge"
	"mochi/package3/dotnet/shimgen"
	"mochi/package3/dotnet/typemap"
)

// ---------- DefaultConfig ----------

func TestDefaultConfig(t *testing.T) {
	cfg := asyncbridge.DefaultConfig()
	if cfg.Mode != asyncbridge.DispatchGetAwaiter {
		t.Errorf("expected DispatchGetAwaiter, got %s", cfg.Mode)
	}
}

// ---------- EmitAsyncWrapper: DispatchGetAwaiter ----------

func TestEmitAsyncWrapper_GetAwaiter_string(t *testing.T) {
	got := asyncbridge.EmitAsyncWrapper("MyClass.Method(arg)", typemap.KindString, asyncbridge.DispatchGetAwaiter)
	if !strings.Contains(got, "return") {
		t.Errorf("expected return in body, got: %s", got)
	}
	if !strings.Contains(got, ".GetAwaiter().GetResult()") {
		t.Errorf("expected .GetAwaiter().GetResult(), got: %s", got)
	}
	if !strings.Contains(got, "MyClass.Method(arg)") {
		t.Errorf("expected call expr in body, got: %s", got)
	}
}

func TestEmitAsyncWrapper_GetAwaiter_unit(t *testing.T) {
	got := asyncbridge.EmitAsyncWrapper("MyClass.DoVoid()", typemap.KindUnit, asyncbridge.DispatchGetAwaiter)
	if strings.HasPrefix(strings.TrimSpace(got), "return") {
		t.Errorf("expected no return for void, got: %s", got)
	}
	if !strings.Contains(got, ".GetAwaiter().GetResult()") {
		t.Errorf("expected .GetAwaiter().GetResult(), got: %s", got)
	}
}

// ---------- EmitAsyncWrapper: DispatchManualReset ----------

func TestEmitAsyncWrapper_ManualReset_string(t *testing.T) {
	got := asyncbridge.EmitAsyncWrapper("Svc.GetData(x)", typemap.KindString, asyncbridge.DispatchManualReset)
	if !strings.Contains(got, "ManualResetEventSlim") {
		t.Errorf("expected ManualResetEventSlim, got: %s", got)
	}
	if !strings.Contains(got, "ConfigureAwait(false)") {
		t.Errorf("expected ConfigureAwait(false), got: %s", got)
	}
	if !strings.Contains(got, "return __result") {
		t.Errorf("expected return __result, got: %s", got)
	}
	if !strings.Contains(got, "ExceptionDispatchInfo") {
		t.Errorf("expected ExceptionDispatchInfo, got: %s", got)
	}
}

func TestEmitAsyncWrapper_ManualReset_unit(t *testing.T) {
	got := asyncbridge.EmitAsyncWrapper("Svc.DoAsync()", typemap.KindUnit, asyncbridge.DispatchManualReset)
	if !strings.Contains(got, "ManualResetEventSlim") {
		t.Errorf("expected ManualResetEventSlim, got: %s", got)
	}
	if strings.Contains(got, "return __result") {
		t.Errorf("should not have return __result for void, got: %s", got)
	}
	if !strings.Contains(got, "__mre.Wait()") {
		t.Errorf("expected __mre.Wait(), got: %s", got)
	}
}

func TestEmitAsyncWrapper_ManualReset_int(t *testing.T) {
	got := asyncbridge.EmitAsyncWrapper("Counter.GetAsync()", typemap.KindInt, asyncbridge.DispatchManualReset)
	if !strings.Contains(got, "int __result") {
		t.Errorf("expected int __result declaration, got: %s", got)
	}
}

func TestEmitAsyncWrapper_ManualReset_bool(t *testing.T) {
	got := asyncbridge.EmitAsyncWrapper("Checker.IsValid()", typemap.KindBool, asyncbridge.DispatchManualReset)
	if !strings.Contains(got, "bool __result") {
		t.Errorf("expected bool __result declaration, got: %s", got)
	}
}

// ---------- EmitAsyncWrapper: DispatchTaskRun ----------

func TestEmitAsyncWrapper_TaskRun_string(t *testing.T) {
	got := asyncbridge.EmitAsyncWrapper("Api.FetchAsync(id)", typemap.KindString, asyncbridge.DispatchTaskRun)
	if !strings.Contains(got, "Task.Run") {
		t.Errorf("expected Task.Run, got: %s", got)
	}
	if !strings.Contains(got, ".GetAwaiter().GetResult()") {
		t.Errorf("expected .GetAwaiter().GetResult(), got: %s", got)
	}
	if !strings.Contains(got, "return") {
		t.Errorf("expected return, got: %s", got)
	}
}

func TestEmitAsyncWrapper_TaskRun_unit(t *testing.T) {
	got := asyncbridge.EmitAsyncWrapper("Api.FireAndForget()", typemap.KindUnit, asyncbridge.DispatchTaskRun)
	if strings.HasPrefix(strings.TrimSpace(got), "return") {
		t.Errorf("expected no return for void, got: %s", got)
	}
	if !strings.Contains(got, "Task.Run") {
		t.Errorf("expected Task.Run, got: %s", got)
	}
}

// ---------- IsAsyncMethod ----------

func TestIsAsyncMethod_nil(t *testing.T) {
	if asyncbridge.IsAsyncMethod(nil) {
		t.Error("expected false for nil")
	}
}

func TestIsAsyncMethod_async(t *testing.T) {
	m := &shimgen.ShimMethod{IsAsync: true}
	if !asyncbridge.IsAsyncMethod(m) {
		t.Error("expected true for IsAsync=true")
	}
}

func TestIsAsyncMethod_sync(t *testing.T) {
	m := &shimgen.ShimMethod{IsAsync: false}
	if asyncbridge.IsAsyncMethod(m) {
		t.Error("expected false for IsAsync=false")
	}
}

// ---------- AsyncMethodCount ----------

func TestAsyncMethodCount_nil(t *testing.T) {
	if asyncbridge.AsyncMethodCount(nil) != 0 {
		t.Error("expected 0 for nil shim")
	}
}

func TestAsyncMethodCount_empty(t *testing.T) {
	s := &shimgen.Shim{}
	if asyncbridge.AsyncMethodCount(s) != 0 {
		t.Error("expected 0 for empty shim")
	}
}

func TestAsyncMethodCount_mixed(t *testing.T) {
	s := &shimgen.Shim{
		Methods: []shimgen.ShimMethod{
			{IsAsync: true},
			{IsAsync: false},
			{IsAsync: true},
			{IsAsync: false},
			{IsAsync: true},
		},
	}
	if got := asyncbridge.AsyncMethodCount(s); got != 3 {
		t.Errorf("expected 3, got %d", got)
	}
}

func TestAsyncMethodCount_allAsync(t *testing.T) {
	s := &shimgen.Shim{
		Methods: []shimgen.ShimMethod{
			{IsAsync: true},
			{IsAsync: true},
		},
	}
	if got := asyncbridge.AsyncMethodCount(s); got != 2 {
		t.Errorf("expected 2, got %d", got)
	}
}

func TestAsyncMethodCount_noneAsync(t *testing.T) {
	s := &shimgen.Shim{
		Methods: []shimgen.ShimMethod{
			{IsAsync: false},
			{IsAsync: false},
		},
	}
	if got := asyncbridge.AsyncMethodCount(s); got != 0 {
		t.Errorf("expected 0, got %d", got)
	}
}

// ---------- RewriteForMode ----------

func makeTestShim() *shimgen.Shim {
	strMapping := typemap.Mapping{Kind: typemap.KindString, MochiType: "string"}
	return &shimgen.Shim{
		Package: "TestPkg",
		Methods: []shimgen.ShimMethod{
			{
				IsAsync:      true,
				CSMethodName: "MyClass_GetAsync",
				UpstreamCall: "TestPkg.MyClass.GetAsync",
				Return:       &strMapping,
				Params: []shimgen.ShimParam{
					{Name: "id", Mapping: typemap.Mapping{Kind: typemap.KindInt}},
				},
			},
			{
				IsAsync:      false,
				CSMethodName: "MyClass_Sync",
				UpstreamCall: "TestPkg.MyClass.Sync",
				Return:       &strMapping,
			},
			{
				IsAsync:      true,
				CSMethodName: "MyClass_DoAsync",
				UpstreamCall: "TestPkg.MyClass.DoAsync",
				Return:       nil, // void
			},
		},
	}
}

func TestRewriteForMode_nil(t *testing.T) {
	result := asyncbridge.RewriteForMode(nil, asyncbridge.DispatchManualReset)
	if result != nil {
		t.Error("expected nil for nil shim")
	}
}

func TestRewriteForMode_GetAwaiter(t *testing.T) {
	s := makeTestShim()
	result := asyncbridge.RewriteForMode(s, asyncbridge.DispatchGetAwaiter)
	if len(result) != 2 {
		t.Fatalf("expected 2 rewrites (async only), got %d", len(result))
	}
	for _, r := range result {
		if !strings.Contains(r.CSharpBody, ".GetAwaiter().GetResult()") {
			t.Errorf("expected GetAwaiter in body for %s, got: %s", r.Method.CSMethodName, r.CSharpBody)
		}
	}
}

func TestRewriteForMode_ManualReset(t *testing.T) {
	s := makeTestShim()
	result := asyncbridge.RewriteForMode(s, asyncbridge.DispatchManualReset)
	if len(result) != 2 {
		t.Fatalf("expected 2 rewrites, got %d", len(result))
	}
	for _, r := range result {
		if !strings.Contains(r.CSharpBody, "ManualResetEventSlim") {
			t.Errorf("expected ManualResetEventSlim for %s", r.Method.CSMethodName)
		}
	}
}

func TestRewriteForMode_TaskRun(t *testing.T) {
	s := makeTestShim()
	result := asyncbridge.RewriteForMode(s, asyncbridge.DispatchTaskRun)
	if len(result) != 2 {
		t.Fatalf("expected 2 rewrites, got %d", len(result))
	}
	for _, r := range result {
		if !strings.Contains(r.CSharpBody, "Task.Run") {
			t.Errorf("expected Task.Run for %s", r.Method.CSMethodName)
		}
	}
}

func TestRewriteForMode_preservesMethod(t *testing.T) {
	s := makeTestShim()
	result := asyncbridge.RewriteForMode(s, asyncbridge.DispatchGetAwaiter)
	if result[0].Method.CSMethodName != "MyClass_GetAsync" {
		t.Errorf("expected MyClass_GetAsync, got %s", result[0].Method.CSMethodName)
	}
}

func TestRewriteForMode_includesCallExpr(t *testing.T) {
	s := makeTestShim()
	result := asyncbridge.RewriteForMode(s, asyncbridge.DispatchGetAwaiter)
	// First async method has param "id", so call expr includes "id".
	if !strings.Contains(result[0].CSharpBody, "GetAsync(id)") {
		t.Errorf("expected GetAsync(id) in body, got: %s", result[0].CSharpBody)
	}
}

// ---------- EmitAsyncRuntimeCS ----------

func TestEmitAsyncRuntimeCS_GetAwaiter_empty(t *testing.T) {
	got := asyncbridge.EmitAsyncRuntimeCS(asyncbridge.DispatchGetAwaiter)
	if got != "" {
		t.Errorf("expected empty string for GetAwaiter, got: %s", got)
	}
}

func TestEmitAsyncRuntimeCS_ManualReset_nonEmpty(t *testing.T) {
	got := asyncbridge.EmitAsyncRuntimeCS(asyncbridge.DispatchManualReset)
	if got == "" {
		t.Error("expected non-empty for ManualReset")
	}
	if !strings.Contains(got, "ManualResetEventSlim") {
		t.Errorf("expected ManualResetEventSlim in helper, got: %s", got)
	}
	if !strings.Contains(got, "AsyncRuntime") {
		t.Errorf("expected AsyncRuntime class in helper")
	}
}

func TestEmitAsyncRuntimeCS_TaskRun_nonEmpty(t *testing.T) {
	got := asyncbridge.EmitAsyncRuntimeCS(asyncbridge.DispatchTaskRun)
	if got == "" {
		t.Error("expected non-empty for TaskRun")
	}
	if !strings.Contains(got, "Task.Run") {
		t.Errorf("expected Task.Run in helper, got: %s", got)
	}
	if !strings.Contains(got, "AsyncRuntime") {
		t.Errorf("expected AsyncRuntime class in helper")
	}
}

func TestEmitAsyncRuntimeCS_unknown_empty(t *testing.T) {
	got := asyncbridge.EmitAsyncRuntimeCS(asyncbridge.DispatchMode("unknown"))
	if got != "" {
		t.Errorf("expected empty for unknown mode, got: %s", got)
	}
}

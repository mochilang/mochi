package wrapper

import (
	"strings"
	"testing"

	"mochi/package3/rust/asyncbridge"
	"mochi/package3/rust/errors"
	"mochi/package3/rust/rustdoc"
)

// asyncSurface mimics a small tokio-style public surface with one
// async fn and one sync fn.
func asyncSurface() *rustdoc.ApiSurface {
	str := rustdoc.Type{Primitive: "str"}
	strBorrow := rustdoc.Type{BorrowedRef: &rustdoc.BorrowedRefType{
		Lifetime: "'a", Type: str,
	}}
	return &rustdoc.ApiSurface{
		CrateName:    "tokio_demo",
		CrateVersion: "0.1.0",
		Functions: []rustdoc.FunctionEntry{
			{
				ID:   "fn:greet_async",
				Path: []string{"tokio_demo", "greet_async"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "name", Type: strBorrow},
				},
				Header: rustdoc.FunctionHeader{IsAsync: true},
			},
			{
				ID:   "fn:greet_sync",
				Path: []string{"tokio_demo", "greet_sync"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "name", Type: strBorrow},
				},
				Header: rustdoc.FunctionHeader{IsAsync: false},
			},
		},
		Skipped: []errors.SkipReport{},
	}
}

func TestSynthMarksAsyncFlag(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	if len(c.Functions) != 2 {
		t.Fatalf("expected 2 fns, got %d", len(c.Functions))
	}
	var hadAsync, hadSync bool
	for _, fn := range c.Functions {
		switch fn.UpstreamPath {
		case "tokio_demo::greet_async":
			hadAsync = fn.IsAsync
		case "tokio_demo::greet_sync":
			hadSync = !fn.IsAsync
		}
	}
	if !hadAsync {
		t.Error("greet_async should carry IsAsync=true")
	}
	if !hadSync {
		t.Error("greet_sync should carry IsAsync=false")
	}
}

func TestHasAsyncDetectsAsyncFn(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	if !c.HasAsync() {
		t.Error("HasAsync should be true when surface has any async fn")
	}
}

func TestHasAsyncFalseWhenAllSync(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	if c.HasAsync() {
		t.Errorf("HasAsync should be false for pure-sync surface (hex)")
	}
}

func TestEmitLibRSWiresMochiRTOnAsync(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	out := EmitLibRS(c)
	if !strings.Contains(out, "pub mod mochi_rt;") {
		t.Errorf("EmitLibRS should declare `pub mod mochi_rt;` when async fn present:\n%s", out)
	}
}

func TestEmitLibRSOmitsMochiRTOnSync(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	out := EmitLibRS(c)
	if strings.Contains(out, "pub mod mochi_rt;") {
		t.Errorf("EmitLibRS must not declare mochi_rt for sync-only crates")
	}
}

func TestEmitLibRSAsyncBodyUsesBlockOn(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	out := EmitLibRS(c)
	if !strings.Contains(out, "mochi_rt::block_on(async { tokio_demo::greet_async(name).await })") {
		t.Errorf("EmitLibRS async body should call mochi_rt::block_on(.. .await):\n%s",
			grepWindow(out, "greet_async"))
	}
}

func TestEmitLibRSSyncBodyDoesNotBlockOn(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	out := EmitLibRS(c)
	// Find the greet_sync fn and assert its body is the direct call.
	idx := strings.Index(out, "fn mochi_tokio_demo_greet_sync")
	if idx < 0 {
		t.Fatalf("greet_sync wrapper not present")
	}
	body := out[idx:]
	end := strings.Index(body, "\n}\n")
	if end > 0 {
		body = body[:end]
	}
	if strings.Contains(body, "mochi_rt::block_on") {
		t.Errorf("sync wrapper body should not use block_on:\n%s", body)
	}
	if !strings.Contains(body, "tokio_demo::greet_sync(name)") {
		t.Errorf("sync wrapper body should call upstream directly:\n%s", body)
	}
}

func TestEmitMochiRTReturnsModule(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	out := EmitMochiRT(c)
	if !strings.Contains(out, "OnceCell<Runtime>") {
		t.Errorf("EmitMochiRT should declare OnceCell<Runtime>:\n%s", out)
	}
	if !strings.Contains(out, "Builder::new_current_thread()") {
		t.Errorf("default flavor should be current-thread:\n%s", out)
	}
}

func TestEmitMochiRTEmptyForSync(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	if out := EmitMochiRT(c); out != "" {
		t.Errorf("EmitMochiRT for sync-only crate should be empty, got %q", out)
	}
}

func TestEmitMochiRTHonorsMultiThreadFlavor(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	c.AsyncFlavor = asyncbridge.FlavorMultiThread
	out := EmitMochiRT(c)
	if !strings.Contains(out, "Builder::new_multi_thread()") {
		t.Errorf("multi-thread flavor should call new_multi_thread:\n%s", out)
	}
}

func TestEmitCargoTOMLAddsTokioForAsync(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	got := EmitCargoTOML(c)
	if !strings.Contains(got, "tokio = { version =") {
		t.Errorf("Cargo.toml should carry tokio dep for async wrapper:\n%s", got)
	}
	if !strings.Contains(got, "once_cell") {
		t.Errorf("Cargo.toml should carry once_cell dep for async wrapper:\n%s", got)
	}
}

func TestEmitCargoTOMLNoTokioForSync(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	got := EmitCargoTOML(c)
	if strings.Contains(got, "tokio") {
		t.Errorf("sync-only Cargo.toml must not carry tokio dep:\n%s", got)
	}
	if strings.Contains(got, "once_cell") {
		t.Errorf("sync-only Cargo.toml must not carry once_cell dep:\n%s", got)
	}
}

func TestEmitCargoTOMLMultiThreadFlavor(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	c.AsyncFlavor = asyncbridge.FlavorMultiThread
	got := EmitCargoTOML(c)
	if !strings.Contains(got, "rt-multi-thread") {
		t.Errorf("multi-thread flavor should pick rt-multi-thread feature:\n%s", got)
	}
}

func TestEmitLibRSAsyncDeterministic(t *testing.T) {
	c := Synth("tokio_demo", "0.1.0", asyncSurface())
	first := EmitLibRS(c)
	for i := 0; i < 5; i++ {
		if EmitLibRS(c) != first {
			t.Errorf("async EmitLibRS non-deterministic at iter %d", i)
		}
	}
}

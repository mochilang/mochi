package asyncbridge

import (
	"strings"
	"testing"
)

// TestPhase11AsyncBridge is the MEP-73 Phase 11 sentinel. It asserts
// the async-fn bridge produces a coherent tokio-runtime singleton and
// a `block_on` body shape per spec §3 (Async bridge) without dragging
// in tokio for sync-only crates.
func TestPhase11AsyncBridge(t *testing.T) {
	t.Run("runtime_module_uses_oncecell_and_runtime", func(t *testing.T) {
		out := RuntimeModule(FlavorCurrentThread)
		for _, want := range []string{
			"use once_cell::sync::OnceCell;",
			"use tokio::runtime::Runtime;",
			"static MOCHI_RT: OnceCell<Runtime>",
			"pub fn get_rt() -> &'static Runtime",
			"pub fn block_on<F: core::future::Future>(fut: F) -> F::Output",
		} {
			if !strings.Contains(out, want) {
				t.Errorf("mochi_rt.rs missing %q:\n%s", want, out)
			}
		}
	})

	t.Run("current_thread_is_default", func(t *testing.T) {
		f, err := ParseFlavor("")
		if err != nil {
			t.Fatal(err)
		}
		if f != FlavorCurrentThread {
			t.Errorf("default flavor = %v, want current-thread", f)
		}
	})

	t.Run("multi_thread_opt_in_flips_features_and_builder", func(t *testing.T) {
		rt := RuntimeModule(FlavorMultiThread)
		if !strings.Contains(rt, "Builder::new_multi_thread()") {
			t.Errorf("multi-thread RuntimeModule should call new_multi_thread:\n%s", rt)
		}
		dep := CargoDepRow(FlavorMultiThread)
		if !strings.Contains(dep, "rt-multi-thread") {
			t.Errorf("multi-thread dep row should opt into rt-multi-thread feature:\n%s", dep)
		}
	})

	t.Run("block_on_body_awaits_upstream", func(t *testing.T) {
		got := FnBody("tokio::time::sleep", []string{"duration"})
		want := "mochi_rt::block_on(async { tokio::time::sleep(duration).await })"
		if got != want {
			t.Errorf("FnBody = %q\n want %q", got, want)
		}
	})

	t.Run("dep_pins_min_tokio_version", func(t *testing.T) {
		dep := CargoDepRow(FlavorCurrentThread)
		if !strings.Contains(dep, MinTokioVersion) {
			t.Errorf("dep row should pin MinTokioVersion %q:\n%s", MinTokioVersion, dep)
		}
	})

	t.Run("byte_stable_outputs", func(t *testing.T) {
		a := RuntimeModule(FlavorCurrentThread)
		b := RuntimeModule(FlavorCurrentThread)
		if a != b {
			t.Error("RuntimeModule not byte-stable")
		}
		c := CargoDepRow(FlavorMultiThread)
		d := CargoDepRow(FlavorMultiThread)
		if c != d {
			t.Error("CargoDepRow not byte-stable")
		}
	})
}

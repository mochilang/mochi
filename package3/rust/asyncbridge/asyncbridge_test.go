package asyncbridge

import (
	"strings"
	"testing"
)

func TestParseFlavorDefaults(t *testing.T) {
	got, err := ParseFlavor("")
	if err != nil {
		t.Fatal(err)
	}
	if got != FlavorCurrentThread {
		t.Errorf("empty flavor = %v, want current-thread", got)
	}
}

func TestParseFlavorAccepts(t *testing.T) {
	cases := []struct {
		in   string
		want Flavor
	}{
		{"current-thread", FlavorCurrentThread},
		{"multi-thread", FlavorMultiThread},
		{"  current-thread  ", FlavorCurrentThread},
	}
	for _, c := range cases {
		got, err := ParseFlavor(c.in)
		if err != nil {
			t.Errorf("ParseFlavor(%q) err = %v", c.in, err)
		}
		if got != c.want {
			t.Errorf("ParseFlavor(%q) = %v, want %v", c.in, got, c.want)
		}
	}
}

func TestParseFlavorRejectsUnknown(t *testing.T) {
	if _, err := ParseFlavor("io-uring"); err == nil {
		t.Error("expected error for unknown flavor")
	}
}

func TestFlavorString(t *testing.T) {
	if FlavorCurrentThread.String() != "current-thread" {
		t.Errorf("current-thread String = %q", FlavorCurrentThread.String())
	}
	if FlavorMultiThread.String() != "multi-thread" {
		t.Errorf("multi-thread String = %q", FlavorMultiThread.String())
	}
}

func TestTokioFeaturesCurrentThread(t *testing.T) {
	got := FlavorCurrentThread.TokioFeatures()
	if len(got) != 2 || got[0] != "rt" || got[1] != "macros" {
		t.Errorf("current-thread features = %v, want [rt macros]", got)
	}
}

func TestTokioFeaturesMultiThread(t *testing.T) {
	got := FlavorMultiThread.TokioFeatures()
	if len(got) != 3 {
		t.Fatalf("multi-thread features = %v, want 3 entries", got)
	}
	has := func(s string) bool {
		for _, g := range got {
			if g == s {
				return true
			}
		}
		return false
	}
	for _, want := range []string{"rt", "rt-multi-thread", "macros"} {
		if !has(want) {
			t.Errorf("multi-thread features missing %q: %v", want, got)
		}
	}
}

func TestRuntimeModuleCurrentThread(t *testing.T) {
	out := RuntimeModule(FlavorCurrentThread)
	if !strings.Contains(out, "Builder::new_current_thread()") {
		t.Errorf("current-thread RuntimeModule should call new_current_thread:\n%s", out)
	}
	if !strings.Contains(out, "OnceCell<Runtime>") {
		t.Errorf("RuntimeModule should declare OnceCell<Runtime>:\n%s", out)
	}
	if !strings.Contains(out, "block_on") {
		t.Errorf("RuntimeModule should expose block_on:\n%s", out)
	}
	if !strings.Contains(out, "enable_all()") {
		t.Errorf("RuntimeModule should call enable_all():\n%s", out)
	}
}

func TestRuntimeModuleMultiThread(t *testing.T) {
	out := RuntimeModule(FlavorMultiThread)
	if !strings.Contains(out, "Builder::new_multi_thread()") {
		t.Errorf("multi-thread RuntimeModule should call new_multi_thread:\n%s", out)
	}
	if strings.Contains(out, "Builder::new_current_thread()") {
		t.Errorf("multi-thread RuntimeModule should not mention new_current_thread:\n%s", out)
	}
}

func TestRuntimeModuleDeterministic(t *testing.T) {
	a := RuntimeModule(FlavorCurrentThread)
	b := RuntimeModule(FlavorCurrentThread)
	if a != b {
		t.Error("RuntimeModule must be byte-stable across calls")
	}
}

func TestFnBodyShape(t *testing.T) {
	got := FnBody("tokio::time::sleep", []string{"duration"})
	want := "mochi_rt::block_on(async { tokio::time::sleep(duration).await })"
	if got != want {
		t.Errorf("FnBody = %q, want %q", got, want)
	}
}

func TestFnBodyMultiArg(t *testing.T) {
	got := FnBody("reqwest::get", []string{"url", "headers"})
	if !strings.Contains(got, "reqwest::get(url, headers).await") {
		t.Errorf("FnBody multi-arg = %q", got)
	}
}

func TestFnBodyNoArgs(t *testing.T) {
	got := FnBody("tokio::task::yield_now", nil)
	if !strings.Contains(got, "yield_now().await") {
		t.Errorf("FnBody no-args = %q", got)
	}
}

func TestFnBodyRejectsEmptyPath(t *testing.T) {
	if got := FnBody("", []string{"x"}); got != "" {
		t.Errorf("FnBody empty path should return empty, got %q", got)
	}
}

func TestCargoDepRowCurrentThread(t *testing.T) {
	got := CargoDepRow(FlavorCurrentThread)
	if !strings.Contains(got, "tokio = { version = \"^1.42\"") {
		t.Errorf("dep row should pin >=1.42: %q", got)
	}
	if !strings.Contains(got, `"rt"`) || !strings.Contains(got, `"macros"`) {
		t.Errorf("dep row should carry rt + macros features: %q", got)
	}
	if strings.Contains(got, "rt-multi-thread") {
		t.Errorf("current-thread dep row should not carry rt-multi-thread: %q", got)
	}
}

func TestCargoDepRowMultiThread(t *testing.T) {
	got := CargoDepRow(FlavorMultiThread)
	if !strings.Contains(got, "rt-multi-thread") {
		t.Errorf("multi-thread dep row should carry rt-multi-thread: %q", got)
	}
}

func TestCargoDepRowDeterministic(t *testing.T) {
	a := CargoDepRow(FlavorMultiThread)
	b := CargoDepRow(FlavorMultiThread)
	if a != b {
		t.Error("CargoDepRow must be byte-stable across calls")
	}
}

func TestOnceCellDepRow(t *testing.T) {
	got := OnceCellDepRow()
	if !strings.Contains(got, "once_cell") {
		t.Errorf("dep row should mention once_cell: %q", got)
	}
}

func TestMinTokioVersion(t *testing.T) {
	if MinTokioVersion == "" {
		t.Error("MinTokioVersion must be set")
	}
}

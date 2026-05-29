package target

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/erlang/portemit"
	"mochi/package3/erlang/typemap"
)

// ── Emit happy path ───────────────────────────────────────────────────────

func TestEmit_CreatesThreeFiles(t *testing.T) {
	dir := t.TempDir()
	res, err := Emit(dir, Opts{Name: "cowboy"})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	for _, path := range []string{res.ShimErlPath, res.AppSrcPath, res.RebarCfgPath} {
		if _, err := os.Stat(path); os.IsNotExist(err) {
			t.Errorf("expected file %s to exist", path)
		}
	}
}

func TestEmit_CreatesSrcAndPrivDirs(t *testing.T) {
	dir := t.TempDir()
	if _, err := Emit(dir, Opts{Name: "ranch"}); err != nil {
		t.Fatalf("Emit: %v", err)
	}
	for _, sub := range []string{"src", "priv"} {
		if _, err := os.Stat(filepath.Join(dir, sub)); os.IsNotExist(err) {
			t.Errorf("expected dir %s/%s to exist", dir, sub)
		}
	}
}

func TestEmit_ShimErlPath(t *testing.T) {
	dir := t.TempDir()
	res, err := Emit(dir, Opts{Name: "cowboy"})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	want := filepath.Join(dir, "src", "cowboy_mochi_shim.erl")
	if res.ShimErlPath != want {
		t.Errorf("ShimErlPath = %q, want %q", res.ShimErlPath, want)
	}
}

func TestEmit_AppSrcPath(t *testing.T) {
	dir := t.TempDir()
	res, err := Emit(dir, Opts{Name: "cowboy"})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	want := filepath.Join(dir, "src", "cowboy.app.src")
	if res.AppSrcPath != want {
		t.Errorf("AppSrcPath = %q, want %q", res.AppSrcPath, want)
	}
}

func TestEmit_RebarConfigPath(t *testing.T) {
	dir := t.TempDir()
	res, err := Emit(dir, Opts{Name: "cowboy"})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	want := filepath.Join(dir, "rebar.config")
	if res.RebarCfgPath != want {
		t.Errorf("RebarCfgPath = %q, want %q", res.RebarCfgPath, want)
	}
}

func TestEmit_EmptyNameError(t *testing.T) {
	_, err := Emit(t.TempDir(), Opts{})
	if err == nil {
		t.Error("expected error for empty Name")
	}
}

func TestEmit_Idempotent(t *testing.T) {
	dir := t.TempDir()
	opts := Opts{Name: "ranch"}
	if _, err := Emit(dir, opts); err != nil {
		t.Fatal(err)
	}
	if _, err := Emit(dir, opts); err != nil {
		t.Fatalf("second Emit: %v", err)
	}
}

// ── shim.erl content ──────────────────────────────────────────────────────

func TestEmit_ShimErlContent_ModuleDecl(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "cowboy"})
	data, _ := os.ReadFile(res.ShimErlPath)
	if !strings.Contains(string(data), "-module(cowboy_mochi_shim)") {
		t.Error("shim.erl missing -module(cowboy_mochi_shim)")
	}
}

func TestEmit_ShimErlContent_GenServer(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "ranch"})
	data, _ := os.ReadFile(res.ShimErlPath)
	src := string(data)
	if !strings.Contains(src, "-behaviour(gen_server)") {
		t.Error("shim.erl missing -behaviour(gen_server)")
	}
	if !strings.Contains(src, "open_port") {
		t.Error("shim.erl missing open_port call")
	}
	if !strings.Contains(src, "{packet, 4}") {
		t.Error("shim.erl missing {packet, 4}")
	}
}

func TestEmit_ShimErlContent_WithFunctions(t *testing.T) {
	dir := t.TempDir()
	fns := []portemit.FnSpec{
		{Module: "cowboy", Function: "start_http", Arity: 3,
			Args:   []typemap.MochiType{typemap.MochiString, typemap.MochiInt},
			Return: typemap.MochiString},
	}
	res, _ := Emit(dir, Opts{Name: "cowboy", Fns: fns})
	data, _ := os.ReadFile(res.ShimErlPath)
	if !strings.Contains(string(data), "start_http") {
		t.Error("shim.erl missing start_http function")
	}
}

// ── .app.src content ──────────────────────────────────────────────────────

func TestEmit_AppSrc_ApplicationAtom(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "cowboy"})
	data, _ := os.ReadFile(res.AppSrcPath)
	if !strings.Contains(string(data), "{application, cowboy,") {
		t.Error(".app.src missing {application, cowboy, ...}")
	}
}

func TestEmit_AppSrc_DefaultVersion(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "ranch"})
	data, _ := os.ReadFile(res.AppSrcPath)
	if !strings.Contains(string(data), `"0.1.0"`) {
		t.Error(".app.src should default to version 0.1.0")
	}
}

func TestEmit_AppSrc_CustomVersion(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "cowboy", Meta: HexMeta{Version: "2.12.0"}})
	data, _ := os.ReadFile(res.AppSrcPath)
	if !strings.Contains(string(data), `"2.12.0"`) {
		t.Error(".app.src missing custom version 2.12.0")
	}
}

func TestEmit_AppSrc_CustomAppName(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "prometheus", Meta: HexMeta{AppName: "prometheus_erl"}})
	data, _ := os.ReadFile(res.AppSrcPath)
	src := string(data)
	if !strings.Contains(src, "{application, prometheus_erl,") {
		t.Errorf(".app.src should use custom app name, got:\n%s", src)
	}
	// shim module should also use the resolved app name
	shimData, _ := os.ReadFile(res.ShimErlPath)
	if !strings.Contains(string(shimData), "-module(prometheus_erl_mochi_shim)") {
		t.Error("shim.erl should use resolved app name prometheus_erl")
	}
}

func TestEmit_AppSrc_KernelStdlibDeps(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "ranch"})
	data, _ := os.ReadFile(res.AppSrcPath)
	src := string(data)
	if !strings.Contains(src, "kernel") || !strings.Contains(src, "stdlib") {
		t.Error(".app.src missing kernel/stdlib in applications list")
	}
}

// ── rebar.config content ──────────────────────────────────────────────────

func TestEmit_RebarConfig_ErlOpts(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "cowboy"})
	data, _ := os.ReadFile(res.RebarCfgPath)
	if !strings.Contains(string(data), "{erl_opts, [debug_info]}") {
		t.Error("rebar.config missing erl_opts")
	}
}

func TestEmit_RebarConfig_DefaultOTPVersion(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "cowboy"})
	data, _ := os.ReadFile(res.RebarCfgPath)
	if !strings.Contains(string(data), `"25"`) {
		t.Error("rebar.config should default to OTP 25")
	}
}

func TestEmit_RebarConfig_CustomOTPVersion(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "cowboy", OTPVersion: "27"})
	data, _ := os.ReadFile(res.RebarCfgPath)
	if !strings.Contains(string(data), `"27"`) {
		t.Error("rebar.config missing custom OTP version 27")
	}
}

func TestEmit_RebarConfig_HexBlockWhenMeta(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{
		Name: "cowboy",
		Meta: HexMeta{
			Description: "A small, fast, modern HTTP server for Erlang/OTP.",
			Version:     "2.12.0",
			Licenses:    []string{"ISC"},
			Links:       map[string]string{"GitHub": "https://github.com/ninenines/cowboy"},
			Maintainers: []string{"essen"},
		},
	})
	data, _ := os.ReadFile(res.RebarCfgPath)
	src := string(data)
	if !strings.Contains(src, "{hex,") {
		t.Error("rebar.config missing {hex, ...} block")
	}
	if !strings.Contains(src, "2.12.0") {
		t.Error("rebar.config hex block missing version")
	}
	if !strings.Contains(src, "ISC") {
		t.Error("rebar.config hex block missing license")
	}
	if !strings.Contains(src, "GitHub") {
		t.Error("rebar.config hex block missing GitHub link")
	}
	if !strings.Contains(src, "essen") {
		t.Error("rebar.config hex block missing maintainer")
	}
}

func TestEmit_RebarConfig_NoHexBlockWhenNoMeta(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{Name: "cowboy"})
	data, _ := os.ReadFile(res.RebarCfgPath)
	if strings.Contains(string(data), "{hex,") {
		t.Error("rebar.config should omit {hex,} block when no meta")
	}
}

func TestEmit_RebarConfig_DefaultBuildTools(t *testing.T) {
	dir := t.TempDir()
	res, _ := Emit(dir, Opts{
		Name: "cowboy",
		Meta: HexMeta{Description: "test"},
	})
	data, _ := os.ReadFile(res.RebarCfgPath)
	if !strings.Contains(string(data), "rebar3") {
		t.Error("rebar.config should default build_tools to [rebar3]")
	}
}

// ── resolvedAppName ───────────────────────────────────────────────────────

func TestResolvedAppName_DefaultsToName(t *testing.T) {
	opts := Opts{Name: "cowboy"}
	if resolvedAppName(opts) != "cowboy" {
		t.Error("resolvedAppName should default to Name")
	}
}

func TestResolvedAppName_CustomAppName(t *testing.T) {
	opts := Opts{Name: "prometheus", Meta: HexMeta{AppName: "prometheus_erl"}}
	if resolvedAppName(opts) != "prometheus_erl" {
		t.Error("resolvedAppName should return Meta.AppName when set")
	}
}

// ── escErl ────────────────────────────────────────────────────────────────

func TestEscErl_NoEscape(t *testing.T) {
	if escErl("hello world") != "hello world" {
		t.Error("escErl should not change plain text")
	}
}

func TestEscErl_EscapesDoubleQuote(t *testing.T) {
	if got := escErl(`say "hi"`); got != `say \"hi\"` {
		t.Errorf("escErl double-quote: got %q", got)
	}
}

func TestEscErl_EscapesBackslash(t *testing.T) {
	if got := escErl(`a\b`); got != `a\\b` {
		t.Errorf("escErl backslash: got %q", got)
	}
}

package coroutines

import (
	"strings"
	"testing"
)

// ─── javaMethodName ───────────────────────────────────────────────────────────

func TestJavaMethodName(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"fetch", "fetch"},
		{"mochi_okhttp_fetch", "mochiOkhttpFetch"},
		{"mochi_okhttp_fetch_async", "mochiOkhttpFetchAsync"},
		{"a_b_c", "aBC"},
		{"already", "already"},
	}
	for _, c := range cases {
		if got := javaMethodName(c.in); got != c.want {
			t.Errorf("javaMethodName(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}

// ─── javaTypeToMochi ──────────────────────────────────────────────────────────

func TestJavaTypeToMochi(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"int", "int"},
		{"Integer", "int"},
		{"long", "int"},
		{"float", "float"},
		{"double", "float"},
		{"boolean", "bool"},
		{"String", "string"},
		{"void", "unit"},
		{"MyCustomType", "any"},
	}
	for _, c := range cases {
		if got := javaTypeToMochi(c.in); got != c.want {
			t.Errorf("javaTypeToMochi(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}

// ─── EmitBlockingStub ─────────────────────────────────────────────────────────

func TestEmitBlockingStub_Void(t *testing.T) {
	fn := SuspendFn{
		CName:          "mochi_client_send",
		KotlinFQN:      "com.example.Client.send",
		Params:         []Param{{Name: "msg", JavaType: "String"}},
		ReturnJavaType: "void",
		Mode:           Blocking,
	}
	stub := EmitBlockingStub(fn)

	mustContain(t, stub, "@CEntryPoint(name = \"mochi_client_send\")")
	mustContain(t, stub, "public static void mochiClientSend(IsolateThread thread, String msg)")
	mustContain(t, stub, "runBlocking")
	mustContain(t, stub, "com.example.Client.send(msg)")
}

func TestEmitBlockingStub_WithReturn(t *testing.T) {
	fn := SuspendFn{
		CName:          "mochi_fetcher_get",
		KotlinFQN:      "com.example.Fetcher.get",
		Params:         []Param{{Name: "url", JavaType: "String"}},
		ReturnJavaType: "String",
		Mode:           Blocking,
	}
	stub := EmitBlockingStub(fn)

	mustContain(t, stub, "public static String mochiFetcherGet(IsolateThread thread")
	mustContain(t, stub, "return (String) kotlinx.coroutines.BuildersKt.runBlocking")
	mustContain(t, stub, "com.example.Fetcher.get(url)")
}

func TestEmitBlockingStub_NoParams(t *testing.T) {
	fn := SuspendFn{
		CName:          "mochi_health_check",
		KotlinFQN:      "com.example.Api.healthCheck",
		ReturnJavaType: "boolean",
		Mode:           Blocking,
	}
	stub := EmitBlockingStub(fn)
	mustContain(t, stub, "public static boolean mochiHealthCheck(IsolateThread thread)")
	mustContain(t, stub, "com.example.Api.healthCheck()")
}

// ─── EmitEventLoopStub ────────────────────────────────────────────────────────

func TestEmitEventLoopStub_SubmitAndPoll(t *testing.T) {
	fn := SuspendFn{
		CName:          "mochi_parser_parse",
		KotlinFQN:      "com.example.Parser.parse",
		Params:         []Param{{Name: "input", JavaType: "String"}},
		ReturnJavaType: "String",
		Mode:           EventLoop,
	}
	stub := EmitEventLoopStub(fn)

	mustContain(t, stub, "@CEntryPoint(name = \"mochi_parser_parse_submit\")")
	mustContain(t, stub, "public static long mochiParserParseSubmit(IsolateThread thread, String input)")
	mustContain(t, stub, "CoroutinesHelper.submit(")

	mustContain(t, stub, "@CEntryPoint(name = \"mochi_parser_parse_poll\")")
	mustContain(t, stub, "public static String mochiParserParsePoll(IsolateThread thread, long handleId)")
	mustContain(t, stub, "CoroutinesHelper.poll(handleId)")
}

// ─── EmitCoroutinesHelper ─────────────────────────────────────────────────────

func TestEmitCoroutinesHelper_Package(t *testing.T) {
	code := EmitCoroutinesHelper("com.mochi.bridge.okhttp")
	mustContain(t, code, "package com.mochi.bridge.okhttp;")
	mustContain(t, code, "class CoroutinesHelper")
	mustContain(t, code, "long submit(")
	mustContain(t, code, "class PendingException")
}

// ─── ShimLines ────────────────────────────────────────────────────────────────

func TestShimLines_Blocking(t *testing.T) {
	fn := SuspendFn{
		CName:          "mochi_api_call",
		KotlinFQN:      "com.example.Api.call",
		Params:         []Param{{Name: "req", JavaType: "String"}},
		ReturnJavaType: "String",
		Mode:           Blocking,
	}
	line := ShimLines(fn, "string")
	if !strings.HasPrefix(line, "extern fn mochi_api_call(") {
		t.Errorf("blocking shim line: %q", line)
	}
	mustContain(t, line, "): string from kotlin")
	mustContain(t, line, "com.example.Api.call")
	// Should be a single line (no newline in the middle).
	if strings.Count(line, "\n") > 0 {
		t.Errorf("blocking shim should be one line, got:\n%s", line)
	}
}

func TestShimLines_EventLoop(t *testing.T) {
	fn := SuspendFn{
		CName:          "mochi_api_call",
		KotlinFQN:      "com.example.Api.call",
		Params:         []Param{{Name: "req", JavaType: "String"}},
		ReturnJavaType: "String",
		Mode:           EventLoop,
	}
	lines := ShimLines(fn, "string")
	if strings.Count(lines, "\n") != 1 {
		t.Errorf("event-loop shim should have 2 lines, got:\n%s", lines)
	}
	mustContain(t, lines, "mochi_api_call_submit")
	mustContain(t, lines, "mochi_api_call_poll")
	mustContain(t, lines, "handle: int")
}

// ─── helper ───────────────────────────────────────────────────────────────────

func mustContain(t *testing.T, s, sub string) {
	t.Helper()
	if !strings.Contains(s, sub) {
		t.Errorf("expected to find %q in:\n%s", sub, s)
	}
}

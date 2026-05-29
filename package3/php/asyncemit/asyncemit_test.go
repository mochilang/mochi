package asyncemit

import (
	"strings"
	"testing"

	"mochi/package3/php/reflect"
)

func TestIsPromiseTypeReact(t *testing.T) {
	cases := []struct {
		t    string
		want bool
	}{
		{`React\Promise\PromiseInterface`, true},
		{`React\Promise\Promise`, true},
		{`\React\Promise\PromiseInterface`, true},
		{`Amp\Promise`, true},
		{`Amp\Future`, true},
		{`GuzzleHttp\Promise\PromiseInterface`, true},
		{`GuzzleHttp\Promise\Promise`, true},
		{`string`, false},
		{`void`, false},
		{`bool`, false},
		{``, false},
	}
	for _, tc := range cases {
		got := IsPromiseType(tc.t, nil)
		if got != tc.want {
			t.Errorf("IsPromiseType(%q) = %v; want %v", tc.t, got, tc.want)
		}
	}
}

func TestIsPromiseTypeExtra(t *testing.T) {
	extra := []string{`My\Custom\Thenable`}
	if !IsPromiseType(`My\Custom\Thenable`, extra) {
		t.Error("expected custom type to be detected")
	}
	if IsPromiseType(`My\Custom\Other`, extra) {
		t.Error("expected non-promise type not to be detected")
	}
}

func buildAsyncSurface() *reflect.ReflectionSurface {
	return &reflect.ReflectionSurface{
		PackageName: "acme/async",
		Classes: []reflect.ClassSurface{
			{
				FQCN: `Acme\Async\Client`,
				Methods: []reflect.MethodSurface{
					{Name: "sendAsync", ReturnType: `React\Promise\PromiseInterface`},
					{Name: "send", ReturnType: `string`},
					{Name: "__construct"},
					{Name: "fetchAsync", ReturnType: `Amp\Future`},
				},
			},
		},
		Interfaces: []reflect.InterfaceSurface{
			{
				FQCN: `Acme\Async\AsyncClientInterface`,
				Methods: []reflect.MethodSurface{
					{Name: "postAsync", ReturnType: `GuzzleHttp\Promise\PromiseInterface`},
					{Name: "get", ReturnType: `int`},
				},
			},
		},
	}
}

func TestEmitDetectsAsyncMethods(t *testing.T) {
	result := Emit(buildAsyncSurface(), Config{})
	if result == nil {
		t.Fatal("expected non-nil result")
	}
	// sendAsync and fetchAsync are async; send is not; __construct skipped
	if result.AsyncMethodCount != 3 {
		t.Errorf("expected 3 async methods; got %d", result.AsyncMethodCount)
	}
}

func TestEmitAsyncExternFnSyntax(t *testing.T) {
	result := Emit(buildAsyncSurface(), Config{})
	src := result.MochiSource
	if !strings.Contains(src, "async extern fn") {
		t.Errorf("expected async extern fn; got:\n%s", src)
	}
	if !strings.Contains(src, "acme_async_client_send_async") {
		t.Errorf("expected acme_async_client_send_async; got:\n%s", src)
	}
	if !strings.Contains(src, "acme_async_client_fetch_async") {
		t.Errorf("expected acme_async_client_fetch_async; got:\n%s", src)
	}
}

func TestEmitSkipsMagicMethods(t *testing.T) {
	surface := &reflect.ReflectionSurface{
		PackageName: "x/y",
		Classes: []reflect.ClassSurface{
			{
				FQCN: `X\Y\Obj`,
				Methods: []reflect.MethodSurface{
					{Name: "__invoke", ReturnType: `React\Promise\PromiseInterface`},
					{Name: "doAsync", ReturnType: `Amp\Future`},
				},
			},
		},
	}
	result := Emit(surface, Config{})
	if result.AsyncMethodCount != 1 {
		t.Errorf("expected 1 async method (magic skipped); got %d", result.AsyncMethodCount)
	}
	if strings.Contains(result.MochiSource, "__invoke") {
		t.Errorf("__invoke should be skipped; got:\n%s", result.MochiSource)
	}
}

func TestEmitNonAsyncSurface(t *testing.T) {
	surface := &reflect.ReflectionSurface{
		PackageName: "sync/pkg",
		Classes: []reflect.ClassSurface{
			{
				FQCN: `Sync\Client`,
				Methods: []reflect.MethodSurface{
					{Name: "get", ReturnType: "string"},
					{Name: "post", ReturnType: "bool"},
				},
			},
		},
	}
	result := Emit(surface, Config{})
	if result.AsyncMethodCount != 0 {
		t.Errorf("expected 0 async methods; got %d", result.AsyncMethodCount)
	}
	if result.MochiSource != "" {
		t.Errorf("expected empty source for non-async surface; got:\n%s", result.MochiSource)
	}
}

func TestEmitExtraPromiseTypes(t *testing.T) {
	surface := &reflect.ReflectionSurface{
		PackageName: "custom/async",
		Classes: []reflect.ClassSurface{
			{
				FQCN: `Custom\Worker`,
				Methods: []reflect.MethodSurface{
					{Name: "run", ReturnType: `My\Thenable`},
				},
			},
		},
	}
	cfg := Config{ExtraPromiseTypes: []string{`My\Thenable`}}
	result := Emit(surface, cfg)
	if result.AsyncMethodCount != 1 {
		t.Errorf("expected 1 async method; got %d", result.AsyncMethodCount)
	}
}

func TestEmitReturnTypeIsUnit(t *testing.T) {
	result := Emit(buildAsyncSurface(), Config{})
	src := result.MochiSource
	// All async methods must return unit.
	for line := range strings.SplitSeq(src, "\n") {
		if strings.HasPrefix(line, "async extern fn") {
			if !strings.HasSuffix(strings.TrimSpace(line), "-> unit") {
				t.Errorf("expected -> unit return type; got: %s", line)
			}
		}
	}
}

func TestEmitSelfParam(t *testing.T) {
	result := Emit(buildAsyncSurface(), Config{})
	src := result.MochiSource
	// Instance methods must have self parameter.
	if !strings.Contains(src, "self: AcmeAsyncClient") {
		t.Errorf("expected self: AcmeAsyncClient param; got:\n%s", src)
	}
}

func TestLoopDriverField(t *testing.T) {
	// LoopDriver values should be distinct.
	if LoopRevolt == LoopReactPHP || LoopReactPHP == LoopAmp {
		t.Error("LoopDriver constants must be distinct")
	}
}

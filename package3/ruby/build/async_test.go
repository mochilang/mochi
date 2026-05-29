package build_test

import (
	"strings"
	"testing"

	"mochi/package3/ruby/build"
)

func TestParseAsyncFramework(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		want    build.AsyncFramework
		wantErr bool
	}{
		{name: "empty string is none", input: "", want: build.AsyncNone},
		{name: "none explicit", input: "none", want: build.AsyncNone},
		{name: "none upper", input: "NONE", want: build.AsyncNone},
		{name: "fiber", input: "fiber", want: build.AsyncFiber},
		{name: "fiber upper", input: "FIBER", want: build.AsyncFiber},
		{name: "async", input: "async", want: build.AsyncAsyncGem},
		{name: "async upper", input: "ASYNC", want: build.AsyncAsyncGem},
		{name: "ractor", input: "ractor", want: build.AsyncRactor},
		{name: "ractor upper", input: "RACTOR", want: build.AsyncRactor},
		{name: "whitespace trimmed", input: "  fiber  ", want: build.AsyncFiber},
		{name: "unknown value errors", input: "goroutine", wantErr: true},
		{name: "partial match errors", input: "fib", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := build.ParseAsyncFramework(tc.input)
			if tc.wantErr {
				if err == nil {
					t.Fatalf("expected error for input %q, got nil", tc.input)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error for input %q: %v", tc.input, err)
			}
			if got != tc.want {
				t.Errorf("ParseAsyncFramework(%q) = %v, want %v", tc.input, got, tc.want)
			}
		})
	}
}

func TestGenerateAsyncBootstrap_None(t *testing.T) {
	t.Run("AsyncNone returns empty string", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(build.AsyncConfig{Framework: build.AsyncNone})
		if got != "" {
			t.Errorf("AsyncNone should return empty string; got %q", got)
		}
	})
}

func TestGenerateAsyncBootstrap_Fiber(t *testing.T) {
	cfg := build.AsyncConfig{Framework: build.AsyncFiber}

	t.Run("non-empty for fiber", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if got == "" {
			t.Error("AsyncFiber should return non-empty Ruby code")
		}
	})

	t.Run("contains require fiber", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.Contains(got, `require "fiber"`) {
			t.Errorf("AsyncFiber bootstrap missing require fiber; got:\n%s", got)
		}
	})

	t.Run("starts with frozen_string_literal", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.HasPrefix(got, "# frozen_string_literal: true\n") {
			t.Errorf("Fiber bootstrap missing frozen_string_literal pragma")
		}
	})

	t.Run("fiber timeout included when set", func(t *testing.T) {
		cfgTimeout := build.AsyncConfig{Framework: build.AsyncFiber, FiberTimeout: 30}
		got := build.GenerateAsyncBootstrap(cfgTimeout)
		if !strings.Contains(got, "30") {
			t.Errorf("FiberTimeout not reflected in output; got:\n%s", got)
		}
	})

	t.Run("no timeout line when zero", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if strings.Contains(got, "MOCHI_FIBER_TIMEOUT") {
			t.Errorf("MOCHI_FIBER_TIMEOUT should not appear when FiberTimeout is 0")
		}
	})
}

func TestGenerateAsyncBootstrap_AsyncGem(t *testing.T) {
	cfg := build.AsyncConfig{Framework: build.AsyncAsyncGem}

	t.Run("contains require async", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.Contains(got, `require "async"`) {
			t.Errorf("AsyncAsyncGem bootstrap missing require async; got:\n%s", got)
		}
	})

	t.Run("contains Async Reactor", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.Contains(got, "Async") {
			t.Errorf("AsyncAsyncGem bootstrap should mention Async; got:\n%s", got)
		}
	})

	t.Run("starts with frozen_string_literal", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.HasPrefix(got, "# frozen_string_literal: true\n") {
			t.Errorf("AsyncGem bootstrap missing frozen_string_literal pragma")
		}
	})
}

func TestGenerateAsyncBootstrap_Ractor(t *testing.T) {
	cfg := build.AsyncConfig{Framework: build.AsyncRactor, RactorSafe: true}

	t.Run("contains Ractor", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.Contains(got, "Ractor") {
			t.Errorf("Ractor bootstrap should mention Ractor; got:\n%s", got)
		}
	})

	t.Run("contains make_shareable", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.Contains(got, "Ractor.make_shareable") {
			t.Errorf("Ractor bootstrap missing make_shareable; got:\n%s", got)
		}
	})

	t.Run("contains warning comment", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.Contains(got, "WARNING") {
			t.Errorf("Ractor bootstrap missing WARNING comment; got:\n%s", got)
		}
	})

	t.Run("starts with frozen_string_literal", func(t *testing.T) {
		got := build.GenerateAsyncBootstrap(cfg)
		if !strings.HasPrefix(got, "# frozen_string_literal: true\n") {
			t.Errorf("Ractor bootstrap missing frozen_string_literal pragma")
		}
	})

	t.Run("not ractor_safe includes extra warning", func(t *testing.T) {
		cfgUnsafe := build.AsyncConfig{Framework: build.AsyncRactor, RactorSafe: false}
		got := build.GenerateAsyncBootstrap(cfgUnsafe)
		if !strings.Contains(got, "ractor_safe is NOT set") {
			t.Errorf("non-ractor-safe config should warn; got:\n%s", got)
		}
	})
}

func TestGenerateAsyncBootstrap_AllFrameworks_ValidRuby(t *testing.T) {
	frameworks := []struct {
		name string
		cfg  build.AsyncConfig
	}{
		{"none", build.AsyncConfig{Framework: build.AsyncNone}},
		{"fiber", build.AsyncConfig{Framework: build.AsyncFiber}},
		{"async", build.AsyncConfig{Framework: build.AsyncAsyncGem}},
		{"ractor", build.AsyncConfig{Framework: build.AsyncRactor, RactorSafe: true}},
	}
	for _, fw := range frameworks {
		t.Run(fw.name+" does not panic", func(t *testing.T) {
			// Just ensure no panic.
			_ = build.GenerateAsyncBootstrap(fw.cfg)
		})
	}
}

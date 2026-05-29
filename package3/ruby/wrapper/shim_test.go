package wrapper_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	pkgerr "mochi/package3/ruby/errors"
	"mochi/package3/ruby/rbs"
	"mochi/package3/ruby/wrapper"
)

// redisFixture builds a minimal Redis GemSurface for use in tests.
func redisFixture() *rbs.GemSurface {
	return &rbs.GemSurface{
		Gem: "redis", Version: "5.0.8", Source: rbs.SourceBundled,
		Classes: []rbs.ClassDecl{{
			Name: "Redis",
			Methods: []rbs.MethodDecl{
				{Name: "get", Kind: rbs.MethodInstance, Types: []rbs.MethodType{{
					Params: []rbs.Param{{Name: "key", Type: &rbs.Type{Kind: rbs.TypeString}}},
					Return: &rbs.Type{Kind: rbs.TypeOptional, Elem: &rbs.Type{Kind: rbs.TypeString}},
				}}},
				{Name: "set", Kind: rbs.MethodInstance, Types: []rbs.MethodType{{
					Params: []rbs.Param{
						{Name: "key", Type: &rbs.Type{Kind: rbs.TypeString}},
						{Name: "value", Type: &rbs.Type{Kind: rbs.TypeString}},
					},
					Return: &rbs.Type{Kind: rbs.TypeString},
				}}},
				{Name: "del", Kind: rbs.MethodInstance, Types: []rbs.MethodType{{
					Params: []rbs.Param{{Name: "key", Type: &rbs.Type{Kind: rbs.TypeString}}},
					Return: &rbs.Type{Kind: rbs.TypeInteger},
				}}},
			},
		}},
	}
}

// nokogiriFixture builds a small Nokogiri-like surface with singleton methods.
func nokogiriFixture() *rbs.GemSurface {
	return &rbs.GemSurface{
		Gem: "nokogiri", Version: "1.16.2", Source: rbs.SourceBundled,
		Classes: []rbs.ClassDecl{
			{
				Name: "Nokogiri::HTML::Document",
				Methods: []rbs.MethodDecl{
					{Name: "parse", Kind: rbs.MethodSingleton, Types: []rbs.MethodType{{
						Params: []rbs.Param{{Name: "input", Type: &rbs.Type{Kind: rbs.TypeString}}},
						Return: &rbs.Type{Kind: rbs.TypeNamed, Name: "Nokogiri::HTML::Document"},
					}}},
					{Name: "css", Kind: rbs.MethodInstance, Types: []rbs.MethodType{{
						Params: []rbs.Param{{Name: "selector", Type: &rbs.Type{Kind: rbs.TypeString}}},
						Return: &rbs.Type{Kind: rbs.TypeNamed, Name: "Nokogiri::NodeSet"},
					}}},
				},
			},
		},
	}
}

// untypedFixture returns a surface with one method that should be skipped due
// to an untyped return.
func untypedFixture() *rbs.GemSurface {
	return &rbs.GemSurface{
		Gem: "some-gem", Version: "1.0.0", Source: rbs.SourceYARD,
		Classes: []rbs.ClassDecl{{
			Name: "SomeGem",
			Methods: []rbs.MethodDecl{
				{Name: "ok", Kind: rbs.MethodInstance, Types: []rbs.MethodType{{
					Params: []rbs.Param{{Name: "x", Type: &rbs.Type{Kind: rbs.TypeInteger}}},
					Return: &rbs.Type{Kind: rbs.TypeString},
				}}},
				{Name: "bad", Kind: rbs.MethodInstance, Types: []rbs.MethodType{{
					Params: []rbs.Param{{Name: "x", Type: &rbs.Type{Kind: rbs.TypeUntyped}}},
					Return: &rbs.Type{Kind: rbs.TypeString},
				}}},
				// Method with no types at all.
				{Name: "noTypes", Kind: rbs.MethodInstance, Types: nil},
			},
		}},
	}
}

func TestGenerateShims_RubyHeader(t *testing.T) {
	t.Run("contains frozen_string_literal", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.RubyShim, "# frozen_string_literal: true") {
			t.Errorf("shim.rb missing frozen_string_literal pragma")
		}
	})

	t.Run("contains require gem", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.RubyShim, `require "redis"`) {
			t.Errorf("shim.rb missing require statement; got:\n%s", res.RubyShim)
		}
	})

	t.Run("contains MochiWrap module", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.RubyShim, "module MochiWrap") {
			t.Errorf("shim.rb missing MochiWrap module; got:\n%s", res.RubyShim)
		}
	})
}

func TestGenerateShims_InstanceMethodReceiver(t *testing.T) {
	t.Run("instance method has __recv param", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.RubyShim, "def self.redis_get(__recv,") &&
			!strings.Contains(res.RubyShim, "def self.redis_get(__recv)") {
			t.Errorf("shim.rb instance method missing __recv; got:\n%s", res.RubyShim)
		}
	})

	t.Run("singleton method has no __recv in ruby shim", func(t *testing.T) {
		res, err := wrapper.GenerateShims(nokogiriFixture(), wrapper.ShimConfig{Gem: "nokogiri", Version: "1.16.2"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		// parse is a singleton; should not have __recv
		if strings.Contains(res.RubyShim, "def self.nokogiri_html_document_parse(__recv") {
			t.Errorf("singleton shim method should not have __recv; got:\n%s", res.RubyShim)
		}
	})
}

func TestGenerateShims_MochiShimExternFun(t *testing.T) {
	t.Run("extern fun present for get", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.MochiShim, "extern fun redis_get(") {
			t.Errorf("shim.mochi missing redis_get; got:\n%s", res.MochiShim)
		}
	})

	t.Run("extern fun for set has two typed params", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.MochiShim, "key: string") || !strings.Contains(res.MochiShim, "value: string") {
			t.Errorf("shim.mochi redis_set missing typed params; got:\n%s", res.MochiShim)
		}
	})

	t.Run("optional return type maps to string?", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		// get returns String? which should become string?
		if !strings.Contains(res.MochiShim, "string?") {
			t.Errorf("optional return should produce string?; got:\n%s", res.MochiShim)
		}
	})

	t.Run("integer return type maps to int", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.MochiShim, "): int") {
			t.Errorf("integer return should produce int; got:\n%s", res.MochiShim)
		}
	})

	t.Run("named return type becomes handle", func(t *testing.T) {
		res, err := wrapper.GenerateShims(nokogiriFixture(), wrapper.ShimConfig{Gem: "nokogiri", Version: "1.16.2"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.MochiShim, "handle<") {
			t.Errorf("named return type should produce handle<>; got:\n%s", res.MochiShim)
		}
	})
}

func TestGenerateShims_SkipCounting(t *testing.T) {
	t.Run("untyped param produces skip", func(t *testing.T) {
		res, err := wrapper.GenerateShims(untypedFixture(), wrapper.ShimConfig{Gem: "some-gem", Version: "1.0.0"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if res.SkipCount == 0 {
			t.Errorf("expected at least one skip, got 0")
		}
	})

	t.Run("no-types method produces skip", func(t *testing.T) {
		res, err := wrapper.GenerateShims(untypedFixture(), wrapper.ShimConfig{Gem: "some-gem", Version: "1.0.0"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		// "bad" (untyped param) and "noTypes" should both be skipped.
		if res.SkipCount < 2 {
			t.Errorf("expected at least 2 skips, got %d", res.SkipCount)
		}
	})

	t.Run("success + skip = total methods", func(t *testing.T) {
		surface := untypedFixture()
		res, err := wrapper.GenerateShims(surface, wrapper.ShimConfig{Gem: "some-gem", Version: "1.0.0"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		total := 0
		for _, cls := range surface.Classes {
			total += len(cls.Methods) + len(cls.Attrs)
		}
		if res.SuccessCount+res.SkipCount != total {
			t.Errorf("SuccessCount(%d) + SkipCount(%d) = %d, want %d",
				res.SuccessCount, res.SkipCount, res.SuccessCount+res.SkipCount, total)
		}
	})

	t.Run("skipped slice length equals SkipCount", func(t *testing.T) {
		res, err := wrapper.GenerateShims(untypedFixture(), wrapper.ShimConfig{Gem: "some-gem", Version: "1.0.0"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if len(res.Skipped) != res.SkipCount {
			t.Errorf("len(Skipped)=%d != SkipCount=%d", len(res.Skipped), res.SkipCount)
		}
	})
}

func TestGenerateShims_SkipReport(t *testing.T) {
	t.Run("skip report has gem name", func(t *testing.T) {
		res, err := wrapper.GenerateShims(untypedFixture(), wrapper.ShimConfig{Gem: "some-gem", Version: "1.0.0"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		for _, sr := range res.Skipped {
			if sr.Gem != "some-gem" {
				t.Errorf("skip report gem = %q, want %q", sr.Gem, "some-gem")
			}
		}
	})

	t.Run("skip report reason non-zero", func(t *testing.T) {
		res, err := wrapper.GenerateShims(untypedFixture(), wrapper.ShimConfig{Gem: "some-gem", Version: "1.0.0"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		for _, sr := range res.Skipped {
			if sr.Reason == pkgerr.SkipUnknown {
				t.Errorf("skip report reason should not be SkipUnknown for item %s", sr.Item)
			}
		}
	})
}

func TestGenerateShims_MochiHeader(t *testing.T) {
	t.Run("mochi header contains gem and version", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.MochiShim, "redis@5.0.8") {
			t.Errorf("mochi header missing gem@version; got:\n%s", res.MochiShim[:200])
		}
	})

	t.Run("mochi header contains skip count fraction", func(t *testing.T) {
		res, err := wrapper.GenerateShims(untypedFixture(), wrapper.ShimConfig{Gem: "some-gem", Version: "1.0.0"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.MochiShim, "skip:") {
			t.Errorf("mochi header missing skip count; got:\n%s", res.MochiShim[:200])
		}
	})
}

func TestGenerateShims_MethodNameMangling(t *testing.T) {
	t.Run("nokogiri parse becomes nokogiri_html_document_parse", func(t *testing.T) {
		res, err := wrapper.GenerateShims(nokogiriFixture(), wrapper.ShimConfig{Gem: "nokogiri", Version: "1.16.2"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.RubyShim, "def self.nokogiri_html_document_parse(") {
			t.Errorf("expected nokogiri_html_document_parse; got:\n%s", res.RubyShim)
		}
	})

	t.Run("redis get becomes redis_get", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if !strings.Contains(res.RubyShim, "def self.redis_get(") {
			t.Errorf("expected redis_get; got:\n%s", res.RubyShim)
		}
	})
}

func TestWriteShims(t *testing.T) {
	t.Run("creates shim.rb and shim.mochi", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		dir := t.TempDir()
		if err := wrapper.WriteShims(res, dir); err != nil {
			t.Fatalf("WriteShims: %v", err)
		}
		rbPath := filepath.Join(dir, "redis", "shim.rb")
		mochiPath := filepath.Join(dir, "redis", "shim.mochi")
		for _, p := range []string{rbPath, mochiPath} {
			if _, err := os.Stat(p); err != nil {
				t.Errorf("expected file %s to exist: %v", p, err)
			}
		}
	})

	t.Run("shim.rb content matches generated", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		dir := t.TempDir()
		if err := wrapper.WriteShims(res, dir); err != nil {
			t.Fatalf("WriteShims: %v", err)
		}
		data, err := os.ReadFile(filepath.Join(dir, "redis", "shim.rb"))
		if err != nil {
			t.Fatalf("ReadFile: %v", err)
		}
		if string(data) != res.RubyShim {
			t.Errorf("written shim.rb does not match RubyShim field")
		}
	})

	t.Run("creates gem subdirectory", func(t *testing.T) {
		res, err := wrapper.GenerateShims(nokogiriFixture(), wrapper.ShimConfig{Gem: "nokogiri", Version: "1.16.2"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		dir := t.TempDir()
		if err := wrapper.WriteShims(res, dir); err != nil {
			t.Fatalf("WriteShims: %v", err)
		}
		if _, err := os.Stat(filepath.Join(dir, "nokogiri")); err != nil {
			t.Errorf("expected gem subdirectory: %v", err)
		}
	})
}

func TestGenerateShims_GemNameFallback(t *testing.T) {
	t.Run("cfg gem overrides surface gem", func(t *testing.T) {
		surface := redisFixture()
		res, err := wrapper.GenerateShims(surface, wrapper.ShimConfig{Gem: "redis-alt", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if res.GemName != "redis-alt" {
			t.Errorf("GemName = %q, want %q", res.GemName, "redis-alt")
		}
	})

	t.Run("empty cfg gem falls back to surface gem", func(t *testing.T) {
		surface := redisFixture()
		res, err := wrapper.GenerateShims(surface, wrapper.ShimConfig{})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		if res.GemName != "redis" {
			t.Errorf("GemName = %q, want %q", res.GemName, "redis")
		}
	})
}

func TestGenerateShims_RubyShimClosedCorrectly(t *testing.T) {
	t.Run("ruby shim ends with end/end", func(t *testing.T) {
		res, err := wrapper.GenerateShims(redisFixture(), wrapper.ShimConfig{Gem: "redis", Version: "5.0.8"})
		if err != nil {
			t.Fatalf("GenerateShims: %v", err)
		}
		trimmed := strings.TrimRight(res.RubyShim, "\n")
		if !strings.HasSuffix(trimmed, "end") {
			t.Errorf("shim.rb should end with 'end'; got suffix: %q", trimmed[max(0, len(trimmed)-20):])
		}
	})
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

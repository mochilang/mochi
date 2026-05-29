// Package errors carries the cross-cutting error types the MEP-76 Ruby gem
// bridge emits at lock time and at build time. The most important one is
// SkipReport, which records why a particular RBS item was not translated into
// a Mochi extern fn binding. See [website/docs/research/0076/05-type-mapping.md]
// for the closed set of refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate an RBS item.
// The set mirrors the table in research note 05 §"Refusal reasons".
type SkipReason int

const (
	// SkipUnknown is the zero value. It must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipUntyped: the RBS type is `untyped`; no static type information.
	SkipUntyped
	// SkipTopBot: the type is `top` or `bot`; no Mochi analogue.
	SkipTopBot
	// SkipSelfType: the type is `self`, `instance`, or `class`; these are
	// open-class Ruby metaprogramming types with no Mochi static analogue.
	SkipSelfType
	// SkipComplexUnion: a union type beyond the `T | nil` pattern (which maps
	// to Mochi's optional T?). Three-way or higher unions are refused.
	SkipComplexUnion
	// SkipPlatformType: `IO`, `File`, `BasicObject`, `Encoding` — platform-
	// dependent types with no portable Mochi mapping.
	SkipPlatformType
	// SkipProc: a block or proc type whose arity or parameter types fall
	// outside the translatable set (arity > 5 or parameter out of table).
	SkipProc
	// SkipNativeExtension: the method is defined by a C extension and has no
	// RBS or YARD coverage; the bridge cannot inspect its type.
	SkipNativeExtension
	// SkipVoidParam: `void` appears in a non-return-type position.
	SkipVoidParam
	// SkipOpenClass: the method is added to a core Ruby class via open-class
	// extension (e.g. String#titleize in activesupport). These are discovered
	// at runtime only; the bridge cannot enumerate them statically.
	SkipOpenClass
)

func (r SkipReason) String() string {
	switch r {
	case SkipUntyped:
		return "untyped"
	case SkipTopBot:
		return "top/bot type"
	case SkipSelfType:
		return "self/instance/class type"
	case SkipComplexUnion:
		return "complex union (beyond T|nil)"
	case SkipPlatformType:
		return "platform-dependent type (IO/File/BasicObject/Encoding)"
	case SkipProc:
		return "proc/lambda arity or parameter out of table"
	case SkipNativeExtension:
		return "C extension method with no RBS/YARD coverage"
	case SkipVoidParam:
		return "void in non-return position"
	case SkipOpenClass:
		return "open-class extension method"
	default:
		return "unknown"
	}
}

// SkipReport records one item that the bridge declined to translate.
// It is emitted to <workdir>/ruby_wrap/<gem>/skip_report.txt and printed
// during `mochi pkg lock`.
type SkipReport struct {
	Gem     string     // gem name, e.g. "nokogiri"
	Version string     // gem version, e.g. "1.16.2"
	Item    string     // qualified item name, e.g. "Nokogiri::HTML::Document#css"
	Reason  SkipReason // why the bridge skipped this item
	RBSType string     // the raw RBS type string that triggered the skip
}

func (s *SkipReport) Error() string {
	return fmt.Sprintf("skip %s %s: %s (rbs: %s)", s.Item, s.Gem, s.Reason, s.RBSType)
}

// LockConflict is returned when Bundler's dependency resolver cannot satisfy
// the combined version constraints from [ruby-dependencies].
type LockConflict struct {
	GemA       string
	ConstraintA string
	GemB       string
	ConstraintB string
	Message    string
}

func (e *LockConflict) Error() string {
	return fmt.Sprintf("ruby lock conflict: %s requires %s but %s requires %s: %s",
		e.GemA, e.ConstraintA, e.GemB, e.ConstraintB, e.Message)
}

// NativeExtensionSkip is returned when a gem has a native C extension and
// neither a pre-built binary gem nor a pure-Ruby alternative is available.
type NativeExtensionSkip struct {
	Gem      string
	Platform string
	Advice   string
}

func (e *NativeExtensionSkip) Error() string {
	return fmt.Sprintf("gem %q has a native extension with no binary gem for platform %q: %s",
		e.Gem, e.Platform, e.Advice)
}

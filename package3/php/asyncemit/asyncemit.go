// Package asyncemit emits async extern declarations for PHP methods that
// return ReactPHP / Amp / Revolt promise types.
//
// PHP async patterns supported:
//   - React\Promise\PromiseInterface (ReactPHP)
//   - React\Promise\Promise (ReactPHP concrete)
//   - Amp\Promise (Amp v2)
//   - Amp\Future (Amp v3 / Revolt)
//   - GuzzleHttp\Promise\PromiseInterface (Guzzle)
//   - GuzzleHttp\Promise\Promise (Guzzle)
//
// For each detected async method the emitter produces:
//
//	async extern fn handle_method_name(self: Handle, ...) -> unit
//
// The inner type of the promise is not recoverable from PHP's reflection
// surface (PHP lacks generic reification), so all async return types map to
// unit. Callers that need typed results should annotate via overlay files.
//
// Usage:
//
//	cfg := asyncemit.Config{LoopDriver: asyncemit.LoopRevolt}
//	result := asyncemit.Emit(surface, cfg)
package asyncemit

import (
	"strings"

	"mochi/package3/php/externemit"
	"mochi/package3/php/reflect"
)

// LoopDriver identifies the PHP async event-loop backend.
type LoopDriver int

const (
	// LoopRevolt uses Revolt PHP fiber-based event loop (recommended for PHP 8.1+).
	LoopRevolt LoopDriver = iota
	// LoopReactPHP uses the ReactPHP event loop.
	LoopReactPHP
	// LoopAmp uses the Amp v3 event loop.
	LoopAmp
)

// Config controls async extern emission.
type Config struct {
	// LoopDriver is the async runtime to target. Default: LoopRevolt.
	LoopDriver LoopDriver
	// ExtraPromiseTypes adds additional fully-qualified PHP type names that
	// should be treated as promise/future types.
	ExtraPromiseTypes []string
}

// EmitResult holds the Mochi source produced by Emit.
type EmitResult struct {
	// MochiSource is the emitted async extern declarations.
	MochiSource string
	// AsyncMethodCount is the number of async methods detected.
	AsyncMethodCount int
}

// knownPromiseTypes is the set of FQCN suffixes (without leading backslash)
// that indicate an async return type.
var knownPromiseTypes = []string{
	`React\Promise\PromiseInterface`,
	`React\Promise\Promise`,
	`Amp\Promise`,
	`Amp\Future`,
	`GuzzleHttp\Promise\PromiseInterface`,
	`GuzzleHttp\Promise\Promise`,
}

// IsPromiseType reports whether the given PHP return type string (as it appears
// in a ReflectionSurface) is a known async promise/future type.
func IsPromiseType(phpType string, extra []string) bool {
	t := strings.TrimPrefix(phpType, "\\")
	for _, known := range knownPromiseTypes {
		if t == known || strings.HasSuffix(t, known) {
			return true
		}
	}
	for _, e := range extra {
		et := strings.TrimPrefix(e, "\\")
		if t == et || strings.HasSuffix(t, et) {
			return true
		}
	}
	return false
}

// Emit scans the surface for methods whose return type is a known promise/
// future type, and emits async extern fn declarations.
func Emit(surface *reflect.ReflectionSurface, cfg Config) *EmitResult {
	e := &emitter{extra: cfg.ExtraPromiseTypes}

	for _, cls := range surface.Classes {
		handle := classHandle(cls.FQCN)
		for _, m := range cls.Methods {
			if strings.HasPrefix(m.Name, "__") {
				continue
			}
			if !IsPromiseType(m.ReturnType, cfg.ExtraPromiseTypes) {
				continue
			}
			e.emitAsyncMethod(handle, m, false)
		}
	}
	for _, iface := range surface.Interfaces {
		handle := classHandle(iface.FQCN)
		for _, m := range iface.Methods {
			if strings.HasPrefix(m.Name, "__") {
				continue
			}
			if !IsPromiseType(m.ReturnType, cfg.ExtraPromiseTypes) {
				continue
			}
			e.emitAsyncMethod(handle, m, false)
		}
	}

	return &EmitResult{
		MochiSource:      e.render(),
		AsyncMethodCount: e.count,
	}
}

type emitter struct {
	extra []string
	lines []string
	count int
}

func (e *emitter) addLine(s string) {
	e.lines = append(e.lines, s)
}

func (e *emitter) render() string {
	if len(e.lines) == 0 {
		return ""
	}
	return strings.Join(e.lines, "\n") + "\n"
}

func (e *emitter) emitAsyncMethod(handle string, m reflect.MethodSurface, static bool) {
	fnName := toSnakeCase(handle) + "_" + toSnakeCase(m.Name)
	var params []string
	if !static {
		params = append(params, "self: "+handle)
	}
	for _, p := range m.Parameters {
		if p.Variadic {
			continue
		}
		params = append(params, toSnakeCase(p.Name)+": unit")
	}
	e.addLine("async extern fn " + fnName + "(" + strings.Join(params, ", ") + ") -> unit")
	e.count++
}

// classHandle converts a FQCN like "GuzzleHttp\\Client" to "GuzzleHttpClient".
func classHandle(fqcn string) string {
	fqcn = strings.TrimPrefix(fqcn, "\\")
	parts := strings.Split(fqcn, "\\")
	var sb strings.Builder
	for _, p := range parts {
		if len(p) > 0 {
			sb.WriteString(strings.ToUpper(p[:1]) + p[1:])
		}
	}
	return sb.String()
}

// toSnakeCase converts PascalCase or camelCase to snake_case.
func toSnakeCase(s string) string {
	return externemit.ToSnakeCase(s)
}

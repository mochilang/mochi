// Package shimgen synthesises the C# shim project for each imported NuGet
// package. The generated code uses [UnmanagedCallersOnly] to expose package
// methods to native code via a stable C ABI. This is analogous to
// package3/rust/wrapper, which synthesises extern "C" Rust wrappers.
package shimgen

import (
	"mochi/package3/dotnet/errors"
	"mochi/package3/dotnet/typemap"
)

// Shim is the synthesised C# shim for one NuGet package.
// Analogous to wrapper.Crate in package3/rust.
type Shim struct {
	// Package is the NuGet package id (e.g. "Newtonsoft.Json").
	Package string
	// PackageVersion is the resolved version (e.g. "13.0.3").
	PackageVersion string
	// TargetFramework is the TFM (e.g. "net8.0").
	TargetFramework string
	// Methods is the list of synthesised shim methods.
	Methods []ShimMethod
	// Skipped is the list of items not translated.
	Skipped []errors.SkipReport
}

// ShimMethod is one [UnmanagedCallersOnly] method in the shim.
type ShimMethod struct {
	// EntryPoint is the exported C symbol name,
	// e.g. "mochi_newtonsoftjson_JsonConvert_SerializeObject".
	EntryPoint string
	// CSMethodName is the C# method name in the Bridge class,
	// e.g. "JsonConvert_SerializeObject".
	CSMethodName string
	// UpstreamCall is the fully-qualified C# call expression,
	// e.g. "Newtonsoft.Json.JsonConvert.SerializeObject".
	UpstreamCall string
	// Params is the list of synthesised parameters.
	Params []ShimParam
	// Return is the return type mapping (nil = void).
	Return *typemap.Mapping
	// IsAsync reports whether the upstream method is async (Task<T> return).
	IsAsync bool
	// IsStatic reports whether the upstream method is static.
	IsStatic bool
	// DocComment is an optional XML-stripped doc string.
	DocComment string
}

// ShimParam is one parameter in a ShimMethod.
type ShimParam struct {
	// Name is the C# parameter name.
	Name string
	// Mapping is the type mapping.
	Mapping typemap.Mapping
}

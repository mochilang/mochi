package ffi

// PythonValue is a placeholder for a value from a Python module.
// Resolution is deferred until used so chains like math.sqrt(x) can be built.
type PythonValue struct {
	Module string
	Attrs  []string
}

// GoValue is a placeholder for a value from a Go module.
type GoValue struct {
	Module string
	Attrs  []string
}

// TSValue is a placeholder for a value from a TypeScript module accessed via Deno.
type TSValue struct {
	Module string
	Attrs  []string
}

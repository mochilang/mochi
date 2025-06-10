package ffi

// Caller invokes a registered foreign function by name with arguments.
type Caller interface {
	Call(name string, args ...any) (any, error)
}

// Registerer allows registration of functions for later invocation.
type Registerer interface {
	Register(name string, fn any) error
}

// Loader loads modules that may register additional functions.
type Loader interface {
	LoadModule(path string) error
}

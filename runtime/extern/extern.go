package extern

import "sync"

// Registry manages a collection of registered objects.
type Registry struct {
	mu      sync.RWMutex
	objects map[string]any
}

// NewRegistry creates a new empty Registry.
func NewRegistry() *Registry {
	return &Registry{objects: make(map[string]any)}
}

// Register associates an object with a name.
func (r *Registry) Register(name string, obj any) {
	r.mu.Lock()
	r.objects[name] = obj
	r.mu.Unlock()
}

// Get retrieves a registered object.
func (r *Registry) Get(name string) (any, bool) {
	r.mu.RLock()
	obj, ok := r.objects[name]
	r.mu.RUnlock()
	return obj, ok
}

// Reset clears all registered objects.
func (r *Registry) Reset() {
	r.mu.Lock()
	r.objects = make(map[string]any)
	r.mu.Unlock()
}

// DefaultRegistry is used when package-level functions are called.
var DefaultRegistry = NewRegistry()

// Register associates an object with a name using the DefaultRegistry.
func Register(name string, obj any) { DefaultRegistry.Register(name, obj) }

// Get retrieves a registered object from the DefaultRegistry.
func Get(name string) (any, bool) { return DefaultRegistry.Get(name) }

// Reset clears all objects from the DefaultRegistry.
func Reset() { DefaultRegistry.Reset() }

package extern

import "sync"

var (
	mu      sync.RWMutex
	objects = make(map[string]any)
)

// Register associates an object with a name.
func Register(name string, obj any) {
	mu.Lock()
	objects[name] = obj
	mu.Unlock()
}

// Get retrieves a registered object.
func Get(name string) (any, bool) {
	mu.RLock()
	obj, ok := objects[name]
	mu.RUnlock()
	return obj, ok
}

// Reset clears all registered objects.
func Reset() {
	mu.Lock()
	objects = make(map[string]any)
	mu.Unlock()
}

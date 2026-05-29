package typemap

// AsyncPolicy configures how CompletableFuture return values are bridged
// back to Mochi. The default policy converts the future to a blocking call
// inside the JNI thread; the goroutine policy schedules a Go callback.
type AsyncPolicy int

const (
	// AsyncBlocking blocks the calling Go goroutine until the Java future
	// resolves. Safe but holds a goroutine for the duration of the future.
	AsyncBlocking AsyncPolicy = iota
	// AsyncGoroutine wires the CompletableFuture.thenAccept callback to a
	// Go goroutine via a JNI callback handle. Requires the JNI runtime to
	// be initialised with an attached JVM (build tag java_jni).
	AsyncGoroutine
)

// AsyncMapping augments a Mapping for a CompletableFuture return type with
// the chosen bridging policy and derived Go channel type.
type AsyncMapping struct {
	// Mapping is the base mapping for the CompletableFuture.
	Mapping *Mapping
	// Policy selects the bridging strategy.
	Policy AsyncPolicy
	// ChannelType is the Go channel element type expression for the resolved
	// value, e.g. "string" for CompletableFuture<String>.
	ChannelType string
}

// NewAsyncMapping wraps a KindFuture mapping with the given policy.
// It returns nil when m is not a KindFuture mapping.
func NewAsyncMapping(m *Mapping, policy AsyncPolicy) *AsyncMapping {
	if m == nil || m.Kind != KindFuture {
		return nil
	}
	chType := "any"
	if m.Elem != nil {
		chType = m.Elem.MochiType
	}
	return &AsyncMapping{Mapping: m, Policy: policy, ChannelType: chType}
}

// String returns a human-readable description of the async policy.
func (p AsyncPolicy) String() string {
	switch p {
	case AsyncBlocking:
		return "blocking"
	case AsyncGoroutine:
		return "goroutine"
	default:
		return "unknown"
	}
}

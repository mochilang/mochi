package stream

import (
	"context"
	"sync"
	"sync/atomic"
	"time"
)

// Event represents a single item in the stream with metadata.
type Event struct {
	Stream string    // Name of the stream
	Tx     int64     // Monotonic transaction ID
	Time   time.Time // Timestamp of the event
	Data   any       // Application-defined payload
}

// Stream defines the public operations for a stream implementation.
type Stream interface {
	Emit(ctx context.Context, data any) (Event, error)
	Subscribe(name string, handler func(*Event) error) Subscriber
	Close()
}

// Subscriber represents an active subscription to a stream.
type Subscriber interface {
	Watch(ctx context.Context, fn func(*Event) error) error
	Close()
}

// streamState holds the in-memory slice of events and stream metadata.
type streamState struct {
	name  string
	ring  []*Event
	tx    int64 // Next tx to assign (exclusive)
	first int64 // First tx retained (inclusive)
	len   int
	mu    sync.RWMutex
}

// Stream is an append-only in-memory log with concurrent access and subscriber tracking.
type stream struct {
	state atomic.Pointer[streamState]
	subs  sync.Map // map[string]*subscriber
}

// New creates a new stream with a given name and initial ring capacity.
func New(name string, capacity int) Stream {
	s := &stream{}
	s.state.Store(&streamState{
		name:  name,
		ring:  make([]*Event, 0, capacity),
		tx:    1,
		first: 1,
	})
	return s
}

// Emit inserts a new event into the stream and notifies all subscribers.
func (s *stream) Emit(ctx context.Context, data any) (Event, error) {
	st := s.state.Load()

	st.mu.Lock()
	defer st.mu.Unlock()

	tx := st.tx
	st.tx++

	ev := Event{
		Stream: st.name,
		Tx:     tx,
		Time:   time.Now(),
		Data:   data,
	}

	offset := tx - st.first
	if int(offset) >= len(st.ring) {
		// Extend ring slice with nils if there's a gap
		for int64(len(st.ring)) <= offset {
			st.ring = append(st.ring, nil)
		}
	}

	st.ring[offset] = &ev
	st.len = int(st.tx - st.first)

	s.subs.Range(func(_, v any) bool {
		sub := v.(*subscriber)
		select {
		case sub.updateCh <- tx:
		default:
		}
		return true
	})

	return ev, nil
}

// Get returns a single event by tx if still available.
func (s *stream) Get(tx int64) (*Event, bool) {
	st := s.state.Load()

	st.mu.RLock()
	defer st.mu.RUnlock()

	if tx < st.first || tx >= st.tx {
		return nil, false
	}
	offset := tx - st.first
	if int(offset) >= len(st.ring) {
		return nil, false
	}
	ev := st.ring[offset]
	if ev == nil || ev.Tx != tx {
		return nil, false
	}
	return ev, true
}

// Read returns up to `count` events starting from `start` tx.
func (s *stream) Read(start, count int64) ([]*Event, error) {
	st := s.state.Load()

	st.mu.RLock()
	defer st.mu.RUnlock()

	var out []*Event
	for i := int64(0); i < count; i++ {
		tx := start + i
		if tx < st.first || tx >= st.tx {
			break
		}
		offset := tx - st.first
		if int(offset) >= len(st.ring) {
			break
		}
		ev := st.ring[offset]
		if ev != nil && ev.Tx == tx {
			out = append(out, ev)
		}
	}
	return out, nil
}

// Compact discards events older than the minimum subscriber cursor.
func (s *stream) Compact() {
	st := s.state.Load()

	st.mu.Lock()
	defer st.mu.Unlock()

	min := s.minCursorLocked()

	// log.Printf("[Compact] first=%d, tx=%d, minCursor=%d, len=%d\n", st.first, st.tx, min, st.len)

	if min <= st.first {
		// log.Println("[Compact] No compaction needed.")
		return
	}

	dropCount := min - st.first
	if dropCount > int64(len(st.ring)) {
		dropCount = int64(len(st.ring))
	}
	st.ring = st.ring[dropCount:]
	st.first = min
	st.len = int(st.tx - st.first)

	// log.Printf("[Compact] Dropped %d events. New first=%d, len=%d\n", dropCount, st.first, st.len)
}

// Len returns the number of events currently retained in memory.
func (s *stream) Len() int {
	st := s.state.Load()

	st.mu.RLock()
	defer st.mu.RUnlock()
	return st.len
}

// FirstTx returns the earliest transaction ID still retained.
func (s *stream) FirstTx() int64 {
	st := s.state.Load()

	st.mu.RLock()
	defer st.mu.RUnlock()
	return st.first
}

// NextTx returns the next transaction ID that will be assigned.
func (s *stream) NextTx() int64 {
	st := s.state.Load()

	st.mu.RLock()
	defer st.mu.RUnlock()
	return st.tx
}

// RegisterSubscriber creates a named subscriber and tracks its cursor.
func (s *stream) RegisterSubscriber(name string) *subscriber {
	st := s.state.Load()
	sub := &subscriber{
		name:     name,
		stream:   s,
		updateCh: make(chan int64, 1),
	}
	sub.cursor.Store(st.first)
	s.subs.Store(name, sub)
	return sub
}

// Subscribe registers a handler and begins watching the stream.
func (s *stream) Subscribe(name string, handler func(*Event) error) Subscriber {
	sub := s.RegisterSubscriber(name)
	ctx, cancel := context.WithCancel(context.Background())
	sub.cancel = cancel
	go func() {
		defer sub.Close()
		_ = sub.Watch(ctx, handler)
	}()
	return sub
}

// minCursorLocked returns the minimum cursor value across all subscribers.
// Must be called under st.mu.
func (s *stream) minCursorLocked() int64 {
	st := s.state.Load()
	min := st.tx

	s.subs.Range(func(name, v any) bool {
		sub := v.(*subscriber)
		cur := sub.cursor.Load()
		// log.Printf("[minCursor] %s: cursor=%d\n", name, cur)
		if cur < min {
			min = cur
		}
		return true
	})

	// log.Printf("[minCursor] min=%d\n", min)
	return min
}

// updateMinCursor forces recalculation of the FirstTx based on subscriber positions.
func (s *stream) updateMinCursor() {
	st := s.state.Load()
	st.mu.Lock()
	defer st.mu.Unlock()

	newMin := s.minCursorLocked()
	if newMin > st.first {
		dropCount := newMin - st.first
		if dropCount > int64(len(st.ring)) {
			dropCount = int64(len(st.ring))
		}
		st.ring = st.ring[dropCount:]
		st.first = newMin
		st.len = int(st.tx - st.first)
	}
}

// Close shuts down the stream and all subscribers.
func (s *stream) Close() {
	s.subs.Range(func(k, v any) bool {
		sub := v.(*subscriber)
		sub.Close()
		return true
	})
}

// --- Subscriber ---

// Subscriber represents a reader that maintains a cursor over the stream.
type subscriber struct {
	name      string
	stream    *stream
	cursor    atomic.Int64
	updateCh  chan int64
	cancel    context.CancelFunc
	closeOnce sync.Once
}

// UpdateCh returns a channel that receives new tx notifications.
func (s *subscriber) UpdateCh() <-chan int64 {
	return s.updateCh
}

// AdvanceTo moves the subscriber cursor forward and updates stream cursor.
func (s *subscriber) AdvanceTo(tx int64) {
	if tx > s.cursor.Load() {
		s.cursor.Store(tx)
		s.stream.updateMinCursor()
	}
}

// Cursor returns the current cursor of the subscriber.
func (s *subscriber) Cursor() int64 {
	return s.cursor.Load()
}

// Watch loops over new updates and calls a handler on each event.
func (s *subscriber) Watch(ctx context.Context, fn func(ev *Event) error) error {
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case tx, ok := <-s.updateCh:
			if !ok {
				return nil
			}
			ev, ok := s.stream.Get(tx)
			if !ok {
				continue
			}
			if err := fn(ev); err != nil {
				return err
			}
			s.AdvanceTo(tx + 1)
		}
	}
}

// Close terminates the subscriber and removes it from its stream.
func (s *subscriber) Close() {
	if s.cancel != nil {
		s.cancel()
	}
	s.stream.subs.Delete(s.name)
	s.closeOnce.Do(func() {
		close(s.updateCh)
	})
	s.stream.updateMinCursor()
}

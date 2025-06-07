package stream

import (
	"context"
	"sync"
	"time"
)

// Event represents a single item in the stream with metadata.
type Event struct {
	Stream string    // Name of the stream
	Tx     int64     // Monotonic transaction ID
	Time   time.Time // Timestamp of the event
	Data   any       // Application-defined payload
}

// Stream represents an append-only in-memory log with fixed capacity.
// Events are stored in a ring buffer. Older events may be evicted via Compact.
type Stream struct {
	mu       sync.RWMutex
	name     string
	capacity int
	ring     []*Event
	tx       int64 // Next tx to assign (exclusive)
	first    int64 // First tx retained (inclusive)
	len      int

	subs map[string]*Subscriber // Registered subscribers
}

// New creates a new stream with a given name and ring buffer capacity.
func New(name string, capacity int) *Stream {
	// fmt.Printf(">> Stream[%s] initialized with capacity=%d\n", name, capacity)
	return &Stream{
		name:     name,
		capacity: capacity,
		ring:     make([]*Event, capacity),
		tx:       1,
		first:    1,
		subs:     make(map[string]*Subscriber),
	}
}

// Append inserts a new event into the stream and notifies all subscribers.
func (s *Stream) Append(ctx context.Context, data any) (Event, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	tx := s.tx
	s.tx++

	ev := Event{
		Stream: s.name,
		Tx:     tx,
		Time:   time.Now(),
		Data:   data,
	}

	idx := tx % int64(s.capacity)
	old := s.ring[idx]
	s.ring[idx] = &ev
	if old == nil {
		s.len++
	}

	// fmt.Printf(">> Append(tx=%d, data=%v) -> slot=%d\n", tx, data, idx)

	for _, sub := range s.subs {
		select {
		case sub.updateCh <- tx:
		default:
		}
	}

	return ev, nil
}

// Read returns up to `count` events starting from `start` tx.
// Events not retained in the ring will be skipped.
func (s *Stream) Read(start, count int64) ([]*Event, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	var out []*Event
	for i := int64(0); i < count; i++ {
		tx := start + i
		if tx < s.first || tx >= s.tx {
			break
		}
		idx := tx % int64(s.capacity)
		ev := s.ring[idx]
		if ev != nil && ev.Tx == tx {
			out = append(out, ev)
		}
	}
	// fmt.Printf(">> Read(start=%d, count=%d) -> %d events\n", start, count, len(out))
	return out, nil
}

// Get returns a single event by tx if still available.
func (s *Stream) Get(tx int64) (*Event, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	if tx < s.first || tx >= s.tx {
		// fmt.Printf(">> Get(tx=%d): out of range [%d,%d)\n", tx, s.first, s.tx)
		return nil, false
	}
	idx := tx % int64(s.capacity)
	ev := s.ring[idx]
	if ev == nil || ev.Tx != tx {
		// fmt.Printf(">> Get(tx=%d): slot empty or mismatch\n", tx)
		return nil, false
	}
	// fmt.Printf(">> Get(tx=%d): found event with data=%v\n", tx, ev.Data)
	return ev, true
}

// Compact discards events older than the minimum subscriber cursor.
func (s *Stream) Compact() {
	s.mu.Lock()
	defer s.mu.Unlock()

	min := s.minCursorUnsafe()
	if min <= s.first {
		// fmt.Printf(">> Compact: no change (min=%d, first=%d)\n", min, s.first)
		return
	}

	dropped := 0
	for tx := s.first; tx < min; tx++ {
		idx := tx % int64(s.capacity)
		if s.ring[idx] != nil {
			// fmt.Printf(">> Compact: drop tx=%d from slot=%d (data=%v)\n", tx, idx, s.ring[idx].Data)
			s.ring[idx] = nil
			dropped++
		}
	}
	s.first = min
	s.len -= dropped
	// fmt.Printf(">> Compact: updated firstTx=%d, dropped=%d, len=%d\n", s.first, dropped, s.len)
}

// Len returns the number of events currently retained in memory.
func (s *Stream) Len() int {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.len
}

// FirstTx returns the earliest transaction ID still retained.
func (s *Stream) FirstTx() int64 {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.first
}

// NextTx returns the next transaction ID that will be assigned.
func (s *Stream) NextTx() int64 {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.tx
}

// RegisterSubscriber creates a named subscriber and tracks its cursor.
func (s *Stream) RegisterSubscriber(name string) *Subscriber {
	s.mu.Lock()
	defer s.mu.Unlock()

	sub := &Subscriber{
		name:     name,
		stream:   s,
		cursor:   s.first,
		updateCh: make(chan int64, 1),
	}
	s.subs[name] = sub
	s.updateMinTxLocked()
	// fmt.Printf(">> Subscriber[%s] registered at cursor=%d\n", name, sub.cursor)
	return sub
}

// updateMinTxLocked updates the minimum retained tx based on all cursors.
func (s *Stream) updateMinTxLocked() {
	min := s.tx
	for _, sub := range s.subs {
		if sub.cursor < min {
			min = sub.cursor
		}
	}
	if min != s.first {
		s.first = min
		// fmt.Printf(">> updated minTx to %d\n", min)
	}
}

// minCursorUnsafe returns the minimum subscriber cursor without locking.
func (s *Stream) minCursorUnsafe() int64 {
	min := s.tx
	for _, sub := range s.subs {
		if sub.cursor < min {
			min = sub.cursor
		}
	}
	return min
}

func (s *Subscriber) Cursor() int64 {
	s.stream.mu.RLock()
	defer s.stream.mu.RUnlock()
	return s.cursor
}

// Close shuts down the stream and notifies all subscribers.
func (s *Stream) Close() {
	s.mu.Lock()
	defer s.mu.Unlock()
	for _, sub := range s.subs {
		close(sub.updateCh)
		// fmt.Printf(">> Closed updateCh for subscriber[%s]\n", sub.name)
	}
	s.subs = nil
}

// --- Subscriber ---

// Subscriber represents a reader that maintains a cursor over the stream.
type Subscriber struct {
	name     string
	stream   *Stream
	cursor   int64
	updateCh chan int64
}

// UpdateCh returns a channel that receives new tx notifications.
func (s *Subscriber) UpdateCh() <-chan int64 {
	return s.updateCh
}

// AdvanceTo moves the subscriber cursor forward, allowing old events to be compacted.
func (s *Subscriber) AdvanceTo(tx int64) {
	s.stream.mu.Lock()
	defer s.stream.mu.Unlock()

	if tx > s.cursor {
		// fmt.Printf(">> Subscriber[%s] AdvanceTo(%d)\n", s.name, tx)
		s.cursor = tx
		s.stream.updateMinTxLocked()
	}
}

// Watch loops over new updates and calls a handler on each event.
func (s *Subscriber) Watch(ctx context.Context, fn func(ev *Event) error) error {
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
			s.AdvanceTo(tx + 1) // ✅ This is required
		}
	}
}

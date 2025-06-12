package stream

import (
	"context"
	"reflect"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestStream_AppendAndRead(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("test", 4, wg).(*stream)

	_, err := s.Emit(context.Background(), "a")
	require.NoError(t, err)

	_, err = s.Emit(context.Background(), "b")
	require.NoError(t, err)

	evs, err := s.Read(1, 2)
	require.NoError(t, err)
	require.Len(t, evs, 2)
	require.Equal(t, "a", evs[0].Data)
	require.Equal(t, "b", evs[1].Data)
}

func TestStream_Get(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("test", 4, wg).(*stream)

	ev1, err := s.Emit(context.Background(), "a")
	require.NoError(t, err)
	ev2, err := s.Emit(context.Background(), "b")
	require.NoError(t, err)

	ev, ok := s.Get(ev1.Tx)
	require.True(t, ok)
	require.Equal(t, "a", ev.Data)

	ev, ok = s.Get(ev2.Tx)
	require.True(t, ok)
	require.Equal(t, "b", ev.Data)
}

func TestStream_SubscriberCursor(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("test", 4, wg).(*stream)

	for i := 0; i < 5; i++ {
		_, err := s.Emit(context.Background(), i)
		require.NoError(t, err)
	}

	sub := s.RegisterSubscriber("reader")
	sub.AdvanceTo(4)

	waitForMinCursor(t, s, 4)

	require.Equal(t, int64(4), s.FirstTx())
}

func TestStream_Compact(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("metrics", 16, wg).(*stream)

	sub := s.RegisterSubscriber("reader") // ✅ move this up

	for i := 0; i < 101; i++ {
		_, err := s.Emit(context.Background(), i)
		require.NoError(t, err)
	}

	sub.AdvanceTo(91)

	waitForMinCursor(t, s, 91)

	s.Compact()

	require.Equal(t, int64(91), s.FirstTx())
	require.Equal(t, int64(102), s.NextTx())
	require.Equal(t, 11, s.Len()) // tx 91..101 inclusive

	_, ok := s.Get(90)
	require.False(t, ok)

	ev, ok := s.Get(91)
	require.True(t, ok)
	require.Equal(t, 90, ev.Data)
}

func TestStream_Close(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("test", 4, wg).(*stream)
	sub1 := s.RegisterSubscriber("a")
	sub2 := s.RegisterSubscriber("b")

	sub1.AdvanceTo(5)
	sub2.AdvanceTo(5)

	s.Close()

	select {
	case _, ok := <-sub1.UpdateCh():
		require.False(t, ok, "sub1 updateCh should be closed")
	case <-time.After(50 * time.Millisecond):
		t.Fatal("timeout waiting for sub1 updateCh to close")
	}
}

// --- Helper ---

func waitForMinCursor(t *testing.T, s *stream, want int64) {
	t.Helper()
	start := time.Now()
	for time.Since(start) < time.Second {
		if s.FirstTx() == want {
			return
		}
		time.Sleep(10 * time.Millisecond)
	}
	t.Fatalf("minCursor did not advance to %d (got %d)", want, s.FirstTx())
}

func TestStream_AdvanceCursorNoShrink(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("metrics", 4, wg).(*stream)

	// Append a few events
	for i := 0; i < 4; i++ {
		_, err := s.Emit(context.Background(), i)
		require.NoError(t, err)
	}

	_ = s.RegisterSubscriber("slow-reader")

	// Compact should be a no-op because sub.cursor == 1
	s.Compact()
	require.Equal(t, int64(1), s.FirstTx())
}

func TestStream_SubscriberUpdateCh(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("test", 4, wg).(*stream)
	sub := s.RegisterSubscriber("watcher")

	go func() {
		time.Sleep(10 * time.Millisecond)
		s.Emit(context.Background(), "event-1")
	}()

	select {
	case tx := <-sub.UpdateCh():
		require.Equal(t, int64(1), tx)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("did not receive update")
	}
}

func TestStream_EmptyRead(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("empty", 4, wg).(*stream)
	evs, err := s.Read(1, 10)
	require.NoError(t, err)
	require.Len(t, evs, 0)
}

func TestStream_AutoGrowRing(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("log", 4, wg).(*stream)

	for i := 0; i < 10; i++ {
		_, err := s.Emit(context.Background(), i)
		require.NoError(t, err)
	}

	require.Equal(t, 10, s.Len())
	require.Equal(t, int64(1), s.FirstTx()) // nothing should be evicted

	// st := s.StateForTest()
	// require.GreaterOrEqual(t, st.capacity, 10) // ensure auto-grow actually happened

	for i := 1; i <= 10; i++ {
		ev, ok := s.Get(int64(i))
		require.True(t, ok)
		require.Equal(t, i-1, ev.Data)
	}
}

func TestSubscriber_AdvanceToAffectsFirstTx(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("subtest", 8, wg).(*stream)

	// Append 10 events: ring should auto-grow to prevent overwrite
	for i := 0; i < 10; i++ {
		_, err := s.Emit(context.Background(), i)
		require.NoError(t, err)
	}

	sub := s.RegisterSubscriber("reader")

	// Ring should have grown — all tx from 1..10 should still be available
	require.Equal(t, int64(1), s.FirstTx(), "Auto-growing ring should preserve all entries")

	// Advance the subscriber's cursor to tx=6
	sub.AdvanceTo(6)

	// Wait for stream to recompute the new minimum cursor
	waitForMinCursor(t, s, 6)

	// FirstTx should reflect compaction triggered by cursor advancement
	require.Equal(t, int64(6), s.FirstTx(), "Compaction should remove tx < 6")
}

func TestSubscriber_UpdateChReceivesTx(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("subtest", 8, wg).(*stream)
	sub := s.RegisterSubscriber("reader")

	_, err := s.Emit(context.Background(), "hello")
	require.NoError(t, err)

	select {
	case tx := <-sub.UpdateCh():
		require.Equal(t, int64(1), tx)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("did not receive tx on UpdateCh")
	}
}

func TestSubscriber_WatchAutoAdvance(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("test", 8, wg).(*stream)
	sub := s.RegisterSubscriber("reader")

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	var mu sync.Mutex
	var got []any
	errCh := make(chan error, 1)

	go func() {
		err := sub.Watch(ctx, func(ev *Event) error {
			mu.Lock()
			got = append(got, ev.Data)
			mu.Unlock()
			return nil
		})
		errCh <- err // send to channel instead of calling require
	}()

	// Append events
	for i := 0; i < 3; i++ {
		_, err := s.Emit(context.Background(), i)
		require.NoError(t, err)
		time.Sleep(20 * time.Millisecond)
	}

	// Wait for got to contain expected values
	require.Eventually(t, func() bool {
		mu.Lock()
		defer mu.Unlock()
		return reflect.DeepEqual(got, []any{0, 1, 2})
	}, time.Second, 10*time.Millisecond)

	// Assert watcher exited cleanly or with context error
	select {
	case err := <-errCh:
		require.ErrorIs(t, err, context.DeadlineExceeded)
	default:
		// still running
	}
}

func TestSubscriber_CloseStreamShutsDown(t *testing.T) {
	wg := &sync.WaitGroup{}
	s := New("subtest", 4, wg).(*stream)
	sub := s.RegisterSubscriber("reader")

	s.Close()

	select {
	case _, ok := <-sub.UpdateCh():
		require.False(t, ok, "UpdateCh should be closed")
	case <-time.After(50 * time.Millisecond):
		t.Fatal("timeout: subscriber not closed after stream.Close()")
	}
}

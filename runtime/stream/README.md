# `mochi/runtime/stream`

## Overview 
This package implements a lightweight in-memory stream abstraction. It provides a transactional append-only log with:

safe concurrent appends
fixed-capacity retention
per-subscriber cursors
notification channels for change watching
explicit compaction and shutdown
Each stream maintains an internal ring buffer that holds the latest N events. Subscribers can independently read, track, and advance through events at their own pace. The design is suitable for implementing observability pipelines, state machines, message buses, or low-overhead coordination between components.

## Usage 
```go
package main

import (
	"context"
	"fmt"
	"time"

	"mochi/runtime/stream"
)

func main() {
	// Create a new stream named "metrics" with a capacity of 128 events.
	s := stream.New("metrics", 128)

	// Append some events to the stream.
	for i := 0; i < 3; i++ {
		ev, err := s.Append(context.Background(), fmt.Sprintf("event-%d", i))
		if err != nil {
			panic(err)
		}
		fmt.Printf("Appended: tx=%d, data=%v\n", ev.Tx, ev.Data)
	}

	// Read from the stream: start at tx=1, read 3 events.
	events, err := s.Read(1, 3)
	if err != nil {
		panic(err)
	}
	for _, ev := range events {
		fmt.Printf("Read: tx=%d, data=%v\n", ev.Tx, ev.Data)
	}

	// Get a specific event by transaction ID.
	ev, ok := s.Get(2)
	if ok {
		fmt.Printf("Get tx=2: data=%v\n", ev.Data)
	}

	// Register a new subscriber named "worker".
	sub := s.RegisterSubscriber("worker")

	// Manual consumption: receive tx updates, get event, then advance cursor.
	go func() {
		for tx := range sub.UpdateCh() {
			ev, ok := s.Get(tx)
			if !ok {
				continue
			}
			fmt.Printf("Manual Watch: tx=%d, data=%v\n", tx, ev.Data)
			sub.AdvanceTo(tx + 1)
		}
	}()

	// Append more events to trigger subscriber updates.
	for i := 3; i < 6; i++ {
		s.Append(context.Background(), fmt.Sprintf("event-%d", i))
	}

	// Sleep to allow subscriber to process.
	time.Sleep(100 * time.Millisecond)

	// Auto-advancing subscriber using Watch.
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	sub2 := s.RegisterSubscriber("auto")
	go sub2.Watch(ctx, func(ev *stream.Event) error {
		fmt.Printf("Auto Watch: tx=%d, data=%v\n", ev.Tx, ev.Data)
		return nil
	})

	// Append more events to trigger auto-watch
	for i := 6; i < 9; i++ {
		s.Append(context.Background(), fmt.Sprintf("event-%d", i))
	}

	// Wait a bit
	time.Sleep(200 * time.Millisecond)

	// Compact the stream to remove old events no longer needed by subscribers
	s.Compact()

	// Shut down the stream (closes all subscriber channels)
	s.Close()
}
```

## How it works

### Event Lifecycle

Each event appended to the stream is represented by the `Event` struct:

```go
type Event struct {
       Stream string
       Tx     int64
       Gtx    int64  // Global transaction ID if a shared counter is used
       Time   time.Time
       Data   any
}
```

Every event is assigned both a per-stream `Tx` and an optional global `Gtx` that is monotonically increasing across all streams when a shared counter is provided. The `Stream` field identifies which logical stream the event belongs to, and `Data` holds arbitrary user content.

### Stream Initialization

A new stream is created with a name and a fixed buffer capacity:

```go
stream := stream.New("my-stream", 16)
```

Internally, the stream maintains a circular buffer (ring) to store events. The core fields are:

```go
ring     []*Event   // fixed-size ring buffer
first    int64      // first retained Tx
tx       int64      // next Tx to assign
len      int        // number of retained items
subs     map[string]*Subscriber
```

The ring index wraps around using modulo logic: `index = tx % capacity`.

### Appending Events

Appending an event adds a new entry to the ring buffer:

```go
event, err := stream.Append(ctx, "hello world")
```

The stream assigns `Tx = stream.tx` and increments the counter. It then writes the event into the ring at the calculated slot:

```go
idx := tx % capacity
ring[idx] = &event
```

If the slot already contains a valid event, it is overwritten. All registered subscribers are notified of the new `tx` via their buffered `updateCh` channel.

### Reading and Accessing Events

To retrieve a batch of events, `Read(start, count)` is used:

```go
events, _ := stream.Read(5, 3)
```

This attempts to read up to `count` events starting from `Tx = start`. It stops early if events fall outside the retained range or are missing.

To fetch a single event, `Get(tx)` is used:

```go
event, ok := stream.Get(7)
```

If the `tx` is out of bounds or overwritten due to compaction, `ok` will be false.

### Subscriber Coordination

Subscribers can register themselves to track progress independently:

```go
sub := stream.RegisterSubscriber("worker-1")
```

Each subscriber maintains its own `cursor`, starting at `stream.first`, and receives new `tx` notifications via `updateCh`.

You can handle updates manually like this:

```go
go func() {
	for tx := range sub.UpdateCh() {
		if ev, ok := stream.Get(tx); ok {
			process(ev)
			sub.AdvanceTo(tx + 1)
		}
	}
}()
```

Or use the built-in auto-advance loop with `Watch()`:

```go
ctx := context.Background()
sub.Watch(ctx, func(ev *Event) error {
	process(ev)
	return nil
})
```

This automatically calls `AdvanceTo(tx + 1)` after each successful callback.

### Stream Compaction

Compaction removes expired events from the ring buffer when all subscribers have moved past them:

```go
stream.Compact()
```

The stream calculates the minimum `cursor` among all subscribers. Events with `tx < min` are removed from the ring:

```go
min := s.minCursorUnsafe()
for tx := s.first; tx < min; tx++ {
	ring[tx % capacity] = nil
}
s.first = min
```

This keeps memory usage bounded and avoids unbounded growth.

### Closing the Stream

Calling `Close()` shuts down the stream and all subscriber channels:

```go
stream.Close()
```

This ensures all `Watch()` or `UpdateCh()` loops exit gracefully. Each subscriber must check for channel closure:

```go
for tx := range sub.UpdateCh() {
	// clean shutdown
}
```

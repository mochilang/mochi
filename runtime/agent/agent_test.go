package agent_test

import (
	"context"
	"fmt"
	"sync"
	"testing"
	"time"

	"mochi/runtime/agent"
	"mochi/runtime/stream"
)

func TestAgent_BasicFlow(t *testing.T) {
	ctx := context.Background()

	// Create a shared wait group for deterministic ordering
	wg := &sync.WaitGroup{}
	// Create a test stream
	s := stream.New("SensorReading", 64, wg)

	// Initialize agent
	a := agent.New(agent.Config{
		Name:    "monitor",
		BufSize: 16,
		WG:      wg,
	})

	// Register stream handler
	err := a.On(s, func(ctx context.Context, ev *stream.Event) {
		data := ev.Data.(map[string]any)

		// Count readings
		count, _ := a.Get("count")
		if count == nil {
			a.Set("count", 1)
		} else {
			a.Set("count", count.(int)+1)
		}

		// Track last temperature
		a.Set("lastTemp", data["temperature"].(float64))
	})
	if err != nil {
		t.Fatalf("On failed: %v", err)
	}

	// Register an intent
	a.RegisterIntent("status", func(ctx context.Context, args ...agent.Value) (agent.Value, error) {
		count, _ := a.Get("count")
		last, _ := a.Get("lastTemp")
		return fmt.Sprintf("Seen %v readings, last temp = %.1f", count, last), nil
	})

	a.Start(ctx)
	defer a.Stop()

	// Emit one reading
	_, err = s.Emit(ctx, map[string]any{
		"id":          "sensor-1",
		"temperature": 38.5,
		"timestamp":   time.Now(),
	})
	if err != nil {
		t.Fatalf("Emit failed: %v", err)
	}

	s.Wait()

	// Call the intent
	result, err := a.Call(ctx, "status")
	if err != nil {
		t.Fatalf("Call failed: %v", err)
	}

	str := result.(string)
	t.Logf("status: %s", str)

	if str != "Seen 1 readings, last temp = 38.5" {
		t.Errorf("unexpected result: %s", str)
	}
}

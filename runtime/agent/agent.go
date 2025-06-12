package agent

import (
	"context"
	"fmt"
	"sync"

	"mochi/runtime/stream"
)

// Value is a dynamic runtime value.
type Value any

// IntentHandler defines the function signature for handling an intent call.
type IntentHandler func(context.Context, ...Value) (Value, error)

// Agent is a reactive runtime component that handles stream events and intent calls.
type Agent struct {
	Name   string
	Inbox  stream.Stream
	sub    stream.Subscriber
	cancel context.CancelFunc
	wg     sync.WaitGroup

	mu       sync.RWMutex
	state    map[string]Value
	handlers map[string]func(context.Context, *stream.Event)
	intents  map[string]IntentHandler
}

// Config provides initialization for Agent.
type Config struct {
	Name    string
	BufSize int
}

// New creates a new Agent with empty state and stream inbox.
func New(cfg Config) *Agent {
	inboxName := cfg.Name
	if inboxName == "" {
		inboxName = "agent"
	}
	return &Agent{
		Name:     cfg.Name,
		Inbox:    stream.New(inboxName+"-inbox", cfg.BufSize),
		state:    make(map[string]Value),
		handlers: make(map[string]func(context.Context, *stream.Event)),
		intents:  make(map[string]IntentHandler),
	}
}

// Start begins the agent’s event processing loop.
func (a *Agent) Start(ctx context.Context) {
	ctx, cancel := context.WithCancel(ctx)
	a.cancel = cancel
	a.sub = a.Inbox.Subscribe(a.Name+"-processor", func(ev *stream.Event) error {
		inner, ok := ev.Data.(*stream.Event)
		if ok && inner != nil {
			if h, ok := a.handlers[inner.Stream]; ok {
				h(ctx, inner)
			}
			inner.Ack()
		}
		ev.Ack()
		return nil
	})

	a.wg.Add(1)
	go func() {
		defer a.wg.Done()
		<-ctx.Done()
		a.sub.Close()
	}()
}

// Stop gracefully shuts down the agent.
func (a *Agent) Stop() {
	if a.cancel != nil {
		a.cancel()
	}
	if a.sub != nil {
		a.sub.Close()
	}
	a.wg.Wait()
	if a.Inbox != nil {
		a.Inbox.Close()
		a.Inbox.Wait()
	}
}

// On registers a stream handler and subscribes to the given stream.
func (a *Agent) On(s stream.Stream, handler func(context.Context, *stream.Event)) error {
	streamName := "" // assigned on first event
	a.handlers[streamName] = handler

	s.Subscribe(a.Name, func(ev *stream.Event) error {
		if streamName == "" {
			streamName = ev.Stream
			a.handlers[streamName] = handler
		}
		_, _ = a.Inbox.Emit(context.Background(), ev)
		return nil
	})
	return nil
}

// RegisterIntent binds a named intent handler to the agent.
func (a *Agent) RegisterIntent(name string, handler IntentHandler) {
	a.mu.Lock()
	defer a.mu.Unlock()
	a.intents[name] = handler
}

// Call invokes a named intent handler.
func (a *Agent) Call(ctx context.Context, method string, args ...Value) (Value, error) {
	a.mu.RLock()
	fn, ok := a.intents[method]
	a.mu.RUnlock()
	if !ok {
		return nil, fmt.Errorf("unknown intent: %s", method)
	}
	return fn(ctx, args...)
}

// State returns a full snapshot of the agent’s internal state.
func (a *Agent) State() map[string]Value {
	a.mu.RLock()
	defer a.mu.RUnlock()
	copy := make(map[string]Value, len(a.state))
	for k, v := range a.state {
		copy[k] = v
	}
	return copy
}

// Set sets a variable in the agent’s state.
func (a *Agent) Set(name string, value Value) {
	a.mu.Lock()
	defer a.mu.Unlock()
	a.state[name] = value
}

// Get returns a variable from the agent’s state.
func (a *Agent) Get(name string) (Value, bool) {
	a.mu.RLock()
	defer a.mu.RUnlock()
	v, ok := a.state[name]
	return v, ok
}

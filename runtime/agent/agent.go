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
	Name     string
	Inbox    chan *stream.Event
	cancel   context.CancelFunc
	wg       sync.WaitGroup
	state    map[string]Value
	handlers map[string]func(context.Context, *stream.Event)
	intents  map[string]IntentHandler
}

// Config provides initialization for Agent.
type Config struct {
	Name    string
	BufSize int
}

// New creates a new Agent with empty state and inbox.
func New(cfg Config) *Agent {
	return &Agent{
		Name:     cfg.Name,
		Inbox:    make(chan *stream.Event, cfg.BufSize),
		state:    make(map[string]Value),
		handlers: make(map[string]func(context.Context, *stream.Event)),
		intents:  make(map[string]IntentHandler),
	}
}

// Start begins the agent’s event processing loop.
func (a *Agent) Start(ctx context.Context) {
	ctx, cancel := context.WithCancel(ctx)
	a.cancel = cancel
	a.wg.Add(1)

	go func() {
		defer a.wg.Done()
		for {
			select {
			case <-ctx.Done():
				return
			case e := <-a.Inbox:
				if h, ok := a.handlers[e.Stream]; ok {
					h(ctx, e)
				}
			}
		}
	}()
}

// Stop gracefully shuts down the agent.
func (a *Agent) Stop() {
	if a.cancel != nil {
		a.cancel()
	}
	a.wg.Wait()
}

// On registers a stream handler and subscribes to the given stream.
func (a *Agent) On(s stream.Stream, handler func(context.Context, *stream.Event)) error {
	streamName := "" // assume first event determines it
	a.handlers[streamName] = handler

	s.Subscribe(a.Name, func(ev *stream.Event) error {
		if streamName == "" {
			streamName = ev.Stream
			a.handlers[streamName] = handler
		}
		select {
		case a.Inbox <- ev:
			return nil
		default:
			return nil // drop if inbox full
		}
	})
	return nil
}

// RegisterIntent binds a named intent handler to the agent.
func (a *Agent) RegisterIntent(name string, handler IntentHandler) {
	a.intents[name] = handler
}

// Call invokes a named intent handler.
func (a *Agent) Call(ctx context.Context, method string, args ...Value) (Value, error) {
	if fn, ok := a.intents[method]; ok {
		return fn(ctx, args...)
	}
	return nil, fmt.Errorf("unknown intent: %s", method)
}

// State returns full agent state map.
func (a *Agent) State() map[string]Value {
	return a.state
}

// Set sets a variable in the agent’s state.
func (a *Agent) Set(name string, value Value) {
	a.state[name] = value
}

// Get returns a variable from the agent’s state.
func (a *Agent) Get(name string) (Value, bool) {
	v, ok := a.state[name]
	return v, ok
}

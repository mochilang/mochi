package llm

import (
	"context"
	"fmt"
	"sync"
)

var (
	mu        sync.RWMutex
	providers = make(map[string]Provider)
)

// Register makes a provider available by name.
// If a provider with the same name is already registered, it panics.
func Register(name string, p Provider) {
	mu.Lock()
	defer mu.Unlock()
	if name == "" {
		panic("llm: empty provider name")
	}
	if p == nil {
		panic("llm: provider is nil")
	}
	if _, dup := providers[name]; dup {
		panic("llm: Register called twice for provider " + name)
	}
	providers[name] = p
}

// Open opens a new LLM client using the named provider.
func Open(providerName string, opts Options) (*Client, error) {
	mu.RLock()
	prv := providers[providerName]
	mu.RUnlock()
	if prv == nil {
		return nil, fmt.Errorf("llm: unknown provider %q", providerName)
	}
	conn, err := prv.Open(opts)
	if err != nil {
		return nil, err
	}
	return &Client{conn: conn}, nil
}

// Client represents a connection to an LLM backend.
type Client struct{ conn Conn }

// Close closes the underlying connection.
func (c *Client) Close() error { return c.conn.Close() }

// Chat sends the messages to the model and returns a single response.
func (c *Client) Chat(ctx context.Context, msgs []Message, opts ...Option) (*ChatResponse, error) {
	req := ChatRequest{Messages: msgs}
	for _, opt := range opts {
		opt(&req)
	}
	return c.conn.Chat(ctx, req)
}

// ChatStream requests a streaming response.
func (c *Client) ChatStream(ctx context.Context, msgs []Message, opts ...Option) (Stream, error) {
	req := ChatRequest{Messages: msgs, Stream: true}
	for _, opt := range opts {
		opt(&req)
	}
	req.Stream = true
	return c.conn.ChatStream(ctx, req)
}

package llm

import (
	"context"
	"errors"
	"log"
	"os"
)

// Default is the client opened on first use via environment variables.
// It is lazy-loaded by Chat or ChatStream.
// Uses:
//   - LLM_PROVIDER (default: "echo")
//   - LLM_DSN
//   - LLM_MODEL
var Default *Client

// tryInitDefault initializes Default only if not set.
// It logs errors but does not panic.
func tryInitDefault() {
	if Default != nil {
		return
	}
	provider := os.Getenv("LLM_PROVIDER")
	if provider == "" {
		provider = "echo"
	}
	dsn := os.Getenv("LLM_DSN")
	model := os.Getenv("LLM_MODEL")

	c, err := Open(provider, dsn, Options{Model: model})
	if err != nil {
		log.Printf("[llm] failed to open default provider %q: %v", provider, err)
		return
	}
	Default = c
}

// ensureDefault ensures Default is initialized, trying lazy init if needed.
func ensureDefault() error {
	if Default != nil {
		return nil
	}
	tryInitDefault()
	if Default == nil {
		return errors.New("llm: default client not initialized")
	}
	return nil
}

// Chat sends messages using the default client.
func Chat(ctx context.Context, msgs []Message, opts ...Option) (*ChatResponse, error) {
	if err := ensureDefault(); err != nil {
		return nil, err
	}
	return Default.Chat(ctx, msgs, opts...)
}

// ChatStream opens a streaming chat using the default client.
func ChatStream(ctx context.Context, msgs []Message, opts ...Option) (Stream, error) {
	if err := ensureDefault(); err != nil {
		return nil, err
	}
	return Default.ChatStream(ctx, msgs, opts...)
}

// Close shuts down the default client if initialized.
func Close() error {
	if Default == nil {
		return nil
	}
	return Default.Close()
}

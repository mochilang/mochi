package llm

import (
	"context"
	"errors"
	"log"
	"os"
)

// Default is the client opened during package initialization.
// It uses environment variables `LLM_PROVIDER`, `LLM_DSN` and `LLM_MODEL`.
// If not set, the provider defaults to "echo" which simply echoes
// the last user message. This is useful for collecting metrics
// without calling an external service.
var Default *Client

func init() {
	prv := os.Getenv("LLM_PROVIDER")
	if prv == "" {
		prv = "echo"
	}
	dsn := os.Getenv("LLM_DSN")
	model := os.Getenv("LLM_MODEL")

	c, err := Open(prv, dsn, Options{Model: model})
	if err != nil {
		log.Printf("[llm] failed to open default provider %q: %v", prv, err)
		return
	}
	Default = c
}

// Chat sends messages using the default client.
func Chat(ctx context.Context, msgs []Message, opts ...Option) (*ChatResponse, error) {
	if Default == nil {
		return nil, errors.New("llm: default client not initialized")
	}
	return Default.Chat(ctx, msgs, opts...)
}

// ChatStream opens a streaming chat using the default client.
func ChatStream(ctx context.Context, msgs []Message, opts ...Option) (Stream, error) {
	if Default == nil {
		return nil, errors.New("llm: default client not initialized")
	}
	return Default.ChatStream(ctx, msgs, opts...)
}

// Close closes the default client if initialized.
func Close() error {
	if Default == nil {
		return nil
	}
	return Default.Close()
}

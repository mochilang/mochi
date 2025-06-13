package llm

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/google/uuid"

	"mochi/tools/db"
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
	// log.Printf("llm: registering provider %s", name)
	providers[name] = p
}

// Open opens a new LLM client using the named provider.
func Open(providerName, dsn string, opts Options) (*Client, error) {
	mu.RLock()
	prv := providers[providerName]
	mu.RUnlock()
	if prv == nil {
		return nil, fmt.Errorf("llm: unknown provider %q, init with `_ \"mochi/runtime/llm/provider/%s\"`",
			providerName, providerName)
	}
	conn, err := prv.Open(dsn, opts)
	if err != nil {
		return nil, err
	}
	return &Client{conn: conn, opts: opts}, nil
}

// Client represents a connection to an LLM backend.
type Client struct {
	conn Conn
	opts Options
}

// Close closes the underlying connection.
func (c *Client) Close() error { return c.conn.Close() }

// Chat sends the messages to the model and returns a single response.
func (c *Client) Chat(ctx context.Context, msgs []Message, opts ...Option) (*ChatResponse, error) {
	req := ChatRequest{Messages: msgs}
	for _, opt := range opts {
		opt(&req)
	}

	if req.Model == "" {
		req.Model = c.opts.Model
	}
	selectedModel := req.Model

	start := time.Now()
	resp, err := c.conn.Chat(ctx, req)
	duration := time.Since(start)

	// Prepare log entry
	reqJSON, _ := json.Marshal(req)
	var respJSON []byte
	var prompt, reply, model, status string
	if err != nil {
		status = "error"
	} else {
		status = "ok"
	}
	if resp != nil {
		respJSON, _ = json.Marshal(resp)
		reply = resp.Message.Content
		model = resp.Model
		if model == "" {
			model = selectedModel
		}
		if prompt == "" {
			prompt = lastUser(msgs)
		}
	} else {
		prompt = lastUser(msgs)
		model = selectedModel
	}
	var pt, rt, tt int
	if resp != nil && resp.Usage != nil {
		pt = resp.Usage.PromptTokens
		rt = resp.Usage.CompletionTokens
		tt = resp.Usage.TotalTokens
	}
	db.LogLLM(ctx, &db.LLMModel{
		SessionID: getSessionID(),
		Agent:     getAgent(),
		Model:     model,
		Request:   reqJSON,
		Response:  respJSON,
		Prompt:    prompt,
		Reply:     reply,
		PromptTok: pt,
		ReplyTok:  rt,
		TotalTok:  tt,
		Duration:  duration,
		Status:    status,
		CreatedAt: time.Now(),
	})

	return resp, err
}

// ChatStream requests a streaming response.
func (c *Client) ChatStream(ctx context.Context, msgs []Message, opts ...Option) (Stream, error) {
	req := ChatRequest{Messages: msgs, Stream: true}
	for _, opt := range opts {
		opt(&req)
	}
	if req.Model == "" {
		req.Model = c.opts.Model
	}
	selectedModel := req.Model
	req.Stream = true
	start := time.Now()
	sid := getSessionID()
	agent := getAgent()
	s, err := c.conn.ChatStream(ctx, req)
	if err != nil {
		db.LogLLM(ctx, &db.LLMModel{
			SessionID: sid,
			Agent:     agent,
			Model:     selectedModel,
			Request:   mustJSON(req),
			Response:  nil,
			Prompt:    lastUser(msgs),
			Reply:     "",
			Status:    "error",
			CreatedAt: time.Now(),
			Duration:  time.Since(start),
		})
		return nil, err
	}
	return &logStream{
		Stream:    s,
		req:       req,
		start:     start,
		sessionID: sid,
		agent:     agent,
		model:     selectedModel,
	}, nil
}

// Embed returns an embedding vector for the given text.
func (c *Client) Embed(ctx context.Context, text string, opts ...EmbedOption) (*EmbedResponse, error) {
	req := EmbedRequest{Text: text}
	for _, opt := range opts {
		opt(&req)
	}
	if req.Model == "" {
		req.Model = c.opts.Model
	}
	return c.conn.Embed(ctx, req)
}

func lastUser(msgs []Message) string {
	for i := len(msgs) - 1; i >= 0; i-- {
		if msgs[i].Role == "user" {
			return msgs[i].Content
		}
	}
	if len(msgs) > 0 {
		return msgs[len(msgs)-1].Content
	}
	return ""
}

func mustJSON(v any) json.RawMessage {
	b, _ := json.Marshal(v)
	return b
}

type logStream struct {
	Stream
	req       ChatRequest
	start     time.Time
	sessionID string
	agent     string
	model     string
	buf       strings.Builder
	logged    bool
}

func (ls *logStream) Recv() (*Chunk, error) {
	ch, err := ls.Stream.Recv()
	if err != nil {
		ls.log("error")
		return ch, err
	}
	if ch != nil {
		ls.buf.WriteString(ch.Content)
		if ch.Done {
			ls.log("ok")
		}
	}
	return ch, nil
}

func (ls *logStream) Close() error {
	err := ls.Stream.Close()
	if !ls.logged {
		if err != nil {
			ls.log("error")
		} else {
			ls.log("ok")
		}
	}
	return err
}

func (ls *logStream) log(status string) {
	ls.logged = true
	db.LogLLM(context.Background(), &db.LLMModel{
		SessionID: ls.sessionID,
		Agent:     ls.agent,
		Model:     ls.model,
		Request:   mustJSON(ls.req),
		Response:  mustJSON(map[string]string{"content": ls.buf.String()}),
		Prompt:    lastUser(ls.req.Messages),
		Reply:     ls.buf.String(),
		Status:    status,
		Duration:  time.Since(ls.start),
		CreatedAt: time.Now(),
	})
}

func getAgent() string {
	if agent := os.Getenv("MOCHI_AGENT"); agent != "" {
		return agent
	}
	return "llm"
}

func getSessionID() string {
	if sid := os.Getenv("MOCHI_SESSION"); sid != "" {
		return sid
	}
	return uuid.NewString()
}

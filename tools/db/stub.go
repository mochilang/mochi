//go:build tinygo

package db

import (
	"context"
	"encoding/json"
	"time"
)

// Stub implementations used when building with TinyGo (e.g. for WebAssembly).

type RunModel struct {
	ID        int64
	SessionID string
	Agent     string
	File      string
	Source    string
	Status    string
	Error     string
	Duration  time.Duration
	CreatedAt time.Time
}

type BuildModel struct {
	ID        int64
	SessionID string
	Agent     string
	File      string
	Out       string
	Target    string
	Source    string
	Status    string
	Error     string
	Duration  time.Duration
	CreatedAt time.Time
}

type GoldenModel struct {
	ID        int64
	SessionID string
	Agent     string
	Name      string
	File      string
	Input     string
	Output    string
	Status    string
	Error     string
	Duration  time.Duration
	CreatedAt time.Time
}

type LLMModel struct {
	ID        int64
	SessionID string
	Agent     string
	Model     string
	Request   json.RawMessage
	Response  json.RawMessage
	Prompt    string
	Reply     string
	PromptTok int
	ReplyTok  int
	TotalTok  int
	Duration  time.Duration
	Status    string
	CreatedAt time.Time
}

func LogRun(ctx context.Context, r *RunModel)       {}
func LogBuild(ctx context.Context, b *BuildModel)   {}
func LogGolden(ctx context.Context, g *GoldenModel) {}
func LogLLM(ctx context.Context, m *LLMModel)       {}

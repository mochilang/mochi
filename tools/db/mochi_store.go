//go:build !tinygo

package db

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"time"
)

type mochiStore struct {
	db *sql.DB
}

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

func newMochiStore(db *sql.DB) *mochiStore {
	return &mochiStore{db: db}
}

func (s *mochiStore) ensureTables() error {
	_, err := s.db.Exec(`
	CREATE TABLE IF NOT EXISTS run (
		id         BIGSERIAL PRIMARY KEY,
		session_id TEXT,
		agent      TEXT,
		file       TEXT,
		source     TEXT,
		status     TEXT,
		error      TEXT,
		duration   INTERVAL,
		created_at TIMESTAMPTZ DEFAULT now()
	);
	CREATE INDEX IF NOT EXISTS run_created_idx ON run (created_at);
	CREATE INDEX IF NOT EXISTS run_session_idx ON run (session_id);
	CREATE INDEX IF NOT EXISTS run_agent_idx ON run (agent);

	CREATE TABLE IF NOT EXISTS build (
		id         BIGSERIAL PRIMARY KEY,
		session_id TEXT,
		agent      TEXT,
		file       TEXT,
		out        TEXT,
		target     TEXT,
		source     TEXT,
		status     TEXT,
		error      TEXT,
		duration   INTERVAL,
		created_at TIMESTAMPTZ DEFAULT now()
	);
	CREATE INDEX IF NOT EXISTS build_created_idx ON build (created_at);
	CREATE INDEX IF NOT EXISTS build_session_idx ON build (session_id);
	CREATE INDEX IF NOT EXISTS build_agent_idx ON build (agent);

	CREATE TABLE IF NOT EXISTS golden (
		id         BIGSERIAL PRIMARY KEY,
		session_id TEXT,
		agent      TEXT,
		name       TEXT,
		file       TEXT,
		input      TEXT,
		output     TEXT,
		status     TEXT,
		error      TEXT,
		duration   INTERVAL,
		created_at TIMESTAMPTZ DEFAULT now()
	);
	CREATE INDEX IF NOT EXISTS golden_created_idx ON golden (created_at);
	CREATE INDEX IF NOT EXISTS golden_session_idx ON golden (session_id);
	CREATE INDEX IF NOT EXISTS golden_agent_idx ON golden (agent);
	`)
	return err
}

func (s *mochiStore) logRun(ctx context.Context, r *RunModel) {
	if ctx == nil {
		ctx = context.Background()
	}
	if r.SessionID == "" {
		r.SessionID = "default"
	}
	if r.CreatedAt.IsZero() {
		r.CreatedAt = time.Now()
	}
	duration := fmt.Sprintf("%.9f seconds", r.Duration.Seconds())
	_, err := s.db.ExecContext(ctx, `
		INSERT INTO run (session_id, agent, file, source, status, error, duration, created_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7::interval, $8)
	`,
		r.SessionID, r.Agent, r.File, r.Source, r.Status, r.Error, duration, r.CreatedAt,
	)
	if err != nil {
		log.Printf("[mochi] insert run: %v", err)
	}
}

func (s *mochiStore) logBuild(ctx context.Context, b *BuildModel) {
	if ctx == nil {
		ctx = context.Background()
	}
	if b.SessionID == "" {
		b.SessionID = "default"
	}
	if b.CreatedAt.IsZero() {
		b.CreatedAt = time.Now()
	}
	duration := fmt.Sprintf("%.9f seconds", b.Duration.Seconds())
	_, err := s.db.ExecContext(ctx, `
		INSERT INTO build (session_id, agent, file, out, target, source, status, error, duration, created_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::interval, $10)
	`,
		b.SessionID, b.Agent, b.File, b.Out, b.Target, b.Source, b.Status, b.Error, duration, b.CreatedAt,
	)
	if err != nil {
		log.Printf("[mochi] insert build: %v", err)
	}
}

func (s *mochiStore) logGolden(ctx context.Context, g *GoldenModel) {
	if ctx == nil {
		ctx = context.Background()
	}
	if g.SessionID == "" {
		g.SessionID = "default"
	}
	if g.CreatedAt.IsZero() {
		g.CreatedAt = time.Now()
	}
	duration := fmt.Sprintf("%.9f seconds", g.Duration.Seconds())
	_, err := s.db.ExecContext(ctx, `
		INSERT INTO golden (session_id, agent, name, file, input, output, status, error, duration, created_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::interval, $10)
	`,
		g.SessionID, g.Agent, g.Name, g.File, g.Input, g.Output, g.Status, g.Error, duration, g.CreatedAt,
	)
	if err != nil {
		log.Printf("[mochi] insert golden: %v", err)
	}
}

func LogRun(ctx context.Context, r *RunModel) {
	if mochi == nil {
		return
	}
	mochi.logRun(ctx, r)
}

func LogBuild(ctx context.Context, b *BuildModel) {
	if mochi == nil {
		return
	}
	mochi.logBuild(ctx, b)
}

func LogGolden(ctx context.Context, g *GoldenModel) {
	if mochi == nil {
		return
	}
	mochi.logGolden(ctx, g)
}

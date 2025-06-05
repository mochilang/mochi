package db

import (
	"context"
	"database/sql"
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
		duration   BIGINT,
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
		duration   BIGINT,
		created_at TIMESTAMPTZ DEFAULT now()
	);
	CREATE INDEX IF NOT EXISTS build_created_idx ON build (created_at);
	CREATE INDEX IF NOT EXISTS build_session_idx ON build (session_id);
	CREATE INDEX IF NOT EXISTS build_agent_idx ON build (agent);
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
	_, err := s.db.ExecContext(ctx, `
		INSERT INTO run (session_id, agent, file, source, status, error, duration, created_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
	`,
		r.SessionID, r.Agent, r.File, r.Source, r.Status, r.Error, r.Duration.Microseconds(), r.CreatedAt,
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
	_, err := s.db.ExecContext(ctx, `
		INSERT INTO build (session_id, agent, file, out, target, source, status, error, duration, created_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
	`,
		b.SessionID, b.Agent, b.File, b.Out, b.Target, b.Source, b.Status, b.Error, b.Duration.Microseconds(), b.CreatedAt,
	)
	if err != nil {
		log.Printf("[mochi] insert build: %v", err)
	}
}

func LogRun(ctx context.Context, r *RunModel) {
	if mochi == nil {
		return
	}
	if ctx == nil {
		ctx = context.Background()
	}
	if r.SessionID == "" {
		r.SessionID = "default"
	}
	if r.CreatedAt.IsZero() {
		r.CreatedAt = time.Now()
	}
	_, err := mochi.db.ExecContext(ctx, `
		INSERT INTO run (session_id, agent, file, source, status, error, duration, created_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
	`,
		r.SessionID, r.Agent, r.File, r.Source, r.Status, r.Error, r.Duration.Microseconds(), r.CreatedAt,
	)
	if err != nil {
		log.Printf("[mochi] insert run: %v", err)
	}
}

func LogBuild(ctx context.Context, b *BuildModel) {
	if mochi == nil {
		return
	}
	if ctx == nil {
		ctx = context.Background()
	}
	if b.SessionID == "" {
		b.SessionID = "default"
	}
	if b.CreatedAt.IsZero() {
		b.CreatedAt = time.Now()
	}
	_, err := mochi.db.ExecContext(ctx, `
		INSERT INTO build (session_id, agent, file, source, out, target, status, error, duration, created_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
	`,
		b.SessionID, b.Agent, b.File, b.Source, b.Out, b.Target, b.Status, b.Error, b.Duration.Microseconds(), b.CreatedAt,
	)
	if err != nil {
		log.Printf("[mochi] insert build: %v", err)
	}
}

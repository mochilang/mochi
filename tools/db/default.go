//go:build !tinygo

package db

import (
	"database/sql"
	"log"
	"os"

	_ "github.com/lib/pq" // Postgres driver
)

var (
	llm   *llmStore
	mochi *mochiStore
)

func init() {
	dsn := os.Getenv("POSTGRES_DSN")
	if dsn == "" {
		// log.Println("[db] warning: POSTGRES_DSN not set, database features disabled")
		return
	}
	db, err := sql.Open("postgres", dsn)
	if err != nil {
		log.Printf("[db] failed to open connection: %v", err)
		return
	}
	if err := db.Ping(); err != nil {
		log.Printf("[db] ping failed: %v", err)
		return
	}

	llm = &llmStore{db: db}
	if err := llm.ensureTable(); err != nil {
		log.Printf("[db] failed to ensure LLM table: %v", err)
	}

	mochi = &mochiStore{db: db}
	if err := mochi.ensureTables(); err != nil {
		log.Printf("[db] failed to ensure Mochi tables: %v", err)
	}
}

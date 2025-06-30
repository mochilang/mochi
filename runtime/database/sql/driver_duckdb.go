//go:build cgo && !js && !wasm

package sql

import (
	_ "github.com/marcboeker/go-duckdb"
)

// The blank import above registers the DuckDB driver when cgo is enabled.

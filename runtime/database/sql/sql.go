package sql

import (
	"database/sql"
	_ "github.com/marcboeker/go-duckdb"
)

// DB wraps *sql.DB for Mochi runtime use.
type DB struct {
	*sql.DB
}

// Open opens a database using the given driver name and DSN.
func Open(driver, dsn string) (*DB, error) {
	db, err := sql.Open(driver, dsn)
	if err != nil {
		return nil, err
	}
	return &DB{db}, nil
}

// Close closes the underlying database.
func (db *DB) Close() error { return db.DB.Close() }

// Exec executes a statement and returns the rows affected.
func (db *DB) Exec(query string, args []any) (int, error) {
	res, err := db.DB.Exec(query, args...)
	if err != nil {
		return 0, err
	}
	n, err := res.RowsAffected()
	if err != nil {
		return 0, err
	}
	return int(n), nil
}

// Query runs the query and returns all rows as []map[string]any.
func (db *DB) Query(query string, args []any) ([]map[string]any, error) {
	rows, err := db.DB.Query(query, args...)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	cols, err := rows.Columns()
	if err != nil {
		return nil, err
	}

	var out []map[string]any
	for rows.Next() {
		vals := make([]any, len(cols))
		ptrs := make([]any, len(cols))
		for i := range vals {
			ptrs[i] = &vals[i]
		}
		if err := rows.Scan(ptrs...); err != nil {
			return nil, err
		}
		m := make(map[string]any, len(cols))
		for i, c := range cols {
			v := vals[i]
			if b, ok := v.([]byte); ok {
				m[c] = string(b)
			} else {
				m[c] = v
			}
		}
		out = append(out, m)
	}
	return out, rows.Err()
}

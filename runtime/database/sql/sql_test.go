package sql

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestDB_Basic(t *testing.T) {
	db, err := Open("duckdb", "")
	require.NoError(t, err)
	defer db.Close()

	_, err = db.Exec("create table foo(id integer, name text)", nil)
	require.NoError(t, err)

	_, err = db.Exec("insert into foo values(?, ?)", []any{1, "a"})
	require.NoError(t, err)

	rows, err := db.Query("select name from foo where id = ?", []any{1})
	require.NoError(t, err)
	require.Equal(t, 1, len(rows))
	require.Equal(t, "a", rows[0]["name"])
}

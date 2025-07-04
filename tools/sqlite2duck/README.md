# sqlite2duck

`sqlite2duck` converts common SQLite SQL syntax to DuckDB equivalents. It performs a few textual rewrites so that queries written for SQLite can run on DuckDB.

## Usage

Run the tool with a path to a `.sql` file or pass `-` to read from standard input:

```bash
sqlite2duck query.sql
cat query.sql | sqlite2duck -
```

The converted query is printed to standard output.

## Supported conversions

The converter currently handles the following patterns:

- `ifnull()` -> `coalesce()`
- `substr()` -> `substring()`
- `group_concat()` -> `string_agg()`
- `datetime('now')` -> `now()`
- `date('now')` -> `current_date`
- `randomblob(n)` -> `random_bytes(n)`
- `total(x)` -> `coalesce(sum(x),0)`
- remove `NOT INDEXED` clauses

These rules cover the most common differences encountered when porting simple
SQLite queries.

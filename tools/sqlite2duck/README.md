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
- `char_length()` -> `length()`
- `character_length()` -> `length()`
- `printf()` -> `format()`
- `instr()` -> `strpos()`
- `datetime('now')` -> `now()`
- `datetime('now','localtime')` -> `now()`
- `datetime('now','utc')` -> `now()`
- `date('now')` -> `current_date`
- `date('now','localtime')` -> `current_date`
- `date('now','utc')` -> `current_date`
- `time('now')` -> `current_time`
- `CURRENT_DATE` -> `current_date`
- `CURRENT_TIME` -> `current_time`
- `CURRENT_TIMESTAMP()` -> `now()`
- `randomblob(n)` -> `random_bytes(n)`
- `julianday('now')` -> `julianday(now())`
- `strftime(..., 'now')` -> `strftime(..., now())`
- `zeroblob(n)` -> `repeat('\x00', n)`
- `total(x)` -> `coalesce(sum(x),0)`
- `hex(randomblob(n))` -> `hex(random_bytes(n))`
- `TRUE`/`FALSE` -> `true`/`false`
- remove `NOT INDEXED` clauses

These rules cover the most common differences encountered when porting simple
SQLite queries.

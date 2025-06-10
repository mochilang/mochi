# `mochi/runtime/datav1`

`data` provides a minimal logical query layer used by the Mochi interpreter to
deal with structured datasets. It defines core types such as `Dataset`,
`Expr` and `Plan` and allows drivers to plug in database or file backends.

The package is intentionally small and mirrors the feel of Go's `database/sql`
package.  Driver implementations register themselves with `data.Register` and
expose connections using `data.Open`.

## Features

* Typed schema with `int`, `float`, `string`, `bool` and `null`
* Row and column oriented access
* SQL-like expression tree with `FieldRef`, `Const` and `BinaryExpr`
* Logical query plans (`Select`, `Where`, `Join`, `GroupBy`)
* Driver interface for connecting to back ends like DuckDB or SQLite

The Mochi interpreter will use this package at runtime when executing `dataset`
blocks to load data, filter rows and join tables.

## Usage

```go
import "mochi/runtime/datav1"

// Open a connection using a registered driver.
conn, err := data.Open("duckdb", "file:example.db")
if err != nil {
    panic(err)
}

defer conn.Close()

// Build a logical plan: SELECT name FROM people WHERE age > 30
plan := data.Select([]data.Expr{
    data.FieldRef{Name: "name"},
}, data.Where(
    data.BinaryExpr{
        Op:    ">",
        Left:  data.FieldRef{Name: "age"},
        Right: data.Const{Value: 30},
    },
    data.From("people.csv", "p"),
))

// Execute the plan in-memory or via the driver.
result, err := conn.Run(context.Background(), plan)
if err != nil {
    panic(err)
}
fmt.Println(result.String())
```

The actual evaluation of plans is driver dependent. Generic evaluation functions
are left unimplemented, allowing each driver to translate the logical plan into
its native query language.

## Integration Notes

Future versions of the Mochi interpreter will compile `dataset { ... }` blocks
into these logical plans. At runtime they will be executed via the selected
`data` driver so that Mochi programs can transparently work with CSV files,
SQL databases or other data sources.

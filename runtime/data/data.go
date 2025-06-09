package data

import (
	"context"
	"fmt"
	"strings"
)

// --- Logical Type System --- //

// Type describes a field's logical type.
type Type string

const (
	Int    Type = "int"
	Float  Type = "float"
	String Type = "string"
	Bool   Type = "bool"
	Null   Type = "null"
)

// Field is a column in a dataset schema.
type Field struct {
	Name string
	Type Type
}

// Schema is a list of typed fields.
type Schema struct {
	Fields []Field
}

// --- Dataset Interface --- //

// Row represents one logical record (row-based access).
type Row = map[string]any

// Column provides column-major access to a typed vector.
type Column interface {
	Len() int
	At(i int) any
	Type() Type
	Slice(start, end int) Column
	Raw() any // []int64, []string, etc.
}

// Dataset is an immutable table of rows/columns.
type Dataset interface {
	Schema() *Schema
	Iterator(ctx context.Context) (Iterator, error)         // row-oriented
	Columns(ctx context.Context) (map[string]Column, error) // column-oriented
	String() string
}

// Iterator yields one row at a time.
type Iterator interface {
	Next(ctx context.Context) (Row, error)
}

// --- Expression System --- //

// Expr is a SQL-like expression for filtering, projection, etc.
type Expr interface {
	Eval(Row) (any, error)
	SQL() (string, []any, error) // to SQL fragment
}

// FieldRef references a named column.
type FieldRef struct {
	Table, Name string
}

func (f FieldRef) Eval(row Row) (any, error) {
	return row[f.Name], nil
}
func (f FieldRef) SQL() (string, []any, error) {
	if f.Table != "" {
		return fmt.Sprintf("%s.%s", f.Table, f.Name), nil, nil
	}
	return f.Name, nil, nil
}

// Const is a literal constant (e.g. 42 or "Alice").
type Const struct {
	Value any
}

func (c Const) Eval(Row) (any, error)       { return c.Value, nil }
func (c Const) SQL() (string, []any, error) { return "?", []any{c.Value}, nil }

// BinaryExpr represents a binary operation like `a > b`
type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b BinaryExpr) Eval(Row) (any, error) {
	return nil, fmt.Errorf("Eval not implemented for BinaryExpr")
}
func (b BinaryExpr) SQL() (string, []any, error) {
	l, la, _ := b.Left.SQL()
	r, ra, _ := b.Right.SQL()
	return fmt.Sprintf("(%s %s %s)", l, b.Op, r), append(la, ra...), nil
}

// --- Logical Plan Interface --- //

// Plan is a logical query plan (SELECT, FILTER, JOIN, etc.).
type Plan interface {
	Eval(context.Context) (Dataset, error) // in-memory evaluation
	SQL() (string, []any, error)           // convert to SQL
}

// --- Plan Constructors (LINQ-style DSL) --- //

func From(path, alias string) Plan {
	return &loadPlan{path: path, alias: alias}
}

func Select(fields []Expr, input Plan) Plan {
	return &selectPlan{fields: fields, input: input}
}

func Where(cond Expr, input Plan) Plan {
	return &wherePlan{cond: cond, input: input}
}

func Join(left, right Plan, onLeft, onRight Expr, joinType string) Plan {
	return &joinPlan{left, right, onLeft, onRight, joinType}
}

func GroupBy(keys []Expr, aggs []Expr, input Plan) Plan {
	return &groupPlan{keys: keys, aggs: aggs, input: input}
}

// --- Plan Implementations --- //

// loadPlan is a placeholder for `from "<file>" as <alias>`.
// It should not emit SQL by default (driver-specific override required).
type loadPlan struct {
	path  string
	alias string
}

func (p *loadPlan) SQL() (string, []any, error) {
	return "", nil, fmt.Errorf("loadPlan: SQL not supported in generic layer")
}
func (p *loadPlan) Eval(ctx context.Context) (Dataset, error) {
	return nil, fmt.Errorf("Eval not implemented for loadPlan")
}

// selectPlan represents SELECT <fields> FROM <input>
type selectPlan struct {
	fields []Expr
	input  Plan
}

func (p *selectPlan) SQL() (string, []any, error) {
	srcSQL, srcArgs, err := p.input.SQL()
	if err != nil {
		return "", nil, err
	}
	cols := []string{}
	args := []any{}
	for _, f := range p.fields {
		s, a, _ := f.SQL()
		cols = append(cols, s)
		args = append(args, a...)
	}
	return fmt.Sprintf("SELECT %s FROM (%s)", strings.Join(cols, ", "), srcSQL), append(srcArgs, args...), nil
}
func (p *selectPlan) Eval(ctx context.Context) (Dataset, error) {
	return nil, fmt.Errorf("Eval not implemented for selectPlan")
}

// wherePlan represents a WHERE filter.
type wherePlan struct {
	cond  Expr
	input Plan
}

func (p *wherePlan) SQL() (string, []any, error) {
	srcSQL, srcArgs, err := p.input.SQL()
	if err != nil {
		return "", nil, err
	}
	condSQL, condArgs, _ := p.cond.SQL()
	return fmt.Sprintf("SELECT * FROM (%s) WHERE %s", srcSQL, condSQL), append(srcArgs, condArgs...), nil
}
func (p *wherePlan) Eval(ctx context.Context) (Dataset, error) {
	return nil, fmt.Errorf("Eval not implemented for wherePlan")
}

// joinPlan represents a JOIN between two datasets.
type joinPlan struct {
	left     Plan
	right    Plan
	onLeft   Expr
	onRight  Expr
	joinType string // "inner", "left", etc.
}

func (p *joinPlan) SQL() (string, []any, error) {
	lsql, largs, _ := p.left.SQL()
	rsql, rargs, _ := p.right.SQL()
	lkey, lka, _ := p.onLeft.SQL()
	rkey, rka, _ := p.onRight.SQL()

	sql := fmt.Sprintf("SELECT * FROM (%s) AS l %s JOIN (%s) AS r ON %s = %s",
		lsql, strings.ToUpper(p.joinType), rsql, lkey, rkey)
	args := append(largs, rargs...)
	args = append(args, lka...)
	args = append(args, rka...)
	return sql, args, nil
}
func (p *joinPlan) Eval(ctx context.Context) (Dataset, error) {
	return nil, fmt.Errorf("Eval not implemented for joinPlan")
}

// groupPlan represents GROUP BY + aggregation.
type groupPlan struct {
	keys  []Expr
	aggs  []Expr
	input Plan
}

func (p *groupPlan) SQL() (string, []any, error) {
	srcSQL, srcArgs, err := p.input.SQL()
	if err != nil {
		return "", nil, err
	}
	keySQL := []string{}
	aggSQL := []string{}
	args := []any{}

	for _, k := range p.keys {
		s, a, _ := k.SQL()
		keySQL = append(keySQL, s)
		args = append(args, a...)
	}
	for _, a := range p.aggs {
		s, ag, _ := a.SQL()
		aggSQL = append(aggSQL, s)
		args = append(args, ag...)
	}

	return fmt.Sprintf("SELECT %s, %s FROM (%s) GROUP BY %s",
		strings.Join(keySQL, ", "),
		strings.Join(aggSQL, ", "),
		srcSQL,
		strings.Join(keySQL, ", "),
	), append(srcArgs, args...), nil
}

func (p *groupPlan) Eval(ctx context.Context) (Dataset, error) {
	return nil, fmt.Errorf("Eval not implemented for groupPlan")
}

// --- Driver Interface (Go-style) --- //

// Driver opens logical connections to a data backend (e.g. "duckdb", "sqlite").
type Driver interface {
	Open(source string) (Conn, error)
}

// Conn is an active connection to a backend system.
type Conn interface {
	Run(ctx context.Context, plan Plan) (Dataset, error)
	Close() error
}

// global registry
var drivers = map[string]Driver{}

// Register a named backend driver (called by drivers' init).
func Register(name string, drv Driver) {
	drivers[name] = drv
}

// Open returns a connection to a registered driver.
func Open(driverName, source string) (Conn, error) {
	drv, ok := drivers[driverName]
	if !ok {
		return nil, fmt.Errorf("driver not registered: %s", driverName)
	}
	return drv.Open(source)
}

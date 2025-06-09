package mem

import (
	"context"
	"fmt"

	"mochi/runtime/data"
)

// Register the in-memory driver as "mem"
func init() {
	data.Register("mem", driver{})
}

type driver struct{}

func (driver) Open(source string) (data.Conn, error) {
	return &conn{datasets: make(map[string]data.Dataset)}, nil
}

// conn holds named datasets in memory
type conn struct {
	datasets map[string]data.Dataset
}

func (c *conn) Close() error { return nil }

// RegisterDataset makes a dataset available via From(path)
func (c *conn) RegisterDataset(path string, ds data.Dataset) {
	c.datasets[path] = ds
}

func (c *conn) Run(ctx context.Context, plan data.Plan) (data.Dataset, error) {
	return plan.Eval(ctx)
}

// --- In-Memory Dataset Implementation --- //

type dataset struct {
	schema *data.Schema
	rows   []data.Row
}

func NewDataset(schema *data.Schema, rows []data.Row) data.Dataset {
	return &dataset{schema: schema, rows: rows}
}

func (d *dataset) Schema() *data.Schema { return d.schema }

func (d *dataset) String() string {
	return fmt.Sprintf("InMemory[%d rows]", len(d.rows))
}

func (d *dataset) Iterator(ctx context.Context) (data.Iterator, error) {
	return &iterator{rows: d.rows}, nil
}

func (d *dataset) Columns(ctx context.Context) (map[string]data.Column, error) {
	cols := map[string][]any{}
	for _, f := range d.schema.Fields {
		cols[f.Name] = make([]any, 0, len(d.rows))
	}
	for _, row := range d.rows {
		for _, f := range d.schema.Fields {
			cols[f.Name] = append(cols[f.Name], row[f.Name])
		}
	}
	out := map[string]data.Column{}
	for _, f := range d.schema.Fields {
		out[f.Name] = &column{
			typ:    f.Type,
			values: cols[f.Name],
		}
	}
	return out, nil
}

// --- Iterator for in-memory rows --- //

type iterator struct {
	rows []data.Row
	pos  int
}

func (it *iterator) Next(ctx context.Context) (data.Row, error) {
	if it.pos >= len(it.rows) {
		return nil, fmt.Errorf("EOF")
	}
	row := it.rows[it.pos]
	it.pos++
	return row, nil
}

// --- In-Memory Column Implementation --- //

type column struct {
	typ    data.Type
	values []any
}

func (c *column) Len() int        { return len(c.values) }
func (c *column) At(i int) any    { return c.values[i] }
func (c *column) Type() data.Type { return c.typ }
func (c *column) Raw() any        { return c.values }
func (c *column) Slice(start, end int) data.Column {
	return &column{
		typ:    c.typ,
		values: c.values[start:end],
	}
}

// Package toml is a minimal TOML reader scoped to the subset that MEP-71
// reads from upstream tools: `uv.lock`, `pylock.toml` (PEP 751), and the
// `[python.*]` tables the bridge writes back into pyproject.toml.
//
// The package is deliberately not a general-purpose TOML implementation. It
// supports the features the bridge needs and rejects everything else loudly:
//
//   - top-level and named tables ([table.subtable])
//   - arrays of tables ([[array]])
//   - basic strings ("..." with the standard escape set) and literal strings ('...')
//   - integers (decimal only, with optional leading sign)
//   - floats (decimal with optional exponent, no infinity or NaN)
//   - booleans (true / false)
//   - arrays of homogeneous scalars or inline tables
//   - inline tables ({key = value, key = value})
//   - comments (# to end of line)
//
// Not supported (rejected with a clear error):
//
//   - multiline strings (TOML """...""" or '''...''')
//   - non-decimal integer literals (0x..., 0o..., 0b...)
//   - datetimes (RFC 3339)
//   - dotted keys on the left of an `=`
//   - bare keys with non-ASCII characters
//
// The output is a map[string]any tree where:
//
//   - tables are map[string]any
//   - arrays of tables are []map[string]any
//   - scalar arrays are []any
//   - strings are string, ints are int64, floats are float64, bools are bool
//
// The Decoder exposes helpers for traversing this tree without panic-y type
// assertions. The bridge sub-packages (uv, pylock) layer their own typed
// decoders on top of the tree.
package toml

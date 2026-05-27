package cerl

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// erlPath finds the erl binary. Tests that require OTP are skipped
// when erl is not available.
func erlPath(t *testing.T) string {
	t.Helper()
	erl, err := exec.LookPath("erl")
	if err != nil {
		t.Skip("erl not on PATH; skipping ETF roundtrip test")
	}
	return erl
}

// evalETF writes etfBytes to a temp file, then runs:
//
//	erl -noshell -eval 'ok = (fun() ->
//	  {ok,B} = file:read_file(Path),
//	  _ = binary_to_term(B),
//	  ok
//	end)(), halt(0).'
//
// Returns nil if binary_to_term succeeds.
func evalETF(t *testing.T, erl string, etfBytes []byte) error {
	t.Helper()
	tmp := t.TempDir()
	etfPath := filepath.Join(tmp, "term.etf")
	if err := os.WriteFile(etfPath, etfBytes, 0o644); err != nil {
		return err
	}
	etfFwd := filepath.ToSlash(etfPath)
	script := `
{ok, B} = file:read_file("` + etfFwd + `"),
_ = binary_to_term(B),
halt(0).`
	cmd := exec.Command(erl, "-noshell", "-eval", script)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return &erlError{out: out, err: err}
	}
	return nil
}

type erlError struct {
	out []byte
	err error
}

func (e *erlError) Error() string {
	return string(e.out) + ": " + e.err.Error()
}

// TestCerlETFRoundTrip verifies that six small cerl Module trees
// serialise to valid ETF that binary_to_term/1 can decode.
func TestCerlETFRoundTrip(t *testing.T) {
	erl := erlPath(t)

	cases := []struct {
		name string
		mod  *Module
	}{
		{
			name: "empty_module",
			mod: &Module{
				Name:    "empty",
				Exports: nil,
				Attrs:   nil,
				Defs:    nil,
			},
		},
		{
			name: "atom_literal",
			mod: &Module{
				Name:    "atoms",
				Exports: []FuncRef{{Name: "ok_val", Arity: 0}},
				Defs: []FuncDef{{
					Name:  "ok_val",
					Arity: 0,
					Vars:  nil,
					Body:  CAtom("ok"),
				}},
			},
		},
		{
			name: "int_literal",
			mod: &Module{
				Name:    "ints",
				Exports: []FuncRef{{Name: "answer", Arity: 0}},
				Defs: []FuncDef{{
					Name:  "answer",
					Arity: 0,
					Vars:  nil,
					Body:  CInt(42),
				}},
			},
		},
		{
			name: "float_literal",
			mod: &Module{
				Name:    "floats",
				Exports: []FuncRef{{Name: "pi", Arity: 0}},
				Defs: []FuncDef{{
					Name:  "pi",
					Arity: 0,
					Vars:  nil,
					Body:  CFloat(3.14),
				}},
			},
		},
		{
			name: "tuple_and_list",
			mod: &Module{
				Name:    "tuples",
				Exports: []FuncRef{{Name: "pair", Arity: 0}},
				Defs: []FuncDef{{
					Name:  "pair",
					Arity: 0,
					Vars:  nil,
					Body:  CTuple([]Expr{CInt(1), CAtom("two")}),
				}},
			},
		},
		{
			name: "binary_string",
			mod: &Module{
				Name:    "strings",
				Exports: []FuncRef{{Name: "hello", Arity: 0}},
				Defs: []FuncDef{{
					Name:  "hello",
					Arity: 0,
					Vars:  nil,
					Body:  CBin([]byte("hello world")),
				}},
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			etf, err := tc.mod.MarshalBinary()
			if err != nil {
				t.Fatalf("MarshalBinary: %v", err)
			}
			if len(etf) < 2 {
				t.Fatalf("ETF too short: %d bytes", len(etf))
			}
			if etf[0] != etfVersion {
				t.Fatalf("ETF missing version magic: got %d want %d", etf[0], etfVersion)
			}
			if err := evalETF(t, erl, etf); err != nil {
				t.Fatalf("binary_to_term failed: %v", err)
			}
		})
	}
}

// TestMarshalIntRanges checks that integers in different BEAM
// encoding ranges serialise without error.
func TestMarshalIntRanges(t *testing.T) {
	cases := []int64{0, 1, 255, 256, -1, -2147483648, 2147483647, 1<<48 - 1}
	for _, n := range cases {
		var buf bytes.Buffer
		EInt(n).appendETF(&buf)
		if buf.Len() == 0 {
			t.Errorf("EInt(%d) produced no bytes", n)
		}
	}
}

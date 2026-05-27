// Package cerl provides Go-side Core Erlang AST types and an
// Erlang External Term Format (ETF) serialiser.
//
// The types mirror the records defined in OTP's cerl.erl. Each
// constructor function returns an Expr that serialises to the
// corresponding Erlang tuple, e.g.:
//
//	CInt(42)  ->  {c_literal, [], 42}
//	CAtom("ok")  ->  {c_literal, [], ok}
//	CVar("X")  ->  {c_var, [], 'X'}
//
// The Module type represents a complete Core Erlang module and
// provides MarshalBinary() which produces a valid ETF binary that
// an Erlang subprocess can decode with binary_to_term/1 and pass
// to compile:forms(Forms, [from_core]).
//
// ETF support covers the subset needed through Phase 4: atoms,
// integers, floats, tuples, proper lists, and binaries. Bit strings,
// compressed terms, and pids are not supported and return an error.
package cerl

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"math"
	"sort"
)

// ETF tag bytes (Erlang External Term Format, OTP documentation §11).
const (
	etfVersion       = 131
	etfNewFloat      = 70
	etfSmallInt      = 97
	etfInt           = 98
	etfSmallAtomUTF8 = 119
	etfAtomUTF8      = 118
	etfSmallTuple    = 104
	etfLargeTuple    = 105
	etfNil           = 106
	etfList          = 108
	etfBinary        = 109
	etfSmallBig      = 110
	etfMap           = 116
)

// Term is a raw Erlang term. It is used for the small number of
// positions in the Core Erlang AST that hold arbitrary Erlang values
// (e.g. map keys, literal values).
type Term interface {
	appendETF(*bytes.Buffer)
}

// EAtom is an Erlang atom.
type EAtom string

func (a EAtom) appendETF(buf *bytes.Buffer) { appendAtom(buf, string(a)) }

// EInt is an Erlang integer (arbitrary-precision via int64 for now;
// big integers are not needed until Phase 2).
type EInt int64

func (i EInt) appendETF(buf *bytes.Buffer) { appendInt(buf, int64(i)) }

// EFloat is an Erlang float (IEEE 754 double).
type EFloat float64

func (f EFloat) appendETF(buf *bytes.Buffer) { appendFloat(buf, float64(f)) }

// ETuple is an Erlang tuple.
type ETuple []Term

func (t ETuple) appendETF(buf *bytes.Buffer) { appendTuple(buf, []Term(t)) }

// EList is a proper Erlang list.
type EList []Term

func (l EList) appendETF(buf *bytes.Buffer) { appendList(buf, []Term(l)) }

// EBin is an Erlang binary.
type EBin []byte

func (b EBin) appendETF(buf *bytes.Buffer) { appendBinary(buf, []byte(b)) }

// anno is the standard empty annotation list used in all cerl nodes.
var anno Term = EList(nil)

// Expr is a Core Erlang expression. All cerl constructor functions
// return an Expr. An Expr serialises to the Erlang tuple representation
// of the corresponding cerl record.
type Expr = Term

// CInt builds a c_literal node for an integer value.
//
//	{c_literal, [], N}
func CInt(n int64) Expr {
	return ETuple{EAtom("c_literal"), anno, EInt(n)}
}

// CFloat builds a c_literal node for a float value.
//
//	{c_literal, [], F}
func CFloat(f float64) Expr {
	return ETuple{EAtom("c_literal"), anno, EFloat(f)}
}

// CAtom builds a c_literal node for an atom value.
//
//	{c_literal, [], Atom}
func CAtom(name string) Expr {
	return ETuple{EAtom("c_literal"), anno, EAtom(name)}
}

// CBool builds a c_literal node for a boolean (true/false atoms).
//
//	{c_literal, [], true}  or  {c_literal, [], false}
func CBool(b bool) Expr {
	name := "false"
	if b {
		name = "true"
	}
	return ETuple{EAtom("c_literal"), anno, EAtom(name)}
}

// CLit wraps an arbitrary raw ETF term in a c_literal node.
// Used to create module attribute values that are complex terms.
//
//	{c_literal, [], Val}
func CLit(val Term) Expr {
	return ETuple{EAtom("c_literal"), anno, val}
}

// CNil builds a c_literal node for the empty list [].
//
//	{c_literal, [], []}
func CNil() Expr {
	return ETuple{EAtom("c_literal"), anno, EList(nil)}
}

// CEmptyMap builds a c_literal node for an empty Erlang map #{}.
// Used as the base argument to CMap() when constructing a new map literal.
//
//	{c_literal, [], #{}}
func CEmptyMap() Expr {
	return ETuple{EAtom("c_literal"), anno, EMap(nil)}
}

// CBin builds a c_literal node for an Erlang binary (used for
// Mochi strings, which lower to UTF-8 binaries).
//
//	{c_literal, [], <<"bytes">>}
func CBin(data []byte) Expr {
	return ETuple{EAtom("c_literal"), anno, EBin(data)}
}

// CVar builds a c_var node for a local variable or a function
// reference. For local variables, name is the variable identifier
// string (e.g. "V_x"). For function references, pass a {Name, Arity}
// tuple via CVarFunc.
//
//	{c_var, [], 'V_x'}
func CVar(name string) Expr {
	return ETuple{EAtom("c_var"), anno, EAtom(name)}
}

// CVarFunc builds a c_var node that names a function definition.
// This is used as the key in the module's Defs list.
//
//	{c_var, [], {FuncName, Arity}}
func CVarFunc(name string, arity int) Expr {
	return ETuple{EAtom("c_var"), anno, ETuple{EAtom(name), EInt(int64(arity))}}
}

// CFun builds a c_fun node (anonymous function / lambda).
//
//	{c_fun, [], [Vars...], Body}
func CFun(vars []Expr, body Expr) Expr {
	varList := make(EList, len(vars))
	for i, v := range vars {
		varList[i] = v
	}
	return ETuple{EAtom("c_fun"), anno, varList, body}
}

// CLet builds a c_let node (sequential let binding).
//
//	{c_let, [], [Vars...], Arg, Body}
func CLet(vars []Expr, arg, body Expr) Expr {
	varList := make(EList, len(vars))
	for i, v := range vars {
		varList[i] = v
	}
	return ETuple{EAtom("c_let"), anno, varList, arg, body}
}

// CSeq builds a c_seq node (sequence: evaluate Arg for side effects,
// then evaluate Body).
//
//	{c_seq, [], Arg, Body}
func CSeq(arg, body Expr) Expr {
	return ETuple{EAtom("c_seq"), anno, arg, body}
}

// CApply builds a c_apply node (local function application).
//
//	{c_apply, [], Op, [Args...]}
func CApply(op Expr, args []Expr) Expr {
	argList := make(EList, len(args))
	for i, a := range args {
		argList[i] = a
	}
	return ETuple{EAtom("c_apply"), anno, op, argList}
}

// CCall builds a c_call node (remote module:function call).
//
//	{c_call, [], ModExpr, NameExpr, [Args...]}
func CCall(mod, name Expr, args []Expr) Expr {
	argList := make(EList, len(args))
	for i, a := range args {
		argList[i] = a
	}
	return ETuple{EAtom("c_call"), anno, mod, name, argList}
}

// CPrimop builds a c_primop node (Core Erlang primitive operation
// such as 'match_fail', 'raise', or 'bs_init').
//
//	{c_primop, [], {c_literal,[],'Name'}, [Args...]}
func CPrimop(name string, args []Expr) Expr {
	argList := make(EList, len(args))
	for i, a := range args {
		argList[i] = a
	}
	return ETuple{EAtom("c_primop"), anno, CAtom(name), argList}
}

// CCase builds a c_case node.
//
//	{c_case, [], Arg, [Clauses...]}
func CCase(arg Expr, clauses []Expr) Expr {
	clauseList := make(EList, len(clauses))
	for i, c := range clauses {
		clauseList[i] = c
	}
	return ETuple{EAtom("c_case"), anno, arg, clauseList}
}

// CClause builds a c_clause node. Guard should be CAtom("true") for
// an unconditional clause.
//
//	{c_clause, [], [Pats...], Guard, Body}
func CClause(pats []Expr, guard, body Expr) Expr {
	patList := make(EList, len(pats))
	for i, p := range pats {
		patList[i] = p
	}
	return ETuple{EAtom("c_clause"), anno, patList, guard, body}
}

// CTuple builds a c_tuple node.
//
//	{c_tuple, [], [Es...]}
func CTuple(es []Expr) Expr {
	esList := make(EList, len(es))
	for i, e := range es {
		esList[i] = e
	}
	return ETuple{EAtom("c_tuple"), anno, esList}
}

// CCons builds a c_cons node (list cons cell).
//
//	{c_cons, [], Hd, Tl}
func CCons(hd, tl Expr) Expr {
	return ETuple{EAtom("c_cons"), anno, hd, tl}
}

// CValues builds a c_values node (multiple return values).
//
//	{c_values, [], [Es...]}
func CValues(es []Expr) Expr {
	esList := make(EList, len(es))
	for i, e := range es {
		esList[i] = e
	}
	return ETuple{EAtom("c_values"), anno, esList}
}

// CMap builds a c_map node (BEAM map expression or pattern).
// arg is the base map expression; use CEmptyMap() for a new map literal.
// isPat should be false for expressions, true for patterns.
//
//	{c_map, [], Arg, [Pairs...], false}
func CMap(arg Expr, pairs []Expr, isPat bool) Expr {
	isPatAtom := EAtom("false")
	if isPat {
		isPatAtom = EAtom("true")
	}
	pairList := make(EList, len(pairs))
	for i, p := range pairs {
		pairList[i] = p
	}
	return ETuple{EAtom("c_map"), anno, arg, pairList, isPatAtom}
}

// CMapPairAssoc builds a c_map_pair with op=assoc (#{K => V}).
// The op field must be a c_literal wrapping the atom.
//
//	{c_map_pair, [], {c_literal, [], assoc}, Key, Val}
func CMapPairAssoc(key, val Expr) Expr {
	opLit := ETuple{EAtom("c_literal"), anno, EAtom("assoc")}
	return ETuple{EAtom("c_map_pair"), anno, opLit, key, val}
}

// CMapPairExact builds a c_map_pair with op=exact (for pattern matching
// on a specific key, equivalent to #{K := V} in Erlang patterns).
//
//	{c_map_pair, [], {c_literal, [], exact}, Key, Val}
func CMapPairExact(key, val Expr) Expr {
	opLit := ETuple{EAtom("c_literal"), anno, EAtom("exact")}
	return ETuple{EAtom("c_map_pair"), anno, opLit, key, val}
}

// CTry builds a c_try node.
//
//	{c_try, [], Arg, [Vars...], Body, [Evars...], Handler}
func CTry(arg Expr, vars []Expr, body Expr, evars []Expr, handler Expr) Expr {
	varList := make(EList, len(vars))
	for i, v := range vars {
		varList[i] = v
	}
	evarList := make(EList, len(evars))
	for i, v := range evars {
		evarList[i] = v
	}
	return ETuple{EAtom("c_try"), anno, arg, varList, body, evarList, handler}
}

// CCatch builds a c_catch node.
//
//	{c_catch, [], Body}
func CCatch(body Expr) Expr {
	return ETuple{EAtom("c_catch"), anno, body}
}

// CReceive builds a c_receive node.
//
//	{c_receive, [], [Clauses...], Timeout, Action}
func CReceive(clauses []Expr, timeout, action Expr) Expr {
	clauseList := make(EList, len(clauses))
	for i, c := range clauses {
		clauseList[i] = c
	}
	return ETuple{EAtom("c_receive"), anno, clauseList, timeout, action}
}

// CAlias builds a c_alias node (as-pattern in a clause).
//
//	{c_alias, [], Var, Pat}
func CAlias(v, pat Expr) Expr {
	return ETuple{EAtom("c_alias"), anno, v, pat}
}

// CLetrec builds a c_letrec node (mutually recursive functions, used
// for loop lowering and recursive closures).
//
//	{c_letrec, [], [Defs...], Body}
func CLetrec(defs []Expr, body Expr) Expr {
	defList := make(EList, len(defs))
	for i, d := range defs {
		defList[i] = d
	}
	return ETuple{EAtom("c_letrec"), anno, defList, body}
}

// FuncDef is one function definition in a module.
type FuncDef struct {
	// Name is the unmangled function name (mangling applied by Module.toTerm).
	Name  string
	Arity int
	Vars  []string // parameter variable names (V_-prefixed)
	Body  Expr
}

// FuncRef names an exported function.
type FuncRef struct {
	Name  string
	Arity int
}

// Attr is a module attribute ({Key, Value} pair).
type Attr struct {
	Key string
	Val Term
}

// Module is the Go-side representation of a Core Erlang module.
// Call MarshalBinary to serialise it to ETF for compile:forms/2.
type Module struct {
	Name    string
	Exports []FuncRef
	Attrs   []Attr
	Defs    []FuncDef
}

// MarshalBinary serialises the module to Erlang External Term Format.
// The returned bytes start with the ETF version magic byte (131) and
// can be decoded by binary_to_term/1 in an Erlang subprocess.
func (m *Module) MarshalBinary() ([]byte, error) {
	term := m.toTerm()
	var buf bytes.Buffer
	buf.WriteByte(etfVersion)
	term.appendETF(&buf)
	return buf.Bytes(), nil
}

// toTerm builds the {c_module, Anno, Name, Exports, Attrs, Defs} tuple.
// Exports must be c_var nodes with {Name, Arity} tuples, not plain tuples.
func (m *Module) toTerm() Term {
	exports := make(EList, len(m.Exports))
	for i, e := range m.Exports {
		exports[i] = CVarFunc(e.Name, e.Arity)
	}

	attrs := make(EList, len(m.Attrs))
	for i, a := range m.Attrs {
		attrs[i] = ETuple{CAtom(a.Key), a.Val}
	}

	// Sort defs by name+arity for deterministic ETF output.
	sorted := make([]FuncDef, len(m.Defs))
	copy(sorted, m.Defs)
	sort.Slice(sorted, func(i, j int) bool {
		if sorted[i].Name != sorted[j].Name {
			return sorted[i].Name < sorted[j].Name
		}
		return sorted[i].Arity < sorted[j].Arity
	})

	defs := make(EList, len(sorted))
	for i, d := range sorted {
		vars := make([]Expr, len(d.Vars))
		for j, v := range d.Vars {
			vars[j] = CVar(v)
		}
		funExpr := CFun(vars, d.Body)
		nameVar := CVarFunc(d.Name, d.Arity)
		defs[i] = ETuple{nameVar, funExpr}
	}

	return ETuple{
		EAtom("c_module"),
		anno,
		CAtom(m.Name),
		exports,
		attrs,
		defs,
	}
}

// ETF encoding helpers.

func appendAtom(buf *bytes.Buffer, name string) {
	b := []byte(name)
	if len(b) <= 255 {
		buf.WriteByte(etfSmallAtomUTF8)
		buf.WriteByte(byte(len(b)))
	} else {
		buf.WriteByte(etfAtomUTF8)
		var lenBytes [2]byte
		binary.BigEndian.PutUint16(lenBytes[:], uint16(len(b)))
		buf.Write(lenBytes[:])
	}
	buf.Write(b)
}

func appendInt(buf *bytes.Buffer, n int64) {
	switch {
	case n >= 0 && n <= 255:
		buf.WriteByte(etfSmallInt)
		buf.WriteByte(byte(n))
	case n >= -2147483648 && n <= 2147483647:
		buf.WriteByte(etfInt)
		var b [4]byte
		binary.BigEndian.PutUint32(b[:], uint32(int32(n)))
		buf.Write(b[:])
	default:
		appendBigInt(buf, n)
	}
}

func appendBigInt(buf *bytes.Buffer, n int64) {
	sign := byte(0)
	if n < 0 {
		sign = 1
		n = -n
	}
	// Collect digits (little-endian bytes of magnitude).
	var digits []byte
	for n > 0 {
		digits = append(digits, byte(n&0xff))
		n >>= 8
	}
	buf.WriteByte(etfSmallBig)
	buf.WriteByte(byte(len(digits)))
	buf.WriteByte(sign)
	buf.Write(digits)
}

func appendFloat(buf *bytes.Buffer, f float64) {
	buf.WriteByte(etfNewFloat)
	var b [8]byte
	binary.BigEndian.PutUint64(b[:], math.Float64bits(f))
	buf.Write(b[:])
}

func appendTuple(buf *bytes.Buffer, elems []Term) {
	n := len(elems)
	if n <= 255 {
		buf.WriteByte(etfSmallTuple)
		buf.WriteByte(byte(n))
	} else {
		buf.WriteByte(etfLargeTuple)
		var b [4]byte
		binary.BigEndian.PutUint32(b[:], uint32(n))
		buf.Write(b[:])
	}
	for _, e := range elems {
		e.appendETF(buf)
	}
}

func appendList(buf *bytes.Buffer, elems []Term) {
	if len(elems) == 0 {
		buf.WriteByte(etfNil)
		return
	}
	buf.WriteByte(etfList)
	var b [4]byte
	binary.BigEndian.PutUint32(b[:], uint32(len(elems)))
	buf.Write(b[:])
	for _, e := range elems {
		e.appendETF(buf)
	}
	buf.WriteByte(etfNil) // proper list tail
}

func appendBinary(buf *bytes.Buffer, data []byte) {
	buf.WriteByte(etfBinary)
	var b [4]byte
	binary.BigEndian.PutUint32(b[:], uint32(len(data)))
	buf.Write(b[:])
	buf.Write(data)
}

func appendMap(buf *bytes.Buffer, pairs [][2]Term) {
	buf.WriteByte(etfMap)
	var b [4]byte
	binary.BigEndian.PutUint32(b[:], uint32(len(pairs)))
	buf.Write(b[:])
	for _, kv := range pairs {
		kv[0].appendETF(buf)
		kv[1].appendETF(buf)
	}
}

// EMap is an Erlang map value (for use inside c_literal nodes).
// Keys must be sorted for reproducibility; use NewEMap.
type EMap [][2]Term

func (m EMap) appendETF(buf *bytes.Buffer) { appendMap(buf, m) }

// Verify that our Term implementations satisfy the interface.
var _ Term = EAtom("")
var _ Term = EInt(0)
var _ Term = EFloat(0)
var _ Term = ETuple(nil)
var _ Term = EList(nil)
var _ Term = EBin(nil)

// String returns the Erlang-syntax representation of a term for
// debugging. Not used in production; only for test error messages.
func String(t Term) string {
	switch v := t.(type) {
	case EAtom:
		return string(v)
	case EInt:
		return fmt.Sprintf("%d", int64(v))
	case EFloat:
		return fmt.Sprintf("%g", float64(v))
	case ETuple:
		if len(v) == 0 {
			return "{}"
		}
		s := "{"
		for i, e := range v {
			if i > 0 {
				s += ","
			}
			s += String(e)
		}
		return s + "}"
	case EList:
		if len(v) == 0 {
			return "[]"
		}
		s := "["
		for i, e := range v {
			if i > 0 {
				s += ","
			}
			s += String(e)
		}
		return s + "]"
	case EBin:
		return fmt.Sprintf("<<\"%s\">>", string(v))
	default:
		return fmt.Sprintf("%v", t)
	}
}

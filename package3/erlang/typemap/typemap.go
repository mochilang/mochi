// Package typemap implements the closed Erlang-typespec-to-Mochi translation
// table from MEP-66 research note 05 (§ "Complete translation table").
//
// Translation is driven by the ETF representation of Erlang type terms as
// produced by the BEAM abstract code parser (beamingest) or, for EDoc
// packages, by parsing the raw typespec string (edocingest).
//
// Erlang type term shapes (from erl_parse abstract form):
//
//	{type, Line, TypeName, Args}          — built-in type
//	{atom, Line, Value}                   — atom literal (used in unions)
//	{integer, Line, Value}                — integer literal
//	{var, Line, VarName}                  — type variable
//	{ann_type, Line, [{var,...}, Type]}   — annotated type (name :: Type)
//	{remote_type, Line, [Mod, Name, Args]}— remote type (Module:TypeName)
//	{user_type, Line, Name, Args}         — user-defined type alias
//
// The closed table (research note 05 §4) maps Erlang types to Mochi:
//
//	boolean()        → bool
//	integer()        → int
//	non_neg_integer()→ int   (note: Mochi int is signed 64-bit)
//	pos_integer()    → int
//	float()          → float
//	atom()           → string  (atoms as strings, noted in binding)
//	binary()         → bytes
//	iodata()         → SKIP SkipIodata
//	iolist()         → SKIP SkipIodata
//	bitstring()      → SKIP SkipBitstring
//	string()         → SKIP SkipCharlist  (Erlang string() = charlist)
//	list(T)          → []T
//	[T]              → []T
//	nil/[]           → []T (empty list literal)
//	{ok, T}          → result<T, string>  (ok/error idiom)
//	{error, Reason}  → (part of ok/error pattern; see above)
//	{ok,T}|{err,R}   → result<T, R>
//	tuple()          → SKIP SkipUntypedTuple
//	{A, B, ...}      → tuple[A, B, ...]  (typed tuple, up to 8 elements)
//	map()            → SKIP SkipUntypedMap
//	#{K := V}        → SKIP SkipTypedMap
//	fun()            → SKIP SkipUntypedFun
//	fun((A)->B)      → fn(A) B
//	any() / term()   → SKIP SkipAnyTerm
//	pid()            → extern "erlang.Pid"
//	port()           → extern "erlang.Port"
//	reference()      → extern "erlang.Reference"
//	union 2-branch   → okError pattern check; else SkipNonOkErrorUnion
//	union 3+ branches→ SKIP SkipComplexUnion
//	remote_type      → SKIP SkipRemoteType
//	recursive type   → SKIP SkipRecursiveType (detected via depth limit)
package typemap

import (
	"fmt"

	breakers "mochi/package3/erlang/errors"
	"mochi/package3/erlang/etf"
)

// MochiType is the Mochi type string produced by the translation table.
// It is a simplified textual representation used in the extern fn declarations.
type MochiType string

const (
	MochiBool      MochiType = "bool"
	MochiInt       MochiType = "int"
	MochiFloat     MochiType = "float"
	MochiString    MochiType = "string"
	MochiBytes     MochiType = "bytes"
	MochiPid       MochiType = `extern "erlang.Pid"`
	MochiPort      MochiType = `extern "erlang.Port"`
	MochiReference MochiType = `extern "erlang.Reference"`
	MochiUnit      MochiType = "unit" // for the atom 'ok' alone
)

// Result represents a result<T, E> Mochi type.
type Result struct {
	Ok  MochiType
	Err MochiType
}

func (r Result) String() string {
	return fmt.Sprintf("result<%s, %s>", r.Ok, r.Err)
}

// List represents a []T Mochi type.
type List struct {
	Elem MochiType
}

func (l List) String() string {
	return "[]" + string(l.Elem)
}

// Tuple represents a tuple[A, B, ...] Mochi type.
type Tuple struct {
	Elems []MochiType
}

func (t Tuple) String() string {
	if len(t.Elems) == 0 {
		return "tuple[]"
	}
	s := "tuple["
	for i, e := range t.Elems {
		if i > 0 {
			s += ", "
		}
		s += string(e)
	}
	return s + "]"
}

// Fn represents a fn(A, B) C Mochi type.
type Fn struct {
	Args   []MochiType
	Return MochiType
}

func (f Fn) String() string {
	if len(f.Args) == 0 {
		return "fn() " + string(f.Return)
	}
	s := "fn("
	for i, a := range f.Args {
		if i > 0 {
			s += ", "
		}
		s += string(a)
	}
	return s + ") " + string(f.Return)
}

// TranslateResult holds the output of a successful type translation.
// Exactly one of Scalar, ResultT, ListT, TupleT, FnT is set.
type TranslateResult struct {
	Scalar  MochiType
	ResultT *Result
	ListT   *List
	TupleT  *Tuple
	FnT     *Fn
}

// String renders a TranslateResult as a Mochi type string.
func (tr TranslateResult) String() string {
	if tr.ResultT != nil {
		return tr.ResultT.String()
	}
	if tr.ListT != nil {
		return tr.ListT.String()
	}
	if tr.TupleT != nil {
		return tr.TupleT.String()
	}
	if tr.FnT != nil {
		return tr.FnT.String()
	}
	return string(tr.Scalar)
}

// TranslateType translates a single Erlang type ETF term to a Mochi type.
// Returns an error whose value satisfies errors.As(*BridgeError) on skip;
// all other errors are hard failures (malformed input).
func TranslateType(term interface{}) (TranslateResult, error) {
	return translateType(term, 0)
}

const maxDepth = 16

func translateType(term interface{}, depth int) (TranslateResult, error) {
	if depth > maxDepth {
		return TranslateResult{}, breakers.Wrap("typemap", "", fmt.Errorf("type recursion depth exceeded"))
	}
	switch t := term.(type) {
	case etf.Tuple:
		return translateTuple(t, depth)
	case etf.Atom:
		// Atom literal in a type position (e.g. 'ok' | 'error').
		switch t {
		case "true", "false":
			return TranslateResult{Scalar: MochiBool}, nil
		case "ok":
			return TranslateResult{Scalar: MochiUnit}, nil
		case "undefined":
			return TranslateResult{Scalar: MochiUnit}, nil
		}
		// Any other atom → string (atoms as enumeration strings).
		return TranslateResult{Scalar: MochiString}, nil
	case etf.List:
		if len(t) == 0 {
			// [] (nil / empty list).
			return TranslateResult{ListT: &List{Elem: MochiUnit}}, nil
		}
	}
	return TranslateResult{}, fmt.Errorf("typemap: unhandled term type %T", term)
}

func translateTuple(t etf.Tuple, depth int) (TranslateResult, error) {
	if len(t) == 0 {
		return TranslateResult{}, fmt.Errorf("typemap: empty tuple term")
	}
	tag, ok := t[0].(etf.Atom)
	if !ok {
		return TranslateResult{}, fmt.Errorf("typemap: tuple tag is not atom: %T", t[0])
	}

	switch tag {
	case "type":
		return translateBuiltinType(t, depth)
	case "ann_type":
		// Annotated type: {ann_type, Line, [VarAnnotation, ActualType]}.
		if len(t) >= 3 {
			if list, ok := t[2].(etf.List); ok && len(list) >= 2 {
				return translateType(list[1], depth+1)
			}
		}
		return TranslateResult{}, fmt.Errorf("typemap: malformed ann_type")
	case "atom":
		if len(t) >= 3 {
			if a, ok := t[2].(etf.Atom); ok {
				return translateType(a, depth+1)
			}
		}
		return TranslateResult{Scalar: MochiString}, nil
	case "integer":
		return TranslateResult{Scalar: MochiInt}, nil
	case "var":
		// Type variable — treat as any() for now; callers may use it.
		return TranslateResult{}, &skipError{breakers.SkipAnyTerm, "type variable"}
	case "remote_type":
		return TranslateResult{}, &skipError{breakers.SkipRemoteType, "remote type reference"}
	case "user_type":
		// User-defined type alias: {user_type, Line, Name, Args}.
		// The type body is not available here; caller must expand.
		return TranslateResult{}, &skipError{breakers.SkipRemoteType, "user type alias (not expanded)"}
	}
	return TranslateResult{}, fmt.Errorf("typemap: unknown tuple tag %q", tag)
}

func translateBuiltinType(t etf.Tuple, depth int) (TranslateResult, error) {
	if len(t) < 3 {
		return TranslateResult{}, fmt.Errorf("typemap: type tuple too short")
	}
	name, ok := t[2].(etf.Atom)
	if !ok {
		return TranslateResult{}, fmt.Errorf("typemap: type name not atom: %T", t[2])
	}
	var args etf.List
	if len(t) >= 4 {
		args, _ = t[3].(etf.List)
	}

	switch name {
	case "boolean":
		return TranslateResult{Scalar: MochiBool}, nil
	case "integer", "non_neg_integer", "pos_integer", "neg_integer":
		return TranslateResult{Scalar: MochiInt}, nil
	case "float":
		return TranslateResult{Scalar: MochiFloat}, nil
	case "atom":
		return TranslateResult{Scalar: MochiString}, nil
	case "binary":
		return TranslateResult{Scalar: MochiBytes}, nil
	case "pid":
		return TranslateResult{Scalar: MochiPid}, nil
	case "port":
		return TranslateResult{Scalar: MochiPort}, nil
	case "reference":
		return TranslateResult{Scalar: MochiReference}, nil

	case "any", "term":
		return TranslateResult{}, &skipError{breakers.SkipAnyTerm, string(name) + "()"}
	case "iodata", "iolist":
		return TranslateResult{}, &skipError{breakers.SkipIodata, string(name) + "()"}
	case "bitstring":
		return TranslateResult{}, &skipError{breakers.SkipBitstring, "bitstring()"}
	case "string":
		return TranslateResult{}, &skipError{breakers.SkipCharlist, "string() is charlist in Erlang"}
	case "tuple":
		if len(args) == 0 {
			return TranslateResult{}, &skipError{breakers.SkipUntypedTuple, "tuple()"}
		}
		// Typed tuple: {type, Line, tuple, [T1, T2, ...]}
		// Translate each element.
		var elems []MochiType
		for _, arg := range args {
			r, err := translateType(arg, depth+1)
			if err != nil {
				return TranslateResult{}, err
			}
			elems = append(elems, MochiType(r.String()))
		}
		return TranslateResult{TupleT: &Tuple{Elems: elems}}, nil
	case "map":
		if len(args) == 0 {
			return TranslateResult{}, &skipError{breakers.SkipUntypedMap, "map()"}
		}
		return TranslateResult{}, &skipError{breakers.SkipTypedMap, "typed map #{...}"}
	case "fun":
		return translateFun(args, depth)
	case "list":
		if len(args) == 0 {
			// list() — untyped list; treat as list(any) → skip.
			return TranslateResult{}, &skipError{breakers.SkipAnyTerm, "list() (untyped)"}
		}
		r, err := translateType(args[0], depth+1)
		if err != nil {
			return TranslateResult{}, err
		}
		return TranslateResult{ListT: &List{Elem: MochiType(r.String())}}, nil
	case "nil":
		return TranslateResult{ListT: &List{Elem: MochiUnit}}, nil
	case "union":
		return translateUnion(args, depth)
	case "ok":
		return TranslateResult{Scalar: MochiUnit}, nil
	case "nonempty_list":
		// nonempty_list(T) — same treatment as list(T).
		if len(args) == 0 {
			return TranslateResult{}, &skipError{breakers.SkipAnyTerm, "nonempty_list() (untyped)"}
		}
		r, err := translateType(args[0], depth+1)
		if err != nil {
			return TranslateResult{}, err
		}
		return TranslateResult{ListT: &List{Elem: MochiType(r.String())}}, nil
	case "maybe_improper_list", "nonempty_improper_list", "nonempty_maybe_improper_list":
		return TranslateResult{}, &skipError{breakers.SkipIodata, string(name) + "()"}
	}

	// Unknown built-in type.
	return TranslateResult{}, &skipError{breakers.SkipAnyTerm, "unknown built-in type: " + string(name)}
}

// translateFun handles the fun() type family.
func translateFun(args etf.List, depth int) (TranslateResult, error) {
	if len(args) == 0 {
		return TranslateResult{}, &skipError{breakers.SkipUntypedFun, "fun() (no type annotation)"}
	}
	// fun(...) has one element: {type, Line, product, [ArgTypes...]} as first arg
	// and the return type as second arg (in the outer type form).
	// Actually the AST shape for fun((A,B)->C) is:
	//   {type, Line, fun, [{type, Line, product, [A, B]}, C]}
	if len(args) < 2 {
		return TranslateResult{}, &skipError{breakers.SkipUntypedFun, "fun() (incomplete)"}
	}
	productTup, ok := args[0].(etf.Tuple)
	if !ok || len(productTup) < 4 {
		return TranslateResult{}, &skipError{breakers.SkipUntypedFun, "fun() (no product type)"}
	}
	productName, _ := productTup[2].(etf.Atom)
	if productName != "product" {
		return TranslateResult{}, &skipError{breakers.SkipUntypedFun, "fun() (unexpected product shape)"}
	}
	argTerms, _ := productTup[3].(etf.List)
	retTerm := args[1]

	var argTypes []MochiType
	for _, a := range argTerms {
		r, err := translateType(a, depth+1)
		if err != nil {
			return TranslateResult{}, &skipError{breakers.SkipFunArgNotInTable, fmt.Sprintf("fun arg: %v", err)}
		}
		argTypes = append(argTypes, MochiType(r.String()))
	}
	ret, err := translateType(retTerm, depth+1)
	if err != nil {
		return TranslateResult{}, &skipError{breakers.SkipFunArgNotInTable, fmt.Sprintf("fun return: %v", err)}
	}
	return TranslateResult{FnT: &Fn{Args: argTypes, Return: MochiType(ret.String())}}, nil
}

// translateUnion handles union types (2+ branches).
func translateUnion(args etf.List, depth int) (TranslateResult, error) {
	if len(args) < 2 {
		// Degenerate union — treat as single type.
		if len(args) == 1 {
			return translateType(args[0], depth+1)
		}
		return TranslateResult{}, fmt.Errorf("typemap: empty union")
	}
	if len(args) > 2 {
		return TranslateResult{}, &skipError{breakers.SkipComplexUnion,
			fmt.Sprintf("union has %d branches", len(args))}
	}
	// Two-branch union: check for {ok, T} | {error, Reason} pattern.
	a, b := args[0], args[1]
	if ok, okType := matchOkTuple(a); ok {
		if matchErrorTuple(b) {
			return TranslateResult{ResultT: &Result{Ok: MochiType(okType.String()), Err: MochiString}}, nil
		}
	}
	if ok, okType := matchOkTuple(b); ok {
		if matchErrorTuple(a) {
			return TranslateResult{ResultT: &Result{Ok: MochiType(okType.String()), Err: MochiString}}, nil
		}
	}
	return TranslateResult{}, &skipError{breakers.SkipNonOkErrorUnion, "2-branch union does not match {ok,T}|{error,Reason}"}
}

// matchOkTuple reports whether term is {ok, T} (as an ETF tuple type form)
// and returns the translated T if so.
func matchOkTuple(term interface{}) (bool, TranslateResult) {
	tup, ok := term.(etf.Tuple)
	if !ok || len(tup) < 4 {
		return false, TranslateResult{}
	}
	tag, _ := tup[0].(etf.Atom)
	if tag != "type" {
		return false, TranslateResult{}
	}
	name, _ := tup[2].(etf.Atom)
	if name != "tuple" {
		return false, TranslateResult{}
	}
	elems, _ := tup[3].(etf.List)
	if len(elems) != 2 {
		return false, TranslateResult{}
	}
	// First element must be atom 'ok'.
	if !isAtomLiteral(elems[0], "ok") {
		return false, TranslateResult{}
	}
	r, err := translateType(elems[1], 1)
	if err != nil {
		return false, TranslateResult{}
	}
	return true, r
}

// matchErrorTuple reports whether term is {error, _}.
func matchErrorTuple(term interface{}) bool {
	tup, ok := term.(etf.Tuple)
	if !ok || len(tup) < 4 {
		return false
	}
	tag, _ := tup[0].(etf.Atom)
	if tag != "type" {
		return false
	}
	name, _ := tup[2].(etf.Atom)
	if name != "tuple" {
		return false
	}
	elems, _ := tup[3].(etf.List)
	if len(elems) < 1 {
		return false
	}
	return isAtomLiteral(elems[0], "error")
}

func isAtomLiteral(term interface{}, want string) bool {
	tup, ok := term.(etf.Tuple)
	if !ok || len(tup) < 3 {
		if a, ok := term.(etf.Atom); ok {
			return string(a) == want
		}
		return false
	}
	tag, _ := tup[0].(etf.Atom)
	if tag != "atom" {
		return false
	}
	a, _ := tup[2].(etf.Atom)
	return string(a) == want
}

// skipError is a lightweight skip wrapper that implements the error interface.
type skipError struct {
	Reason breakers.SkipReason
	Detail string
}

func (e *skipError) Error() string {
	return fmt.Sprintf("SKIP %s: %s", e.Reason, e.Detail)
}

// IsSkip reports whether err is a skip error and returns the reason/detail.
func IsSkip(err error) (breakers.SkipReason, string, bool) {
	if se, ok := err.(*skipError); ok {
		return se.Reason, se.Detail, true
	}
	return 0, "", false
}

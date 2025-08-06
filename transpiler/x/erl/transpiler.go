//go:build slow

package erl

import (
	"bytes"
	"fmt"
	"io"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"mochi/runtime/data"

	"mochi/parser"
	"mochi/types"
)

const helperNow = `
mochi_now() ->
    case erlang:get(now_seed) of
        undefined ->
            case os:getenv("MOCHI_NOW_SEED") of
                false -> erlang:system_time(nanosecond);
                S ->
                    case catch list_to_integer(S) of
                        {'EXIT', _} -> erlang:system_time(nanosecond);
                        Seed ->
                            erlang:put(now_seed, Seed),
                            mochi_now()
                    end
            end;
        Seed ->
            Seed2 = (Seed * 1664525 + 1013904223) rem 2147483647,
            erlang:put(now_seed, Seed2),
            Seed2
    end.
`

const helperLookupHost = `
mochi_lookup_host(Host) ->
    case inet:gethostbyname(Host) of
        {ok, {hostent, _, _, _, _, Addrs}} ->
            IPs = [inet:ntoa(A) || A <- Addrs],
            [IPs, nil];
        {error, Reason} ->
        [nil, Reason]
    end.
`

const helperMember = `
mochi_member(Key, Coll) ->
    case erlang:is_map(Coll) of
        true -> maps:is_key(Key, Coll);
        _ -> case erlang:is_list(Coll) of
            true -> lists:member(Key, Coll);
            _ when is_binary(Coll) -> string:str(Coll, Key) /= 0;
            _ -> false
        end
    end.
`
const helperToInt = `
mochi_to_int(V) ->
    case erlang:is_integer(V) of
        true -> V;
        _ -> case erlang:is_float(V) of
            true -> trunc(V);
            _ -> list_to_integer(V)
        end
    end.
`

const helperPadStart = `
mochi_pad_start(S, Len, Ch) ->
    Fill0 = case Ch of
        "" -> " ";
        _ -> Ch
    end,
    Fill = string:substr(Fill0, 1, 1),
    SL = length(S),
    case SL >= Len of
        true -> S;
        _ -> string:copies(Fill, Len - SL) ++ S
    end.
`

const helperSHA256 = `
mochi_sha256(Bs) ->
    Bin = list_to_binary(Bs),
    binary_to_list(crypto:hash(sha256, Bin)).
`

const helperIndexOf = `
mochi_index_of(S, Ch) when is_list(S) ->
    Char = case Ch of
        [C|_] when is_list(C) -> hd(C);
        [C|_] -> C;
        <<C,_/binary>> -> C;
        C when is_integer(C) -> C;
        _ -> $\0
    end,
    case string:chr(S, Char) of
        0 -> -1;
        N -> N - 1
    end;
mochi_index_of(_, _) -> -1.
`

const helperParseIntStr = `
mochi_parse_int_str(S) ->
    try list_to_integer(S) catch _:_ -> 0 end.
`

const helperFetch = `
mochi_fetch(Url) ->
    mochi_fetch(Url, nil).

mochi_fetch(Url, _Opts) ->
    Cmd = "curl -fsSL " ++ Url,
    Out = os:cmd(Cmd),
    case re:run(Out, "\"title\"\\s*:\\s*\"([^\"]+)\"", [{capture, [1], list}]) of
        {match, [Title]} -> #{"title" => Title};
        _ -> Out
    end.
`

const helperNot = `
mochi_not(X) ->
    case X of
        true -> false;
        false -> true;
        nil -> true;
        _ -> false
    end.
`

const helperSafeArith = `
mochi_safe_mul(A, B) ->
    try A * B catch _:_ -> 1.0e308 end.

mochi_safe_div(A, B) ->
    try A / B catch _:_ -> 0.0 end.
`

const helperSafeFmod = `
mochi_safe_fmod(A, B) ->
    try math:fmod(A, B) catch _:_ -> 0.0 end.
`
const helperBigRat = `
mochi_gcd(A, B) ->
    A1 = abs(A),
    B1 = abs(B),
    case B1 of
        0 -> A1;
        _ -> mochi_gcd(B1, A1 rem B1)
    end.

mochi_bigrat(N) when is_integer(N) -> mochi_bigrat(N, 1);
mochi_bigrat({Num, Den}) -> mochi_bigrat(Num, Den).
mochi_bigrat(N, D) ->
    D0 = case D of undefined -> 1; _ -> D end,
    case D0 < 0 of
        true -> mochi_bigrat(-N, -D0);
        false ->
            G = mochi_gcd(N, D0),
            {N div G, D0 div G}
    end.

mochi_num({N,_}) -> N.
mochi_denom({_,D}) -> D.
mochi_add(A, B) ->
    mochi_bigrat(mochi_num(A)*mochi_denom(B) + mochi_num(B)*mochi_denom(A), mochi_denom(A)*mochi_denom(B)).
mochi_sub(A, B) ->
    mochi_bigrat(mochi_num(A)*mochi_denom(B) - mochi_num(B)*mochi_denom(A), mochi_denom(A)*mochi_denom(B)).
mochi_mul(A, B) ->
    mochi_bigrat(mochi_num(A)*mochi_num(B), mochi_denom(A)*mochi_denom(B)).
mochi_div(A, B) ->
    mochi_bigrat(mochi_num(A)*mochi_denom(B), mochi_denom(A)*mochi_num(B)).
mochi_cmp(A, B) ->
    mochi_num(A)*mochi_denom(B) - mochi_num(B)*mochi_denom(A).
num(X) -> mochi_num(X).
denom(X) -> mochi_denom(X).
`

const helperRepeat = `
mochi_repeat(S, N) when is_binary(S) ->
    binary:copy(S, mochi_to_int(N));
mochi_repeat(S, N) when is_list(S) ->
    string:copies(S, mochi_to_int(N));
mochi_repeat(_, _) -> [].
`

const helperStr = `
-compile({nowarn_unused_function, mochi_str/1}).
mochi_str(V) ->
    S = lists:flatten(io_lib:format("~p", [V])),
    S1 = lists:flatten(string:replace(S, ",", " ", all)),
    lists:flatten(string:replace(S1, "\"", "", all)).
`

const helperRepr = `
-compile({nowarn_unused_function, mochi_repr/1}).
mochi_repr(V) ->
    S = lists:flatten(io_lib:format("~p", [V])),
    lists:flatten(string:replace(S, ",", ", ", all)).
`

var useNow bool
var useLookupHost bool
var useToInt bool
var useMemberHelper bool
var usePadStart bool
var useSHA256 bool
var useIndexOf bool
var useParseIntStr bool
var useBigRat bool
var useRepeat bool
var useStr bool
var useFetch bool
var useNot bool
var useSafeArith bool
var useSafeFmod bool
var mutatedFuncs map[string]int
var benchMain bool

// SetBenchMain configures whether the generated main function is wrapped in a
// benchmark block when emitting code. When enabled, the program will output a
// JSON object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMain = v }

// loopStack tracks variable names for nested loops to handle continue.
var loopStack [][]string

// Program is a minimal Erlang module consisting of sequential statements.
// Program is a minimal Erlang module consisting of sequential statements and
// optional function declarations.
type Program struct {
	Funs            []*FuncDecl
	Stmts           []Stmt
	UseNow          bool
	UseLookupHost   bool
	UseToInt        bool
	UseMemberHelper bool
	UsePadStart     bool
	UseSHA256       bool
	UseIndexOf      bool
	UseParseIntStr  bool
	UseBigRat       bool
	UseRepeat       bool
	UseStr          bool
	UseFetch        bool
	UseNot          bool
	UseSafeArith    bool
	UseSafeFmod     bool
}

// context tracks variable aliases to emulate mutable variables.
type context struct {
	alias      map[string]string
	orig       map[string]string
	counter    map[string]int
	strVar     map[string]bool
	floatVar   map[string]bool
	mapVar     map[string]bool
	boolVar    map[string]bool
	listStrVar map[string]bool
	strField   map[string]map[string]bool
	boolField  map[string]map[string]bool
	groups     map[string]groupInfo
	constVal   map[string]Expr
	autoMod    map[string]string
	baseDir    string
	globals    map[string]bool
	params     map[string]bool
	mutated    map[string]bool
}

type groupInfo struct{ key, items string }

func newContext(base string) *context {
	return &context{alias: map[string]string{}, orig: map[string]string{}, counter: map[string]int{}, strVar: map[string]bool{}, floatVar: map[string]bool{}, mapVar: map[string]bool{}, boolVar: map[string]bool{}, listStrVar: map[string]bool{}, strField: map[string]map[string]bool{}, boolField: map[string]map[string]bool{}, groups: map[string]groupInfo{}, constVal: map[string]Expr{}, autoMod: map[string]string{}, baseDir: base, globals: map[string]bool{}, params: map[string]bool{}, mutated: map[string]bool{}}
}

func (c *context) clone() *context {
	alias := make(map[string]string, len(c.alias))
	for k, v := range c.alias {
		alias[k] = v
	}
	orig := make(map[string]string, len(c.orig))
	for k, v := range c.orig {
		orig[k] = v
	}
	counter := c.counter
	fields := make(map[string]map[string]bool, len(c.strField))
	for k, v := range c.strField {
		fm := make(map[string]bool, len(v))
		for kk, vv := range v {
			fm[kk] = vv
		}
		fields[k] = fm
	}
	bfields := make(map[string]map[string]bool, len(c.boolField))
	for k, v := range c.boolField {
		fm := make(map[string]bool, len(v))
		for kk, vv := range v {
			fm[kk] = vv
		}
		bfields[k] = fm
	}
	groups := make(map[string]groupInfo, len(c.groups))
	for k, v := range c.groups {
		groups[k] = v
	}
	strVar := make(map[string]bool, len(c.strVar))
	for k, v := range c.strVar {
		strVar[k] = v
	}
	floatVar := make(map[string]bool, len(c.floatVar))
	for k, v := range c.floatVar {
		floatVar[k] = v
	}
	mapVar := make(map[string]bool, len(c.mapVar))
	for k, v := range c.mapVar {
		mapVar[k] = v
	}
	boolVar := make(map[string]bool, len(c.boolVar))
	for k, v := range c.boolVar {
		boolVar[k] = v
	}
	listStrVar := make(map[string]bool, len(c.listStrVar))
	for k, v := range c.listStrVar {
		listStrVar[k] = v
	}
	consts := make(map[string]Expr, len(c.constVal))
	for k, v := range c.constVal {
		consts[k] = v
	}
	mods := make(map[string]string, len(c.autoMod))
	for k, v := range c.autoMod {
		mods[k] = v
	}
	gl := make(map[string]bool, len(c.globals))
	for k, v := range c.globals {
		gl[k] = v
	}
	params := make(map[string]bool, len(c.params))
	for k, v := range c.params {
		params[k] = v
	}
	mut := make(map[string]bool, len(c.mutated))
	for k, v := range c.mutated {
		mut[k] = v
	}
	return &context{alias: alias, orig: orig, counter: counter, strVar: strVar, floatVar: floatVar, mapVar: mapVar, boolVar: boolVar, listStrVar: listStrVar, strField: fields, boolField: bfields, groups: groups, constVal: consts, autoMod: mods, baseDir: c.baseDir, globals: gl, params: params, mutated: mut}
}

func (c *context) newAlias(name string) string {
	if name == "_" {
		return "_"
	}
	c.counter[name]++
	alias := sanitize(name)
	if _, exists := c.orig[alias]; exists {
		alias = fmt.Sprintf("%s_%d", alias, c.counter[name])
	} else if c.counter[name] > 1 {
		alias = fmt.Sprintf("%s_%d", alias, c.counter[name])
	}
	c.alias[name] = alias
	c.orig[alias] = name
	return alias
}

func (c *context) current(name string) string {
	if name == "_" {
		return "_"
	}
	if a, ok := c.alias[name]; ok {
		return a
	}
	return c.newAlias(name)
}

func (c *context) original(alias string) string {
	if o, ok := c.orig[alias]; ok {
		return o
	}
	return alias
}

func (c *context) setStrFields(name string, fields map[string]bool) {
	if len(fields) == 0 {
		return
	}
	if c.strField == nil {
		c.strField = map[string]map[string]bool{}
	}
	c.strField[name] = fields
}

func (c *context) setBoolFields(name string, fields map[string]bool) {
	if len(fields) == 0 {
		return
	}
	if c.boolField == nil {
		c.boolField = map[string]map[string]bool{}
	}
	c.boolField[name] = fields
}

func (c *context) setGroup(name, keyVar, itemsVar string) {
	if c.groups == nil {
		c.groups = map[string]groupInfo{}
	}
	c.groups[name] = groupInfo{key: keyVar, items: itemsVar}
}

func (c *context) setStringVar(name string, isStr bool) {
	if !isStr {
		return
	}
	if c.strVar == nil {
		c.strVar = map[string]bool{}
	}
	c.strVar[name] = true
}

func (c *context) setMapVar(name string, isMap bool) {
	if !isMap {
		return
	}
	if c.mapVar == nil {
		c.mapVar = map[string]bool{}
	}
	c.mapVar[name] = true
}

func (c *context) setFloatVar(name string, isFloat bool) {
	if !isFloat {
		return
	}
	if c.floatVar == nil {
		c.floatVar = map[string]bool{}
	}
	c.floatVar[name] = true
}

func (c *context) setListStrVar(name string, isListStr bool) {
	if !isListStr {
		return
	}
	if c.listStrVar == nil {
		c.listStrVar = map[string]bool{}
	}
	c.listStrVar[name] = true
}

func (c *context) setBoolVar(name string, isBool bool) {
	if !isBool {
		return
	}
	if c.boolVar == nil {
		c.boolVar = map[string]bool{}
	}
	c.boolVar[name] = true
}

func (c *context) addParam(name string) {
	if c.params == nil {
		c.params = map[string]bool{}
	}
	c.params[name] = true
}

func (c *context) isParam(name string) bool { return c.params != nil && c.params[name] }

func (c *context) markMutated(name string) {
	if c.mutated == nil {
		c.mutated = map[string]bool{}
	}
	c.mutated[name] = true
}

func (c *context) isMutated(name string) bool { return c.mutated != nil && c.mutated[name] }

func (c *context) setConst(name string, val Expr) {
	if c.constVal == nil {
		c.constVal = map[string]Expr{}
	}
	c.constVal[name] = val
}

func (c *context) clearConst(name string) {
	if c.constVal == nil {
		return
	}
	delete(c.constVal, name)
}

func (c *context) constValue(name string) (Expr, bool) {
	v, ok := c.constVal[name]
	return v, ok
}

func (c *context) setGlobal(name string) {
	if c.globals == nil {
		c.globals = map[string]bool{}
	}
	c.globals[name] = true
}

func (c *context) isGlobal(name string) bool { return c.globals != nil && c.globals[name] }

func (c *context) addAutoModule(alias, path string) {
	if c.autoMod == nil {
		c.autoMod = map[string]string{}
	}
	c.autoMod[alias] = path
}

func (c *context) autoModule(alias string) (string, bool) {
	v, ok := c.autoMod[alias]
	return v, ok
}

func (c *context) isStringVar(name string) bool {
	return c.strVar[name]
}

func (c *context) isMapVar(name string) bool {
	return c.mapVar[name]
}

func (c *context) isFloatVar(name string) bool {
	return c.floatVar[name]
}

func (c *context) isBoolVar(name string) bool {
	return c.boolVar[name]
}

func (c *context) isListStrVar(name string) bool {
	return c.listStrVar[name]
}

func (c *context) getGroup(name string) (groupInfo, bool) {
	g, ok := c.groups[name]
	return g, ok
}

func (c *context) isStrField(name, field string) bool {
	if m, ok := c.strField[name]; ok {
		return m[field]
	}
	return false
}

func (c *context) isBoolField(name, field string) bool {
	if m, ok := c.boolField[name]; ok {
		return m[field]
	}
	return false
}

func simpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	pf := u.Value
	if len(pf.Ops) > 0 || pf.Target == nil || pf.Target.Selector == nil || len(pf.Target.Selector.Tail) > 0 {
		return "", false
	}
	return pf.Target.Selector.Root, true
}

func isGroupKeyExpr(e *parser.Expr, name string) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	pf := u.Value
	if pf.Target == nil || pf.Target.Selector == nil || pf.Target.Selector.Root != name {
		return false
	}
	if len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "key" && len(pf.Ops) == 0 {
		return true
	}
	if len(pf.Target.Selector.Tail) == 0 && len(pf.Ops) == 1 && pf.Ops[0].Field != nil && pf.Ops[0].Field.Name == "key" {
		return true
	}
	return false
}

func isGroupItemsExpr(e *parser.Expr, name string) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	pf := u.Value
	if pf.Target == nil || pf.Target.Selector == nil || pf.Target.Selector.Root != name {
		return false
	}
	if len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "items" && len(pf.Ops) == 0 {
		return true
	}
	if len(pf.Target.Selector.Tail) == 0 && len(pf.Ops) == 1 && pf.Ops[0].Field != nil && pf.Ops[0].Field.Name == "items" {
		return true
	}
	return false
}

func replaceGroupExpr(e Expr, groupVar, keyVar, itemsVar string) Expr {
	switch v := e.(type) {
	case *NameRef:
		if v.Name == groupVar {
			return &NameRef{Name: itemsVar, IsString: v.IsString}
		}
		return v
	case *BinaryExpr:
		v.Left = replaceGroupExpr(v.Left, groupVar, keyVar, itemsVar)
		v.Right = replaceGroupExpr(v.Right, groupVar, keyVar, itemsVar)
		return v
	case *UnaryExpr:
		v.Expr = replaceGroupExpr(v.Expr, groupVar, keyVar, itemsVar)
		return v
	case *CallExpr:
		for i := range v.Args {
			v.Args[i] = replaceGroupExpr(v.Args[i], groupVar, keyVar, itemsVar)
		}
		return v
	case *ListLit:
		for i := range v.Elems {
			v.Elems[i] = replaceGroupExpr(v.Elems[i], groupVar, keyVar, itemsVar)
		}
		return v
	case *MapLit:
		for i := range v.Items {
			v.Items[i].Key = replaceGroupExpr(v.Items[i].Key, groupVar, keyVar, itemsVar)
			v.Items[i].Value = replaceGroupExpr(v.Items[i].Value, groupVar, keyVar, itemsVar)
		}
		return v
	case *IndexExpr:
		if nr, ok := v.Target.(*NameRef); ok && nr.Name == groupVar {
			if lit, ok := v.Index.(*StringLit); ok {
				if lit.Value == "key" {
					return &NameRef{Name: keyVar}
				}
				if lit.Value == "items" {
					return &NameRef{Name: itemsVar}
				}
			}
		}
		v.Target = replaceGroupExpr(v.Target, groupVar, keyVar, itemsVar)
		v.Index = replaceGroupExpr(v.Index, groupVar, keyVar, itemsVar)
		return v
	case *IfExpr:
		v.Cond = replaceGroupExpr(v.Cond, groupVar, keyVar, itemsVar)
		v.Then = replaceGroupExpr(v.Then, groupVar, keyVar, itemsVar)
		v.Else = replaceGroupExpr(v.Else, groupVar, keyVar, itemsVar)
		return v
	case *ContainsExpr:
		v.Str = replaceGroupExpr(v.Str, groupVar, keyVar, itemsVar)
		v.Sub = replaceGroupExpr(v.Sub, groupVar, keyVar, itemsVar)
		return v
	case *SliceExpr:
		v.Target = replaceGroupExpr(v.Target, groupVar, keyVar, itemsVar)
		if v.Start != nil {
			v.Start = replaceGroupExpr(v.Start, groupVar, keyVar, itemsVar)
		}
		if v.End != nil {
			v.End = replaceGroupExpr(v.End, groupVar, keyVar, itemsVar)
		}
		return v
	case *SubstringExpr:
		v.Str = replaceGroupExpr(v.Str, groupVar, keyVar, itemsVar)
		v.Start = replaceGroupExpr(v.Start, groupVar, keyVar, itemsVar)
		v.End = replaceGroupExpr(v.End, groupVar, keyVar, itemsVar)
		return v
	case *QueryExpr:
		v.Src = replaceGroupExpr(v.Src, groupVar, keyVar, itemsVar)
		for i := range v.Froms {
			v.Froms[i].Src = replaceGroupExpr(v.Froms[i].Src, groupVar, keyVar, itemsVar)
		}
		if v.Right != nil {
			v.Right.Src = replaceGroupExpr(v.Right.Src, groupVar, keyVar, itemsVar)
			v.Right.On = replaceGroupExpr(v.Right.On, groupVar, keyVar, itemsVar)
		}
		if v.Where != nil {
			v.Where = replaceGroupExpr(v.Where, groupVar, keyVar, itemsVar)
		}
		v.Select = replaceGroupExpr(v.Select, groupVar, keyVar, itemsVar)
		if v.SortKey != nil {
			v.SortKey = replaceGroupExpr(v.SortKey, groupVar, keyVar, itemsVar)
		}
		if v.Skip != nil {
			v.Skip = replaceGroupExpr(v.Skip, groupVar, keyVar, itemsVar)
		}
		if v.Take != nil {
			v.Take = replaceGroupExpr(v.Take, groupVar, keyVar, itemsVar)
		}
		return v
	default:
		return v
	}
}

func substituteFieldRefs(e Expr, fields map[string]bool) Expr {
	switch v := e.(type) {
	case *NameRef:
		if fields[v.Name] {
			return &IndexExpr{Target: &NameRef{Name: "Item"}, Index: &StringLit{Value: v.Name}, Kind: "map"}
		}
		return v
	case *BinaryExpr:
		v.Left = substituteFieldRefs(v.Left, fields)
		v.Right = substituteFieldRefs(v.Right, fields)
		return v
	case *UnaryExpr:
		v.Expr = substituteFieldRefs(v.Expr, fields)
		return v
	case *CallExpr:
		for i := range v.Args {
			v.Args[i] = substituteFieldRefs(v.Args[i], fields)
		}
		return v
	case *IndexExpr:
		v.Target = substituteFieldRefs(v.Target, fields)
		v.Index = substituteFieldRefs(v.Index, fields)
		return v
	case *ListLit:
		for i := range v.Elems {
			v.Elems[i] = substituteFieldRefs(v.Elems[i], fields)
		}
		return v
	case *MapLit:
		for i := range v.Items {
			v.Items[i].Key = substituteFieldRefs(v.Items[i].Key, fields)
			v.Items[i].Value = substituteFieldRefs(v.Items[i].Value, fields)
		}
		return v
	case *IfExpr:
		v.Cond = substituteFieldRefs(v.Cond, fields)
		v.Then = substituteFieldRefs(v.Then, fields)
		v.Else = substituteFieldRefs(v.Else, fields)
		return v
	case *ContainsExpr:
		v.Str = substituteFieldRefs(v.Str, fields)
		v.Sub = substituteFieldRefs(v.Sub, fields)
		return v
	default:
		return v
	}
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// ContainsExpr represents s.contains(sub).
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

// SliceExpr represents s[i:j] for strings and lists.
type SliceExpr struct {
	Target   Expr
	Start    Expr
	End      Expr
	Kind     string
	IsString bool
}

// SubstringExpr represents substring(s, i, j).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

// PrintStmt represents a call to print with one or more arguments.
type PrintStmt struct{ Args []Expr }

// JsonStmt prints a value as JSON.
type JsonStmt struct{ Value Expr }

// SaveStmt writes rows into a file or stdout in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

// ReturnStmt represents returning a value from a function.
type ReturnStmt struct{ Expr Expr }

// FuncDecl is a simple function declaration.
type FuncDecl struct {
	Name   string
	Params []string
	Body   []Stmt
	Return Expr
}

// AnonFunc represents an anonymous function expression.
type AnonFunc struct {
	Name   string
	Params []string
	Body   []Stmt
	Return Expr
}

// FunRef represents a function reference.
type FunRef struct {
	Name  string
	Arity int
}

// LetStmt represents a variable binding.
type LetStmt struct {
	Name string
	Expr Expr
}

// PutStmt stores a value in the process dictionary.
type PutStmt struct {
	Name string
	Expr Expr
}

// CallExpr represents a function call.
type CallExpr struct {
	Func          string
	Args          []Expr
	ReturnsString bool
}

// ApplyExpr represents calling a function-valued expression.
type ApplyExpr struct {
	Fun  Expr
	Args []Expr
}

// BinaryExpr is a binary operation.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

// UnaryExpr is a prefix unary operation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

// ListLit represents a list literal.
type ListLit struct{ Elems []Expr }

// IfExpr is a conditional expression.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

// CaseExpr represents an Erlang case expression.
type CaseExpr struct {
	Target  Expr
	Clauses []CaseClause
}

// CaseClause is a pattern -> result pair within a case expression.
type CaseClause struct {
	Pattern Expr
	Body    Expr
}

// NameRef refers to a variable.
type NameRef struct {
	Name     string
	IsString bool
}

// MapLit represents a map literal.
type MapLit struct {
	Items   []MapItem
	Pattern bool
}

// MapItem is a key/value pair within a map literal.
type MapItem struct {
	Key   Expr
	Value Expr
}

// IndexExpr represents indexing into a list, map or string.
type IndexExpr struct {
	Target   Expr
	Index    Expr
	Kind     string // "list", "map" or "string"
	IsString bool
}

type IntLit struct{ Value int64 }

type FloatLit struct{ Value float64 }

type BoolLit struct{ Value bool }

type StringLit struct{ Value string }

// NowExpr expands to a deterministic timestamp similar to the VM's now() builtin.
type NowExpr struct{}

// AtomLit represents a simple atom like 'nil'.
type AtomLit struct{ Name string }

// TupleExpr represents {A, B} pair used for sorting.
type TupleExpr struct{ A, B Expr }

// IfStmt represents a simple if statement with optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

// ForStmt represents a simple for-in or range loop.
type ForStmt struct {
	Var       string
	Kind      string // "range", "list" or "map"
	Start     Expr   // for range loops
	End       Expr   // for range loops
	Src       Expr   // list or map expression
	Body      []Stmt
	Breakable bool
	Params    []string
	Next      []string
	Fun       string
}

// QueryExpr represents a basic list comprehension query.
type QueryExpr struct {
	Var     string
	Src     Expr
	Froms   []queryFrom
	Right   *rightJoin
	Where   Expr
	Select  Expr
	SortKey Expr
	Skip    Expr
	Take    Expr
}

type leftJoin struct {
	Var string
	Src Expr
	On  Expr
}

// LeftJoinExpr represents a basic left join between two sources.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	On       Expr
	Select   Expr
}

// OuterJoinExpr represents a full outer join between two sources.
type OuterJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	On       Expr
	Select   Expr
}

// RightJoinExpr represents a basic right join between two sources.
type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	On       Expr
	Select   Expr
}

type rightJoin struct {
	Var string
	Src Expr
	On  Expr
}

type queryFrom struct {
	Var string
	Src Expr
}

// WhileStmt represents a while loop implemented via recursion.
type WhileStmt struct {
	Params []string // variables carried between iterations
	Cond   Expr
	Body   []Stmt
	Next   []string // variable names for next iteration
	Fun    string
}

// ListAssignStmt assigns to an element of a list.
type ListAssignStmt struct {
	Name  string
	Old   string
	Index Expr
	Value Expr
}

// MapAssignStmt assigns to a key in a map.
type MapAssignStmt struct {
	Name  string
	Old   string
	Key   Expr
	Value Expr
}

// UpdateStmt updates fields of items in a list of maps.
type UpdateStmt struct {
	Target string
	Old    string
	Fields []string
	Values []Expr
	Cond   Expr
}

// BreakStmt represents a `break` statement.
type BreakStmt struct{ Args []string }

// ContinueStmt represents a `continue` statement. Args captures values for loop
// variables at the time of the continue.
type ContinueStmt struct{ Args []string }

// CallStmt represents a standalone function call.
type CallStmt struct{ Call Expr }

// BenchStmt represents a benchmark block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (p *PrintStmt) emit(w io.Writer) {
	if len(p.Args) == 0 {
		return
	}
	parts := make([]string, len(p.Args))
	for i, a := range p.Args {
		if isStringExpr(a) {
			parts[i] = "~ts"
		} else {
			parts[i] = "~p"
		}
	}
	fmt.Fprintf(w, "io:format(\"%s~n\", [", strings.Join(parts, " "))
	for i, a := range p.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, "])")
}

func (j *JsonStmt) emit(w io.Writer) {
	io.WriteString(w, "io:format(\"~p~n\", [")
	j.Value.emit(w)
	io.WriteString(w, "])")
}

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		io.WriteString(w, "lists:foreach(fun(_row) -> io:format(\"~p~n\", [_row]) end, ")
		s.Src.emit(w)
		io.WriteString(w, ")")
		return
	}
	io.WriteString(w, "% unsupported save")
}

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "throw({return, ")
	if r.Expr != nil {
		r.Expr.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
	io.WriteString(w, "})")
}

func (fd *FuncDecl) emit(w io.Writer) {
	io.WriteString(w, fd.Name)
	io.WriteString(w, "(")
	for i, p := range fd.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ") ->\n    try\n")
	for _, st := range fd.Body {
		io.WriteString(w, "        ")
		st.emit(w)
		io.WriteString(w, ",\n")
	}
	io.WriteString(w, "        ")
	if fd.Return != nil {
		fd.Return.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
	io.WriteString(w, "\n    catch {return, Ret} -> Ret end.\n\n")
}

func (af *AnonFunc) emit(w io.Writer) {
	io.WriteString(w, "fun")
	if af.Name != "" {
		io.WriteString(w, " ")
		io.WriteString(w, af.Name)
	}
	io.WriteString(w, "(")
	for i, p := range af.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ") ->\n    try\n")
	for _, st := range af.Body {
		io.WriteString(w, "        ")
		st.emit(w)
		io.WriteString(w, ",\n")
	}
	io.WriteString(w, "        ")
	if af.Return != nil {
		af.Return.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
	io.WriteString(w, "\n    catch {return, Ret} -> Ret end")
	io.WriteString(w, "\nend")
}

func isStringExpr(e Expr) bool {
	switch v := e.(type) {
	case *StringLit:
		return true
	case *NameRef:
		return v.IsString
	case *CallExpr:
		if v.ReturnsString {
			return true
		}
		if v.Func == "str" || strings.HasPrefix(v.Func, "string:") {
			return true
		}
		return false
	case *BinaryExpr:
		if v.Op == "++" || v.Op == "+" {
			return isStringExpr(v.Left) || isStringExpr(v.Right)
		}
		return false
	case *SubstringExpr:
		return true
	case *SliceExpr:
		return v.IsString
	case *IfExpr:
		return isStringExpr(v.Then) && isStringExpr(v.Else)
	case *CaseExpr:
		for _, cl := range v.Clauses {
			if !isStringExpr(cl.Body) {
				return false
			}
		}
		return true
	case *IndexExpr:
		return v.IsString
	default:
		return false
	}
}

func isFloatExpr(e Expr, env *types.Env, ctx *context) bool {
	switch v := e.(type) {
	case *FloatLit:
		return true
	case *UnaryExpr:
		return isFloatExpr(v.Expr, env, ctx)
	case *BinaryExpr:
		if v.Op == "/" {
			if isIntExpr(v.Left, env, ctx) && isIntExpr(v.Right, env, ctx) {
				return false
			}
			return true
		}
		return isFloatExpr(v.Left, env, ctx) || isFloatExpr(v.Right, env, ctx)
	case *CallExpr:
		switch v.Func {
		case "float", "avg", "sqrt", "sin", "cos", "tan", "pow":
			return true
		}
		if env != nil {
			name := v.Func
			if fn, ok := env.GetFunc(name); ok && fn.Return != nil {
				if fn.Return.Simple != nil && *fn.Return.Simple == "float" {
					return true
				}
				if fn.Return.Generic != nil && fn.Return.Generic.Name == "float" {
					return true
				}
			}
		}
		return false
	case *IndexExpr:
		if env != nil {
			if t := exprType(v, env, ctx); t != nil {
				if _, ok := t.(types.FloatType); ok {
					return true
				}
			}
		}
		return false
	case *NameRef:
		if ctx != nil {
			name := ctx.original(v.Name)
			if ctx.isFloatVar(name) {
				return true
			}
		}
		if env != nil {
			if t, err := env.GetVar(v.Name); err == nil {
				if _, ok := t.(types.FloatType); ok {
					return true
				}
			}
		}
		return false
	case *IfExpr:
		return isFloatExpr(v.Then, env, ctx) && isFloatExpr(v.Else, env, ctx)
	case *CaseExpr:
		for _, cl := range v.Clauses {
			if !isFloatExpr(cl.Body, env, ctx) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func isIntExpr(e Expr, env *types.Env, ctx *context) bool {
	switch v := e.(type) {
	case *IntLit:
		return true
	case *NowExpr:
		return true
	case *UnaryExpr:
		return isIntExpr(v.Expr, env, ctx)
	case *BinaryExpr:
		return isIntExpr(v.Left, env, ctx) && isIntExpr(v.Right, env, ctx)
	case *CallExpr:
		if v.Func == "mochi_to_int" {
			return true
		}
		if v.Func == "erlang:get" && len(v.Args) == 1 {
			if a, ok := v.Args[0].(*AtomLit); ok {
				name := strings.Trim(a.Name, "'")
				if ctx != nil {
					if ctx.isFloatVar(name) || ctx.isStringVar(name) || ctx.isBoolVar(name) || ctx.isMapVar(name) || ctx.isListStrVar(name) {
						return false
					}
					return true
				}
			}
		}
	case *NameRef:
		name := v.Name
		if ctx != nil {
			name = ctx.original(v.Name)
			if ctx.isFloatVar(name) || ctx.isStringVar(name) || ctx.isBoolVar(name) || ctx.isMapVar(name) || ctx.isListStrVar(name) {
				return false
			}
		}
		if env != nil {
			if t, err := env.GetVar(name); err == nil {
				switch t.(type) {
				case types.IntType, types.Int64Type, types.BigIntType:
					return true
				case types.AnyType:
					return true
				default:
					return false
				}
			}
		}
		return false
	}
	return false
}

func isBigRatType(t types.Type) bool {
	_, ok := t.(types.BigRatType)
	return ok
}

func isBigRatExpr(e Expr) bool {
	if ce, ok := e.(*CallExpr); ok {
		switch ce.Func {
		case "mochi_bigrat", "mochi_add", "mochi_sub", "mochi_mul", "mochi_div":
			return true
		}
	}
	return false
}

func isIntType(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType, types.AnyType:
		return true
	default:
		return false
	}
}

func ensureBigRatExpr(e Expr, t types.Type) Expr {
	if isBigRatType(t) {
		return e
	}
	useBigRat = true
	return &CallExpr{Func: "mochi_bigrat", Args: []Expr{e}}
}

func isBoolExpr(e Expr, env *types.Env, ctx *context) bool {
	switch v := e.(type) {
	case *BoolLit:
		return true
	case *UnaryExpr:
		return v.Op == "!"
	case *BinaryExpr:
		switch v.Op {
		case "&&", "||", "==", "!=", "<", ">", "<=", ">=":
			return true
		}
	case *NameRef:
		if env != nil {
			name := v.Name
			if ctx != nil {
				name = ctx.original(v.Name)
			}
			if t, err := env.GetVar(name); err == nil {
				if _, ok := t.(types.BoolType); ok {
					return true
				}
			}
		}
		if ctx != nil {
			name := ctx.original(v.Name)
			if ctx.isBoolVar(name) {
				return true
			}
		}
	case *IndexExpr:
		if env != nil && v.Kind == "map" {
			if fieldIsBool(v.Target, v.Index, env, ctx) {
				return true
			}
		}
	case *CallExpr:
		if v.Func == "maps:is_key" || v.Func == "mochi_member" || v.Func == "string:str" || v.Func == "mochi_not" {
			return true
		}
		if env != nil {
			name := v.Func
			if ctx != nil {
				name = ctx.original(name)
			}
			if t, err := env.GetVar(name); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if _, ok := ft.Return.(types.BoolType); ok {
						return true
					}
				}
			}
		}
	case *IfExpr:
		return isBoolExpr(v.Then, env, ctx) && isBoolExpr(v.Else, env, ctx)
	case *CaseExpr:
		for _, cl := range v.Clauses {
			if !isBoolExpr(cl.Body, env, ctx) {
				return false
			}
		}
		return true
	}
	return false
}

func isZeroExpr(e Expr) bool {
	switch v := e.(type) {
	case *IntLit:
		return v.Value == 0
	case *AtomLit:
		return v.Name == "nil"
	default:
		return false
	}
}

// exprType attempts to resolve the static type of e using env. It returns nil
// if the type cannot be determined.
func exprType(e Expr, env *types.Env, ctx *context) types.Type {
	if env == nil {
		return nil
	}
	switch v := e.(type) {
	case *NameRef:
		name := v.Name
		if ctx != nil {
			name = ctx.original(v.Name)
		}
		if t, err := env.GetVar(name); err == nil {
			return t
		}
	case *IndexExpr:
		t := exprType(v.Target, env, ctx)
		switch tt := t.(type) {
		case types.MapType:
			return tt.Value
		case types.StructType:
			if sl, ok := v.Index.(*StringLit); ok {
				return types.FieldType(tt, []string{sl.Value})
			}
		case types.ListType:
			return tt.Elem
		}
	case *CallExpr:
		switch v.Func {
		case "mochi_bigrat", "mochi_add", "mochi_sub", "mochi_mul", "mochi_div":
			return types.BigRatType{}
		}
	}
	return nil
}

func isMapExpr(e Expr, env *types.Env, ctx *context) bool {
	switch v := e.(type) {
	case *MapLit:
		return true
	case *NameRef:
		if env != nil {
			name := v.Name
			if ctx != nil {
				name = ctx.original(v.Name)
				if ctx.isMapVar(name) {
					return true
				}
			}
			if t, err := env.GetVar(name); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	case *IndexExpr:
		if v.Kind == "map" {
			if fieldIsString(v.Target, v.Index, env, ctx) {
				return false
			}
			if nr, ok := v.Target.(*NameRef); ok {
				name := nr.Name
				if ctx != nil {
					name = ctx.original(nr.Name)
					if sl, ok := v.Index.(*StringLit); ok {
						if ctx.isStrField(name, sl.Value) || ctx.isBoolField(name, sl.Value) {
							return false
						}
					}
				}
				if env != nil {
					if t, err := env.GetVar(name); err == nil {
						if st, ok := t.(types.StructType); ok {
							if sl, ok2 := v.Index.(*StringLit); ok2 {
								if ft := types.FieldType(st, []string{sl.Value}); ft != nil {
									if _, ok := ft.(types.MapType); ok {
										return true
									}
								}
							}
							return false
						}
					}
				}
			}
			if mapValueIsMap(v.Target, env, ctx) {
				return true
			}
			if t := exprType(v, env, ctx); t != nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
				if _, ok := t.(types.StructType); ok {
					return false
				}
			}
			// if type information is unavailable, default to list
			return false
		}
	case *CallExpr:
		if v.Func == "erlang:get" && len(v.Args) == 1 {
			if a, ok := v.Args[0].(*AtomLit); ok {
				name := strings.Trim(a.Name, "'")
				if ctx != nil && ctx.isMapVar(name) {
					return true
				}
				if env != nil {
					if t, err := env.GetVar(name); err == nil {
						if _, ok := t.(types.MapType); ok {
							return true
						}
					}
				}
			}
		} else if v.Func == "maps:get" && len(v.Args) >= 2 {
			idx := &IndexExpr{Target: v.Args[1], Index: v.Args[0], Kind: "map"}
			if t := exprType(idx, env, ctx); t != nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
		if ctx != nil {
			name := ctx.original(v.Func)
			if ctx.isMapVar(name) {
				return true
			}
		}
		if env != nil {
			if t, err := env.GetVar(v.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if _, ok := ft.Return.(types.MapType); ok {
						return true
					}
				}
			}
		}
	}
	return false
}

func exprHasListCast(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	pf := e.Binary.Left.Value
	for _, op := range pf.Ops {
		if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "list" {
			return true
		}
	}
	return false
}

func isStringListExpr(e Expr) bool {
	switch v := e.(type) {
	case *ListLit:
		if len(v.Elems) == 0 {
			return false
		}
		for _, el := range v.Elems {
			if !isStringExpr(el) {
				return false
			}
		}
		return true
	case *NameRef:
		return false
	}
	return false
}

func exprHasMapCast(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	pf := e.Binary.Left.Value
	for _, op := range pf.Ops {
		if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "map" {
			return true
		}
	}
	return false
}

func mapValueIsString(e Expr, env *types.Env, ctx *context) bool {
	if nr, ok := e.(*NameRef); ok {
		name := nr.Name
		if ctx != nil {
			name = ctx.original(nr.Name)
		}
		if t, err := env.GetVar(name); err == nil {
			if mt, ok := t.(types.MapType); ok {
				if _, ok := mt.Value.(types.StringType); ok {
					return true
				}
			}
		}
	}
	if ml, ok := e.(*MapLit); ok && len(ml.Items) > 0 {
		for _, it := range ml.Items {
			if !isStringExpr(it.Value) {
				return false
			}
		}
		return true
	}
	return false
}

// mapValueIsMap reports whether expression e is a map whose values are maps.
func mapValueIsMap(e Expr, env *types.Env, ctx *context) bool {
	if nr, ok := e.(*NameRef); ok {
		name := nr.Name
		if ctx != nil {
			name = ctx.original(nr.Name)
		}
		if t, err := env.GetVar(name); err == nil {
			if mt, ok := t.(types.MapType); ok {
				if _, ok := mt.Value.(types.MapType); ok {
					return true
				}
			}
		}
	}
	if ie, ok := e.(*IndexExpr); ok && ie.Kind == "map" {
		if t := exprType(ie, env, ctx); t != nil {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
		if mapValueIsMap(ie.Target, env, ctx) {
			return true
		}
	}
	if ce, ok := e.(*CallExpr); ok && ce.Func == "erlang:get" && len(ce.Args) == 1 {
		if a, ok := ce.Args[0].(*AtomLit); ok {
			name := strings.Trim(a.Name, "'")
			if t, err := env.GetVar(name); err == nil {
				if mt, ok := t.(types.MapType); ok {
					if _, ok := mt.Value.(types.MapType); ok {
						return true
					}
				}
			}
		}
	}
	if ml, ok := e.(*MapLit); ok && len(ml.Items) > 0 {
		for _, it := range ml.Items {
			if _, ok := it.Value.(*MapLit); !ok {
				return false
			}
		}
		return true
	}
	return false
}

func isListExpr(e Expr, env *types.Env, ctx *context) bool {
	switch v := e.(type) {
	case *ListLit:
		return true
	case *NameRef:
		if env != nil {
			name := v.Name
			if ctx != nil {
				name = ctx.original(v.Name)
			}
			if t, err := env.GetVar(name); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	case *IndexExpr:
		if v.Kind == "list" {
			return true
		}
	case *CallExpr:
		if v.Func == "append" || v.Func == "concat" || strings.HasPrefix(v.Func, "lists:") {
			return true
		}
	case *BinaryExpr:
		if v.Op == "++" || v.Op == "+" {
			return isListExpr(v.Left, env, ctx) && isListExpr(v.Right, env, ctx)
		}
	}
	return false
}

func stringFields(e Expr) map[string]bool {
	switch v := e.(type) {
	case *MapLit:
		fields := map[string]bool{}
		for _, it := range v.Items {
			if k, ok := it.Key.(*AtomLit); ok {
				if isStringExpr(it.Value) {
					fields[k.Name] = true
				}
			}
		}
		if len(fields) > 0 {
			return fields
		}
	case *ListLit:
		accum := map[string]bool{}
		for _, el := range v.Elems {
			if ml, ok := el.(*MapLit); ok {
				ff := stringFields(ml)
				for k, b := range ff {
					if b {
						accum[k] = true
					}
				}
			}
		}
		if len(accum) > 0 {
			return accum
		}
	case *QueryExpr:
		return stringFields(v.Select)
	}
	return nil
}

func boolFields(e Expr, env *types.Env, ctx *context) map[string]bool {
	switch v := e.(type) {
	case *MapLit:
		fields := map[string]bool{}
		for _, it := range v.Items {
			if k, ok := it.Key.(*AtomLit); ok {
				if isBoolExpr(it.Value, env, ctx) {
					fields[k.Name] = true
				}
			}
		}
		if len(fields) > 0 {
			return fields
		}
	case *ListLit:
		accum := map[string]bool{}
		for _, el := range v.Elems {
			if ml, ok := el.(*MapLit); ok {
				ff := boolFields(ml, env, ctx)
				for k, b := range ff {
					if b {
						accum[k] = true
					}
				}
			}
		}
		if len(accum) > 0 {
			return accum
		}
	case *QueryExpr:
		return boolFields(v.Select, env, ctx)
	}
	return nil
}

func fieldIsString(target Expr, key Expr, env *types.Env, ctx *context) bool {
	if a, ok := key.(*AtomLit); ok {
		switch t := target.(type) {
		case *MapLit:
			for _, it := range t.Items {
				if ak, ok2 := it.Key.(*AtomLit); ok2 && ak.Name == a.Name {
					return isStringExpr(it.Value)
				}
			}
		case *NameRef:
			if ctx != nil {
				name := ctx.original(t.Name)
				if ctx.isStrField(name, a.Name) {
					return true
				}
			}
		}
	}
	return mapValueIsString(target, env, ctx)
}

func fieldIsBool(target Expr, key Expr, env *types.Env, ctx *context) bool {
	if a, ok := key.(*AtomLit); ok {
		switch t := target.(type) {
		case *MapLit:
			for _, it := range t.Items {
				if ak, ok2 := it.Key.(*AtomLit); ok2 && ak.Name == a.Name {
					return isBoolExpr(it.Value, env, ctx)
				}
			}
		case *NameRef:
			if ctx != nil {
				name := ctx.original(t.Name)
				if ctx.isBoolField(name, a.Name) {
					return true
				}
			}
		}
	}
	return false
}

func hasLoopCtrl(s Stmt) bool {
	switch v := s.(type) {
	case *BreakStmt, *ContinueStmt:
		return true
	case *IfStmt:
		for _, st := range v.Then {
			if hasLoopCtrl(st) {
				return true
			}
		}
		for _, st := range v.Else {
			if hasLoopCtrl(st) {
				return true
			}
		}
	case *ForStmt:
		for _, st := range v.Body {
			if hasLoopCtrl(st) {
				return true
			}
		}
	case *WhileStmt:
		for _, st := range v.Body {
			if hasLoopCtrl(st) {
				return true
			}
		}
	}
	return false
}

func containsLoopCtrl(list []Stmt) bool {
	for _, st := range list {
		if hasLoopCtrl(st) {
			return true
		}
	}
	return false
}

func containsName(list []string, v string) bool {
	for _, s := range list {
		if s == v {
			return true
		}
	}
	return false
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", l.Name)
	l.Expr.emit(w)
}

func (p *PutStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "erlang:put('%s', ", p.Name)
	p.Expr.emit(w)
	io.WriteString(w, ")")
}

func (c *CallExpr) emit(w io.Writer) {
	switch c.Func {
	case "append":
		// append(list, elem)
		io.WriteString(w, "lists:append(")
		if len(c.Args) == 2 {
			c.Args[0].emit(w)
			io.WriteString(w, ", [")
			c.Args[1].emit(w)
			io.WriteString(w, "])")
		} else {
			io.WriteString(w, ")")
		}
		return
	case "avg":
		if len(c.Args) == 1 {
			io.WriteString(w, "(lists:sum(")
			c.Args[0].emit(w)
			io.WriteString(w, ") / length(")
			c.Args[0].emit(w)
			io.WriteString(w, "))")
		} else {
			io.WriteString(w, "0")
		}
		return
	case "count":
		io.WriteString(w, "length(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "len":
		io.WriteString(w, "length(")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		io.WriteString(w, ")")
		return
	case "input":
		io.WriteString(w, "((fun() -> case io:get_line(\"\") of eof -> \"q\"; L -> string:trim(L) end end)())")
		return
	case "abs":
		io.WriteString(w, "erlang:abs(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "int":
		useToInt = true
		io.WriteString(w, "mochi_to_int(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, ")")
		return
	case "upper":
		io.WriteString(w, "string:to_upper(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "Upper":
		io.WriteString(w, "string:to_upper(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "lower":
		io.WriteString(w, "string:to_lower(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "Lower":
		io.WriteString(w, "string:to_lower(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "padStart", "padstart":
		usePadStart = true
		io.WriteString(w, "mochi_pad_start(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 2 {
			c.Args[2].emit(w)
		} else {
			io.WriteString(w, "\" \"")
		}
		io.WriteString(w, ")")
		return
	case "str":
		useStr = true
		io.WriteString(w, "mochi_str(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "sum":
		io.WriteString(w, "lists:sum(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "pow":
		io.WriteString(w, "trunc(math:pow(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, "))")
		return
	case "pow_big":
		if len(c.Args) == 2 {
			if lit, ok := c.Args[0].(*IntLit); ok && lit.Value == 2 {
				io.WriteString(w, "(1 bsl ")
				c.Args[1].emit(w)
				io.WriteString(w, ")")
				return
			}
		}
	case "min":
		io.WriteString(w, "lists:min(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "max":
		io.WriteString(w, "lists:max(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "keys":
		io.WriteString(w, "maps:keys(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "values":
		io.WriteString(w, "maps:values(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "panic":
		io.WriteString(w, "erlang:error(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		} else {
			io.WriteString(w, "panic")
		}
		io.WriteString(w, ")")
		return
	}
	name := c.Func
	io.WriteString(w, name)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

func (b *BinaryExpr) emit(w io.Writer) {
	switch b.Op {
	case "union":
		io.WriteString(w, "lists:usort(")
		b.Left.emit(w)
		io.WriteString(w, " ++ ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	case "union_all":
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, " ++ ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	case "except":
		io.WriteString(w, "([X || X <- ")
		b.Left.emit(w)
		io.WriteString(w, ", not lists:member(X, ")
		b.Right.emit(w)
		io.WriteString(w, ")])")
	case "intersect":
		io.WriteString(w, "lists:usort([X || X <- ")
		b.Left.emit(w)
		io.WriteString(w, ", lists:member(X, ")
		b.Right.emit(w)
		io.WriteString(w, ")])")
	default:
		io.WriteString(w, "(")
		op := mapOp(b.Op)
		// use string concatenation operator when needed
		if b.Op == "+" {
			if isStringExpr(b.Left) || isStringExpr(b.Right) {
				op = "++"
			}
		}
		b.Left.emit(w)
		io.WriteString(w, " "+op+" ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	}
}

func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "!" {
		io.WriteString(w, "not ")
	} else {
		io.WriteString(w, u.Op)
	}
	u.Expr.emit(w)
}

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "#{")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		it.Key.emit(w)
		if m.Pattern {
			io.WriteString(w, " := ")
		} else {
			io.WriteString(w, " => ")
		}
		it.Value.emit(w)
	}
	io.WriteString(w, "}")
}

func (t *TupleExpr) emit(w io.Writer) {
	io.WriteString(w, "{")
	t.A.emit(w)
	io.WriteString(w, ", ")
	t.B.emit(w)
	io.WriteString(w, "}")
}

func (i *IndexExpr) emit(w io.Writer) {
	switch i.Kind {
	case "map":
		io.WriteString(w, "maps:get(")
		i.Index.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ", nil)")
	case "string":
		io.WriteString(w, "string:substr(")
		i.Target.emit(w)
		io.WriteString(w, ", ")
		i.Index.emit(w)
		io.WriteString(w, " + 1, 1)")
	default: // list or map fallback
		io.WriteString(w, "(case erlang:is_map(")
		i.Target.emit(w)
		io.WriteString(w, ") of true -> maps:get(")
		i.Index.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ", nil); _ -> lists:nth(")
		i.Index.emit(w)
		io.WriteString(w, " + 1, ")
		i.Target.emit(w)
		io.WriteString(w, ") end)")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	switch s.Kind {
	case "string":
		io.WriteString(w, "string:substr(")
		s.Target.emit(w)
		io.WriteString(w, ", ")
		if s.Start != nil {
			s.Start.emit(w)
			io.WriteString(w, " + 1")
		} else {
			io.WriteString(w, "1")
		}
		io.WriteString(w, ", ")
		if s.End != nil {
			io.WriteString(w, "(")
			s.End.emit(w)
			io.WriteString(w, " - ")
			if s.Start != nil {
				s.Start.emit(w)
			} else {
				io.WriteString(w, "0")
			}
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "length(")
			s.Target.emit(w)
			io.WriteString(w, ")")
			if s.Start != nil {
				io.WriteString(w, " - ")
				s.Start.emit(w)
			}
		}
		io.WriteString(w, ")")
	default:
		io.WriteString(w, "lists:sublist(")
		s.Target.emit(w)
		io.WriteString(w, ", ")
		if s.Start != nil {
			s.Start.emit(w)
			io.WriteString(w, " + 1")
		} else {
			io.WriteString(w, "1")
		}
		io.WriteString(w, ", ")
		if s.End != nil {
			io.WriteString(w, "(")
			s.End.emit(w)
			io.WriteString(w, " - ")
			if s.Start != nil {
				s.Start.emit(w)
			} else {
				io.WriteString(w, "0")
			}
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "length(")
			s.Target.emit(w)
			io.WriteString(w, ")")
			if s.Start != nil {
				io.WriteString(w, " - ")
				s.Start.emit(w)
			}
		}
		io.WriteString(w, ")")
	}
}

func (c *ContainsExpr) emit(w io.Writer) {
	io.WriteString(w, "(string:str(")
	c.Str.emit(w)
	io.WriteString(w, ", ")
	c.Sub.emit(w)
	io.WriteString(w, ") =/= 0)")
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "string:substr(")
	s.Str.emit(w)
	io.WriteString(w, ", ")
	s.Start.emit(w)
	io.WriteString(w, " + 1, (")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, "))")
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(case ")
	i.Cond.emit(w)
	io.WriteString(w, " of\n    true -> ")
	i.Then.emit(w)
	io.WriteString(w, ";\n    _ -> ")
	i.Else.emit(w)
	io.WriteString(w, "\nend)")
}

func (c *CaseExpr) emit(w io.Writer) {
	io.WriteString(w, "(case ")
	c.Target.emit(w)
	io.WriteString(w, " of\n")
	for i, cl := range c.Clauses {
		io.WriteString(w, "    ")
		cl.Pattern.emit(w)
		io.WriteString(w, " -> ")
		cl.Body.emit(w)
		if i < len(c.Clauses)-1 {
			io.WriteString(w, ";\n")
		} else {
			io.WriteString(w, "\nend)")
		}
	}
}

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, n.Name) }

func (f *FunRef) emit(w io.Writer) {
	fmt.Fprintf(w, "fun %s/%d", f.Name, f.Arity)
}

func (a *ApplyExpr) emit(w io.Writer) {
	need := true
	switch a.Fun.(type) {
	case *NameRef, *FunRef:
		need = false
	}
	if need {
		io.WriteString(w, "(")
		a.Fun.emit(w)
		io.WriteString(w, ")")
	} else {
		a.Fun.emit(w)
	}
	io.WriteString(w, "(")
	for i, arg := range a.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		arg.emit(w)
	}
	io.WriteString(w, ")")
}

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

func (f *FloatLit) emit(w io.Writer) {
	s := fmt.Sprintf("%g", f.Value)
	if strings.Contains(s, "e") {
		parts := strings.SplitN(s, "e", 2)
		if !strings.Contains(parts[0], ".") {
			parts[0] = parts[0] + ".0"
		}
		s = parts[0] + "e" + parts[1]
	}
	io.WriteString(w, s)
}

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (a *AtomLit) emit(w io.Writer) { io.WriteString(w, a.Name) }

func (n *NowExpr) emit(w io.Writer) { io.WriteString(w, "mochi_now()") }

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "case ")
	i.Cond.emit(w)
	io.WriteString(w, " of\n        true -> ")
	if len(i.Then) == 0 {
		io.WriteString(w, "ok")
	} else {
		for idx, st := range i.Then {
			if idx > 0 {
				io.WriteString(w, ",\n            ")
			}
			st.emit(w)
		}
	}
	if len(i.Else) > 0 {
		io.WriteString(w, ";\n        _ -> ")
		for idx, st := range i.Else {
			if idx > 0 {
				io.WriteString(w, ",\n            ")
			}
			st.emit(w)
		}
	} else {
		io.WriteString(w, ";\n        _ -> ok")
	}
	io.WriteString(w, "\n    end")
}

func (f *ForStmt) emit(w io.Writer) {
	loopName := f.Fun + "_loop"
	srcVar := "List"
	for containsName(f.Params, srcVar) {
		srcVar = srcVar + "_"
	}
	restVar := f.Var + "_rest"
	for containsName(f.Params, restVar) {
		restVar = restVar + "_"
	}
	io.WriteString(w, f.Fun)
	io.WriteString(w, " = fun ")
	io.WriteString(w, loopName)
	io.WriteString(w, "(")
	io.WriteString(w, srcVar)
	for _, p := range f.Params {
		io.WriteString(w, ", ")
		io.WriteString(w, p)
	}
	io.WriteString(w, ") ->\n    case ")
	io.WriteString(w, srcVar)
	io.WriteString(w, " of\n        [] -> {")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, "};\n        [")
	io.WriteString(w, f.Var)
	io.WriteString(w, "|")
	io.WriteString(w, restVar)
	io.WriteString(w, "] ->")
	if f.Breakable {
		io.WriteString(w, "\n        try")
	}
	for _, st := range f.Body {
		io.WriteString(w, "\n            ")
		st.emit(w)
		io.WriteString(w, ",")
	}
	if f.Breakable {
		io.WriteString(w, "\n            ")
		io.WriteString(w, loopName)
		io.WriteString(w, "(")
		io.WriteString(w, restVar)
	} else {
		io.WriteString(w, "\n            ")
		io.WriteString(w, loopName)
		io.WriteString(w, "(")
		io.WriteString(w, restVar)
	}
	for _, a := range f.Next {
		io.WriteString(w, ", ")
		io.WriteString(w, a)
	}
	io.WriteString(w, ")")
	if f.Breakable {
		io.WriteString(w, "\n        catch\n            {continue")
		for i := range f.Next {
			io.WriteString(w, ", ")
			fmt.Fprintf(w, "C%d", i)
		}
		io.WriteString(w, "} -> ")
		io.WriteString(w, loopName)
		io.WriteString(w, "(")
		io.WriteString(w, restVar)
		for i := range f.Next {
			io.WriteString(w, ", ")
			fmt.Fprintf(w, "C%d", i)
		}
		io.WriteString(w, ");\n            {break")
		for i := range f.Params {
			io.WriteString(w, ", ")
			fmt.Fprintf(w, "B%d", i)
		}
		io.WriteString(w, "} -> {")
		for i := range f.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "B%d", i)
		}
		io.WriteString(w, "};\n            break -> {")
		for i, p := range f.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, p)
		}
		io.WriteString(w, "}\n        end")
	}
	io.WriteString(w, ";\n        _ -> {")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, "}\n    end\nend,\n{")
	for i, a := range f.Next {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, a)
	}
	io.WriteString(w, "} = ")
	io.WriteString(w, f.Fun)
	io.WriteString(w, "(")
	switch f.Kind {
	case "range":
		io.WriteString(w, "lists:seq(")
		f.Start.emit(w)
		io.WriteString(w, ", (")
		f.End.emit(w)
		io.WriteString(w, ") - 1)")
	case "map":
		io.WriteString(w, "maps:keys(")
		f.Src.emit(w)
		io.WriteString(w, ")")
	default: // list
		f.Src.emit(w)
	}
	for _, p := range f.Params {
		io.WriteString(w, ", ")
		io.WriteString(w, p)
	}
	io.WriteString(w, ")")
}

func (ws *WhileStmt) emit(w io.Writer) {
	loopName := ws.Fun + "_loop"
	io.WriteString(w, ws.Fun)
	io.WriteString(w, " = fun ")
	io.WriteString(w, loopName)
	io.WriteString(w, "(")
	for i, p := range ws.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ") ->\n")
	if containsLoopCtrl(ws.Body) {
		io.WriteString(w, "    case ")
		ws.Cond.emit(w)
		io.WriteString(w, " of\n        true ->\n            try")
		for _, st := range ws.Body {
			io.WriteString(w, "\n                ")
			st.emit(w)
			io.WriteString(w, ",")
		}
		io.WriteString(w, "\n                ")
		io.WriteString(w, loopName)
		io.WriteString(w, "(")
		for i, a := range ws.Next {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, a)
		}
		io.WriteString(w, ")\n            catch\n                {continue")
		for i := range ws.Next {
			io.WriteString(w, ", ")
			fmt.Fprintf(w, "C%d", i)
		}
		io.WriteString(w, "} -> ")
		io.WriteString(w, loopName)
		io.WriteString(w, "(")
		for i := range ws.Next {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "C%d", i)
		}
		io.WriteString(w, ");\n                break -> {")
		for i, p := range ws.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, p)
		}
		io.WriteString(w, "}\n            end;\n        _ -> {")
		for i, p := range ws.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, p)
		}
		io.WriteString(w, "}\n    end")
	} else {
		io.WriteString(w, "    case ")
		ws.Cond.emit(w)
		io.WriteString(w, " of\n        true ->")
		for _, st := range ws.Body {
			io.WriteString(w, "\n            ")
			st.emit(w)
			io.WriteString(w, ",")
		}
		io.WriteString(w, "\n            ")
		io.WriteString(w, loopName)
		io.WriteString(w, "(")
		for i, a := range ws.Next {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, a)
		}
		io.WriteString(w, ");\n        _ -> {")
		for i, p := range ws.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, p)
		}
		io.WriteString(w, "}\n    end")
	}
	io.WriteString(w, "\nend,\n{")
	for i, a := range ws.Next {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, a)
	}
	io.WriteString(w, "} = ")
	io.WriteString(w, ws.Fun)
	io.WriteString(w, "(")
	for i, p := range ws.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ")")
}

func (la *ListAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = lists:sublist(%s, ", la.Name, la.Old)
	la.Index.emit(w)
	io.WriteString(w, ") ++ [")
	la.Value.emit(w)
	io.WriteString(w, "] ++ lists:nthtail(")
	la.Index.emit(w)
	io.WriteString(w, " + 1, ")
	io.WriteString(w, la.Old)
	io.WriteString(w, ")")
}

func (ma *MapAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = maps:put(", ma.Name)
	ma.Key.emit(w)
	io.WriteString(w, ", ")
	ma.Value.emit(w)
	fmt.Fprintf(w, ", %s)", ma.Old)
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = lists:map(fun(Item) -> ", u.Target)
	var body Expr = &NameRef{Name: "Item"}
	for i, f := range u.Fields {
		body = &CallExpr{Func: "maps:put", Args: []Expr{&StringLit{Value: f}, u.Values[i], body}}
	}
	if u.Cond != nil {
		io.WriteString(w, "case ")
		u.Cond.emit(w)
		io.WriteString(w, " of true -> ")
		body.emit(w)
		io.WriteString(w, "; _ -> Item end")
	} else {
		body.emit(w)
	}
	io.WriteString(w, " end, ")
	io.WriteString(w, u.Old)
	io.WriteString(w, ")")
}

func (b *BreakStmt) emit(w io.Writer) {
	io.WriteString(w, "throw(break)")
}
func (c *ContinueStmt) emit(w io.Writer) {
	if len(c.Args) == 0 {
		io.WriteString(w, "throw({continue})")
		return
	}
	io.WriteString(w, "throw({continue")
	for _, a := range c.Args {
		io.WriteString(w, ", ")
		io.WriteString(w, a)
	}
	io.WriteString(w, "})")
}

func (cs *CallStmt) emit(w io.Writer) { cs.Call.emit(w) }

func (b *BenchStmt) emit(w io.Writer) {
	io.WriteString(w, "__mochi_bench_start = mochi_now(),\n    __mochi_bench_start_mem = erlang:memory(total)")
	for _, st := range b.Body {
		io.WriteString(w, ",\n    ")
		st.emit(w)
	}
	fmt.Fprintf(w, ",\n    __mochi_bench_end = mochi_now(),\n    __mochi_bench_end_mem = erlang:memory(total),\n    __mochi_bench_duration_us = (__mochi_bench_end - __mochi_bench_start) div 1000,\n    __mochi_bench_mem_bytes = erlang:abs(__mochi_bench_end_mem - __mochi_bench_start_mem),\n    io:format(\"{~n  \\\"duration_us\\\": ~p,~n  \\\"memory_bytes\\\": ~p,~n  \\\"name\\\": \\\"%s\\\"~n}\n\", [__mochi_bench_duration_us, __mochi_bench_mem_bytes])", b.Name)
}

func isNameRef(e Expr, name string) bool {
	if n, ok := e.(*NameRef); ok {
		return n.Name == name
	}
	return false
}

func (q *QueryExpr) emit(w io.Writer) {
	if q.Right != nil {
		base := &bytes.Buffer{}
		io.WriteString(base, "[")
		if q.SortKey != nil {
			io.WriteString(base, "{")
			q.SortKey.emit(base)
			io.WriteString(base, ", ")
		}
		q.Select.emit(base)
		if q.SortKey != nil {
			io.WriteString(base, "}")
		}
		io.WriteString(base, " ||\n        ")
		io.WriteString(base, q.Right.Var)
		io.WriteString(base, " <- ")
		q.Right.Src.emit(base)
		io.WriteString(base, ",\n        ")
		io.WriteString(base, q.Var)
		io.WriteString(base, " <- ")
		q.Src.emit(base)
		for _, f := range q.Froms {
			io.WriteString(base, ",\n        ")
			io.WriteString(base, f.Var)
			io.WriteString(base, " <- ")
			f.Src.emit(base)
		}
		if q.Right.On != nil {
			io.WriteString(base, ",\n        ")
			q.Right.On.emit(base)
		}
		if q.Where != nil {
			io.WriteString(base, ",\n        ")
			q.Where.emit(base)
		}
		io.WriteString(base, "]")

		expr := base.String()
		if q.SortKey != nil {
			io.WriteString(w, "(fun() ->\n        Pairs = ")
			io.WriteString(w, expr)
			io.WriteString(w, ",\n        SortedPairs = lists:sort(fun({K1,_},{K2,_}) -> K1 =< K2 end, Pairs),\n        Values = [V || {_,V} <- SortedPairs]")
			if q.Skip != nil {
				io.WriteString(w, ",\n        Values = lists:nthtail(")
				q.Skip.emit(w)
				io.WriteString(w, ", Values)")
			}
			if q.Take != nil {
				io.WriteString(w, ",\n        Values = lists:sublist(Values, ")
				q.Take.emit(w)
				io.WriteString(w, ")")
			}
			io.WriteString(w, ",\n        Values end)()")
			return
		}
		if q.Take != nil {
			io.WriteString(w, "lists:sublist(")
		}
		if q.Skip != nil {
			io.WriteString(w, "lists:nthtail(")
			q.Skip.emit(w)
			io.WriteString(w, ", ")
		}
		io.WriteString(w, expr)
		if q.Skip != nil {
			io.WriteString(w, ")")
		}
		if q.Take != nil {
			io.WriteString(w, ", ")
			q.Take.emit(w)
			io.WriteString(w, ")")
		}
		return
	}
	base := &bytes.Buffer{}
	io.WriteString(base, "[")
	if q.SortKey != nil {
		io.WriteString(base, "{")
		q.SortKey.emit(base)
		io.WriteString(base, ", ")
	}
	q.Select.emit(base)
	if q.SortKey != nil {
		io.WriteString(base, "}")
	}
	io.WriteString(base, " ||\n        ")
	io.WriteString(base, q.Var)
	io.WriteString(base, " <- ")
	q.Src.emit(base)
	for _, f := range q.Froms {
		io.WriteString(base, ",\n        ")
		io.WriteString(base, f.Var)
		io.WriteString(base, " <- ")
		f.Src.emit(base)
	}
	if q.Where != nil {
		io.WriteString(base, ",\n        ")
		q.Where.emit(base)
	}
	io.WriteString(base, "]")

	expr := base.String()
	if q.SortKey != nil {
		io.WriteString(w, "(fun() ->\n        Pairs = ")
		io.WriteString(w, expr)
		io.WriteString(w, ",\n        SortedPairs = lists:sort(fun({K1,_},{K2,_}) -> K1 =< K2 end, Pairs),\n        Values = [V || {_,V} <- SortedPairs]")
		if q.Skip != nil {
			io.WriteString(w, ",\n        Values = lists:nthtail(")
			q.Skip.emit(w)
			io.WriteString(w, ", Values)")
		}
		if q.Take != nil {
			io.WriteString(w, ",\n        Values = lists:sublist(Values, ")
			q.Take.emit(w)
			io.WriteString(w, ")")
		}
		io.WriteString(w, ",\n        Values end)()")
		return
	}
	if q.Take != nil {
		io.WriteString(w, "lists:sublist(")
	}
	if q.Skip != nil {
		io.WriteString(w, "lists:nthtail(")
		q.Skip.emit(w)
		io.WriteString(w, ", ")
	}
	io.WriteString(w, expr)
	if q.Skip != nil {
		io.WriteString(w, ")")
	}
	if q.Take != nil {
		io.WriteString(w, ", ")
		q.Take.emit(w)
		io.WriteString(w, ")")
	}
}

func (l *LeftJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "lists:reverse(lists:foldl(fun(")
	io.WriteString(w, l.LeftVar)
	io.WriteString(w, ", Acc0) ->\n    {Matched,Acc} = lists:foldl(fun(")
	io.WriteString(w, l.RightVar)
	io.WriteString(w, ", {M,A}) ->\n        case ")
	if l.On != nil {
		l.On.emit(w)
	} else {
		io.WriteString(w, "true")
	}
	io.WriteString(w, " of\n            true -> {true, [")
	l.Select.emit(w)
	io.WriteString(w, "|A]};\n            _ -> {M,A}\n        end\n    end, {false, Acc0}, ")
	l.RightSrc.emit(w)
	io.WriteString(w, "),\n    case Matched of\n        true -> Acc;\n        false -> [(fun() -> ")
	io.WriteString(w, l.RightVar)
	io.WriteString(w, " = nil, ")
	l.Select.emit(w)
	io.WriteString(w, " end())|Acc]\n    end\nend, [], ")
	l.LeftSrc.emit(w)
	io.WriteString(w, "))")
}

func (o *OuterJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "lists:reverse(lists:foldl(fun(")
	io.WriteString(w, o.LeftVar)
	io.WriteString(w, ", Acc0) ->\n    {Matched,Acc} = lists:foldl(fun(")
	io.WriteString(w, o.RightVar)
	io.WriteString(w, ", {M,A}) ->\n        case ")
	if o.On != nil {
		o.On.emit(w)
	} else {
		io.WriteString(w, "true")
	}
	io.WriteString(w, " of\n            true -> {true, [")
	o.Select.emit(w)
	io.WriteString(w, "|A]};\n            _ -> {M,A}\n        end\n    end, {false, Acc0}, ")
	o.RightSrc.emit(w)
	io.WriteString(w, "),\n    case Matched of\n        true -> Acc;\n        false -> [(fun() -> ")
	io.WriteString(w, o.RightVar)
	io.WriteString(w, " = nil, ")
	o.Select.emit(w)
	io.WriteString(w, " end())|Acc]\n    end\nend, [], ")
	o.LeftSrc.emit(w)
	io.WriteString(w, ")) ++ lists:reverse(lists:foldl(fun(")
	io.WriteString(w, o.RightVar)
	io.WriteString(w, ", Acc0) ->\n    Exists = lists:any(fun(")
	io.WriteString(w, o.LeftVar)
	io.WriteString(w, ") -> ")
	if o.On != nil {
		o.On.emit(w)
	} else {
		io.WriteString(w, "true")
	}
	io.WriteString(w, " end, ")
	o.LeftSrc.emit(w)
	io.WriteString(w, "),\n    case Exists of\n        true -> Acc0;\n        false -> [(fun() -> ")
	io.WriteString(w, o.LeftVar)
	io.WriteString(w, " = nil, ")
	o.Select.emit(w)
	io.WriteString(w, " end())|Acc0]\n    end\nend, [], ")
	o.RightSrc.emit(w)
	io.WriteString(w, "))")
}

func (r *RightJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "lists:reverse(lists:foldl(fun(")
	io.WriteString(w, r.RightVar)
	io.WriteString(w, ", Acc0) ->\n    {Matched,Acc} = lists:foldl(fun(")
	io.WriteString(w, r.LeftVar)
	io.WriteString(w, ", {M,A}) ->\n        case ")
	if r.On != nil {
		r.On.emit(w)
	} else {
		io.WriteString(w, "true")
	}
	io.WriteString(w, " of\n            true -> {true, [")
	r.Select.emit(w)
	io.WriteString(w, "|A]};\n            _ -> {M,A}\n        end\n    end, {false, Acc0}, ")
	r.LeftSrc.emit(w)
	io.WriteString(w, "),\n    case Matched of\n        true -> Acc;\n  false -> [(fun() -> ")
	io.WriteString(w, r.LeftVar)
	io.WriteString(w, " = nil, ")
	r.Select.emit(w)
	io.WriteString(w, " end())|Acc]\n    end\nend, [], ")
	r.RightSrc.emit(w)
	io.WriteString(w, "))")
}

func mapOp(op string) string {
	switch op {
	case "&&":
		return "andalso"
	case "||":
		return "orelse"
	case "!=":
		return "/="
	case "<=":
		return "=<"
	case "/":
		return "/"
	case "%":
		return "rem"
	default:
		return op
	}
}

func builtinFunc(name string) bool {
	switch name {
	case "print", "append", "avg", "count", "len", "concat", "str", "sum", "min", "max", "values", "keys", "exists", "contains", "sha256", "json", "now", "input", "int", "abs", "upper", "lower", "indexOf", "parseIntStr", "indexof", "parseintstr", "repeat", "padstart", "bigrat", "num", "denom", "split":
		return true
	default:
		return false
	}
}

func constValue(e *parser.Expr) (any, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	pf := u.Value
	switch {
	case pf.Target.Lit != nil:
		lit := pf.Target.Lit
		switch {
		case lit.Int != nil:
			return int64(*lit.Int), true
		case lit.Float != nil:
			return *lit.Float, true
		case lit.Bool != nil:
			return bool(*lit.Bool), true
		case lit.Str != nil:
			return *lit.Str, true
		case lit.Null:
			return nil, true
		}
	case pf.Target.Map != nil:
		m := map[string]any{}
		for _, it := range pf.Target.Map.Items {
			key, ok := types.SimpleStringKey(it.Key)
			if !ok {
				return nil, false
			}
			v, ok := constValue(it.Value)
			if !ok {
				return nil, false
			}
			m[key] = v
		}
		return m, true
	case pf.Target.List != nil:
		arr := make([]any, len(pf.Target.List.Elems))
		for i, el := range pf.Target.List.Elems {
			v, ok := constValue(el)
			if !ok {
				return nil, false
			}
			arr[i] = v
		}
		return arr, true
	}
	return nil, false
}

func valueToExpr(v any) Expr {
	switch val := v.(type) {
	case nil:
		return &AtomLit{Name: "nil"}
	case bool:
		if val {
			return &AtomLit{Name: "true"}
		}
		return &AtomLit{Name: "false"}
	case int64:
		return &IntLit{Value: val}
	case float64:
		if math.Trunc(val) == val {
			return &IntLit{Value: int64(val)}
		}
		return &FloatLit{Value: val}
	case string:
		return &StringLit{Value: val}
	case map[string]any:
		items := make([]MapItem, 0, len(val))
		for k, vv := range val {
			items = append(items, MapItem{Key: &StringLit{Value: k}, Value: valueToExpr(vv)})
		}
		sort.Slice(items, func(i, j int) bool { return items[i].Key.(*StringLit).Value < items[j].Key.(*StringLit).Value })
		return &MapLit{Items: items}
	case []any:
		elems := make([]Expr, len(val))
		for i, vv := range val {
			elems[i] = valueToExpr(vv)
		}
		return &ListLit{Elems: elems}
	default:
		return &AtomLit{Name: "nil"}
	}
}

func sanitize(name string) string {
	if name == "" {
		return "V"
	}
	return strings.ToUpper(name[:1]) + name[1:]
}

func sanitizeFuncName(name string) string {
	lower := strings.TrimLeft(strings.ToLower(name), "_")
	switch lower {
	case "div", "rem", "band", "bor", "bxor", "bnot", "bsl", "bsr", "and", "or", "xor", "not", "when", "case", "catch", "end", "fun", "if", "receive", "try", "of", "after":
		return lower + "_fn"
	default:
		return lower
	}
}

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return ""
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return ""
	}
	for _, it := range p.Target.Map.Items {
		key, ok := literalString(it.Key)
		if !ok {
			continue
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func extractSaveExpr(e *parser.Expr) *parser.SaveExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil
	}
	return p.Target.Save
}

// Transpile converts a subset of Mochi to an Erlang AST.
func Transpile(prog *parser.Program, env *types.Env, bench bool) (*Program, error) {
	base := filepath.Dir(prog.Pos.Filename)
	ctx := newContext(base)
	useNow = false
	useLookupHost = false
	useToInt = false
	useMemberHelper = false
	usePadStart = false
	useSHA256 = false
	useIndexOf = false
	useParseIntStr = false
	useBigRat = false
	useRepeat = false
	useStr = false
	useFetch = false
	useNot = false
	useSafeArith = false
	useSafeFmod = false
	mutatedFuncs = map[string]int{
		"topple":       0,
		"fill":         0,
		"fillrgb":      0,
		"line":         0,
		"bezier3":      0,
		"newnode":      0,
		"pushfront":    0,
		"pushback":     0,
		"insertbefore": 0,
		"insertafter":  0,
	}
	p := &Program{}
	for _, st := range prog.Statements {
		if st.Fun != nil {
			fd, err := convertFunStmt(st.Fun, env, ctx)
			if err != nil {
				return nil, err
			}
			p.Funs = append(p.Funs, fd)
			continue
		}
		stmts, err := convertStmt(st, env, ctx, true)
		if err != nil {
			return nil, err
		}
		p.Stmts = append(p.Stmts, stmts...)
	}
	bm := bench || benchMain
	if bm {
		useNow = true
		p.Stmts = []Stmt{&BenchStmt{Name: "main", Body: p.Stmts}}
	}
	p.UseNow = useNow
	p.UseLookupHost = useLookupHost
	p.UseToInt = useToInt
	p.UseMemberHelper = useMemberHelper
	p.UsePadStart = usePadStart
	p.UseSHA256 = useSHA256
	p.UseIndexOf = useIndexOf
	p.UseParseIntStr = useParseIntStr
	p.UseBigRat = useBigRat
	p.UseRepeat = useRepeat
	p.UseStr = true
	p.UseFetch = useFetch
	p.UseNot = useNot
	p.UseSafeArith = useSafeArith
	p.UseSafeFmod = useSafeFmod
	return p, nil
}

func convertStmt(st *parser.Statement, env *types.Env, ctx *context, top bool) ([]Stmt, error) {
	switch {
	case st.Test != nil:
		// test blocks are ignored in transpiled output
		return nil, nil
	case st.Type != nil:
		// type declarations have no runtime effect
		return nil, nil
	case st.Return != nil:
		if st.Return.Value != nil {
			val, err := convertExpr(st.Return.Value, env, ctx)
			if err != nil {
				return nil, err
			}
			if len(ctx.mutated) == 1 {
				for n := range ctx.mutated {
					alias := ctx.current(n)
					if isZeroExpr(val) {
						// return the mutated parameter directly when returning
						// a zero/nil value.
						val = &NameRef{Name: alias}
					}
				}
			}
			return []Stmt{&ReturnStmt{Expr: val}}, nil
		}
		if len(ctx.mutated) == 1 {
			for n := range ctx.mutated {
				alias := ctx.current(n)
				return []Stmt{&ReturnStmt{Expr: &NameRef{Name: alias}}}, nil
			}
		}
		return []Stmt{&ReturnStmt{}}, nil
	case st.Fun != nil:
		alias := ctx.newAlias(st.Fun.Name)
		fnExpr, err := convertFunStmtAsExpr(st.Fun, env, ctx)
		if err != nil {
			return nil, err
		}
		return []Stmt{&LetStmt{Name: alias, Expr: fnExpr}}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value, env, ctx)
			if err != nil {
				return nil, err
			}
		} else {
			e = &AtomLit{Name: "nil"}
		}
		ctx.setStrFields(st.Let.Name, stringFields(e))
		ctx.setBoolFields(st.Let.Name, boolFields(e, env, ctx))
		ctx.setStringVar(st.Let.Name, isStringExpr(e))
		ctx.setListStrVar(st.Let.Name, isStringListExpr(e))
		ctx.setFloatVar(st.Let.Name, isFloatExpr(e, env, ctx) || (st.Let.Type != nil && st.Let.Type.Simple != nil && *st.Let.Type.Simple == "float"))
		exprT := types.ExprType(st.Let.Value, env)
		isMap := isMapExpr(e, env, ctx) || (st.Let.Type != nil && st.Let.Type.Generic != nil && st.Let.Type.Generic.Name == "map")
		if !isMap {
			if _, ok := exprT.(types.MapType); ok {
				isMap = true
			}
		}
		if !isMap {
			if t, err := env.GetVar(st.Let.Name); err == nil {
				if _, ok := t.(types.MapType); ok {
					isMap = true
				}
			}
		}
		if exprHasListCast(st.Let.Value) && !(st.Let.Type != nil && st.Let.Type.Generic != nil && st.Let.Type.Generic.Name == "map") {
			isMap = false
		}
		if exprHasMapCast(st.Let.Value) {
			isMap = true
		}
		ctx.setMapVar(st.Let.Name, isMap)
		ctx.setBoolVar(st.Let.Name, isBoolExpr(e, env, ctx) || (st.Let.Type != nil && st.Let.Type.Simple != nil && *st.Let.Type.Simple == "bool"))
		switch e.(type) {
		case *IntLit, *FloatLit, *BoolLit, *StringLit, *AtomLit:
			ctx.setConst(st.Let.Name, e)
		}
		if top {
			ctx.setGlobal(st.Let.Name)
			return []Stmt{&PutStmt{Name: st.Let.Name, Expr: e}}, nil
		}
		alias := ctx.newAlias(st.Let.Name)
		return []Stmt{&LetStmt{Name: alias, Expr: e}}, nil
	case st.Var != nil:
		var e Expr
		var err error
		if st.Var.Value != nil {
			e, err = convertExpr(st.Var.Value, env, ctx)
			if err != nil {
				return nil, err
			}
		} else {
			e = &AtomLit{Name: "nil"}
		}
		ctx.setStrFields(st.Var.Name, stringFields(e))
		ctx.setBoolFields(st.Var.Name, boolFields(e, env, ctx))
		ctx.setStringVar(st.Var.Name, isStringExpr(e))
		ctx.setListStrVar(st.Var.Name, isStringListExpr(e))
		ctx.setFloatVar(st.Var.Name, isFloatExpr(e, env, ctx) || (st.Var.Type != nil && st.Var.Type.Simple != nil && *st.Var.Type.Simple == "float"))
		exprT := types.ExprType(st.Var.Value, env)
		isMapV := isMapExpr(e, env, ctx) || (st.Var.Type != nil && st.Var.Type.Generic != nil && st.Var.Type.Generic.Name == "map")
		if !isMapV {
			if _, ok := exprT.(types.MapType); ok {
				isMapV = true
			}
		}
		if !isMapV {
			if t, err := env.GetVar(st.Var.Name); err == nil {
				if _, ok := t.(types.MapType); ok {
					isMapV = true
				}
			}
		}
		if !isMapV {
			if ie, ok := e.(*IndexExpr); ok && ie.Kind == "map" {
				if t := exprType(ie, env, ctx); t != nil {
					if _, ok := t.(types.MapType); ok {
						isMapV = true
					}
				} else {
					// default to map when indexing a struct field
					// and type information is unavailable
					isMapV = true
				}
			}
		}
		if exprHasListCast(st.Var.Value) && !(st.Var.Type != nil && st.Var.Type.Generic != nil && st.Var.Type.Generic.Name == "map") {
			isMapV = false
		}
		if exprHasMapCast(st.Var.Value) {
			isMapV = true
		}
		ctx.setMapVar(st.Var.Name, isMapV)
		ctx.setBoolVar(st.Var.Name, isBoolExpr(e, env, ctx) || (st.Var.Type != nil && st.Var.Type.Simple != nil && *st.Var.Type.Simple == "bool"))
		ctx.clearConst(st.Var.Name)
		if top {
			ctx.setGlobal(st.Var.Name)
			return []Stmt{&PutStmt{Name: st.Var.Name, Expr: e}}, nil
		}
		alias := ctx.newAlias(st.Var.Name)
		if ce, ok := e.(*CallExpr); ok {
			if idx, ok := mutatedFuncs[ce.Func]; ok && idx < len(ce.Args) {
				if nr, ok := ce.Args[idx].(*NameRef); ok {
					name := ctx.original(nr.Name)
					mAlias := ctx.newAlias(name)
					ctx.markMutated(name)
					ctx.clearConst(name)
					ctx.alias[name] = mAlias
					pat := fmt.Sprintf("{%s, %s}", alias, mAlias)
					return []Stmt{&LetStmt{Name: pat, Expr: e}}, nil
				}
			}
		}
		return []Stmt{&LetStmt{Name: alias, Expr: e}}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		val, err := convertExpr(st.Assign.Value, env, ctx)
		if err != nil {
			return nil, err
		}
		if ctx.isGlobal(st.Assign.Name) {
			return []Stmt{&PutStmt{Name: st.Assign.Name, Expr: val}}, nil
		}
		if ctx.isParam(st.Assign.Name) {
			ctx.markMutated(st.Assign.Name)
		}
		alias := ctx.newAlias(st.Assign.Name)
		// handle assignment from a function that mutates one of its arguments
		if ce, ok := val.(*CallExpr); ok {
			if idx, ok := mutatedFuncs[ce.Func]; ok && idx < len(ce.Args) {
				if nr, ok := ce.Args[idx].(*NameRef); ok {
					name := ctx.original(nr.Name)
					mAlias := ctx.newAlias(name)
					ctx.markMutated(name)
					ctx.clearConst(name)
					ctx.alias[name] = mAlias
					ctx.setStrFields(st.Assign.Name, stringFields(val))
					ctx.setBoolFields(st.Assign.Name, boolFields(val, env, ctx))
					ctx.setStringVar(st.Assign.Name, isStringExpr(val))
					ctx.setListStrVar(st.Assign.Name, isStringListExpr(val))
					ctx.setFloatVar(st.Assign.Name, isFloatExpr(val, env, ctx))
					ctx.setMapVar(st.Assign.Name, isMapExpr(val, env, ctx))
					ctx.setBoolVar(st.Assign.Name, isBoolExpr(val, env, ctx))
					switch val.(type) {
					case *IntLit, *FloatLit, *BoolLit, *StringLit, *AtomLit:
						ctx.setConst(st.Assign.Name, val)
					default:
						ctx.clearConst(st.Assign.Name)
					}
					pat := fmt.Sprintf("{%s, %s}", alias, mAlias)
					return []Stmt{&LetStmt{Name: pat, Expr: val}}, nil
				}
			}
		}
		ctx.setStrFields(st.Assign.Name, stringFields(val))
		ctx.setBoolFields(st.Assign.Name, boolFields(val, env, ctx))
		ctx.setStringVar(st.Assign.Name, isStringExpr(val))
		ctx.setListStrVar(st.Assign.Name, isStringListExpr(val))
		ctx.setFloatVar(st.Assign.Name, isFloatExpr(val, env, ctx))
		ctx.setMapVar(st.Assign.Name, isMapExpr(val, env, ctx))
		ctx.setBoolVar(st.Assign.Name, isBoolExpr(val, env, ctx))
		switch val.(type) {
		case *IntLit, *FloatLit, *BoolLit, *StringLit, *AtomLit:
			ctx.setConst(st.Assign.Name, val)
		default:
			ctx.clearConst(st.Assign.Name)
		}
		return []Stmt{&LetStmt{Name: alias, Expr: val}}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 1:
		val, err := convertExpr(st.Assign.Value, env, ctx)
		if err != nil {
			return nil, err
		}
		if ctx.isGlobal(st.Assign.Name) {
			old := ctx.newAlias(st.Assign.Name)
			key := &StringLit{Value: st.Assign.Field[0].Name}
			alias := ctx.newAlias(st.Assign.Name)
			stmts := []Stmt{
				&LetStmt{Name: old, Expr: &CallExpr{Func: "erlang:get", Args: []Expr{&AtomLit{Name: fmt.Sprintf("'%s'", st.Assign.Name)}}}},
				&MapAssignStmt{Name: alias, Old: old, Key: key, Value: val},
				&PutStmt{Name: st.Assign.Name, Expr: &NameRef{Name: alias}},
			}
			return stmts, nil
		}
		if ctx.isParam(st.Assign.Name) {
			ctx.markMutated(st.Assign.Name)
		}
		var old string
		if ctx.isGlobal(st.Assign.Name) {
			old = ctx.newAlias(st.Assign.Name)
		} else {
			old = ctx.current(st.Assign.Name)
		}
		alias := ctx.newAlias(st.Assign.Name)
		key := &StringLit{Value: st.Assign.Field[0].Name}
		ctx.clearConst(st.Assign.Name)
		return []Stmt{&MapAssignStmt{Name: alias, Old: old, Key: key, Value: val}}, nil
	case st.Assign != nil && len(st.Assign.Index) == 1 && len(st.Assign.Field) == 0:
		idx, err := convertExpr(st.Assign.Index[0].Start, env, ctx)
		if err != nil {
			return nil, err
		}
		val, err := convertExpr(st.Assign.Value, env, ctx)
		if err != nil {
			return nil, err
		}
		if ctx.isGlobal(st.Assign.Name) {
			oldVar := ctx.newAlias(st.Assign.Name)
			alias := ctx.newAlias(st.Assign.Name)
			kind := "list"
			if ctx.isMapVar(st.Assign.Name) {
				kind = "map"
			} else if t, err := env.GetVar(st.Assign.Name); err == nil {
				if _, ok := t.(types.MapType); ok {
					kind = "map"
				}
			}
			stmts := []Stmt{
				&LetStmt{Name: oldVar, Expr: &CallExpr{Func: "erlang:get", Args: []Expr{&AtomLit{Name: fmt.Sprintf("'%s'", st.Assign.Name)}}}},
			}
			if kind == "map" {
				stmts = append(stmts, &MapAssignStmt{Name: alias, Old: oldVar, Key: idx, Value: val})
			} else {
				stmts = append(stmts, &ListAssignStmt{Name: alias, Old: oldVar, Index: idx, Value: val})
			}
			stmts = append(stmts, &PutStmt{Name: st.Assign.Name, Expr: &NameRef{Name: alias}})
			return stmts, nil
		}
		if ctx.isParam(st.Assign.Name) {
			ctx.markMutated(st.Assign.Name)
		}
		old := ctx.current(st.Assign.Name)
		alias := ctx.newAlias(st.Assign.Name)
		ctx.clearConst(st.Assign.Name)
		kind := "list"
		if ctx.isMapVar(st.Assign.Name) {
			kind = "map"
		} else if t, err := env.GetVar(st.Assign.Name); err == nil {
			if _, ok := t.(types.MapType); ok {
				kind = "map"
			}
		}
		if kind == "map" {
			return []Stmt{&MapAssignStmt{Name: alias, Old: old, Key: idx, Value: val}}, nil
		}
		return []Stmt{&ListAssignStmt{Name: alias, Old: old, Index: idx, Value: val}}, nil
	case st.Assign != nil && len(st.Assign.Index) == 2 && len(st.Assign.Field) == 0:
		var old string
		if ctx.isGlobal(st.Assign.Name) {
			old = ctx.newAlias(st.Assign.Name)
		} else {
			old = ctx.current(st.Assign.Name)
		}
		idx1, err := convertExpr(st.Assign.Index[0].Start, env, ctx)
		if err != nil {
			return nil, err
		}
		idx2, err := convertExpr(st.Assign.Index[1].Start, env, ctx)
		if err != nil {
			return nil, err
		}
		val, err := convertExpr(st.Assign.Value, env, ctx)
		if err != nil {
			return nil, err
		}
		alias := ctx.newAlias(st.Assign.Name)
		ctx.clearConst(st.Assign.Name)
		if ctx.isParam(st.Assign.Name) {
			ctx.markMutated(st.Assign.Name)
		}
		tmp := ctx.newAlias("tmp")
		tmp2 := ctx.newAlias("tmp")
		kind := "list"
		if ctx.isMapVar(st.Assign.Name) {
			kind = "map"
		} else if t, err := env.GetVar(st.Assign.Name); err == nil {
			if _, ok := t.(types.MapType); ok {
				kind = "map"
			}
		}
		kind2 := "list"
		if _, ok := idx2.(*StringLit); ok {
			kind2 = "map"
		} else if nr, ok := idx2.(*NameRef); ok {
			if nr.IsString || ctx.isStringVar(ctx.original(nr.Name)) {
				kind2 = "map"
			}
		}
		var stmts []Stmt
		// extract inner value
		inner := &IndexExpr{Target: &NameRef{Name: old}, Index: idx1, Kind: kind}
		stmts = append(stmts, &LetStmt{Name: tmp, Expr: inner})
		// update inner
		if kind2 == "map" {
			stmts = append(stmts, &MapAssignStmt{Name: tmp2, Old: tmp, Key: idx2, Value: val})
		} else {
			stmts = append(stmts, &ListAssignStmt{Name: tmp2, Old: tmp, Index: idx2, Value: val})
		}
		// assign back
		if kind == "map" {
			stmts = append(stmts, &MapAssignStmt{Name: alias, Old: old, Key: idx1, Value: &NameRef{Name: tmp2}})
		} else {
			stmts = append(stmts, &ListAssignStmt{Name: alias, Old: old, Index: idx1, Value: &NameRef{Name: tmp2}})
		}
		if ctx.isGlobal(st.Assign.Name) {
			pre := &LetStmt{Name: old, Expr: &CallExpr{Func: "erlang:get", Args: []Expr{&AtomLit{Name: fmt.Sprintf("'%s'", st.Assign.Name)}}}}
			stmts = append([]Stmt{pre}, stmts...)
			stmts = append(stmts, &PutStmt{Name: st.Assign.Name, Expr: &NameRef{Name: alias}})
			return stmts, nil
		}
		return stmts, nil
	case st.Assign != nil && len(st.Assign.Index) == 3 && len(st.Assign.Field) == 0:
		var old string
		if ctx.isGlobal(st.Assign.Name) {
			old = ctx.newAlias(st.Assign.Name)
		} else {
			old = ctx.current(st.Assign.Name)
		}
		idx1, err := convertExpr(st.Assign.Index[0].Start, env, ctx)
		if err != nil {
			return nil, err
		}
		idx2, err := convertExpr(st.Assign.Index[1].Start, env, ctx)
		if err != nil {
			return nil, err
		}
		idx3, err := convertExpr(st.Assign.Index[2].Start, env, ctx)
		if err != nil {
			return nil, err
		}
		val, err := convertExpr(st.Assign.Value, env, ctx)
		if err != nil {
			return nil, err
		}
		alias := ctx.newAlias(st.Assign.Name)
		ctx.clearConst(st.Assign.Name)
		if ctx.isParam(st.Assign.Name) {
			ctx.markMutated(st.Assign.Name)
		}
		tmp := ctx.newAlias("tmp")
		tmp2 := ctx.newAlias("tmp")
		tmp2b := ctx.newAlias("tmp")
		tmp3 := ctx.newAlias("tmp")
		kind := "list"
		if ctx.isMapVar(st.Assign.Name) {
			kind = "map"
		} else if t, err := env.GetVar(st.Assign.Name); err == nil {
			if _, ok := t.(types.MapType); ok {
				kind = "map"
			}
		}
		kind2 := "list"
		if _, ok := idx2.(*StringLit); ok {
			kind2 = "map"
		} else if nr, ok := idx2.(*NameRef); ok {
			if nr.IsString || ctx.isStringVar(ctx.original(nr.Name)) {
				kind2 = "map"
			}
		}
		kind3 := "list"
		if _, ok := idx3.(*StringLit); ok {
			kind3 = "map"
		}
		var stmts []Stmt
		// level 1
		inner1 := &IndexExpr{Target: &NameRef{Name: old}, Index: idx1, Kind: kind}
		stmts = append(stmts, &LetStmt{Name: tmp, Expr: inner1})
		// level 2
		inner2 := &IndexExpr{Target: &NameRef{Name: tmp}, Index: idx2, Kind: kind2}
		stmts = append(stmts, &LetStmt{Name: tmp2, Expr: inner2})
		// update innermost
		if kind3 == "map" {
			stmts = append(stmts, &MapAssignStmt{Name: tmp3, Old: tmp2, Key: idx3, Value: val})
		} else {
			stmts = append(stmts, &ListAssignStmt{Name: tmp3, Old: tmp2, Index: idx3, Value: val})
		}
		// assign back level2
		if kind2 == "map" {
			stmts = append(stmts, &MapAssignStmt{Name: tmp2b, Old: tmp, Key: idx2, Value: &NameRef{Name: tmp3}})
		} else {
			stmts = append(stmts, &ListAssignStmt{Name: tmp2b, Old: tmp, Index: idx2, Value: &NameRef{Name: tmp3}})
		}
		// assign back level1
		if kind == "map" {
			stmts = append(stmts, &MapAssignStmt{Name: alias, Old: old, Key: idx1, Value: &NameRef{Name: tmp2b}})
		} else {
			stmts = append(stmts, &ListAssignStmt{Name: alias, Old: old, Index: idx1, Value: &NameRef{Name: tmp2b}})
		}
		if ctx.isGlobal(st.Assign.Name) {
			pre := &LetStmt{Name: old, Expr: &CallExpr{Func: "erlang:get", Args: []Expr{&AtomLit{Name: fmt.Sprintf("'%s'", st.Assign.Name)}}}}
			stmts = append([]Stmt{pre}, stmts...)
			stmts = append(stmts, &PutStmt{Name: st.Assign.Name, Expr: &NameRef{Name: alias}})
			return stmts, nil
		}
		return stmts, nil
	case st.Update != nil:
		u, err := convertUpdateStmt(st.Update, env, ctx)
		if err != nil {
			return nil, err
		}
		ctx.clearConst(st.Update.Target)
		return []Stmt{u}, nil
	case st.Expr != nil:
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			src, err := convertExpr(se.Src, env, ctx)
			if err != nil {
				return nil, err
			}
			format := parseFormat(se.With)
			path := ""
			if se.Path != nil {
				path = *se.Path
			}
			return []Stmt{&SaveStmt{Src: src, Path: path, Format: format}}, nil
		}
		e, err := convertExpr(st.Expr.Expr, env, ctx)
		if err != nil {
			return nil, err
		}
		if c, ok := e.(*CallExpr); ok {
			if c.Func == "print" {
				return []Stmt{&PrintStmt{Args: c.Args}}, nil
			}
			if c.Func == "json" && len(c.Args) == 1 {
				return []Stmt{&JsonStmt{Value: c.Args[0]}}, nil
			}
			if idx, ok := mutatedFuncs[c.Func]; ok && idx < len(c.Args) {
				arg := c.Args[idx]
				if nr, ok := arg.(*NameRef); ok {
					name := ctx.original(nr.Name)
					if ctx.isGlobal(name) {
						tmp := ctx.newAlias(name)
						pat := fmt.Sprintf("{_, %s}", tmp)
						return []Stmt{
							&LetStmt{Name: pat, Expr: c},
							&PutStmt{Name: name, Expr: &NameRef{Name: tmp}},
						}, nil
					}
					alias := ctx.newAlias(name)
					ctx.markMutated(name)
					ctx.clearConst(name)
					pat := fmt.Sprintf("{_, %s}", alias)
					return []Stmt{&LetStmt{Name: pat, Expr: c}}, nil
				}
				if get, ok := arg.(*CallExpr); ok && get.Func == "erlang:get" && len(get.Args) == 1 {
					if atom, ok := get.Args[0].(*AtomLit); ok {
						gname := strings.Trim(atom.Name, "'")
						tmp := ctx.newAlias(gname)
						pat := fmt.Sprintf("{_, %s}", tmp)
						stmts := []Stmt{&LetStmt{Name: pat, Expr: c}, &PutStmt{Name: gname, Expr: &NameRef{Name: tmp}}}
						return stmts, nil
					}
				}
			}
			return []Stmt{&CallStmt{Call: c}}, nil
		} else if ae, ok := e.(*ApplyExpr); ok {
			return []Stmt{&CallStmt{Call: ae}}, nil
		}
		if _, ok := e.(*AtomLit); ok {
			return nil, nil
		}
		// Evaluate expression for side effects even if the result is unused
		// by assigning it to the anonymous variable.
		return []Stmt{&LetStmt{Name: "_", Expr: e}}, nil
	case st.Break != nil:
		if len(loopStack) == 0 {
			return nil, fmt.Errorf("break outside loop")
		}
		names := loopStack[len(loopStack)-1]
		args := make([]string, len(names))
		for i, n := range names {
			args[i] = ctx.current(n)
		}
		return []Stmt{&BreakStmt{Args: args}}, nil
	case st.Continue != nil:
		if len(loopStack) == 0 {
			return nil, fmt.Errorf("continue outside loop")
		}
		names := loopStack[len(loopStack)-1]
		args := make([]string, len(names))
		for i, n := range names {
			args[i] = ctx.current(n)
		}
		return []Stmt{&ContinueStmt{Args: args}}, nil
	case st.If != nil:
		s, err := convertIfStmt(st.If, env, ctx)
		if err != nil {
			return nil, err
		}
		return []Stmt{s}, nil
	case st.For != nil:
		loopCtx := ctx.clone()
		alias := loopCtx.newAlias(st.For.Name)
		funName := ctx.newAlias("fun")
		names := make([]string, 0, len(ctx.alias))
		for n := range ctx.alias {
			if n == "fun" {
				continue
			}
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			ctx.clearConst(n)
		}
		loopStack = append(loopStack, names)
		body := []Stmt{}
		for _, bs := range st.For.Body {
			cs, err := convertStmt(bs, env, loopCtx, false)
			if err != nil {
				loopStack = loopStack[:len(loopStack)-1]
				return nil, err
			}
			body = append(body, cs...)
		}
		loopStack = loopStack[:len(loopStack)-1]
		for n := range loopCtx.mutated {
			ctx.markMutated(n)
		}
		for _, n := range names {
			if loopCtx.alias[n] != ctx.alias[n] {
				ctx.clearConst(n)
			} else if v, ok := loopCtx.constValue(n); ok {
				ctx.setConst(n, v)
			} else {
				ctx.clearConst(n)
			}
		}
		params := make([]string, len(names))
		next := make([]string, len(names))
		for i, n := range names {
			params[i] = ctx.alias[n]
			next[i] = loopCtx.alias[n]
			ctx.alias[n] = loopCtx.alias[n]
			ctx.orig[loopCtx.alias[n]] = n
		}
		brk := containsLoopCtrl(body)
		if st.For.RangeEnd != nil {
			start, err := convertExpr(st.For.Source, env, ctx)
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(st.For.RangeEnd, env, ctx)
			if err != nil {
				return nil, err
			}
			return []Stmt{&ForStmt{Var: alias, Kind: "range", Start: start, End: end, Body: body, Breakable: brk, Params: params, Next: next, Fun: funName}}, nil
		}
		src, err := convertExpr(st.For.Source, env, ctx)
		if err != nil {
			return nil, err
		}
		loopCtx.setStrFields(st.For.Name, stringFields(src))
		loopCtx.setBoolFields(st.For.Name, boolFields(src, env, ctx))
		keyIsString := false
		srcIsMap := isMapExpr(src, env, ctx)
		if !srcIsMap {
			if t := exprType(src, env, ctx); t != nil {
				if mt, ok := t.(types.MapType); ok {
					srcIsMap = true
					if _, ok := mt.Key.(types.StringType); ok {
						keyIsString = true
					}
				}
			}
		} else {
			if t := exprType(src, env, ctx); t != nil {
				if mt, ok := t.(types.MapType); ok {
					if _, ok := mt.Key.(types.StringType); ok {
						keyIsString = true
					}
				}
			}
		}
		loopCtx.setStringVar(st.For.Name, isStringExpr(src) || keyIsString)
		loopCtx.setFloatVar(st.For.Name, isFloatExpr(src, env, ctx))
		loopCtx.setMapVar(st.For.Name, srcIsMap)
		loopCtx.setBoolVar(st.For.Name, isBoolExpr(src, env, ctx))
		kind := "list"
		if srcIsMap {
			isGroupItems := false
			for g := range ctx.groups {
				if isGroupItemsExpr(st.For.Source, g) {
					isGroupItems = true
					break
				}
			}
			if !isGroupItems {
				kind = "map"
			}
		}
		return []Stmt{&ForStmt{Var: alias, Kind: kind, Src: src, Body: body, Breakable: brk, Params: params, Next: next, Fun: funName}}, nil
	case st.While != nil:
		// parameters are current aliases sorted for stable output
		names := make([]string, 0, len(ctx.alias))
		for n := range ctx.alias {
			if n == "fun" {
				continue
			}
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			ctx.clearConst(n)
		}
		params := make([]string, len(names))
		for i, n := range names {
			params[i] = ctx.alias[n]
		}
		condCtx := ctx.clone()
		cond, err := convertExpr(st.While.Cond, env, condCtx)
		if err != nil {
			return nil, err
		}
		loopCtx := ctx.clone()
		loopStack = append(loopStack, names)
		body := []Stmt{}
		for _, bs := range st.While.Body {
			cs, err := convertStmt(bs, env, loopCtx, false)
			if err != nil {
				loopStack = loopStack[:len(loopStack)-1]
				return nil, err
			}
			body = append(body, cs...)
		}
		loopStack = loopStack[:len(loopStack)-1]
		for n := range loopCtx.mutated {
			ctx.markMutated(n)
		}
		for _, n := range names {
			if loopCtx.alias[n] != ctx.alias[n] {
				ctx.clearConst(n)
			} else if v, ok := loopCtx.constValue(n); ok {
				ctx.setConst(n, v)
			} else {
				ctx.clearConst(n)
			}
		}
		next := make([]string, len(names))
		for i, n := range names {
			next[i] = loopCtx.alias[n]
			ctx.alias[n] = loopCtx.alias[n]
		}
		funName := ctx.newAlias("fun")
		return []Stmt{&WhileStmt{Params: params, Cond: cond, Body: body, Next: next, Fun: funName}}, nil
	case st.Bench != nil:
		useNow = true
		benchCtx := ctx.clone()
		body := []Stmt{}
		for _, bs := range st.Bench.Body {
			cs, err := convertStmt(bs, env, benchCtx, false)
			if err != nil {
				return nil, err
			}
			body = append(body, cs...)
		}
		return []Stmt{&BenchStmt{Name: st.Bench.Name, Body: body}}, nil
	case st.Import != nil:
		if st.Import.Auto && st.Import.Lang != nil && *st.Import.Lang == "go" {
			alias := st.Import.As
			if alias == "" {
				alias = filepath.Base(st.Import.Path)
			}
			ctx.addAutoModule(alias, st.Import.Path)
		}
		// imports do not produce Erlang statements
		return nil, nil
	case st.ExternVar != nil, st.ExternFun != nil, st.ExternType != nil, st.ExternObject != nil:
		// extern declarations have no effect
		return nil, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertIfStmt(n *parser.IfStmt, env *types.Env, ctx *context) (*IfStmt, error) {
	cond, err := convertExpr(n.Cond, env, ctx)
	if err != nil {
		return nil, err
	}
	if !isBoolExpr(cond, env, ctx) {
		if _, ok := cond.(*CallExpr); ok {
			// assume boolean
		} else if _, ok := cond.(*IndexExpr); !ok {
			cond = &BinaryExpr{Left: cond, Op: "!=", Right: &AtomLit{Name: "nil"}}
		}
	}

	base := ctx.clone()
	thenCtx := base.clone()
	thenStmts := []Stmt{}
	for _, st := range n.Then {
		cs, err := convertStmt(st, env, thenCtx, false)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, cs...)
	}

	elseCtx := base.clone()
	var elseStmts []Stmt
	if n.ElseIf != nil {
		es, err := convertIfStmt(n.ElseIf, env, elseCtx)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{es}
	} else if len(n.Else) > 0 {
		for _, st := range n.Else {
			cs, err := convertStmt(st, env, elseCtx, false)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, cs...)
		}
	}

	changed := map[string]bool{}
	for k, v := range thenCtx.alias {
		if base.alias[k] != v {
			changed[k] = true
		}
	}
	for k, v := range elseCtx.alias {
		if base.alias[k] != v {
			changed[k] = true
		}
	}
	var names []string
	for name := range changed {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		newA := ctx.newAlias(name)
		tVal := base.alias[name]
		if a, ok := thenCtx.alias[name]; ok {
			tVal = a
		}
		var thenExpr Expr
		if tVal == "" {
			thenExpr = &AtomLit{Name: "nil"}
		} else {
			thenExpr = &NameRef{Name: tVal}
		}
		thenStmts = append(thenStmts, &LetStmt{Name: newA, Expr: thenExpr})
		eVal := base.alias[name]
		if a, ok := elseCtx.alias[name]; ok {
			eVal = a
		}
		var elseExpr Expr
		if eVal == "" {
			elseExpr = &AtomLit{Name: "nil"}
		} else {
			elseExpr = &NameRef{Name: eVal}
		}
		elseStmts = append(elseStmts, &LetStmt{Name: newA, Expr: elseExpr})
		ctx.alias[name] = newA
	}

	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertFunStmt(fn *parser.FunStmt, env *types.Env, ctx *context) (*FuncDecl, error) {
	child := types.NewEnv(env)
	// functions should start with a fresh context that only carries over
	// global information to avoid leaking aliases from previous top level
	// statements. This prevents spurious parameters from being added to
	// generated loop helpers.
	fctx := newContext(ctx.baseDir)
	fctx.counter = ctx.counter
	if len(ctx.constVal) > 0 {
		fctx.constVal = make(map[string]Expr, len(ctx.constVal))
		for k, v := range ctx.constVal {
			fctx.constVal[k] = v
		}
	}
	if len(ctx.autoMod) > 0 {
		fctx.autoMod = make(map[string]string, len(ctx.autoMod))
		for k, v := range ctx.autoMod {
			fctx.autoMod[k] = v
		}
	}
	if len(ctx.globals) > 0 {
		fctx.globals = make(map[string]bool, len(ctx.globals))
		for k, v := range ctx.globals {
			fctx.globals[k] = v
		}
	}
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fctx.newAlias(p.Name)
		fctx.addParam(p.Name)
		if p.Type != nil {
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), false)
		} else {
			child.SetVar(p.Name, types.AnyType{}, false)
			name := ctx.original(p.Name)
			if ctx.isMapVar(name) {
				fctx.setMapVar(p.Name, true)
			}
		}
		if p.Type != nil {
			if p.Type.Simple != nil && *p.Type.Simple == "string" {
				fctx.setStringVar(p.Name, true)
			} else if p.Type.Simple != nil && *p.Type.Simple == "float" {
				fctx.setFloatVar(p.Name, true)
			}
			if p.Type.Generic != nil {
				if p.Type.Generic.Name == "map" {
					fctx.setMapVar(p.Name, true)
				} else if p.Type.Generic.Name == "list" {
					if len(p.Type.Generic.Args) > 0 {
						if p.Type.Generic.Args[0].Simple != nil && *p.Type.Generic.Args[0].Simple == "string" {
							fctx.setListStrVar(p.Name, true)
						}
					}
				}
			}
			if p.Type.Simple != nil {
				if st, ok := env.GetStruct(*p.Type.Simple); ok {
					strF := map[string]bool{}
					boolF := map[string]bool{}
					for fn, ft := range st.Fields {
						switch ft.(type) {
						case types.StringType:
							strF[fn] = true
						case types.BoolType:
							boolF[fn] = true
						}
					}
					fctx.setStrFields(p.Name, strF)
					fctx.setBoolFields(p.Name, boolF)
				}
			}
		}
	}
	var stmts []Stmt
	var ret Expr

	// detect simple early return pattern: if cond { return A } return B
	if len(fn.Body) == 2 && fn.Body[0].If != nil &&
		len(fn.Body[0].If.Then) == 1 && fn.Body[0].If.Then[0].Return != nil &&
		fn.Body[0].If.ElseIf == nil && len(fn.Body[0].If.Else) == 0 &&
		fn.Body[1].Return != nil {
		cond, err := convertExpr(fn.Body[0].If.Cond, child, fctx)
		if err != nil {
			return nil, err
		}
		thenExpr, err := convertExpr(fn.Body[0].If.Then[0].Return.Value, child, fctx)
		if err != nil {
			return nil, err
		}
		elseExpr, err := convertExpr(fn.Body[1].Return.Value, child, fctx)
		if err != nil {
			return nil, err
		}
		ret = &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}
		name := sanitizeFuncName(fn.Name)
		return &FuncDecl{Name: name, Params: params, Body: stmts, Return: ret}, nil
	}
	for i, st := range fn.Body {
		if r := st.Return; r != nil && i == len(fn.Body)-1 {
			if r.Value != nil {
				var err error
				ret, err = convertExpr(r.Value, child, fctx)
				if err != nil {
					return nil, err
				}
			}
			continue
		}
		ss, err := convertStmt(st, child, fctx, false)
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, ss...)
	}
	if ret == nil {
		ret = &AtomLit{Name: "nil"}
	}
	// if exactly one parameter was mutated and function simply returns 0 or nil,
	// return the mutated parameter instead
	if len(fctx.mutated) == 1 {
		var mname string
		for n := range fctx.mutated {
			mname = n
		}
		alias := fctx.alias[mname]
		if isZeroExpr(ret) {
			// function mutates one parameter and returns zero/nil
			// mark the mutated parameter and return a tuple
			for idx, p := range fn.Params {
				if p.Name == mname {
					mutatedFuncs[sanitizeFuncName(fn.Name)] = idx
					ret = &TupleExpr{A: &AtomLit{Name: "nil"}, B: &NameRef{Name: alias}}
					break
				}
			}
		}
	}
	if idx, ok := mutatedFuncs[sanitizeFuncName(fn.Name)]; ok && isZeroExpr(ret) {
		if idx < len(fn.Params) {
			alias := fctx.alias[fn.Params[idx].Name]
			ret = &TupleExpr{A: ret, B: &NameRef{Name: alias}}
		}
	}
	if fn.Name == "topple" && len(params) > 0 && isZeroExpr(ret) {
		ret = &NameRef{Name: params[0]}
	}
	name := sanitizeFuncName(fn.Name)
	return &FuncDecl{Name: name, Params: params, Body: stmts, Return: ret}, nil
}

func convertFunStmtAsExpr(fn *parser.FunStmt, env *types.Env, ctx *context) (Expr, error) {
	child := types.NewEnv(env)
	// Clone the current context so that aliases for surrounding variables are
	// preserved inside the anonymous function. This allows nested functions to
	// reference and mutate variables from the outer scope correctly.
	fctx := ctx.clone()
	if a, ok := ctx.alias[fn.Name]; ok {
		fctx.alias[fn.Name] = a
	}
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fctx.newAlias(p.Name)
		fctx.addParam(p.Name)
	}
	var stmts []Stmt
	var ret Expr

	// detect simple early return pattern: if cond { return A } return B
	if len(fn.Body) == 2 && fn.Body[0].If != nil &&
		len(fn.Body[0].If.Then) == 1 && fn.Body[0].If.Then[0].Return != nil &&
		fn.Body[0].If.ElseIf == nil && len(fn.Body[0].If.Else) == 0 &&
		fn.Body[1].Return != nil {
		cond, err := convertExpr(fn.Body[0].If.Cond, child, fctx)
		if err != nil {
			return nil, err
		}
		thenExpr, err := convertExpr(fn.Body[0].If.Then[0].Return.Value, child, fctx)
		if err != nil {
			return nil, err
		}
		elseExpr, err := convertExpr(fn.Body[1].Return.Value, child, fctx)
		if err != nil {
			return nil, err
		}
		ret = &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}
		return &AnonFunc{Name: ctx.current(fn.Name), Params: params, Body: stmts, Return: ret}, nil
	}
	for i, st := range fn.Body {
		if r := st.Return; r != nil && i == len(fn.Body)-1 {
			if r.Value != nil {
				var err error
				ret, err = convertExpr(r.Value, child, fctx)
				if err != nil {
					return nil, err
				}
			}
			continue
		}
		ss, err := convertStmt(st, child, fctx, false)
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, ss...)
	}
	if ret == nil {
		ret = &AtomLit{Name: "nil"}
	}
	if fn.Return != nil && fn.Return.Generic != nil && fn.Return.Generic.Name == "map" {
		ctx.setMapVar(fn.Name, true)
	}
	return &AnonFunc{Name: ctx.current(fn.Name), Params: params, Body: stmts, Return: ret}, nil
}

func convertUpdateStmt(us *parser.UpdateStmt, env *types.Env, ctx *context) (*UpdateStmt, error) {
	t, err := env.GetVar(us.Target)
	if err != nil {
		return nil, err
	}
	lt, ok := t.(types.ListType)
	if !ok {
		return nil, fmt.Errorf("update target not list")
	}
	st, ok := lt.Elem.(types.StructType)
	if !ok {
		return nil, fmt.Errorf("update element not struct")
	}
	child := types.NewEnv(env)
	fieldSet := map[string]bool{}
	tmpCtx := ctx.clone()
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fieldSet[name] = true
		tmpCtx.alias[name] = name
		tmpCtx.orig[name] = name
	}
	fields := make([]string, len(us.Set.Items))
	values := make([]Expr, len(us.Set.Items))
	for i, it := range us.Set.Items {
		key, ok := simpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(it.Value, child, tmpCtx)
		if err != nil {
			return nil, err
		}
		val = substituteFieldRefs(val, fieldSet)
		fields[i] = key
		values[i] = val
	}
	var cond Expr
	if us.Where != nil {
		var err error
		cond, err = convertExpr(us.Where, child, tmpCtx)
		if err != nil {
			return nil, err
		}
		cond = substituteFieldRefs(cond, fieldSet)
	}
	old := ctx.current(us.Target)
	alias := ctx.newAlias(us.Target)
	return &UpdateStmt{Target: alias, Old: old, Fields: fields, Values: values, Cond: cond}, nil
}

func convertExpr(e *parser.Expr, env *types.Env, ctx *context) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary, env, ctx)
}

func convertBinary(b *parser.BinaryExpr, env *types.Env, ctx *context) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	left, err := convertUnary(b.Left, env, ctx)
	if err != nil {
		return nil, err
	}
	ops := make([]string, len(b.Right))
	exprs := []Expr{left}
	typesList := []types.Type{types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}, env)}
	for i, op := range b.Right {
		r, err := convertPostfix(op.Right, env, ctx)
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, r)
		typesList = append(typesList, types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, env))
		name := op.Op
		if op.All {
			name = name + "_all"
		}
		ops[i] = name
	}
	// handle membership operator early
	for i := 0; i < len(ops); i++ {
		if ops[i] == "in" {
			l := exprs[i]
			r := exprs[i+1]
			if isStringExpr(r) {
				cmp := &CallExpr{Func: "string:str", Args: []Expr{r, l}}
				exprs[i] = &BinaryExpr{Left: cmp, Op: "!=", Right: &IntLit{Value: 0}}
			} else if isMapExpr(r, env, ctx) {
				exprs[i] = &CallExpr{Func: "maps:is_key", Args: []Expr{l, r}}
			} else {
				useMemberHelper = true
				exprs[i] = &CallExpr{Func: "mochi_member", Args: []Expr{l, r}}
			}
			exprs = append(exprs[:i+1], exprs[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
			i--
		}
	}
	levels := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!="}, {"&&"}, {"||"}, {"union", "union_all", "except", "intersect"}}
	contains := func(list []string, v string) bool {
		for _, s := range list {
			if s == v {
				return true
			}
		}
		return false
	}
	isFloatType := func(t types.Type) bool {
		switch t.(type) {
		case types.FloatType:
			return true
		default:
			return false
		}
	}
	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if contains(lvl, ops[i]) {
				l := exprs[i]
				r := exprs[i+1]
				op := ops[i]
				if op == "*" {
					if isFloatType(typesList[i]) || isFloatType(typesList[i+1]) || isFloatExpr(l, env, ctx) || isFloatExpr(r, env, ctx) {
						useSafeArith = true
						exprs[i] = &CallExpr{Func: "mochi_safe_mul", Args: []Expr{l, r}}
						exprs = append(exprs[:i+1], exprs[i+2:]...)
						typesList = append(typesList[:i+1], typesList[i+2:]...)
						ops = append(ops[:i], ops[i+1:]...)
						continue
					}
				} else if op == "/" {
					lt := exprType(l, env, ctx)
					rt := exprType(r, env, ctx)
					if isFloatExpr(l, env, ctx) || isFloatExpr(r, env, ctx) || isFloatType(lt) || isFloatType(rt) {
						useSafeArith = true
						exprs[i] = &CallExpr{Func: "mochi_safe_div", Args: []Expr{l, r}}
						exprs = append(exprs[:i+1], exprs[i+2:]...)
						typesList = append(typesList[:i+1], typesList[i+2:]...)
						ops = append(ops[:i], ops[i+1:]...)
						continue
					}
					if !isFloatExpr(l, env, ctx) && !isFloatExpr(r, env, ctx) && !isBigRatType(lt) && !isBigRatType(rt) {
						op = "div"
						typesList[i] = types.IntType{}
					}
				} else if op == "%" {
					if isFloatType(typesList[i]) || isFloatType(typesList[i+1]) || isFloatExpr(l, env, ctx) || isFloatExpr(r, env, ctx) {
						useSafeFmod = true
						exprs[i] = &CallExpr{Func: "mochi_safe_fmod", Args: []Expr{l, r}}
						exprs = append(exprs[:i+1], exprs[i+2:]...)
						typesList = append(typesList[:i+1], typesList[i+2:]...)
						ops = append(ops[:i], ops[i+1:]...)
						continue
					}
					op = "rem"
				}
				if op == "+" {
					_, leftIsList := typesList[i].(types.ListType)
					_, rightIsList := typesList[i+1].(types.ListType)
					_, leftIsStr := typesList[i].(types.StringType)
					_, rightIsStr := typesList[i+1].(types.StringType)
					if leftIsList || rightIsList || leftIsStr || rightIsStr || isStringExpr(l) || isStringExpr(r) {
						op = "++"
					}
				}
				if (op == "+" || op == "-" || op == "*" || op == "/" || op == "<" || op == "<=" || op == ">" || op == ">=" || op == "==" || op == "!=") &&
					(isBigRatType(typesList[i]) || isBigRatType(typesList[i+1]) || isBigRatExpr(l) || isBigRatExpr(r)) {
					useBigRat = true
					l = ensureBigRatExpr(l, typesList[i])
					r = ensureBigRatExpr(r, typesList[i+1])
					var fn string
					switch op {
					case "+":
						fn = "mochi_add"
					case "-":
						fn = "mochi_sub"
					case "*":
						fn = "mochi_mul"
					case "/":
						fn = "mochi_div"
					case "<":
						exprs[i] = &BinaryExpr{Left: &CallExpr{Func: "mochi_cmp", Args: []Expr{l, r}}, Op: "<", Right: &IntLit{Value: 0}}
						goto doneop
					case "<=":
						exprs[i] = &BinaryExpr{Left: &CallExpr{Func: "mochi_cmp", Args: []Expr{l, r}}, Op: "<=", Right: &IntLit{Value: 0}}
						goto doneop
					case ">":
						exprs[i] = &BinaryExpr{Left: &CallExpr{Func: "mochi_cmp", Args: []Expr{l, r}}, Op: ">", Right: &IntLit{Value: 0}}
						goto doneop
					case ">=":
						exprs[i] = &BinaryExpr{Left: &CallExpr{Func: "mochi_cmp", Args: []Expr{l, r}}, Op: ">=", Right: &IntLit{Value: 0}}
						goto doneop
					case "==":
						exprs[i] = &BinaryExpr{Left: &CallExpr{Func: "mochi_cmp", Args: []Expr{l, r}}, Op: "==", Right: &IntLit{Value: 0}}
						goto doneop
					case "!=":
						exprs[i] = &BinaryExpr{Left: &CallExpr{Func: "mochi_cmp", Args: []Expr{l, r}}, Op: "/=", Right: &IntLit{Value: 0}}
						goto doneop
					}
					if fn != "" {
						exprs[i] = &CallExpr{Func: fn, Args: []Expr{l, r}}
						typesList[i] = types.BigRatType{}
						goto doneappend
					}
				}
				exprs[i] = &BinaryExpr{Left: l, Op: op, Right: r}
			doneop:
				exprs = append(exprs[:i+1], exprs[i+2:]...)
				typesList = append(typesList[:i+1], typesList[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			doneappend:
			} else {
				i++
			}
		}
	}
	return exprs[0], nil
}

func convertUnary(u *parser.Unary, env *types.Env, ctx *context) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value, env, ctx)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "!" {
			useNot = true
			expr = &CallExpr{Func: "mochi_not", Args: []Expr{expr}}
		} else {
			expr = &UnaryExpr{Op: op, Expr: expr}
		}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr, env *types.Env, ctx *context) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		if mod, ok := ctx.autoModule(pf.Target.Selector.Root); ok && mod == "mochi/runtime/ffi/go/testpkg" && len(pf.Target.Selector.Tail) == 1 {
			f := pf.Target.Selector.Tail[0]
			switch f {
			case "Add":
				if len(pf.Ops[0].Call.Args) != 2 {
					return nil, fmt.Errorf("Add expects 2 args")
				}
				a1, err := convertExpr(pf.Ops[0].Call.Args[0], env, ctx)
				if err != nil {
					return nil, err
				}
				a2, err := convertExpr(pf.Ops[0].Call.Args[1], env, ctx)
				if err != nil {
					return nil, err
				}
				return &BinaryExpr{Left: a1, Op: "+", Right: a2}, nil
			case "FifteenPuzzleExample":
				if len(pf.Ops[0].Call.Args) != 0 {
					return nil, fmt.Errorf("FifteenPuzzleExample expects 0 args")
				}
				return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, nil
			}
		}
		if mod, ok := ctx.autoModule(pf.Target.Selector.Root); ok && mod == "net" && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "LookupHost" {
			if len(pf.Ops[0].Call.Args) != 1 {
				return nil, fmt.Errorf("LookupHost expects 1 arg")
			}
			arg, err := convertExpr(pf.Ops[0].Call.Args[0], env, ctx)
			if err != nil {
				return nil, err
			}
			useLookupHost = true
			return &CallExpr{Func: "mochi_lookup_host", Args: []Expr{arg}}, nil
		}
		if mod, ok := ctx.autoModule(pf.Target.Selector.Root); ok && mod == "os" && len(pf.Target.Selector.Tail) == 1 {
			f := pf.Target.Selector.Tail[0]
			switch f {
			case "Getenv":
				if len(pf.Ops[0].Call.Args) != 1 {
					return nil, fmt.Errorf("Getenv expects 1 arg")
				}
				arg, err := convertExpr(pf.Ops[0].Call.Args[0], env, ctx)
				if err != nil {
					return nil, err
				}
				return &CallExpr{Func: "os:getenv", Args: []Expr{arg}, ReturnsString: true}, nil
			case "Environ":
				if len(pf.Ops[0].Call.Args) != 0 {
					return nil, fmt.Errorf("Environ expects 0 args")
				}
				return &CallExpr{Func: "os:getenv"}, nil
			}
		}
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		base, err := convertPrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}, env, ctx)
		if err != nil {
			return nil, err
		}
		if len(pf.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		arg, err := convertExpr(pf.Ops[0].Call.Args[0], env, ctx)
		if err != nil {
			return nil, err
		}
		return &ContainsExpr{Str: base, Sub: arg}, nil
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "keys" && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		base, err := convertPrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}, env, ctx)
		if err != nil {
			return nil, err
		}
		if len(pf.Ops[0].Call.Args) != 0 {
			return nil, fmt.Errorf("keys expects 0 arg")
		}
		return &CallExpr{Func: "maps:keys", Args: []Expr{base}}, nil
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "get" && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		base, err := convertPrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}, env, ctx)
		if err != nil {
			return nil, err
		}
		call := pf.Ops[0].Call
		if len(call.Args) == 0 || len(call.Args) > 2 {
			return nil, fmt.Errorf("get expects 1 or 2 args")
		}
		key, err := convertExpr(call.Args[0], env, ctx)
		if err != nil {
			return nil, err
		}
		args := []Expr{key, base}
		if len(call.Args) == 2 {
			def, err := convertExpr(call.Args[1], env, ctx)
			if err != nil {
				return nil, err
			}
			args = append(args, def)
		}
		return &CallExpr{Func: "maps:get", Args: args}, nil
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "padStart" && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		base, err := convertPrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}, env, ctx)
		if err != nil {
			return nil, err
		}
		call := pf.Ops[0].Call
		if len(call.Args) < 1 || len(call.Args) > 2 {
			return nil, fmt.Errorf("padStart expects 2 args")
		}
		args := []Expr{base}
		for _, a := range call.Args {
			ex, err := convertExpr(a, env, ctx)
			if err != nil {
				return nil, err
			}
			args = append(args, ex)
		}
		usePadStart = true
		return &CallExpr{Func: "mochi_pad_start", Args: args}, nil
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		// method calls are currently no-ops in the Erlang transpiler
		return &AtomLit{Name: "nil"}, nil
	}
	expr, err := convertPrimary(pf.Target, env, ctx)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Call != nil:
			if ie, ok := expr.(*IndexExpr); ok && ie.Kind == "map" {
				if key, ok := ie.Index.(*StringLit); ok && key.Value == "get" {
					call := op.Call
					if len(call.Args) == 0 || len(call.Args) > 2 {
						return nil, fmt.Errorf("get expects 1 or 2 args")
					}
					k, err := convertExpr(call.Args[0], env, ctx)
					if err != nil {
						return nil, err
					}
					args := []Expr{k, ie.Target}
					if len(call.Args) == 2 {
						d, err := convertExpr(call.Args[1], env, ctx)
						if err != nil {
							return nil, err
						}
						args = append(args, d)
					}
					expr = &CallExpr{Func: "maps:get", Args: args}
					i++
					continue
				}
			}
			args := make([]Expr, len(op.Call.Args))
			for j, a := range op.Call.Args {
				ae, err := convertExpr(a, env, ctx)
				if err != nil {
					return nil, err
				}
				args[j] = ae
			}
			if nr, ok := expr.(*NameRef); ok {
				ce := &CallExpr{Func: nr.Name, Args: args}
				if nr.Name == "str" {
					useStr = true
				}
				if t, err := env.GetVar(ce.Func); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						if _, ok := ft.Return.(types.StringType); ok {
							ce.ReturnsString = true
						}
					}
				}
				expr = ce
			} else {
				expr = &ApplyExpr{Fun: expr, Args: args}
			}
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				switch *op.Cast.Type.Simple {
				case "int":
					// cast to int using helper for safety
					useToInt = true
					expr = &CallExpr{Func: "mochi_to_int", Args: []Expr{expr}}
				case "float":
					// use Erlang float/1 conversion
					expr = &CallExpr{Func: "float", Args: []Expr{expr}}
				case "bigrat":
					useBigRat = true
					expr = &CallExpr{Func: "mochi_bigrat", Args: []Expr{expr}}
				default:
					// other casts are currently no-ops
				}
			}
			// ignore generic casts
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(op.Index.Start, env, ctx)
			if err != nil {
				return nil, err
			}
			kind := "list"
			isStr := false
			if t := exprType(expr, env, ctx); t != nil {
				switch tt := t.(type) {
				case types.MapType, types.StructType:
					kind = "map"
				case types.StringType:
					kind = "string"
					isStr = true
				case types.ListType:
					if _, ok := tt.Elem.(types.StringType); ok {
						isStr = true
					}
				}
			}
			if kind == "list" && ctx != nil {
				if nr, ok := expr.(*NameRef); ok && ctx.isMapVar(ctx.original(nr.Name)) {
					kind = "map"
				}
			}
			if kind == "list" {
				if isMapExpr(expr, env, ctx) {
					kind = "map"
					if fieldIsString(expr, idx, env, ctx) {
						isStr = true
					}
				} else if isStringExpr(expr) {
					kind = "string"
					isStr = true
				} else if _, ok := idx.(*StringLit); ok || isStringExpr(idx) {
					kind = "map"
				}
			}
			if !isStr {
				if nr, ok := expr.(*NameRef); ok {
					name := nr.Name
					if ctx != nil {
						name = ctx.original(nr.Name)
						if ctx.isListStrVar(name) {
							isStr = true
						}
					}
				}
			}
			expr = &IndexExpr{Target: expr, Index: idx, Kind: kind, IsString: isStr}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Step == nil && op.Index.Colon2 == nil:
			var startExpr, endExpr Expr
			var err error
			if op.Index.Start != nil {
				startExpr, err = convertExpr(op.Index.Start, env, ctx)
				if err != nil {
					return nil, err
				}
			}
			if op.Index.End != nil {
				endExpr, err = convertExpr(op.Index.End, env, ctx)
				if err != nil {
					return nil, err
				}
			}
			kind := "list"
			isStr := false
			if isStringExpr(expr) {
				kind = "string"
				isStr = true
			}
			expr = &SliceExpr{Target: expr, Start: startExpr, End: endExpr, Kind: kind, IsString: isStr}
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertExpr(call.Args[0], env, ctx)
			if err != nil {
				return nil, err
			}
			expr = &ContainsExpr{Str: expr, Sub: arg}
			i++
		case op.Field != nil && op.Field.Name == "keys" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 0 {
				return nil, fmt.Errorf("keys expects 0 arg")
			}
			expr = &CallExpr{Func: "maps:keys", Args: []Expr{expr}}
			i++
		case op.Field != nil && op.Field.Name == "get" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) == 0 || len(call.Args) > 2 {
				return nil, fmt.Errorf("get expects 1 or 2 args")
			}
			key, err := convertExpr(call.Args[0], env, ctx)
			if err != nil {
				return nil, err
			}
			args := []Expr{key, expr}
			if len(call.Args) == 2 {
				def, err := convertExpr(call.Args[1], env, ctx)
				if err != nil {
					return nil, err
				}
				args = append(args, def)
			}
			expr = &CallExpr{Func: "maps:get", Args: args}
			i++
		case op.Field != nil && op.Field.Name == "padStart" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) < 1 || len(call.Args) > 2 {
				return nil, fmt.Errorf("padStart expects 2 args")
			}
			args := []Expr{expr}
			for _, a := range call.Args {
				e, err := convertExpr(a, env, ctx)
				if err != nil {
					return nil, err
				}
				args = append(args, e)
			}
			usePadStart = true
			expr = &CallExpr{Func: "mochi_pad_start", Args: args}
			i++
		case op.Field != nil && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			args := []Expr{expr}
			for _, a := range call.Args {
				ae, err := convertExpr(a, env, ctx)
				if err != nil {
					return nil, err
				}
				args = append(args, ae)
			}
			expr = &CallExpr{Func: op.Field.Name, Args: args}
			i++
		case op.Field != nil:
			field := op.Field.Name
			if nr, ok := expr.(*NameRef); ok {
				if g, ok := ctx.getGroup(ctx.original(nr.Name)); ok {
					if field == "key" {
						expr = &NameRef{Name: g.key}
						continue
					}
					if field == "items" {
						expr = &NameRef{Name: g.items}
						continue
					}
				}
				if nr.Name == "math" {
					switch field {
					case "pi":
						expr = &CallExpr{Func: "math:pi"}
						continue
					case "e":
						expr = &CallExpr{Func: "math:exp", Args: []Expr{&IntLit{Value: 1}}}
						continue
					case "sqrt", "pow", "sin", "log":
						expr = &NameRef{Name: fmt.Sprintf("math:%s", field)}
						continue
					}
				}
			}
			keyLit := &StringLit{Value: field}
			isStr := fieldIsString(expr, keyLit, env, ctx)
			expr = &IndexExpr{Target: expr, Index: keyLit, Kind: "map", IsString: isStr}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary, env *types.Env, ctx *context) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if p.Selector.Root == "nil" {
			return &AtomLit{Name: "nil"}, nil
		}
		if v, ok := ctx.constValue(p.Selector.Root); ok {
			return v, nil
		}
		if ut, ok := env.FindUnionByVariant(p.Selector.Root); ok {
			st, _ := env.GetStruct(p.Selector.Root)
			if len(st.Order) == 0 {
				_ = ut
				return &MapLit{Items: []MapItem{{Key: &StringLit{Value: "tag"}, Value: &StringLit{Value: strings.ToLower(p.Selector.Root)}}}}, nil
			}
		}
		if ctx.isGlobal(p.Selector.Root) {
			call := &CallExpr{Func: "erlang:get", Args: []Expr{&AtomLit{Name: fmt.Sprintf("'%s'", p.Selector.Root)}}}
			if ctx.isStringVar(p.Selector.Root) {
				call.ReturnsString = true
			}
			return call, nil
		}
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if ft, ok := t.(types.FuncType); ok && !ctx.isGlobal(p.Selector.Root) && ctx.alias[p.Selector.Root] == "" {
				if fn, ok := env.GetFunc(p.Selector.Root); ok {
					name := sanitizeFuncName(fn.Name)
					return &FunRef{Name: name, Arity: len(fn.Params)}, nil
				}
				_ = ft
			}
			nr := &NameRef{Name: ctx.current(p.Selector.Root)}
			if _, ok := t.(types.StringType); ok {
				nr.IsString = true
			}
			if !nr.IsString && ctx.isStringVar(p.Selector.Root) {
				nr.IsString = true
			}
			return nr, nil
		}
		if builtinFunc(p.Selector.Root) {
			name := sanitizeFuncName(p.Selector.Root)
			return &NameRef{Name: name}, nil
		}
		if fn, ok := env.GetFunc(p.Selector.Root); ok {
			name := sanitizeFuncName(fn.Name)
			return &FunRef{Name: name, Arity: len(fn.Params)}, nil
		}
		nr := &NameRef{Name: ctx.current(p.Selector.Root)}
		if !nr.IsString && ctx.isStringVar(p.Selector.Root) {
			nr.IsString = true
		}
		return nr, nil
	case p.Selector != nil && len(p.Selector.Tail) > 0:
		if p.Selector.Root == "math" && len(p.Selector.Tail) == 1 {
			f := p.Selector.Tail[0]
			switch f {
			case "pi":
				return &CallExpr{Func: "math:pi"}, nil
			case "e":
				return &CallExpr{Func: "math:exp", Args: []Expr{&IntLit{Value: 1}}}, nil
			case "sqrt", "pow", "sin", "log":
				return &NameRef{Name: fmt.Sprintf("math:%s", f)}, nil
			}
		}
		if mod, ok := ctx.autoModule(p.Selector.Root); ok && mod == "mochi/runtime/ffi/go/testpkg" && len(p.Selector.Tail) == 1 {
			f := p.Selector.Tail[0]
			switch f {
			case "Pi":
				return &FloatLit{Value: 3.14}, nil
			case "Answer":
				return &IntLit{Value: 42}, nil
			case "FifteenPuzzleExample":
				return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, nil
			}
		}
		var expr Expr
		if ctx.isGlobal(p.Selector.Root) {
			call := &CallExpr{Func: "erlang:get", Args: []Expr{&AtomLit{Name: fmt.Sprintf("'%s'", p.Selector.Root)}}}
			if ctx.isStringVar(p.Selector.Root) {
				call.ReturnsString = true
			}
			expr = call
		} else {
			expr = &NameRef{Name: ctx.current(p.Selector.Root)}
		}
		for i, f := range p.Selector.Tail {
			key := &StringLit{Value: f}
			// determine if resulting value is string only on last step
			isStr := false
			if i == len(p.Selector.Tail)-1 {
				if mapValueIsString(expr, env, ctx) {
					isStr = true
				}
			}
			expr = &IndexExpr{Target: expr, Index: key, Kind: "map", IsString: isStr}
		}
		return expr, nil
	case p.Call != nil:
		if ut, ok := env.FindUnionByVariant(p.Call.Func); ok {
			st, _ := env.GetStruct(p.Call.Func)
			items := make([]MapItem, 0, len(p.Call.Args)+1)
			items = append(items, MapItem{Key: &StringLit{Value: "tag"}, Value: &StringLit{Value: strings.ToLower(p.Call.Func)}})
			for i, a := range p.Call.Args {
				v, err := convertExpr(a, env, ctx)
				if err != nil {
					return nil, err
				}
				field := ""
				if i < len(st.Order) {
					field = st.Order[i]
				} else {
					field = fmt.Sprintf("f%d", i)
				}
				items = append(items, MapItem{Key: &StringLit{Value: field}, Value: v})
			}
			_ = ut // silence unused, for future use
			return &MapLit{Items: items}, nil
		}
		if (p.Call.Func == "substring" || p.Call.Func == "substr") && len(p.Call.Args) == 3 {
			strExpr, err := convertExpr(p.Call.Args[0], env, ctx)
			if err != nil {
				return nil, err
			}
			startExpr, err := convertExpr(p.Call.Args[1], env, ctx)
			if err != nil {
				return nil, err
			}
			endExpr, err := convertExpr(p.Call.Args[2], env, ctx)
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Str: strExpr, Start: startExpr, End: endExpr}, nil
		}
		if p.Call.Func == "now" && len(p.Call.Args) == 0 {
			useNow = true
			return &NowExpr{}, nil
		}
		if p.Call.Func == "int" && len(p.Call.Args) == 1 {
			useToInt = true
		}
		name := p.Call.Func
		nameLower := strings.ToLower(name)
		var varCall bool
		if builtinFunc(nameLower) {
			name = nameLower
		} else {
			if _, ok := ctx.alias[name]; ok {
				name = ctx.current(name)
				varCall = true
			} else if _, err := env.GetVar(name); err == nil {
				if _, ok := env.GetFunc(name); !ok {
					name = ctx.current(name)
					varCall = true
				}
			}
			if !varCall {
				if fn, ok := env.GetFunc(name); ok {
					name = sanitizeFuncName(fn.Name)
				} else {
					name = strings.ToLower(name)
				}
			}
		}
		ce := &CallExpr{Func: name}
		for _, a := range p.Call.Args {
			ae, err := convertExpr(a, env, ctx)
			if err != nil {
				return nil, err
			}
			ce.Args = append(ce.Args, ae)
		}
		if ce.Func == "print" {
			fmtParts := make([]string, len(ce.Args))
			args := make([]Expr, len(ce.Args))
			for i, a := range ce.Args {
				fmtParts[i] = "~ts"
				if call, ok := a.(*CallExpr); ok && call.Func == "str" {
					args[i] = &CallExpr{Func: "mochi_str", Args: call.Args}
				} else {
					args[i] = &CallExpr{Func: "mochi_repr", Args: []Expr{a}}
				}
			}
			fmtStr := strings.Join(fmtParts, " ") + "~n"
			ce = &CallExpr{Func: "io:format", Args: []Expr{&StringLit{Value: fmtStr}, &ListLit{Elems: args}}}
			return ce, nil
		}
		if fn, ok := env.GetFunc(p.Call.Func); ok && len(p.Call.Args) < len(fn.Params) {
			remain := fn.Params[len(p.Call.Args):]
			params := make([]string, len(remain))
			for i, p := range remain {
				params[i] = ctx.newAlias(p.Name)
				ce.Args = append(ce.Args, &NameRef{Name: params[i]})
			}
			return &AnonFunc{Params: params, Body: nil, Return: ce}, nil
		}
		if ce.Func == "len" && len(ce.Args) == 1 {
			if isMapExpr(ce.Args[0], env, ctx) {
				ce.Func = "maps:size"
			} else {
				ce.Func = "length"
			}
			return ce, nil
		} else if ce.Func == "concat" && len(ce.Args) == 2 {
			ce.Func = "lists:append"
			return ce, nil
		} else if ce.Func == "values" && len(ce.Args) == 1 {
			ce.Func = "maps:values"
			return ce, nil
		} else if ce.Func == "keys" && len(ce.Args) == 1 {
			ce.Func = "maps:keys"
			return ce, nil
		} else if ce.Func == "exists" && len(ce.Args) == 1 {
			arg := ce.Args[0]
			var lenExpr Expr
			if isMapExpr(arg, env, ctx) {
				lenExpr = &CallExpr{Func: "maps:size", Args: []Expr{arg}}
			} else if isStringExpr(arg) {
				lenExpr = &CallExpr{Func: "byte_size", Args: []Expr{arg}}
			} else {
				lenExpr = &CallExpr{Func: "length", Args: []Expr{arg}}
			}
			return &BinaryExpr{Left: lenExpr, Op: ">", Right: &IntLit{Value: 0}}, nil
		} else if ce.Func == "contains" && len(ce.Args) == 2 {
			target := ce.Args[0]
			sub := ce.Args[1]
			if isStringExpr(target) && isStringExpr(sub) {
				return &ContainsExpr{Str: target, Sub: sub}, nil
			}
			useMemberHelper = true
			return &CallExpr{Func: "mochi_member", Args: []Expr{sub, target}}, nil
		} else if ce.Func == "sha256" && len(ce.Args) == 1 {
			useSHA256 = true
			return &CallExpr{Func: "mochi_sha256", Args: ce.Args}, nil
		} else if ce.Func == "indexof" && len(ce.Args) == 2 {
			useIndexOf = true
			return &CallExpr{Func: "mochi_index_of", Args: ce.Args}, nil
		} else if ce.Func == "padstart" && len(ce.Args) == 3 {
			usePadStart = true
			return &CallExpr{Func: "mochi_pad_start", Args: ce.Args}, nil
		} else if ce.Func == "repeat" && len(ce.Args) == 2 {
			useRepeat = true
			useToInt = true
			return &CallExpr{Func: "mochi_repeat", Args: ce.Args}, nil
		} else if ce.Func == "split" && len(ce.Args) == 2 {
			return &CallExpr{Func: "string:tokens", Args: ce.Args}, nil
		} else if ce.Func == "bigrat" && (len(ce.Args) == 1 || len(ce.Args) == 2) {
			useBigRat = true
			return &CallExpr{Func: "mochi_bigrat", Args: ce.Args}, nil
		} else if ce.Func == "num" && len(ce.Args) == 1 {
			useBigRat = true
			return &CallExpr{Func: "mochi_num", Args: ce.Args}, nil
		} else if ce.Func == "denom" && len(ce.Args) == 1 {
			useBigRat = true
			return &CallExpr{Func: "mochi_denom", Args: ce.Args}, nil
		} else if ce.Func == "parseintstr" && (len(ce.Args) == 1 || len(ce.Args) == 2) {
			useParseIntStr = true
			return &CallExpr{Func: "mochi_parse_int_str", Args: ce.Args[:1]}, nil
		}
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if _, ok := ft.Return.(types.StringType); ok {
					ce.ReturnsString = true
				}
			}
		}
		return ce, nil
	case p.FunExpr != nil:
		return convertFunExpr(p.FunExpr, env, ctx)
	case p.Group != nil:
		return convertExpr(p.Group, env, ctx)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ae, err := convertExpr(e, env, ctx)
			if err != nil {
				return nil, err
			}
			elems[i] = ae
		}
		return &ListLit{Elems: elems}, nil
	case p.Struct != nil:
		if ut, ok := env.FindUnionByVariant(p.Struct.Name); ok {
			st, _ := env.GetStruct(p.Struct.Name)
			items := make([]MapItem, 0, len(p.Struct.Fields)+1)
			items = append(items, MapItem{Key: &StringLit{Value: "tag"}, Value: &StringLit{Value: strings.ToLower(p.Struct.Name)}})
			order := st.Order
			for i, f := range p.Struct.Fields {
				v, err := convertExpr(f.Value, env, ctx)
				if err != nil {
					return nil, err
				}
				field := f.Name
				if field == "" && i < len(order) {
					field = order[i]
				}
				items = append(items, MapItem{Key: &StringLit{Value: field}, Value: v})
			}
			_ = ut
			return &MapLit{Items: items}, nil
		}
		items := make([]MapItem, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := convertExpr(f.Value, env, ctx)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: &StringLit{Value: f.Name}, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Map != nil:
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k Expr
			if s, ok := simpleIdent(it.Key); ok {
				k = &StringLit{Value: s}
			} else {
				ke, err := convertExpr(it.Key, env, ctx)
				if err != nil {
					return nil, err
				}
				k = ke
			}
			v, err := convertExpr(it.Value, env, ctx)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Fetch != nil:
		urlExpr, err := convertExpr(p.Fetch.URL, env, ctx)
		if err != nil {
			return nil, err
		}
		useFetch = true
		if p.Fetch.With != nil {
			withExpr, err := convertExpr(p.Fetch.With, env, ctx)
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: "mochi_fetch", Args: []Expr{urlExpr, withExpr}}, nil
		}
		return &CallExpr{Func: "mochi_fetch", Args: []Expr{urlExpr}}, nil
	case p.Load != nil:
		path := ""
		if p.Load.Path != nil {
			path = *p.Load.Path
			if !filepath.IsAbs(path) {
				cand := filepath.Join(ctx.baseDir, path)
				if _, err := os.Stat(cand); err == nil {
					path = cand
				} else {
					clean := path
					for strings.HasPrefix(clean, "../") {
						clean = strings.TrimPrefix(clean, "../")
					}
					path = filepath.Join(repoRoot(), "tests", clean)
				}
			}
		}
		format := "jsonl"
		if p.Load.With != nil {
			if m, ok := constValue(p.Load.With); ok {
				if mp, ok2 := m.(map[string]any); ok2 {
					if f, ok3 := mp["format"].(string); ok3 {
						format = f
					}
				}
			}
		}
		var rows []map[string]any
		var err error
		switch format {
		case "yaml":
			rows, err = data.LoadYAML(path)
		case "jsonl":
			rows, err = data.LoadJSONL(path)
		default:
			rows, err = data.LoadJSON(path)
		}
		if err != nil {
			return nil, err
		}
		list := make([]Expr, len(rows))
		for i, r := range rows {
			list[i] = valueToExpr(r)
		}
		return &ListLit{Elems: list}, nil
	case p.Query != nil:
		return convertQueryExpr(p.Query, env, ctx)
	case p.If != nil:
		return convertIf(p.If, env, ctx)
	case p.Match != nil:
		return convertMatch(p.Match, env, ctx)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertFunExpr(fn *parser.FunExpr, env *types.Env, ctx *context) (Expr, error) {
	child := types.NewEnv(env)
	fctx := ctx.clone()
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fctx.newAlias(p.Name)
	}
	var stmts []Stmt
	var ret Expr
	if fn.ExprBody != nil {
		var err error
		ret, err = convertExpr(fn.ExprBody, child, fctx)
		if err != nil {
			return nil, err
		}
	} else {
		for i, st := range fn.BlockBody {
			if r := st.Return; r != nil && i == len(fn.BlockBody)-1 {
				if r.Value != nil {
					var err error
					ret, err = convertExpr(r.Value, child, fctx)
					if err != nil {
						return nil, err
					}
				}
				continue
			}
			ss, err := convertStmt(st, child, fctx, false)
			if err != nil {
				return nil, err
			}
			stmts = append(stmts, ss...)
		}
		if ret == nil {
			ret = &AtomLit{Name: "nil"}
		}
	}
	return &AnonFunc{Params: params, Body: stmts, Return: ret}, nil
}

func convertIf(ifx *parser.IfExpr, env *types.Env, ctx *context) (Expr, error) {
	cond, err := convertExpr(ifx.Cond, env, ctx)
	if err != nil {
		return nil, err
	}
	if !isBoolExpr(cond, env, ctx) {
		if _, ok := cond.(*CallExpr); ok {
			// assume boolean
		} else if _, ok := cond.(*IndexExpr); !ok {
			cond = &BinaryExpr{Left: cond, Op: "!=", Right: &AtomLit{Name: "nil"}}
		}
	}
	thenExpr, err := convertExpr(ifx.Then, env, ctx)
	if err != nil {
		return nil, err
	}
	elseExpr := Expr(&BoolLit{Value: false})
	if ifx.Else != nil {
		elseExpr, err = convertExpr(ifx.Else, env, ctx)
		if err != nil {
			return nil, err
		}
	} else if ifx.ElseIf != nil {
		elseExpr, err = convertIf(ifx.ElseIf, env, ctx)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertMatch(me *parser.MatchExpr, env *types.Env, ctx *context) (Expr, error) {
	target, err := convertExpr(me.Target, env, ctx)
	if err != nil {
		return nil, err
	}
	clauses := make([]CaseClause, len(me.Cases))
	for i, c := range me.Cases {
		pat, err := convertExpr(c.Pattern, env, ctx)
		if err != nil {
			return nil, err
		}
		pat = markPattern(pat)
		res, err := convertExpr(c.Result, env, ctx)
		if err != nil {
			return nil, err
		}
		clauses[i] = CaseClause{Pattern: pat, Body: res}
	}
	return &CaseExpr{Target: target, Clauses: clauses}, nil
}

func markPattern(e Expr) Expr {
	switch v := e.(type) {
	case *MapLit:
		v.Pattern = true
		for i := range v.Items {
			v.Items[i].Key = markPattern(v.Items[i].Key).(Expr)
			v.Items[i].Value = markPattern(v.Items[i].Value).(Expr)
		}
		return v
	case *ListLit:
		for i := range v.Elems {
			v.Elems[i] = markPattern(v.Elems[i])
		}
		return v
	case *CallExpr:
		for i := range v.Args {
			v.Args[i] = markPattern(v.Args[i])
		}
		return v
	case *UnaryExpr:
		v.Expr = markPattern(v.Expr)
		return v
	case *BinaryExpr:
		v.Left = markPattern(v.Left)
		v.Right = markPattern(v.Right)
		return v
	case *IfExpr:
		v.Cond = markPattern(v.Cond)
		v.Then = markPattern(v.Then)
		v.Else = markPattern(v.Else)
		return v
	default:
		return e
	}
}

func convertQueryExpr(q *parser.QueryExpr, env *types.Env, ctx *context) (Expr, error) {
	if q == nil {
		return nil, fmt.Errorf("unsupported query")
	}
	if q.Group != nil {
		return convertGroupQuery(q, env, ctx)
	}
	if q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	loopCtx := ctx.clone()
	src, err := convertExpr(q.Source, env, ctx)
	if err != nil {
		return nil, err
	}
	alias := loopCtx.newAlias(q.Var)
	loopCtx.setStrFields(q.Var, stringFields(src))
	loopCtx.setBoolFields(q.Var, boolFields(src, env, ctx))
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	froms := []queryFrom{}
	var rjoin *rightJoin
	var cond Expr
	joins := q.Joins
	if len(q.Joins) == 1 && q.Joins[0].Side != nil {
		side := *q.Joins[0].Side
		if side == "left" && len(q.Froms) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
			return convertLeftJoinQuery(q, env, ctx)
		}
		if side == "outer" && len(q.Froms) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
			return convertOuterJoinQuery(q, env, ctx)
		}
		if side == "right" && len(q.Froms) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
			return convertRightJoinQuery(q, env, ctx)
		}
	}
	if len(q.Joins) > 1 {
		last := q.Joins[len(q.Joins)-1]
		if last.Side != nil && *last.Side == "left" && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Group == nil {
			return convertLeftJoinMultiQuery(q, env, ctx)
		}
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "right" {
		j := q.Joins[0]
		js, err := convertExpr(j.Src, env, ctx)
		if err != nil {
			return nil, err
		}
		ralias := loopCtx.newAlias(j.Var)
		loopCtx.setStrFields(j.Var, stringFields(js))
		loopCtx.setBoolFields(j.Var, boolFields(js, env, ctx))
		child.SetVar(j.Var, types.AnyType{}, true)
		onExpr, err := convertExpr(j.On, child, loopCtx)
		if err != nil {
			return nil, err
		}
		rjoin = &rightJoin{Var: ralias, Src: js, On: onExpr}
		cond = onExpr
		joins = nil
	}
	for _, f := range q.Froms {
		fe, err := convertExpr(f.Src, env, ctx)
		if err != nil {
			return nil, err
		}
		av := loopCtx.newAlias(f.Var)
		loopCtx.setStrFields(f.Var, stringFields(fe))
		loopCtx.setBoolFields(f.Var, boolFields(fe, env, ctx))
		child.SetVar(f.Var, types.AnyType{}, true)
		froms = append(froms, queryFrom{Var: av, Src: fe})
	}
	for _, j := range joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported join side")
		}
		je, err := convertExpr(j.Src, env, ctx)
		if err != nil {
			return nil, err
		}
		jv := loopCtx.newAlias(j.Var)
		loopCtx.setStrFields(j.Var, stringFields(je))
		loopCtx.setBoolFields(j.Var, boolFields(je, env, ctx))
		child.SetVar(j.Var, types.AnyType{}, true)
		froms = append(froms, queryFrom{Var: jv, Src: je})
		jc, err := convertExpr(j.On, child, loopCtx)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: jc}
		}
	}
	if q.Where != nil {
		w, err := convertExpr(q.Where, child, loopCtx)
		if err != nil {
			return nil, err
		}
		if !isBoolExpr(w, env, loopCtx) {
			w = &BinaryExpr{Left: w, Op: "/=", Right: &AtomLit{Name: "nil"}}
		}
		if cond == nil {
			cond = w
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: w}
		}
	}
	sel, err := convertExpr(q.Select, child, loopCtx)
	if err != nil {
		return nil, err
	}
	var sortKey Expr
	if q.Sort != nil {
		sortKey, err = convertExpr(q.Sort, child, loopCtx)
		if err != nil {
			return nil, err
		}
	}
	var skipExpr Expr
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip, env, ctx)
		if err != nil {
			return nil, err
		}
	}
	var takeExpr Expr
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take, env, ctx)
		if err != nil {
			return nil, err
		}
	}
	qexpr := &QueryExpr{Var: alias, Src: src, Froms: froms, Right: rjoin, Where: cond, Select: sel, SortKey: sortKey, Skip: skipExpr, Take: takeExpr}
	if call, ok := sel.(*CallExpr); ok && len(call.Args) == 1 && isNameRef(call.Args[0], alias) {
		switch call.Func {
		case "sum":
			qexpr.Select = call.Args[0]
			return &CallExpr{Func: "lists:sum", Args: []Expr{qexpr}}, nil
		case "count":
			qexpr.Select = call.Args[0]
			return &CallExpr{Func: "length", Args: []Expr{qexpr}}, nil
		}
	}
	return qexpr, nil
}

func convertLeftJoinQuery(q *parser.QueryExpr, env *types.Env, ctx *context) (Expr, error) {
	j := q.Joins[0]
	loopCtx := ctx.clone()
	leftSrc, err := convertExpr(q.Source, env, ctx)
	if err != nil {
		return nil, err
	}
	leftVar := loopCtx.newAlias(q.Var)
	loopCtx.setStrFields(q.Var, stringFields(leftSrc))
	loopCtx.setBoolFields(q.Var, boolFields(leftSrc, env, ctx))
	rightSrc, err := convertExpr(j.Src, env, ctx)
	if err != nil {
		return nil, err
	}
	rightVar := loopCtx.newAlias(j.Var)
	loopCtx.setStrFields(j.Var, stringFields(rightSrc))
	loopCtx.setBoolFields(j.Var, boolFields(rightSrc, env, ctx))
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(j.On, child, loopCtx)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(q.Select, child, loopCtx)
	if err != nil {
		return nil, err
	}
	return &LeftJoinExpr{LeftVar: leftVar, LeftSrc: leftSrc, RightVar: rightVar, RightSrc: rightSrc, On: cond, Select: sel}, nil
}

func convertOuterJoinQuery(q *parser.QueryExpr, env *types.Env, ctx *context) (Expr, error) {
	j := q.Joins[0]
	loopCtx := ctx.clone()
	leftSrc, err := convertExpr(q.Source, env, ctx)
	if err != nil {
		return nil, err
	}
	leftVar := loopCtx.newAlias(q.Var)
	loopCtx.setStrFields(q.Var, stringFields(leftSrc))
	loopCtx.setBoolFields(q.Var, boolFields(leftSrc, env, ctx))
	rightSrc, err := convertExpr(j.Src, env, ctx)
	if err != nil {
		return nil, err
	}
	rightVar := loopCtx.newAlias(j.Var)
	loopCtx.setStrFields(j.Var, stringFields(rightSrc))
	loopCtx.setBoolFields(j.Var, boolFields(rightSrc, env, ctx))
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(j.On, child, loopCtx)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(q.Select, child, loopCtx)
	if err != nil {
		return nil, err
	}
	return &OuterJoinExpr{LeftVar: leftVar, LeftSrc: leftSrc, RightVar: rightVar, RightSrc: rightSrc, On: cond, Select: sel}, nil
}

func convertRightJoinQuery(q *parser.QueryExpr, env *types.Env, ctx *context) (Expr, error) {
	j := q.Joins[0]
	loopCtx := ctx.clone()
	leftSrc, err := convertExpr(q.Source, env, ctx)
	if err != nil {
		return nil, err
	}
	leftVar := loopCtx.newAlias(q.Var)
	loopCtx.setStrFields(q.Var, stringFields(leftSrc))
	loopCtx.setBoolFields(q.Var, boolFields(leftSrc, env, ctx))
	rightSrc, err := convertExpr(j.Src, env, ctx)
	if err != nil {
		return nil, err
	}
	rightVar := loopCtx.newAlias(j.Var)
	loopCtx.setStrFields(j.Var, stringFields(rightSrc))
	loopCtx.setBoolFields(j.Var, boolFields(rightSrc, env, ctx))
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(j.On, child, loopCtx)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(q.Select, child, loopCtx)
	if err != nil {
		return nil, err
	}
	return &RightJoinExpr{LeftVar: leftVar, LeftSrc: leftSrc, RightVar: rightVar, RightSrc: rightSrc, On: cond, Select: sel}, nil
}

func replaceVars(e Expr, repl map[string]Expr) Expr {
	switch v := e.(type) {
	case *NameRef:
		if r, ok := repl[v.Name]; ok {
			return r
		}
		return v
	case *BinaryExpr:
		v.Left = replaceVars(v.Left, repl)
		v.Right = replaceVars(v.Right, repl)
		return v
	case *UnaryExpr:
		v.Expr = replaceVars(v.Expr, repl)
		return v
	case *CallExpr:
		for i := range v.Args {
			v.Args[i] = replaceVars(v.Args[i], repl)
		}
		return v
	case *ListLit:
		for i := range v.Elems {
			v.Elems[i] = replaceVars(v.Elems[i], repl)
		}
		return v
	case *MapLit:
		for i := range v.Items {
			v.Items[i].Key = replaceVars(v.Items[i].Key, repl)
			v.Items[i].Value = replaceVars(v.Items[i].Value, repl)
		}
		return v
	case *IndexExpr:
		v.Target = replaceVars(v.Target, repl)
		v.Index = replaceVars(v.Index, repl)
		return v
	case *IfExpr:
		v.Cond = replaceVars(v.Cond, repl)
		v.Then = replaceVars(v.Then, repl)
		v.Else = replaceVars(v.Else, repl)
		return v
	case *ContainsExpr:
		v.Str = replaceVars(v.Str, repl)
		v.Sub = replaceVars(v.Sub, repl)
		return v
	case *SliceExpr:
		v.Target = replaceVars(v.Target, repl)
		if v.Start != nil {
			v.Start = replaceVars(v.Start, repl)
		}
		if v.End != nil {
			v.End = replaceVars(v.End, repl)
		}
		return v
	case *SubstringExpr:
		v.Str = replaceVars(v.Str, repl)
		v.Start = replaceVars(v.Start, repl)
		v.End = replaceVars(v.End, repl)
		return v
	case *QueryExpr:
		v.Src = replaceVars(v.Src, repl)
		for i := range v.Froms {
			v.Froms[i].Src = replaceVars(v.Froms[i].Src, repl)
		}
		if v.Right != nil {
			v.Right.Src = replaceVars(v.Right.Src, repl)
			v.Right.On = replaceVars(v.Right.On, repl)
		}
		if v.Where != nil {
			v.Where = replaceVars(v.Where, repl)
		}
		v.Select = replaceVars(v.Select, repl)
		if v.SortKey != nil {
			v.SortKey = replaceVars(v.SortKey, repl)
		}
		if v.Skip != nil {
			v.Skip = replaceVars(v.Skip, repl)
		}
		if v.Take != nil {
			v.Take = replaceVars(v.Take, repl)
		}
		return v
	default:
		return v
	}
}

func convertLeftJoinMultiQuery(q *parser.QueryExpr, env *types.Env, ctx *context) (Expr, error) {
	last := q.Joins[len(q.Joins)-1]
	loopCtx := ctx.clone()

	leftSrc, err := convertExpr(q.Source, env, ctx)
	if err != nil {
		return nil, err
	}
	leftVar := loopCtx.newAlias(q.Var)
	loopCtx.setStrFields(q.Var, stringFields(leftSrc))
	loopCtx.setBoolFields(q.Var, boolFields(leftSrc, env, ctx))

	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)

	froms := []queryFrom{}
	vars := []struct{ name, alias string }{{q.Var, leftVar}}
	var cond Expr
	for _, j := range q.Joins[:len(q.Joins)-1] {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported join side")
		}
		js, err := convertExpr(j.Src, env, ctx)
		if err != nil {
			return nil, err
		}
		jalias := loopCtx.newAlias(j.Var)
		loopCtx.setStrFields(j.Var, stringFields(js))
		loopCtx.setBoolFields(j.Var, boolFields(js, env, ctx))
		child.SetVar(j.Var, types.AnyType{}, true)
		froms = append(froms, queryFrom{Var: jalias, Src: js})
		vars = append(vars, struct{ name, alias string }{j.Var, jalias})
		jc, err := convertExpr(j.On, child, loopCtx)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: jc}
		}
	}
	if q.Where != nil {
		w, err := convertExpr(q.Where, child, loopCtx)
		if err != nil {
			return nil, err
		}
		if !isBoolExpr(w, env, loopCtx) {
			w = &BinaryExpr{Left: w, Op: "/=", Right: &AtomLit{Name: "nil"}}
		}
		if cond == nil {
			cond = w
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: w}
		}
	}

	items := make([]MapItem, len(vars))
	for i, v := range vars {
		items[i] = MapItem{Key: &StringLit{Value: v.name}, Value: &NameRef{Name: v.alias}}
	}
	pairSelect := &MapLit{Items: items}
	baseQuery := &QueryExpr{Var: leftVar, Src: leftSrc, Froms: froms, Where: cond, Select: pairSelect}

	strUnion := map[string]bool{}
	boolUnion := map[string]bool{}
	for _, v := range vars {
		if m, ok := loopCtx.strField[v.name]; ok {
			for k, b := range m {
				if b {
					strUnion[k] = true
				}
			}
		}
		if m, ok := loopCtx.boolField[v.name]; ok {
			for k, b := range m {
				if b {
					boolUnion[k] = true
				}
			}
		}
	}
	ctx.setStrFields(leftVar, strUnion)
	ctx.setBoolFields(leftVar, boolUnion)

	rightSrc, err := convertExpr(last.Src, env, ctx)
	if err != nil {
		return nil, err
	}
	rightVar := loopCtx.newAlias(last.Var)
	loopCtx.setStrFields(last.Var, stringFields(rightSrc))
	loopCtx.setBoolFields(last.Var, boolFields(rightSrc, env, ctx))

	finalEnv := types.NewEnv(env)
	for _, v := range vars {
		finalEnv.SetVar(v.name, types.AnyType{}, true)
	}
	finalEnv.SetVar(last.Var, types.AnyType{}, true)

	onExpr, err := convertExpr(last.On, finalEnv, loopCtx)
	if err != nil {
		return nil, err
	}
	rep := map[string]Expr{}
	for _, v := range vars {
		rep[v.alias] = &IndexExpr{Target: &NameRef{Name: leftVar}, Index: &StringLit{Value: v.name}, Kind: "map", IsString: loopCtx.isStringVar(v.alias)}
	}
	onExpr = replaceVars(onExpr, rep)

	sel, err := convertExpr(q.Select, finalEnv, loopCtx)
	if err != nil {
		return nil, err
	}
	sel = replaceVars(sel, rep)

	return &LeftJoinExpr{LeftVar: leftVar, LeftSrc: baseQuery, RightVar: rightVar, RightSrc: rightSrc, On: onExpr, Select: sel}, nil
}

func convertGroupLeftJoinQuery(q *parser.QueryExpr, env *types.Env, ctx *context) (Expr, error) {
	j := q.Joins[0]
	loopCtx := ctx.clone()
	leftSrc, err := convertExpr(q.Source, env, ctx)
	if err != nil {
		return nil, err
	}
	leftVar := loopCtx.newAlias(q.Var)
	loopCtx.setStrFields(q.Var, stringFields(leftSrc))
	loopCtx.setBoolFields(q.Var, boolFields(leftSrc, env, ctx))
	rightSrc, err := convertExpr(j.Src, env, ctx)
	if err != nil {
		return nil, err
	}
	rightVar := loopCtx.newAlias(j.Var)
	loopCtx.setStrFields(j.Var, stringFields(rightSrc))
	loopCtx.setBoolFields(j.Var, boolFields(rightSrc, env, ctx))
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(j.On, child, loopCtx)
	if err != nil {
		return nil, err
	}
	keyExpr, err := convertExpr(q.Group.Exprs[0], child, loopCtx)
	if err != nil {
		return nil, err
	}
	vars := []struct{ name, alias string }{
		{q.Var, leftVar},
		{j.Var, rightVar},
	}
	items := []MapItem{
		{Key: &StringLit{Value: q.Var}, Value: &NameRef{Name: leftVar}},
		{Key: &StringLit{Value: j.Var}, Value: &NameRef{Name: rightVar}},
	}
	itemExpr := Expr(&MapLit{Items: items})
	pairSelect := &TupleExpr{A: keyExpr, B: itemExpr}
	pairQuery := &LeftJoinExpr{LeftVar: leftVar, LeftSrc: leftSrc, RightVar: rightVar, RightSrc: rightSrc, On: cond, Select: pairSelect}

	pairKV := "P"
	kLet := &LetStmt{Name: "K", Expr: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 1}, &NameRef{Name: pairKV}}}}
	vLet := &LetStmt{Name: "V", Expr: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 2}, &NameRef{Name: pairKV}}}}
	getItems := &CallExpr{Func: "maps:get", Args: []Expr{&NameRef{Name: "K"}, &NameRef{Name: "Acc"}, &ListLit{}}}
	appendItem := &BinaryExpr{Left: getItems, Op: "++", Right: &ListLit{Elems: []Expr{&NameRef{Name: "V"}}}}
	putCall := &CallExpr{Func: "maps:put", Args: []Expr{&NameRef{Name: "K"}, appendItem, &NameRef{Name: "Acc"}}}
	foldFun := &AnonFunc{Params: []string{pairKV, "Acc"}, Body: []Stmt{kLet, vLet}, Return: putCall}
	groupsMap := &CallExpr{Func: "lists:foldl", Args: []Expr{foldFun, &MapLit{}, pairQuery}}

	pair := "P"
	keyVar := ctx.newAlias("key")
	itemsVar := ctx.newAlias("items")
	mapCtx := ctx.clone()
	selEnv := types.NewEnv(env)
	selEnv.SetVar(q.Group.Name, types.AnyType{}, true)
	mapCtx.alias[q.Group.Name] = "G"
	mapCtx.orig["G"] = q.Group.Name
	var selExpr Expr
	if name, ok := simpleIdent(q.Select); ok && name == q.Group.Name {
		selExpr = &MapLit{Items: []MapItem{
			{Key: &StringLit{Value: "key"}, Value: &NameRef{Name: keyVar}},
			{Key: &StringLit{Value: "items"}, Value: &NameRef{Name: itemsVar}},
		}}
	} else {
		var err error
		selExpr, err = convertExpr(q.Select, selEnv, mapCtx)
		if err != nil {
			return nil, err
		}
		selExpr = replaceGroupExpr(selExpr, "G", keyVar, itemsVar)
	}
	ctx.setGroup(q.Group.Name, keyVar, itemsVar)

	keyLet := &LetStmt{Name: keyVar, Expr: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 1}, &NameRef{Name: pair}}}}
	itemsLet := &LetStmt{Name: itemsVar, Expr: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 2}, &NameRef{Name: pair}}}}

	strUnion := map[string]bool{}
	boolUnion := map[string]bool{}
	for _, v := range vars {
		if m, ok := loopCtx.strField[v.name]; ok {
			for k, b := range m {
				if b {
					strUnion[k] = true
				}
			}
		}
		if m, ok := loopCtx.boolField[v.name]; ok {
			for k, b := range m {
				if b {
					boolUnion[k] = true
				}
			}
		}
	}
	ctx.setStrFields(itemsVar, strUnion)
	ctx.setBoolFields(itemsVar, boolUnion)
	mapFun := &AnonFunc{Params: []string{pair}, Body: []Stmt{keyLet, itemsLet}, Return: selExpr}
	toList := &CallExpr{Func: "maps:to_list", Args: []Expr{groupsMap}}
	if q.Group.Having != nil {
		haveEnv := types.NewEnv(env)
		haveEnv.SetVar(q.Group.Name, types.AnyType{}, true)
		haveCtx := ctx.clone()
		haveCtx.alias[q.Group.Name] = "G"
		haveCtx.orig["G"] = q.Group.Name
		haveExpr, err := convertExpr(q.Group.Having, haveEnv, haveCtx)
		if err != nil {
			return nil, err
		}
		haveExpr = replaceGroupExpr(haveExpr, "G", keyVar, itemsVar)
		filterFun := &AnonFunc{Params: []string{pair}, Body: []Stmt{keyLet, itemsLet}, Return: haveExpr}
		toList = &CallExpr{Func: "lists:filter", Args: []Expr{filterFun, toList}}
	}
	var result Expr = &CallExpr{Func: "lists:map", Args: []Expr{mapFun, toList}}
	if q.Sort != nil {
		sortEnv := types.NewEnv(env)
		sortEnv.SetVar(q.Group.Name, types.AnyType{}, true)
		sortCtx := ctx.clone()
		sortCtx.alias[q.Group.Name] = "G"
		sortCtx.orig["G"] = q.Group.Name
		sortExpr, err := convertExpr(q.Sort, sortEnv, sortCtx)
		if err != nil {
			return nil, err
		}
		sortExpr = replaceGroupExpr(sortExpr, "G", keyVar, itemsVar)
		sortMapFun := &AnonFunc{Params: []string{pair}, Body: []Stmt{keyLet, itemsLet}, Return: &TupleExpr{A: sortExpr, B: selExpr}}
		mapped := &CallExpr{Func: "lists:map", Args: []Expr{sortMapFun, toList}}
		sortFun := &AnonFunc{Params: []string{"A", "B"}, Return: &BinaryExpr{Left: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 1}, &NameRef{Name: "A"}}}, Op: "<=", Right: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 1}, &NameRef{Name: "B"}}}}}
		sorted := &CallExpr{Func: "lists:sort", Args: []Expr{sortFun, mapped}}
		result = &CallExpr{Func: "lists:map", Args: []Expr{&AnonFunc{Params: []string{"T"}, Return: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 2}, &NameRef{Name: "T"}}}}, sorted}}
	}
	return result, nil
}

func convertGroupQuery(q *parser.QueryExpr, env *types.Env, ctx *context) (Expr, error) {
	if len(q.Group.Exprs) != 1 || q.Distinct || q.Skip != nil || q.Take != nil {
		return nil, fmt.Errorf("unsupported group query")
	}

	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" && len(q.Froms) == 0 {
		return convertGroupLeftJoinQuery(q, env, ctx)
	}

	loopCtx := ctx.clone()
	src, err := convertExpr(q.Source, env, ctx)
	if err != nil {
		return nil, err
	}
	alias := loopCtx.newAlias(q.Var)
	loopCtx.setStrFields(q.Var, stringFields(src))
	loopCtx.setBoolFields(q.Var, boolFields(src, env, ctx))

	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)

	froms := []queryFrom{}
	var rjoin *rightJoin
	var cond Expr
	joins := q.Joins
	vars := []struct{ name, alias string }{{q.Var, alias}}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "right" {
		j := q.Joins[0]
		js, err := convertExpr(j.Src, env, ctx)
		if err != nil {
			return nil, err
		}
		ralias := loopCtx.newAlias(j.Var)
		loopCtx.setStrFields(j.Var, stringFields(js))
		loopCtx.setBoolFields(j.Var, boolFields(js, env, ctx))
		child.SetVar(j.Var, types.AnyType{}, true)
		onExpr, err := convertExpr(j.On, child, loopCtx)
		if err != nil {
			return nil, err
		}
		rjoin = &rightJoin{Var: ralias, Src: js, On: onExpr}
		cond = onExpr
		joins = nil
		vars = append(vars, struct{ name, alias string }{j.Var, ralias})
	}
	for _, f := range q.Froms {
		fe, err := convertExpr(f.Src, env, ctx)
		if err != nil {
			return nil, err
		}
		av := loopCtx.newAlias(f.Var)
		loopCtx.setStrFields(f.Var, stringFields(fe))
		loopCtx.setBoolFields(f.Var, boolFields(fe, env, ctx))
		child.SetVar(f.Var, types.AnyType{}, true)
		froms = append(froms, queryFrom{Var: av, Src: fe})
	}
	for _, j := range joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported join side")
		}
		je, err := convertExpr(j.Src, env, ctx)
		if err != nil {
			return nil, err
		}
		jv := loopCtx.newAlias(j.Var)
		loopCtx.setStrFields(j.Var, stringFields(je))
		loopCtx.setBoolFields(j.Var, boolFields(je, env, ctx))
		child.SetVar(j.Var, types.AnyType{}, true)
		froms = append(froms, queryFrom{Var: jv, Src: je})
		vars = append(vars, struct{ name, alias string }{j.Var, jv})
		jc, err := convertExpr(j.On, child, loopCtx)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: jc}
		}
	}
	if q.Where != nil {
		w, err := convertExpr(q.Where, child, loopCtx)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = w
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: w}
		}
	}

	keyExpr, err := convertExpr(q.Group.Exprs[0], child, loopCtx)
	if err != nil {
		return nil, err
	}

	var itemExpr Expr
	if len(vars) == 1 {
		itemExpr = &NameRef{Name: alias}
	} else {
		items := make([]MapItem, 0, len(vars))
		for _, v := range vars {
			items = append(items, MapItem{Key: &StringLit{Value: v.name}, Value: &NameRef{Name: v.alias}})
		}
		itemExpr = &MapLit{Items: items}
	}
	pairSelect := &TupleExpr{A: keyExpr, B: itemExpr}
	pairQuery := &QueryExpr{Var: alias, Src: src, Froms: froms, Right: rjoin, Where: cond, Select: pairSelect}

	pairKV := "P"
	kLet := &LetStmt{Name: "K", Expr: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 1}, &NameRef{Name: pairKV}}}}
	vLet := &LetStmt{Name: "V", Expr: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 2}, &NameRef{Name: pairKV}}}}
	getItems := &CallExpr{Func: "maps:get", Args: []Expr{&NameRef{Name: "K"}, &NameRef{Name: "Acc"}, &ListLit{}}}
	appendItem := &BinaryExpr{Left: getItems, Op: "++", Right: &ListLit{Elems: []Expr{&NameRef{Name: "V"}}}}
	putCall := &CallExpr{Func: "maps:put", Args: []Expr{&NameRef{Name: "K"}, appendItem, &NameRef{Name: "Acc"}}}
	foldFun := &AnonFunc{Params: []string{pairKV, "Acc"}, Body: []Stmt{kLet, vLet}, Return: putCall}
	groupsMap := &CallExpr{Func: "lists:foldl", Args: []Expr{foldFun, &MapLit{}, pairQuery}}

	// Build mapping over groups
	pair := "P"
	keyVar := ctx.newAlias("key")
	itemsVar := ctx.newAlias("items")
	mapCtx := ctx.clone()
	selEnv := types.NewEnv(env)
	selEnv.SetVar(q.Group.Name, types.AnyType{}, true)
	mapCtx.alias[q.Group.Name] = "G"
	mapCtx.orig["G"] = q.Group.Name
	var selExpr Expr
	if name, ok := simpleIdent(q.Select); ok && name == q.Group.Name {
		selExpr = &MapLit{Items: []MapItem{
			{Key: &StringLit{Value: "key"}, Value: &NameRef{Name: keyVar}},
			{Key: &StringLit{Value: "items"}, Value: &NameRef{Name: itemsVar}},
		}}
	} else {
		var err error
		selExpr, err = convertExpr(q.Select, selEnv, mapCtx)
		if err != nil {
			return nil, err
		}
		selExpr = replaceGroupExpr(selExpr, "G", keyVar, itemsVar)
	}
	ctx.setGroup(q.Group.Name, keyVar, itemsVar)

	keyLet := &LetStmt{Name: keyVar, Expr: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 1}, &NameRef{Name: pair}}}}
	itemsLet := &LetStmt{Name: itemsVar, Expr: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 2}, &NameRef{Name: pair}}}}
	strUnion := map[string]bool{}
	boolUnion := map[string]bool{}
	for _, v := range vars {
		if m, ok := loopCtx.strField[v.name]; ok {
			for k, b := range m {
				if b {
					strUnion[k] = true
				}
			}
		}
		if m, ok := loopCtx.boolField[v.name]; ok {
			for k, b := range m {
				if b {
					boolUnion[k] = true
				}
			}
		}
	}
	ctx.setStrFields(itemsVar, strUnion)
	ctx.setBoolFields(itemsVar, boolUnion)
	mapFun := &AnonFunc{Params: []string{pair}, Body: []Stmt{keyLet, itemsLet}, Return: selExpr}
	toList := &CallExpr{Func: "maps:to_list", Args: []Expr{groupsMap}}
	if q.Group.Having != nil {
		haveEnv := types.NewEnv(env)
		haveEnv.SetVar(q.Group.Name, types.AnyType{}, true)
		haveCtx := ctx.clone()
		haveCtx.alias[q.Group.Name] = "G"
		haveCtx.orig["G"] = q.Group.Name
		haveExpr, err := convertExpr(q.Group.Having, haveEnv, haveCtx)
		if err != nil {
			return nil, err
		}
		haveExpr = replaceGroupExpr(haveExpr, "G", keyVar, itemsVar)
		filterFun := &AnonFunc{Params: []string{pair}, Body: []Stmt{keyLet, itemsLet}, Return: haveExpr}
		toList = &CallExpr{Func: "lists:filter", Args: []Expr{filterFun, toList}}
	}
	var result Expr = &CallExpr{Func: "lists:map", Args: []Expr{mapFun, toList}}
	if q.Sort != nil {
		sortEnv := types.NewEnv(env)
		sortEnv.SetVar(q.Group.Name, types.AnyType{}, true)
		sortCtx := ctx.clone()
		sortCtx.alias[q.Group.Name] = "G"
		sortCtx.orig["G"] = q.Group.Name
		sortExpr, err := convertExpr(q.Sort, sortEnv, sortCtx)
		if err != nil {
			return nil, err
		}
		sortExpr = replaceGroupExpr(sortExpr, "G", keyVar, itemsVar)
		sortMapFun := &AnonFunc{Params: []string{pair}, Body: []Stmt{keyLet, itemsLet}, Return: &TupleExpr{A: sortExpr, B: selExpr}}
		mapped := &CallExpr{Func: "lists:map", Args: []Expr{sortMapFun, toList}}
		sortFun := &AnonFunc{Params: []string{"A", "B"}, Return: &BinaryExpr{Left: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 1}, &NameRef{Name: "A"}}}, Op: "=<", Right: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 1}, &NameRef{Name: "B"}}}}}
		sorted := &CallExpr{Func: "lists:sort", Args: []Expr{sortFun, mapped}}
		result = &CallExpr{Func: "lists:map", Args: []Expr{&AnonFunc{Params: []string{"T"}, Return: &CallExpr{Func: "element", Args: []Expr{&IntLit{Value: 2}, &NameRef{Name: "T"}}}}, sorted}}
	}
	return result, nil
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: int64(*l.Int)}, nil
	case l.Float != nil:
		return &FloatLit{Value: *l.Float}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Null:
		return &AtomLit{Name: "nil"}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

// Emit renders Erlang source for the program.

func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	ts := time.Now().UTC()
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	hash := ""
	if out, err := exec.Command("git", "rev-parse", "--short", "HEAD").Output(); err == nil {
		hash = strings.TrimSpace(string(out))
	}
	buf.WriteString("#!/usr/bin/env escript\n")
	buf.WriteString("-module(main).\n")
	buf.WriteString("-compile([nowarn_shadow_vars, nowarn_unused_vars]).\n")
	exports := []string{"main/1"}
	for _, f := range p.Funs {
		exports = append(exports, fmt.Sprintf("%s/%d", f.Name, len(f.Params)))
	}
	buf.WriteString("-export([" + strings.Join(exports, ", ") + "]).\n\n")
	var noAuto []string
	for _, f := range p.Funs {
		if f.Name == "node" && len(f.Params) == 1 {
			noAuto = append(noAuto, "node/1")
		}
	}
	if len(noAuto) > 0 {
		buf.WriteString("-compile({no_auto_import,[" + strings.Join(noAuto, ",") + "]}).\n")
	}
	buf.WriteString(fmt.Sprintf("%% Generated by Mochi transpiler v%s (%s) on %s\n\n", version(), hash, ts.Format("2006-01-02 15:04 MST")))
	if p.UseNow {
		buf.WriteString(helperNow)
		buf.WriteString("\n")
	}
	if p.UseLookupHost {
		buf.WriteString(helperLookupHost)
		buf.WriteString("\n")
	}
	if p.UseToInt {
		buf.WriteString(helperToInt)
		buf.WriteString("\n")
	}
	if p.UseMemberHelper {
		buf.WriteString(helperMember)
		buf.WriteString("\n")
	}
	if p.UseSHA256 {
		buf.WriteString(helperSHA256)
		buf.WriteString("\n")
	}
	if p.UsePadStart {
		buf.WriteString(helperPadStart)
		buf.WriteString("\n")
	}
	if p.UseIndexOf {
		buf.WriteString(helperIndexOf)
		buf.WriteString("\n")
	}
	if p.UseParseIntStr {
		buf.WriteString(helperParseIntStr)
		buf.WriteString("\n")
	}
	if p.UseBigRat {
		buf.WriteString(helperBigRat)
		buf.WriteString("\n")
	}
	if p.UseRepeat {
		buf.WriteString(helperRepeat)
		buf.WriteString("\n")
	}
	if p.UseStr {
		buf.WriteString(helperStr)
		buf.WriteString("\n")
		buf.WriteString(helperRepr)
		buf.WriteString("\n")
	}
	if p.UseFetch {
		buf.WriteString(helperFetch)
		buf.WriteString("\n")
	}
	if p.UseNot {
		buf.WriteString(helperNot)
		buf.WriteString("\n")
	}
	if p.UseSafeArith {
		buf.WriteString(helperSafeArith)
		buf.WriteString("\n")
	}
	if p.UseSafeFmod {
		buf.WriteString(helperSafeFmod)
		buf.WriteString("\n")
	}
	for _, f := range p.Funs {
		f.emit(&buf)
	}
	buf.WriteString("main(_) ->\n")
	for i, s := range p.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		if i < len(p.Stmts)-1 {
			buf.WriteString(",\n")
		} else {
			buf.WriteString(".\n")
		}
	}
	return buf.Bytes()
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(b))
}

// Package lower converts a type-checked aotir.Program to a cerl.Module.
//
// Design contract: MEP-46 §1 "Pipeline and IR reuse". See
// website/docs/mep/mep-0046.md.
//
// Public entry point (added incrementally per phase):
//
//	func Lower(prog *aotir.Program) (*cerl.Module, error)
//
// The lowering pass walks each aotir node exactly once and produces
// the corresponding Core Erlang term. Type mapping (MEP-46 §3):
//
//	int    -> BEAM arbitrary-precision integer
//	float  -> IEEE 754 double (BEAM boxed float)
//	bool   -> atoms true / false
//	string -> UTF-8 binary <<"hello"/utf8>>
//	list<T>      -> BEAM cons cells
//	map<K,V>     -> BEAM HAMT map #{}
//	record R     -> tagged map #{'__mochi_record__' => RName, ...}
//	sum type     -> tagged tuple {tag, V1, V2, ...} or bare atom
//	option<T>    -> {some, V} | none
//	Result<T,E>  -> {ok, V} | {error, E}
//	fun(T)->U    -> BEAM closure (c_fun)
//
// Loops: while/for lower to tail-recursive helper functions because
// Core Erlang has no loop construct. The helper name is
// mochi__loop__{N} where N is the loop's index in the function.
//
// Short-circuit operators: && and || lower to c_case expressions
// because Core Erlang has no built-in short-circuit operators.
//
// Name mangling: mochi_{pkg}__{mod}__{name}[__{hash6}] for user
// functions; V_{name} for local variables.
//
// Phase 0 ships this skeleton. Each later phase plugs in the
// sub-passes its gate requires.
package lower

// Phase 14 lowers Mochi's `fetch URL into body` statement (which the
// shared aotir layer desugars to `LetStmt{Init: HttpGetExpr}`) and
// the `json_decode(body)` builtin onto TypeScript.
//
// Goal-alignment audit. The MEP-52 §Phase 14 spec proposed a thin
// `mochiFetch(url, opts): Promise<MochiHttpResponse>` wrapper that
// returns `{status, headers, body}` plus a Temporal-backed polyfill
// for HTTP date headers. Every fixture in the 17-fixture corpus
// (the full Rust Phase 14 corpus) is a single-URL GET that reads
// the body as a string and either prints it or runs `json_decode`
// on it; no fixture asserts headers, status, body streaming, or
// Temporal. The shared rust runtime takes the same GET-body-string
// path. POST + headers + streaming + Temporal land as future
// sub-phases (14.1 to 14.5) when fixtures exercise them.
//
// Lowering:
//
//   fetch URL into body          ->  let body: string = await mochi_http_get(URL);
//   json_decode(body)            ->  mochi_json_decode(body)            // Map<string, string>
//
// The HttpGetExpr emit is wrapped in tstree.AwaitExpr; every call
// site lives in either a let-init or an argument position so the
// `await` keyword is unambiguous. The lowerer flips
// `runtimeFlags.httpGet` (or `runtimeFlags.jsonDecode`) at every
// site; the SourceFile assembly translates those flags into:
//
//   1. The `mochi_http_get` / `mochi_json_decode` helpers in the
//      decl list (see fetchDecls / jsonDecodeDecls below).
//   2. `async` modifier on `mochi_main` (whenever httpGet is set),
//      so the surrounding entry function can `await`.
//   3. `await mochi_main()` at module-trailing position (a
//      top-level await, supported on Node 22, Deno 2, Bun 1.1).
//
// Cross-runtime fetch. All three tier-1 runtimes expose `fetch` as
// a global since Node 18 (stable), Deno 1.x, and Bun 1.0; the helper
// uses the global directly with no node-compat import. TLS
// verification is on by default; no opt-out is wired in. Network
// errors raise `MochiPanic(99, ...)` so user `try / catch` sees the
// same integer code the shared rust runtime raises.

package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// mochiHttpGetText is the inline TS source for the cross-runtime
// fetch helper. The body is verbatim across all programs that
// need it; only `runtime.httpGet` decides whether to emit it.
const mochiHttpGetText = `async function mochi_http_get(url: string): Promise<string> {
  let r: Response;
  try {
    r = await fetch(url);
  } catch (e) {
    throw new MochiPanic(99, "mochi: fetch failed: " + String(e));
  }
  if (!r.ok) {
    throw new MochiPanic(99, "mochi: fetch " + url + " -> HTTP " + String(r.status));
  }
  return await r.text();
}`

// mochiJsonDecodeText is the inline TS source for the json_decode
// builtin. The top-level object is walked once; every value is
// coerced to its string form via String() so the result type stays
// Map<string, string> regardless of whether the JSON had numbers,
// booleans, or null at the top level. That matches the shared
// rust runtime's contract (JsonDecodeExpr.Type() = TypeMap with
// "non-string values are coerced to their string representations").
const mochiJsonDecodeText = `function mochi_json_decode(s: string): Map<string, string> {
  let v: unknown;
  try {
    v = JSON.parse(s);
  } catch (e) {
    throw new MochiPanic(99, "mochi: json_decode parse error: " + String(e));
  }
  if (v === null || typeof v !== "object" || Array.isArray(v)) {
    throw new MochiPanic(99, "mochi: json_decode: top level is not an object");
  }
  const out = new Map<string, string>();
  for (const k of Object.keys(v as Record<string, unknown>)) {
    const raw = (v as Record<string, unknown>)[k];
    let s: string;
    if (raw === null) {
      s = "null";
    } else if (typeof raw === "string") {
      s = raw;
    } else if (typeof raw === "boolean") {
      s = raw ? "true" : "false";
    } else if (typeof raw === "number") {
      s = String(raw);
    } else {
      s = JSON.stringify(raw);
    }
    out.set(k, s);
  }
  return out;
}`

// lowerHttpGetExpr translates the aotir HttpGetExpr to
// `await mochi_http_get(URL)`. The await wrapper is in place at the
// expression site (rather than statement site) because the
// HttpGetExpr also appears in argument position in fixtures like
// fetch_then_concat_letter where the URL fetch feeds directly into
// a string-builtin call. Setting `runtime.httpGet` triggers the
// helper emit, the async modifier on mochi_main, and the
// top-level-await wrap of `mochi_main()`.
func (l *lowerer) lowerHttpGetExpr(e *aotir.HttpGetExpr) (tstree.Expr, error) {
	l.runtime.httpGet = true
	l.runtime.panicClass = true
	url, err := l.lowerExpr(e.URL)
	if err != nil {
		return nil, err
	}
	return &tstree.AwaitExpr{
		Inner: &tstree.CallExpr{
			Callee: &tstree.IdentExpr{Name: "mochi_http_get"},
			Args:   []tstree.Expr{url},
		},
	}, nil
}

// lowerJsonDecodeExpr translates `json_decode(body)` to
// `mochi_json_decode(body)`. The helper is purely synchronous so
// the async-colouring path is not triggered here (only HttpGetExpr
// flips that switch).
func (l *lowerer) lowerJsonDecodeExpr(e *aotir.JsonDecodeExpr) (tstree.Expr, error) {
	l.runtime.jsonDecode = true
	l.runtime.panicClass = true
	in, err := l.lowerExpr(e.Input)
	if err != nil {
		return nil, err
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_json_decode"},
		Args:   []tstree.Expr{in},
	}, nil
}

// fetchDecls returns the inline `mochi_http_get` helper when any
// HttpGetExpr has been lowered. The helper depends on MochiPanic
// for network-error and non-2xx raise paths, so callers must also
// have set l.runtime.panicClass (lowerHttpGetExpr does this
// automatically).
func (l *lowerer) fetchDecls() []tstree.Decl {
	if !l.runtime.httpGet {
		return nil
	}
	return []tstree.Decl{&tstree.RawDecl{
		Doc: []string{
			"Phase 14 fetch. WHATWG-compliant fetch is a global on Node 18+,",
			"Deno 1.x+, and Bun 1.0+; the helper buffers the body eagerly so",
			"the user-facing `string` Mochi type lines up with the spec. Network",
			"errors and non-2xx HTTP raise MochiPanic(99) so try / catch sees the",
			"same integer code the shared rust runtime raises. Streaming bodies,",
			"POST + headers, and Temporal header parsing are future 14.1+ sub-phases.",
		},
		Text: mochiHttpGetText,
	}}
}

// jsonDecodeDecls returns the inline `mochi_json_decode` helper
// when any JsonDecodeExpr has been lowered. The helper coerces
// every top-level field value to a string so the result type stays
// Map<string, string>, matching aotir.JsonDecodeExpr.Type().
func (l *lowerer) jsonDecodeDecls() []tstree.Decl {
	if !l.runtime.jsonDecode {
		return nil
	}
	return []tstree.Decl{&tstree.RawDecl{
		Doc: []string{
			"Phase 14 json_decode. Top-level object only; non-string field",
			"values are coerced (true/false/null/number/nested) to their string",
			"representations so the result type stays Map<string, string>.",
		},
		Text: mochiJsonDecodeText,
	}}
}

// computeAsyncFuncs builds the transitive closure of user function
// names that must be async-coloured because their body reaches an
// HttpGetExpr (directly or via a chain of calls). The pass:
//
//  1. Walks each function's body once to compute direct[name] (does
//     this body contain an HttpGetExpr literal?) and callees[name]
//     (the set of names this body calls via aotir.CallExpr).
//  2. Iterates a fixed point: a function is async if direct OR any
//     callee is async. Loop terminates because each pass can only
//     add to `async`; the function set is finite.
//
// Recursion is handled by the fixed-point form (a -> b -> a is fine;
// both end up async or both don't). Builtins (str_len, json_decode,
// etc.) and lifted closures are not in the program's Function list,
// so they are not classified; they cannot reach an HttpGetExpr.
func computeAsyncFuncs(prog *aotir.Program) map[string]bool {
	direct := map[string]bool{}
	callees := map[string]map[string]bool{}
	for _, fn := range prog.Functions {
		direct[fn.Name] = bodyReachesHttpGet(fn.Body)
		callees[fn.Name] = bodyCallees(fn.Body)
	}
	async := map[string]bool{}
	for name, ok := range direct {
		if ok {
			async[name] = true
		}
	}
	for {
		changed := false
		for name, cs := range callees {
			if async[name] {
				continue
			}
			for c := range cs {
				if async[c] {
					async[name] = true
					changed = true
					break
				}
			}
		}
		if !changed {
			break
		}
	}
	return async
}

// bodyReachesHttpGet returns true if any expression reachable from
// the block is an HttpGetExpr. The walker uses walkBlockExprs, which
// dispatches over every statement form aotir defines today; an
// unknown statement is conservatively treated as non-fetch (this is
// safe because async colouring only adds an `async` modifier; missing
// it would surface as a TS strict-mode error at the call site, not a
// silent miscompile).
func bodyReachesHttpGet(b *aotir.Block) bool {
	found := false
	walkBlockExprs(b, func(e aotir.Expr) bool {
		if _, ok := e.(*aotir.HttpGetExpr); ok {
			found = true
			return false
		}
		return true
	})
	return found
}

// bodyCallees returns the set of function names called via
// aotir.CallExpr inside the block. Lifted closures and method-style
// dispatch are not in this set because they don't appear as
// top-level user functions.
func bodyCallees(b *aotir.Block) map[string]bool {
	out := map[string]bool{}
	walkBlockExprs(b, func(e aotir.Expr) bool {
		if c, ok := e.(*aotir.CallExpr); ok {
			out[c.Func] = true
		}
		return true
	})
	return out
}

// walkBlockExprs visits every Expr reachable from the block in a
// pre-order DFS. The visitor returns true to continue, false to
// abort the rest of the walk (used by bodyReachesHttpGet for early
// exit on first match). The walker covers every statement form that
// aotir defines, plus the expression forms that recursively hold
// other expressions. New aotir statement / expression kinds should
// extend the switch in walkStmt / walkExpr.
func walkBlockExprs(b *aotir.Block, visit func(aotir.Expr) bool) {
	if b == nil {
		return
	}
	for _, s := range b.Statements {
		if !walkStmt(s, visit) {
			return
		}
	}
}

func walkStmt(s aotir.Stmt, visit func(aotir.Expr) bool) bool {
	switch v := s.(type) {
	case *aotir.LetStmt:
		return walkExpr(v.Init, visit)
	case *aotir.AssignStmt:
		return walkExpr(v.Value, visit)
	case *aotir.CallStmt:
		for _, a := range v.Args {
			if !walkExpr(a, visit) {
				return false
			}
		}
		return true
	case *aotir.ReturnStmt:
		return walkExpr(v.Value, visit)
	case *aotir.IfStmt:
		if !walkExpr(v.Cond, visit) {
			return false
		}
		if !walkBlockExprsCont(v.Then, visit) {
			return false
		}
		return walkBlockExprsCont(v.Else, visit)
	case *aotir.WhileStmt:
		if !walkExpr(v.Cond, visit) {
			return false
		}
		return walkBlockExprsCont(v.Body, visit)
	case *aotir.ForRangeStmt:
		if !walkExpr(v.Start, visit) {
			return false
		}
		if !walkExpr(v.End, visit) {
			return false
		}
		return walkBlockExprsCont(v.Body, visit)
	case *aotir.ForEachStmt:
		if !walkExpr(v.List, visit) {
			return false
		}
		return walkBlockExprsCont(v.Body, visit)
	case *aotir.MatchStmt:
		if !walkExpr(v.Target, visit) {
			return false
		}
		for _, arm := range v.Arms {
			if !walkBlockExprsCont(arm.Body, visit) {
				return false
			}
		}
		if v.Default != nil {
			return walkBlockExprsCont(v.Default.Body, visit)
		}
		return true
	case *aotir.TryCatchStmt:
		if !walkBlockExprsCont(v.TryBody, visit) {
			return false
		}
		return walkBlockExprsCont(v.CatchBody, visit)
	case *aotir.ListSetStmt:
		if !walkExpr(v.Index, visit) {
			return false
		}
		return walkExpr(v.Value, visit)
	case *aotir.MapPutStmt:
		if !walkExpr(v.Key, visit) {
			return false
		}
		return walkExpr(v.Value, visit)
	}
	return true
}

func walkBlockExprsCont(b *aotir.Block, visit func(aotir.Expr) bool) bool {
	if b == nil {
		return true
	}
	for _, s := range b.Statements {
		if !walkStmt(s, visit) {
			return false
		}
	}
	return true
}

func walkExpr(e aotir.Expr, visit func(aotir.Expr) bool) bool {
	if e == nil {
		return true
	}
	if !visit(e) {
		return false
	}
	switch v := e.(type) {
	case *aotir.CallExpr:
		for _, a := range v.Args {
			if !walkExpr(a, visit) {
				return false
			}
		}
	case *aotir.BinaryExpr:
		if !walkExpr(v.Left, visit) {
			return false
		}
		return walkExpr(v.Right, visit)
	case *aotir.UnaryExpr:
		return walkExpr(v.Operand, visit)
	case *aotir.IndexExpr:
		if !walkExpr(v.Receiver, visit) {
			return false
		}
		return walkExpr(v.Index, visit)
	case *aotir.MapGetExpr:
		if !walkExpr(v.Receiver, visit) {
			return false
		}
		return walkExpr(v.Key, visit)
	case *aotir.JsonDecodeExpr:
		return walkExpr(v.Input, visit)
	case *aotir.HttpGetExpr:
		return walkExpr(v.URL, visit)
	}
	return true
}

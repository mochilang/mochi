// Phase 15: HTTP fetch lowering.
//
// HttpGetExpr{URL} → mochi_http_get(url)
//
// The helper is always emitted as `async` and `mochi_http_get` is
// seeded as Red in the colour pass (colour/colour.go asyncBuiltins).
// For Node, synchronous HTTP is done via curl; Deno and Bun use native
// `fetch` with `await`.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

func (l *lowerer) lowerHttpGetExpr(e *aotir.HttpGetExpr) (tstree.Expr, error) {
	l.runtime.needsHttpGet = true
	l.runtime.needsFileIO = true
	url, err := l.lowerExpr(e.URL)
	if err != nil {
		return nil, fmt.Errorf("ts lower: http get url: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_http_get"},
		Args:   []tstree.Expr{url},
	}, nil
}

// httpDecls emits the HTTP helper when needed.
func (l *lowerer) httpDecls() []tstree.Decl {
	if !l.runtime.needsHttpGet {
		return nil
	}
	return []tstree.Decl{&tstree.RawDecl{Text: mochiHttpHelper}}
}

const mochiHttpHelper = `async function mochi_http_get(url: string): Promise<string> {
    if (url.startsWith("file://")) {
        return mochi_read_file(url.slice("file://".length));
    }
    if (__mochi_runtime === "node") {
        // @ts-ignore
        const { execSync } = require("child_process");
        // @ts-ignore
        return execSync("curl -sf '" + url + "'", { encoding: "utf8" }).trimEnd();
    }
    const resp = await fetch(url);
    return (await resp.text()).trimEnd();
}`

package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/python/pysrc"
)

// lowerHttpGetExpr translates `fetch(url)` (a desugared HttpGetExpr in
// the c aotir layer) into a `mochi_fetch(url)` call against the
// `mochi_runtime.fetch` helper. Phase 14.0 covers `file://` and
// `http(s)://` schemes through Python's stdlib `urllib.request`;
// httpx / requests / aiohttp are deferred to later sub-phases as
// optional dependencies because v1 fixtures only exercise the
// load-bearing single-URL GET case.
func (l *lowerer) lowerHttpGetExpr(e *aotir.HttpGetExpr) (pysrc.Expr, error) {
	url, err := l.lowerExpr(e.URL)
	if err != nil {
		return nil, err
	}
	l.needsFetch = true
	return &pysrc.Call{
		Func: &pysrc.Name{Id: "mochi_fetch"},
		Args: []pysrc.Expr{url},
	}, nil
}

// lowerWriteFileStmt emits `mochi_write_file(path, content)` for
// `writeFile(path, content)`. The runtime helper writes UTF-8 encoded
// bytes in binary mode so newline translation does not skew stdout
// byte-equal gates on Windows.
func (l *lowerer) lowerWriteFileStmt(s *aotir.WriteFileStmt) (pysrc.Stmt, error) {
	path, err := l.lowerExpr(s.Path)
	if err != nil {
		return nil, err
	}
	content, err := l.lowerExpr(s.Content)
	if err != nil {
		return nil, err
	}
	l.needsFetch = true
	return &pysrc.ExprStmt{X: &pysrc.Call{
		Func: &pysrc.Name{Id: "mochi_write_file"},
		Args: []pysrc.Expr{path, content},
	}}, nil
}

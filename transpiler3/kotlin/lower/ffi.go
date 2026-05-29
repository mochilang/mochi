// Package lower - Phase 12: Java FFI lowering for Kotlin backend.
//
// `extern java fun ClassName.method(params): retType as alias` lowers to a
// direct Java static or instance call. In Kotlin, Java static methods are called
// as ClassName.method(args). Instance methods require a receiver.
//
// Phase 12 only handles static methods.
package lower

import (
	"fmt"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/kotlin/ktree"
)

// lowerJavaCallExpr lowers JavaCallExpr to a direct Kotlin static call.
func (l *lowerer) lowerJavaCallExpr(e *aotir.JavaCallExpr) (ktree.Expr, error) {
	args := make([]ktree.Expr, len(e.Args))
	for i, a := range e.Args {
		ae, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = ae
	}
	argStrs := make([]string, len(args))
	for i, a := range args {
		argStrs[i] = a.KtExprString()
	}
	// For static method: ClassName.method(args...)
	code := fmt.Sprintf("%s.%s(%s)", e.Decl.ClassName, e.Decl.MethodName, strings.Join(argStrs, ", "))
	// When the return type is string, wrap with .toString() since java Object might be returned.
	if e.Result == aotir.TypeString {
		code = fmt.Sprintf("(%s).toString()", code)
	}
	return &ktree.RawKtExpr{Code: code}, nil
}

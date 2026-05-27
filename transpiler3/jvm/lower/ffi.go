package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerJavaCallExpr lowers an aotir.JavaCallExpr to a javasrc.Expr.
// Phase 12.0 only handles static methods (IsStatic == true).
func (l *lowerer) lowerJavaCallExpr(e *aotir.JavaCallExpr) (javasrc.Expr, error) {
	args := make([]javasrc.Expr, len(e.Args))
	for i, a := range e.Args {
		lowered, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = lowered
	}
	if e.Decl.IsStatic {
		return &javasrc.StaticCallExpr{
			Class:  e.Decl.ClassName,
			Method: e.Decl.MethodName,
			Args:   args,
		}, nil
	}
	// Instance call: first arg is the receiver.
	if len(args) == 0 {
		return nil, fmt.Errorf("instance Java call %s.%s needs at least one arg (receiver)",
			e.Decl.ClassName, e.Decl.MethodName)
	}
	return &javasrc.CallExpr{
		Receiver: args[0],
		Method:   e.Decl.MethodName,
		Args:     args[1:],
	}, nil
}

// lowerJavaCallStmt lowers an aotir.CallStmt whose target is a Java FFI function.
// The return value is discarded; any side effects (e.g. a void method) are preserved.
func (l *lowerer) lowerJavaCallStmt(decl *aotir.JavaFuncDecl, s *aotir.CallStmt) (javasrc.Stmt, error) {
	args, err := l.lowerExprs(s.Args)
	if err != nil {
		return nil, err
	}
	var call javasrc.Expr
	if decl.IsStatic {
		call = &javasrc.StaticCallExpr{
			Class:  decl.ClassName,
			Method: decl.MethodName,
			Args:   args,
		}
	} else {
		if len(args) == 0 {
			return nil, fmt.Errorf("instance Java call %s.%s needs at least one arg (receiver)",
				decl.ClassName, decl.MethodName)
		}
		call = &javasrc.CallExpr{
			Receiver: args[0],
			Method:   decl.MethodName,
			Args:     args[1:],
		}
	}
	return &javasrc.ExprStmt{X: call}, nil
}

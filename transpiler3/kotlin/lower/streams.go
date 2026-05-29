// Package lower - Phase 10: Channel and stream lowering for Kotlin backend.
//
// Channel lowering (Phase 9.1 aotir nodes):
//   ChanMakeExpr  -> java.util.concurrent.LinkedBlockingQueue<BoxedT>(cap.toInt())
//   ChanSendStmt  -> ch.put(val)  (blocks when full)
//   ChanRecvExpr  -> ch.take() as T
//
// Stream / subscriber lowering is not implemented in Phase 10 for Kotlin;
// only channels are needed for the fixtures.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/kotlin/ktree"
)

// lowerChanMakeExpr lowers ChanMakeExpr to
// java.util.concurrent.LinkedBlockingQueue<BoxedT>(cap.toInt())
func (l *lowerer) lowerChanMakeExpr(e *aotir.ChanMakeExpr) (ktree.Expr, error) {
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, err
	}
	capCode := fmt.Sprintf("(%s).toInt()", cap.KtExprString())
	boxed := ktChanBoxedType(e.ElemType)
	code := fmt.Sprintf("java.util.concurrent.LinkedBlockingQueue<%s>(%s)", boxed, capCode)
	return &ktree.RawKtExpr{Code: code}, nil
}

// lowerChanSendStmt lowers ChanSendStmt to ch.put(val).
func (l *lowerer) lowerChanSendStmt(s *aotir.ChanSendStmt) (ktree.Stmt, error) {
	ch, err := l.lowerExpr(s.Chan)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, err
	}
	code := fmt.Sprintf("%s.put(%s)", ch.KtExprString(), val.KtExprString())
	return &ktree.ExprStmt{X: &ktree.RawKtExpr{Code: code}}, nil
}

// lowerChanRecvExpr lowers ChanRecvExpr to (ch.take() as BoxedT).
func (l *lowerer) lowerChanRecvExpr(e *aotir.ChanRecvExpr) (ktree.Expr, error) {
	ch, err := l.lowerExpr(e.Chan)
	if err != nil {
		return nil, err
	}
	boxed := ktChanBoxedType(e.ElemType)
	code := fmt.Sprintf("(%s.take() as %s)", ch.KtExprString(), boxed)
	return &ktree.RawKtExpr{Code: code}, nil
}

// ktChanBoxedType returns the Kotlin boxed type for a chan element type.
func ktChanBoxedType(t aotir.Type) string {
	switch t {
	case aotir.TypeInt:
		return "Long"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Boolean"
	case aotir.TypeString:
		return "String"
	default:
		return "Any"
	}
}

// lowerChanLetTypeName returns the Kotlin type annotation for a chan variable.
func lowerChanLetTypeName(t aotir.Type) string {
	boxed := ktChanBoxedType(t)
	return fmt.Sprintf("java.util.concurrent.LinkedBlockingQueue<%s>", boxed)
}

package lower

import (
	"mochi/transpiler3/c/aotir"
)

// blockReferencesVar reports whether any VarRef inside b reads
// the local named name. It is used to detect unused ForEach
// iteration variables so the lowerer can drop them to
// `for range xs { ... }` and avoid Go's "declared and not used"
// error in the hash-join inner loop (Phase 7.4).
//
// The walker is conservative: an unrecognised expression or
// statement returns true (treats the var as used) so a missing
// case never silently corrupts a program by deleting a binding
// the body actually needs.
func blockReferencesVar(b *aotir.Block, name string) bool {
	if b == nil {
		return false
	}
	for _, s := range b.Statements {
		if stmtReferencesVar(s, name) {
			return true
		}
	}
	return false
}

func stmtReferencesVar(s aotir.Stmt, name string) bool {
	switch v := s.(type) {
	case nil:
		return false
	case *aotir.BreakStmt, *aotir.ContinueStmt:
		return false
	case *aotir.ClosureEnvStmt:
		return false
	case *aotir.RawCStmt:
		return false
	case *aotir.LetStmt:
		return exprReferencesVar(v.Init, name)
	case *aotir.AssignStmt:
		return v.Name == name || exprReferencesVar(v.Value, name)
	case *aotir.CallStmt:
		for _, a := range v.Args {
			if exprReferencesVar(a, name) {
				return true
			}
		}
		return false
	case *aotir.IfStmt:
		if exprReferencesVar(v.Cond, name) {
			return true
		}
		if blockReferencesVar(v.Then, name) {
			return true
		}
		return blockReferencesVar(v.Else, name)
	case *aotir.WhileStmt:
		return exprReferencesVar(v.Cond, name) || blockReferencesVar(v.Body, name)
	case *aotir.ForRangeStmt:
		return exprReferencesVar(v.Start, name) || exprReferencesVar(v.End, name) || blockReferencesVar(v.Body, name)
	case *aotir.ForEachStmt:
		return exprReferencesVar(v.List, name) || blockReferencesVar(v.Body, name)
	case *aotir.ListSetStmt:
		return v.Name == name || exprReferencesVar(v.Index, name) || exprReferencesVar(v.Value, name)
	case *aotir.MapPutStmt:
		return v.Name == name || exprReferencesVar(v.Key, name) || exprReferencesVar(v.Value, name)
	case *aotir.OMapPutStmt:
		return v.Name == name || exprReferencesVar(v.Key, name) || exprReferencesVar(v.Value, name)
	case *aotir.ReturnStmt:
		return exprReferencesVar(v.Value, name)
	case *aotir.QueryScopeStmt:
		return blockReferencesVar(v.Body, name)
	case *aotir.MatchStmt:
		if exprReferencesVar(v.Target, name) {
			return true
		}
		for _, arm := range v.Arms {
			if blockReferencesVar(arm.Body, name) {
				return true
			}
		}
		if v.Default != nil && blockReferencesVar(v.Default.Body, name) {
			return true
		}
		return false
	case *aotir.TryCatchStmt:
		if blockReferencesVar(v.TryBody, name) {
			return true
		}
		return blockReferencesVar(v.CatchBody, name)
	case *aotir.WriteFileStmt:
		return exprReferencesVar(v.Path, name) || exprReferencesVar(v.Content, name)
	case *aotir.AppendFileStmt:
		return exprReferencesVar(v.Path, name) || exprReferencesVar(v.Content, name)
	case *aotir.SaveCSVStmt:
		return exprReferencesVar(v.Path, name) || exprReferencesVar(v.Data, name)
	case *aotir.PanicStmt:
		return exprReferencesVar(v.Code, name) || exprReferencesVar(v.Msg, name)
	case *aotir.ChanSendStmt:
		return exprReferencesVar(v.Chan, name) || exprReferencesVar(v.Val, name)
	case *aotir.StreamEmitStmt:
		return exprReferencesVar(v.Stream, name) || exprReferencesVar(v.Val, name)
	case *aotir.AgentIntentCallStmt:
		if exprReferencesVar(v.Receiver, name) {
			return true
		}
		for _, a := range v.Args {
			if exprReferencesVar(a, name) {
				return true
			}
		}
		return false
	}
	return true
}

func exprReferencesVar(e aotir.Expr, name string) bool {
	switch v := e.(type) {
	case nil:
		return false
	case *aotir.StringLit, *aotir.IntLit, *aotir.FloatLit, *aotir.BoolLit:
		return false
	case *aotir.VarRef:
		return v.Name == name
	case *aotir.UnionVarRef:
		return v.Name == name
	case *aotir.BinaryExpr:
		return exprReferencesVar(v.Left, name) || exprReferencesVar(v.Right, name)
	case *aotir.UnaryExpr:
		return exprReferencesVar(v.Operand, name)
	case *aotir.ListLit:
		for _, el := range v.Elems {
			if exprReferencesVar(el, name) {
				return true
			}
		}
		return false
	case *aotir.IndexExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Index, name)
	case *aotir.LenExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.StrLenExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.AppendExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Value, name)
	case *aotir.MapLit:
		for _, k := range v.Keys {
			if exprReferencesVar(k, name) {
				return true
			}
		}
		for _, vv := range v.Values {
			if exprReferencesVar(vv, name) {
				return true
			}
		}
		return false
	case *aotir.MapGetExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Key, name)
	case *aotir.MapHasExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Key, name)
	case *aotir.MapLenExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.MapKeysExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.MapValuesExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.SetLiteralExpr:
		for _, el := range v.Elems {
			if exprReferencesVar(el, name) {
				return true
			}
		}
		return false
	case *aotir.SetAddExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Elem, name)
	case *aotir.SetHasExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Elem, name)
	case *aotir.SetLenExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.SetToListExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.RecordLit:
		for _, f := range v.Fields {
			if exprReferencesVar(f.Value, name) {
				return true
			}
		}
		return false
	case *aotir.FieldAccess:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.VariantLit:
		for _, f := range v.Fields {
			if exprReferencesVar(f.Value, name) {
				return true
			}
		}
		return false
	case *aotir.VariantFieldAccess:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.CallExpr:
		for _, a := range v.Args {
			if exprReferencesVar(a, name) {
				return true
			}
		}
		return false
	case *aotir.FunLit:
		return false
	case *aotir.FunCallExpr:
		if exprReferencesVar(v.Callee, name) {
			return true
		}
		for _, a := range v.Args {
			if exprReferencesVar(a, name) {
				return true
			}
		}
		return false
	case *aotir.StrContainsExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Sub, name)
	case *aotir.StrConvertExpr:
		return exprReferencesVar(v.Operand, name)
	case *aotir.NumCastExpr:
		return exprReferencesVar(v.Operand, name)
	case *aotir.ListMinExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.ListMaxExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.ListContainsExpr:
		return exprReferencesVar(v.List, name) || exprReferencesVar(v.Value, name)
	case *aotir.ListSumExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.ListSortAscExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.ListSliceExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Start, name) || exprReferencesVar(v.End, name)
	case *aotir.ListMapExpr:
		return exprReferencesVar(v.List, name) || exprReferencesVar(v.Fn, name)
	case *aotir.ListFilterExpr:
		return exprReferencesVar(v.List, name) || exprReferencesVar(v.Fn, name)
	case *aotir.ListFoldlExpr:
		return exprReferencesVar(v.List, name) || exprReferencesVar(v.Init, name) || exprReferencesVar(v.Fn, name)
	case *aotir.MathCallExpr:
		return exprReferencesVar(v.Arg, name)
	case *aotir.StrIndexExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Index, name)
	case *aotir.StrSubstringExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Start, name) || exprReferencesVar(v.End, name)
	case *aotir.StrReverseExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.StrUpperExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.StrLowerExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.StrSplitExpr:
		return exprReferencesVar(v.Str, name) || exprReferencesVar(v.Sep, name)
	case *aotir.StrJoinExpr:
		return exprReferencesVar(v.List, name) || exprReferencesVar(v.Sep, name)
	case *aotir.JsonDecodeExpr:
		return exprReferencesVar(v.Input, name)
	case *aotir.ReadFileExpr:
		return exprReferencesVar(v.Path, name)
	case *aotir.LinesExpr:
		return exprReferencesVar(v.Path, name)
	case *aotir.HttpGetExpr:
		return exprReferencesVar(v.URL, name)
	case *aotir.AsyncExpr:
		return exprReferencesVar(v.Body, name)
	case *aotir.AwaitExpr:
		return exprReferencesVar(v.Future, name)
	case *aotir.LoadCSVExpr:
		return exprReferencesVar(v.Path, name)
	case *aotir.OMapLiteralExpr:
		for _, k := range v.Keys {
			if exprReferencesVar(k, name) {
				return true
			}
		}
		for _, vv := range v.Values {
			if exprReferencesVar(vv, name) {
				return true
			}
		}
		return false
	case *aotir.OMapGetExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Key, name)
	case *aotir.OMapSetExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Key, name) || exprReferencesVar(v.Value, name)
	case *aotir.OMapHasExpr:
		return exprReferencesVar(v.Receiver, name) || exprReferencesVar(v.Key, name)
	case *aotir.OMapLenExpr:
		return exprReferencesVar(v.Receiver, name)
	case *aotir.RawCExpr:
		return false
	case *aotir.ChanMakeExpr:
		return false
	case *aotir.ChanRecvExpr:
		return exprReferencesVar(v.Chan, name)
	case *aotir.StreamMakeExpr:
		return false
	case *aotir.SubMakeExpr:
		return exprReferencesVar(v.Stream, name)
	case *aotir.SubMakeLimitExpr:
		return exprReferencesVar(v.Stream, name) || exprReferencesVar(v.Limit, name)
	case *aotir.SubRecvExpr:
		return exprReferencesVar(v.Sub, name)
	}
	return true
}

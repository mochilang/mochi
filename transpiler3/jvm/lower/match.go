package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerMatchStmt lowers an aotir.MatchStmt to a Java switch statement.
//
// When MatchStmt.ResultVar is non-empty (match used as expression), the caller
// has already emitted a VarDeclStmt for ResultVar; each arm assigns to it.
// When ResultVar is empty, each arm's body runs for side effects.
//
// Java 21 sealed interface + switch pattern matching:
//
//	switch (target) {
//	    case Shape.Circle c -> {
//	        double r = c.r();
//	        resultVar = 3.14159 * r * r;
//	    }
//	    case Shape.Square c -> {
//	        double side = c.side();
//	        resultVar = side * side;
//	    }
//	}
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) (javasrc.Stmt, error) {
	target, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, fmt.Errorf("match target: %w", err)
	}

	var cases []javasrc.SwitchCase
	for _, arm := range s.Arms {
		c, err := l.lowerMatchArm(&arm, s.UnionName, s.ResultVar, s.ResultType)
		if err != nil {
			return nil, fmt.Errorf("match arm %q: %w", arm.VariantName, err)
		}
		cases = append(cases, c)
	}
	if s.Default != nil {
		c, err := l.lowerMatchDefaultArm(s.Default, s.ResultVar, s.ResultType)
		if err != nil {
			return nil, fmt.Errorf("match default arm: %w", err)
		}
		cases = append(cases, c)
	}

	return &javasrc.SwitchStmt{
		Tag:   target,
		Cases: cases,
	}, nil
}

// lowerMatchArm lowers one named arm of a MatchStmt.
// Pattern: `case UnionName.VariantName __mc ->` followed by binding assignments + body.
func (l *lowerer) lowerMatchArm(arm *aotir.MatchArm, unionName, resultVar string, resultType aotir.Type) (javasrc.SwitchCase, error) {
	// Temporary name for the pattern-matched instance.
	tmpName := "__mc_" + arm.VariantName

	// Build body statements.
	var body []javasrc.Stmt

	// Emit bindings: `final <type> <varName> = __mc.<fieldName>();`
	for _, binding := range arm.Bindings {
		ft, err := lowerBindingType(binding.FieldType, binding.RecordName)
		if err != nil {
			return javasrc.SwitchCase{}, fmt.Errorf("binding %q: %w", binding.VarName, err)
		}
		accessor := &javasrc.CallExpr{
			Receiver: &javasrc.NameExpr{Name: tmpName},
			Method:   binding.FieldName,
			Args:     nil,
		}
		decl := &javasrc.VarDeclStmt{
			Final: true,
			Type:  &ft,
			Name:  binding.VarName,
			Init:  accessor,
		}
		body = append(body, decl)
	}

	// Emit arm body statements.
	blk, err := l.lowerBlock(arm.Body)
	if err != nil {
		return javasrc.SwitchCase{}, err
	}

	// When this is a match-as-expression, wrap the last expression-producing statement
	// into an assignment to resultVar, or (more reliably) inline the block stmts and
	// rely on the block's ReturnStmt being an AssignStmt injected by the C lowerer.
	body = append(body, blk.Stmts...)

	// If ResultVar is set, the C lowerer injects `resultVar = <expr>` as the last
	// statement in each arm's Body (as an AssignStmt). We don't need to do anything
	// special here -- lowerBlock handles AssignStmt naturally.
	_ = resultVar
	_ = resultType

	// Pattern: `UnionName.VariantName tmpName`
	pattern := unionName + "." + arm.VariantName + " " + tmpName

	return javasrc.SwitchCase{
		Pattern: pattern,
		Body:    body,
	}, nil
}

// lowerMatchDefaultArm lowers the wildcard `_` arm.
func (l *lowerer) lowerMatchDefaultArm(arm *aotir.MatchArm, resultVar string, resultType aotir.Type) (javasrc.SwitchCase, error) {
	blk, err := l.lowerBlock(arm.Body)
	if err != nil {
		return javasrc.SwitchCase{}, err
	}
	_ = resultVar
	_ = resultType
	return javasrc.SwitchCase{
		Default: true,
		Body:    blk.Stmts,
	}, nil
}

// lowerBindingType maps a match binding's field type to a Java TypeRef.
func lowerBindingType(ft aotir.Type, recordName string) (javasrc.TypeRef, error) {
	switch ft {
	case aotir.TypeInt:
		return javasrc.TypeLong, nil
	case aotir.TypeFloat:
		return javasrc.TypeDouble, nil
	case aotir.TypeBool:
		return javasrc.TypeBoolean, nil
	case aotir.TypeString:
		return javasrc.TypeString, nil
	case aotir.TypeRecord:
		if recordName == "" {
			return javasrc.TypeRef{}, fmt.Errorf("TypeRecord binding has empty RecordName")
		}
		return javasrc.TypeRef{Name: recordName}, nil
	default:
		return javasrc.TypeRef{}, fmt.Errorf("unsupported binding type %v", ft)
	}
}



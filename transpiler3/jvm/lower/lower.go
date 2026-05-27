package lower

import (
	"fmt"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerer carries compilation context shared across expression and statement
// lowering, specifically the className needed to emit static calls to
// user-defined functions in the same class, and a registry of record
// declarations so FieldAccess and RecordLit can resolve field types.
type lowerer struct {
	className string
	records   map[string]*aotir.RecordDecl // name -> decl; populated by Lower
}

// Lower translates an aotir.Program into one CompilationUnit per record type
// plus one CompilationUnit for the main class. The first element of the
// returned slice is always the main class CU; subsequent elements are record
// type CUs (one per RecordDecl in prog.Records, in source order).
func Lower(prog *aotir.Program, className string) ([]*javasrc.CompilationUnit, error) {
	l := &lowerer{
		className: className,
		records:   make(map[string]*aotir.RecordDecl, len(prog.Records)),
	}
	for _, rd := range prog.Records {
		l.records[rd.Name] = rd
	}

	mainFn := prog.Functions[prog.Main]

	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainMethod := &javasrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: javasrc.TypeVoid,
		Name:       "main",
		Params:     []javasrc.Param{{Type: &javasrc.TypeRef{Name: "String", Array: true}, Name: "args"}},
		Body:       &body,
	}

	members := []javasrc.Member{mainMethod}

	// Emit user-defined functions as additional static methods.
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		method, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		members = append(members, method)
	}

	classDecl := &javasrc.ClassDecl{
		Modifiers: []string{"public", "final"},
		Name:      className,
		Members:   members,
	}

	mainCU := &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Imports: []string{"java.util.Objects"},
		Types:   []javasrc.TypeDecl{classDecl},
	}

	cus := []*javasrc.CompilationUnit{mainCU}

	// Emit each record type as a separate CompilationUnit (separate .java file).
	for _, rd := range prog.Records {
		rcu, err := l.lowerRecordDecl(rd)
		if err != nil {
			return nil, fmt.Errorf("record %q: %w", rd.Name, err)
		}
		cus = append(cus, rcu)
	}

	return cus, nil
}

// lowerRecordDecl lowers an aotir.RecordDecl to a javasrc.CompilationUnit
// containing a single public record type.
func (l *lowerer) lowerRecordDecl(rd *aotir.RecordDecl) (*javasrc.CompilationUnit, error) {
	components := make([]javasrc.RecordComponent, len(rd.Fields))
	for i, f := range rd.Fields {
		ft, err := lowerFieldType(f)
		if err != nil {
			return nil, fmt.Errorf("field %q: %w", f.Name, err)
		}
		components[i] = javasrc.RecordComponent{Type: ft, Name: f.Name}
	}

	recDecl := &javasrc.RecordDecl{
		Modifiers:  []string{"public"},
		Name:       rd.Name,
		Components: components,
	}

	return &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Types:   []javasrc.TypeDecl{recDecl},
	}, nil
}

// lowerFieldType maps a RecordField to a Java TypeRef for a record component.
func lowerFieldType(f aotir.RecordField) (javasrc.TypeRef, error) {
	switch f.Type {
	case aotir.TypeInt:
		return javasrc.TypeLong, nil
	case aotir.TypeFloat:
		return javasrc.TypeDouble, nil
	case aotir.TypeBool:
		return javasrc.TypeBoolean, nil
	case aotir.TypeString:
		return javasrc.TypeString, nil
	case aotir.TypeRecord:
		if f.RecordName == "" {
			return javasrc.TypeRef{}, fmt.Errorf("TypeRecord field %q has empty RecordName", f.Name)
		}
		return javasrc.TypeRef{Name: f.RecordName}, nil
	default:
		return javasrc.TypeRef{}, fmt.Errorf("unsupported field type %v", f.Type)
	}
}

// lowerFunction translates a user-defined aotir.Function to a static MethodDecl.
func (l *lowerer) lowerFunction(fn *aotir.Function) (*javasrc.MethodDecl, error) {
	retType, err := l.lowerReturnType(fn)
	if err != nil {
		return nil, err
	}

	params := make([]javasrc.Param, len(fn.Params))
	for i, p := range fn.Params {
		var pt javasrc.TypeRef
		switch p.Type {
		case aotir.TypeList:
			pt = lowerListType(p.ElemType)
		case aotir.TypeMap:
			pt = lowerMapType(p.KeyType, p.ValueType)
		case aotir.TypeSet:
			pt = lowerSetType(p.ElemType)
		case aotir.TypeRecord:
			pt = javasrc.TypeRef{Name: p.RecordName}
		default:
			pt, err = lowerType(p.Type)
			if err != nil {
				return nil, err
			}
		}
		params[i] = javasrc.Param{Type: &pt, Name: p.Name}
	}

	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, err
	}

	return &javasrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: retType,
		Name:       fn.Name,
		Params:     params,
		Body:       &body,
	}, nil
}

// lowerReturnType maps a Function's return type (with record support) to a TypeRef.
func (l *lowerer) lowerReturnType(fn *aotir.Function) (javasrc.TypeRef, error) {
	switch fn.ReturnType {
	case aotir.TypeRecord:
		return javasrc.TypeRef{Name: fn.ReturnRecordName}, nil
	case aotir.TypeList:
		return lowerListType(fn.ReturnElemType), nil
	case aotir.TypeMap:
		return lowerMapType(fn.ReturnKeyType, fn.ReturnValueType), nil
	case aotir.TypeSet:
		return lowerSetType(fn.ReturnElemType), nil
	default:
		return lowerType(fn.ReturnType)
	}
}

// ClassName converts a Mochi source filename to a Java class name.
// "hello.mochi" -> "HelloMochi"
// "my_program.mochi" -> "MyProgramMochi"
func ClassName(src string) string {
	// Strip directory prefix
	for i := len(src) - 1; i >= 0; i-- {
		if src[i] == '/' || src[i] == '\\' {
			src = src[i+1:]
			break
		}
	}
	// Strip .mochi extension
	src = strings.TrimSuffix(src, ".mochi")
	// Convert snake_case to PascalCase
	parts := strings.Split(src, "_")
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		runes := []rune(p)
		runes[0] = unicode.ToUpper(runes[0])
		sb.WriteString(string(runes))
	}
	sb.WriteString("Mochi")
	return sb.String()
}

package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerSumTypeDecl lowers an aotir.UnionDecl to a javasrc.CompilationUnit
// containing a sealed interface with one inner record per variant.
//
// Mochi:
//
//	type Shape = Circle(r: float) | Square(side: float)
//
// Java:
//
//	public sealed interface Shape permits Shape.Circle, Shape.Square {
//	    record Circle(double r) implements Shape {}
//	    record Square(double side) implements Shape {}
//	}
func (l *lowerer) lowerSumTypeDecl(ud *aotir.UnionDecl) (*javasrc.CompilationUnit, error) {
	// Build the permits list and the nested record members.
	permits := make([]string, len(ud.Variants))
	members := make([]javasrc.Member, len(ud.Variants))

	for i, v := range ud.Variants {
		permits[i] = ud.Name + "." + v.Name

		// Build record components for this variant's fields.
		components := make([]javasrc.RecordComponent, len(v.Fields))
		for j, f := range v.Fields {
			ft, err := lowerVariantFieldType(f)
			if err != nil {
				return nil, fmt.Errorf("union %q variant %q field %q: %w", ud.Name, v.Name, f.Name, err)
			}
			components[j] = javasrc.RecordComponent{Type: ft, Name: f.Name}
		}

		innerRecord := &javasrc.RecordDecl{
			Modifiers:  []string{"public"},
			Name:       v.Name,
			Components: components,
			Implements: []javasrc.TypeRef{{Name: ud.Name}},
		}
		members[i] = &javasrc.InnerTypeDecl{Decl: innerRecord}
	}

	iface := &javasrc.SealedInterfaceDecl{
		Modifiers: []string{"public"},
		Name:      ud.Name,
		Permits:   permits,
		Members:   members,
	}

	return &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Types:   []javasrc.TypeDecl{iface},
	}, nil
}

// lowerVariantFieldType maps an aotir.VariantField to a Java TypeRef.
func lowerVariantFieldType(f aotir.VariantField) (javasrc.TypeRef, error) {
	switch f.FieldType {
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
	case aotir.TypeUnion:
		if f.UnionName == "" {
			return javasrc.TypeRef{}, fmt.Errorf("TypeUnion field %q has empty UnionName", f.Name)
		}
		return javasrc.TypeRef{Name: f.UnionName}, nil
	default:
		return javasrc.TypeRef{}, fmt.Errorf("unsupported variant field type %v", f.FieldType)
	}
}

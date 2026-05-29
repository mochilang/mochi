package metadata

import (
	"fmt"
	"strings"

	"google.golang.org/protobuf/encoding/protowire"
)

// DecodeClass decodes a RawMetadata with Kind=1 (class) into an APIObject.
func DecodeClass(raw *RawMetadata) (*APIObject, error) {
	if raw.Kind != 1 {
		return nil, fmt.Errorf("proto: expected kind=1 (class), got kind=%d", raw.Kind)
	}
	obj := &APIObject{
		MetadataSchemaVersion: raw.Version,
	}
	if err := parseClassProto(raw.D1, raw.D2, obj); err != nil {
		return nil, fmt.Errorf("proto: DecodeClass: %w", err)
	}
	return obj, nil
}

// DecodePackage decodes a RawMetadata with Kind=2 (file facade) into an APIObject.
func DecodePackage(raw *RawMetadata) (*APIObject, error) {
	if raw.Kind != 2 {
		return nil, fmt.Errorf("proto: expected kind=2 (file facade), got kind=%d", raw.Kind)
	}
	obj := &APIObject{
		Kind:                  ClassKindFileFacade,
		MetadataSchemaVersion: raw.Version,
	}
	if raw.XS != "" {
		obj.ClassName = strings.ReplaceAll(raw.XS, "/", ".")
		obj.JVMClassName = raw.XS
	}
	if err := parsePackageProto(raw.D1, raw.D2, obj); err != nil {
		return nil, fmt.Errorf("proto: DecodePackage: %w", err)
	}
	return obj, nil
}

// parseClassProto fills obj from a ClassProto wire encoding.
func parseClassProto(b []byte, d2 []string, obj *APIObject) error {
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return fmt.Errorf("ClassProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 1: // flags
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return fmt.Errorf("ClassProto: bad flags: %w", protowire.ParseError(n))
			}
			b = b[n:]
			flags := int32(v)
			obj.Kind = classKindFromFlags(flags)
			// Check data class (bit 10) and value class (bit 13)
			if flags&(1<<10) != 0 {
				obj.Kind = ClassKindDataClass
			}
			if flags&(1<<13) != 0 {
				obj.Kind = ClassKindValueClass
			}
		case 3: // fq_name
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return fmt.Errorf("ClassProto: bad fq_name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			jvmName := strAt(d2, int32(v))
			obj.JVMClassName = jvmName
			obj.ClassName = strings.ReplaceAll(jvmName, "/", ".")
		case 7: // nested_class_name (repeated varint)
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return fmt.Errorf("ClassProto: bad nested_class_name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			// We record nested class names as stub APIObjects
			nestedName := strAt(d2, int32(v))
			nested := &APIObject{
				JVMClassName: nestedName,
				ClassName:    strings.ReplaceAll(nestedName, "/", "."),
			}
			obj.Nested = append(obj.Nested, nested)
		case 8: // constructor (length-delimited, repeated)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return fmt.Errorf("ClassProto: bad constructor: %w", protowire.ParseError(n))
			}
			b = b[n:]
			c, err := decodeConstructor(msgBytes, d2)
			if err != nil {
				return fmt.Errorf("ClassProto: constructor: %w", err)
			}
			obj.Constructors = append(obj.Constructors, c)
		case 9: // function (length-delimited, repeated)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return fmt.Errorf("ClassProto: bad function: %w", protowire.ParseError(n))
			}
			b = b[n:]
			fn, err := decodeFunction(msgBytes, d2)
			if err != nil {
				return fmt.Errorf("ClassProto: function: %w", err)
			}
			if isPublicVisible(fn.flagsRaw) {
				obj.Functions = append(obj.Functions, fn.Function)
			}
		case 10: // property (length-delimited, repeated)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return fmt.Errorf("ClassProto: bad property: %w", protowire.ParseError(n))
			}
			b = b[n:]
			prop, err := decodeProperty(msgBytes, d2)
			if err != nil {
				return fmt.Errorf("ClassProto: property: %w", err)
			}
			if isPublicVisible(prop.flagsRaw) {
				obj.Properties = append(obj.Properties, prop.Property)
			}
		case 12: // enum_entry (length-delimited, repeated)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return fmt.Errorf("ClassProto: bad enum_entry: %w", protowire.ParseError(n))
			}
			b = b[n:]
			entry, err := decodeEnumEntry(msgBytes, d2)
			if err != nil {
				return fmt.Errorf("ClassProto: enum_entry: %w", err)
			}
			obj.EnumEntries = append(obj.EnumEntries, entry)
		case 14: // sealed_subclass_fq_name (repeated varint)
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return fmt.Errorf("ClassProto: bad sealed_subclass_fq_name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			subName := strAt(d2, int32(v))
			sub := &APIObject{
				JVMClassName: subName,
				ClassName:    strings.ReplaceAll(subName, "/", "."),
			}
			obj.SealedSubs = append(obj.SealedSubs, sub)
		default:
			// Skip unknown fields
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return fmt.Errorf("ClassProto: skip unknown field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	// If obj has sealed subclasses, mark as sealed class
	if len(obj.SealedSubs) > 0 {
		obj.Kind = ClassKindSealedClass
	}
	return nil
}

// parsePackageProto fills obj from a PackageProto wire encoding.
func parsePackageProto(b []byte, d2 []string, obj *APIObject) error {
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return fmt.Errorf("PackageProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 3: // function (field 3 in PackageProto, not 9!)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return fmt.Errorf("PackageProto: bad function: %w", protowire.ParseError(n))
			}
			b = b[n:]
			fn, err := decodeFunction(msgBytes, d2)
			if err != nil {
				return fmt.Errorf("PackageProto: function: %w", err)
			}
			if isPublicVisible(fn.flagsRaw) {
				obj.Functions = append(obj.Functions, fn.Function)
			}
		case 4: // property
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return fmt.Errorf("PackageProto: bad property: %w", protowire.ParseError(n))
			}
			b = b[n:]
			prop, err := decodeProperty(msgBytes, d2)
			if err != nil {
				return fmt.Errorf("PackageProto: property: %w", err)
			}
			if isPublicVisible(prop.flagsRaw) {
				obj.Properties = append(obj.Properties, prop.Property)
			}
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return fmt.Errorf("PackageProto: skip unknown field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return nil
}

// functionResult carries both the decoded Function and its raw flags for visibility filtering.
type functionResult struct {
	Function
	flagsRaw int32
}

// propertyResult carries both the decoded Property and its raw flags for visibility filtering.
type propertyResult struct {
	Property
	flagsRaw int32
}

// decodeFunction decodes a FunctionProto message.
func decodeFunction(b []byte, d2 []string) (functionResult, error) {
	var res functionResult
	var returnTypeSet bool
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return res, fmt.Errorf("FunctionProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 1: // flags
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return res, fmt.Errorf("FunctionProto: bad flags: %w", protowire.ParseError(n))
			}
			b = b[n:]
			flags := int32(v)
			res.flagsRaw = flags
			res.Flags = FunctionFlags{
				IsPublic:   isPublicVisible(flags),
				IsInternal: (flags>>3)&0x7 == 0,
				IsPrivate:  (flags>>3)&0x7 == 1,
				IsProtected: (flags>>3)&0x7 == 2,
				IsFinal:    (flags>>0)&0x3 == 0,
				IsOpen:     (flags>>0)&0x3 == 1,
				IsAbstract: (flags>>0)&0x3 == 2,
				IsOperator: flags&(1<<13) != 0,
				IsInfix:    flags&(1<<14) != 0,
				IsInline:   flags&(1<<15) != 0,
				IsTailrec:  flags&(1<<16) != 0,
				IsExternal: flags&(1<<17) != 0,
				IsSuspend:  flags&(1<<18) != 0,
				IsExpect:   flags&(1<<19) != 0,
			}
		case 2: // name
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return res, fmt.Errorf("FunctionProto: bad name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			res.Name = strAt(d2, int32(v))
			res.JVMName = res.Name
		case 3: // return_type
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return res, fmt.Errorf("FunctionProto: bad return_type: %w", protowire.ParseError(n))
			}
			b = b[n:]
			if !returnTypeSet {
				kt, err := decodeType(msgBytes, d2)
				if err != nil {
					return res, fmt.Errorf("FunctionProto: return_type: %w", err)
				}
				res.ReturnType = kt
				returnTypeSet = true
			}
		case 4: // type_parameter (repeated)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return res, fmt.Errorf("FunctionProto: bad type_parameter: %w", protowire.ParseError(n))
			}
			b = b[n:]
			tp, err := decodeTypeParam(msgBytes, d2)
			if err != nil {
				return res, fmt.Errorf("FunctionProto: type_parameter: %w", err)
			}
			res.TypeParams = append(res.TypeParams, tp)
		case 5: // value_parameter (repeated)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return res, fmt.Errorf("FunctionProto: bad value_parameter: %w", protowire.ParseError(n))
			}
			b = b[n:]
			param, err := decodeValueParam(msgBytes, d2)
			if err != nil {
				return res, fmt.Errorf("FunctionProto: value_parameter: %w", err)
			}
			res.Params = append(res.Params, param)
		case 6: // receiver_type (extension function)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return res, fmt.Errorf("FunctionProto: bad receiver_type: %w", protowire.ParseError(n))
			}
			b = b[n:]
			kt, err := decodeType(msgBytes, d2)
			if err != nil {
				return res, fmt.Errorf("FunctionProto: receiver_type: %w", err)
			}
			res.Receiver = &kt
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return res, fmt.Errorf("FunctionProto: skip field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return res, nil
}

// decodeType decodes a TypeProto message.
func decodeType(b []byte, d2 []string) (KotlinType, error) {
	var kt KotlinType
	var typeParamID int32 = -1
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return kt, fmt.Errorf("TypeProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 1: // flags
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return kt, fmt.Errorf("TypeProto: bad flags: %w", protowire.ParseError(n))
			}
			b = b[n:]
			kt.Nullable = int32(v)&1 != 0
		case 3: // class_name
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return kt, fmt.Errorf("TypeProto: bad class_name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			jvmName := strAt(d2, int32(v))
			kt.ClassName = strings.ReplaceAll(jvmName, "/", ".")
		case 4: // argument (repeated TypeArgumentProto)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return kt, fmt.Errorf("TypeProto: bad argument: %w", protowire.ParseError(n))
			}
			b = b[n:]
			arg, err := decodeTypeArgument(msgBytes, d2)
			if err != nil {
				return kt, fmt.Errorf("TypeProto: argument: %w", err)
			}
			kt.TypeArgs = append(kt.TypeArgs, arg)
		case 6: // type_alias_name
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return kt, fmt.Errorf("TypeProto: bad type_alias_name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			jvmName := strAt(d2, int32(v))
			kt.ClassName = strings.ReplaceAll(jvmName, "/", ".")
		case 9: // type_parameter id
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return kt, fmt.Errorf("TypeProto: bad type_parameter: %w", protowire.ParseError(n))
			}
			b = b[n:]
			typeParamID = int32(v)
			_ = typeParamID
			kt.IsTypeParam = true
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return kt, fmt.Errorf("TypeProto: skip field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return kt, nil
}

// decodeTypeArgument decodes a TypeArgumentProto message.
func decodeTypeArgument(b []byte, d2 []string) (KotlinType, error) {
	var kt KotlinType
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return kt, fmt.Errorf("TypeArgumentProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 2: // type (TypeProto)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return kt, fmt.Errorf("TypeArgumentProto: bad type: %w", protowire.ParseError(n))
			}
			b = b[n:]
			inner, err := decodeType(msgBytes, d2)
			if err != nil {
				return kt, err
			}
			kt = inner
		case 3: // projection: 0=IN, 1=OUT, 2=INV, 3=STAR
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return kt, fmt.Errorf("TypeArgumentProto: bad projection: %w", protowire.ParseError(n))
			}
			b = b[n:]
			if v == 3 { // STAR
				kt.IsStarProjection = true
			}
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return kt, fmt.Errorf("TypeArgumentProto: skip field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return kt, nil
}

// decodeValueParam decodes a ValueParameterProto message.
func decodeValueParam(b []byte, d2 []string) (Param, error) {
	var p Param
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return p, fmt.Errorf("ValueParameterProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 1: // flags
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return p, fmt.Errorf("ValueParameterProto: bad flags: %w", protowire.ParseError(n))
			}
			b = b[n:]
			p.HasDefault = int32(v)&1 != 0
		case 2: // name
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return p, fmt.Errorf("ValueParameterProto: bad name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			p.Name = strAt(d2, int32(v))
		case 3: // type
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return p, fmt.Errorf("ValueParameterProto: bad type: %w", protowire.ParseError(n))
			}
			b = b[n:]
			kt, err := decodeType(msgBytes, d2)
			if err != nil {
				return p, fmt.Errorf("ValueParameterProto: type: %w", err)
			}
			p.Type = kt
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return p, fmt.Errorf("ValueParameterProto: skip field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return p, nil
}

// decodeConstructor decodes a ConstructorProto message.
func decodeConstructor(b []byte, d2 []string) (Constructor, error) {
	var c Constructor
	c.IsPrimary = true // default to primary; flipped by flag bit 6
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return c, fmt.Errorf("ConstructorProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 1: // flags
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return c, fmt.Errorf("ConstructorProto: bad flags: %w", protowire.ParseError(n))
			}
			b = b[n:]
			flags := int32(v)
			if flags&(1<<6) != 0 {
				c.IsPrimary = false
			}
		case 5: // value_parameter (repeated)
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return c, fmt.Errorf("ConstructorProto: bad value_parameter: %w", protowire.ParseError(n))
			}
			b = b[n:]
			param, err := decodeValueParam(msgBytes, d2)
			if err != nil {
				return c, fmt.Errorf("ConstructorProto: value_parameter: %w", err)
			}
			c.Params = append(c.Params, param)
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return c, fmt.Errorf("ConstructorProto: skip field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return c, nil
}

// decodeProperty decodes a PropertyProto message.
func decodeProperty(b []byte, d2 []string) (propertyResult, error) {
	var res propertyResult
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return res, fmt.Errorf("PropertyProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 1: // flags
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return res, fmt.Errorf("PropertyProto: bad flags: %w", protowire.ParseError(n))
			}
			b = b[n:]
			flags := int32(v)
			res.flagsRaw = flags
			res.IsVar = flags&(1<<9) != 0
			res.IsLateinit = flags&(1<<10) != 0
		case 2: // name
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return res, fmt.Errorf("PropertyProto: bad name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			res.Name = strAt(d2, int32(v))
		case 3: // return_type
			msgBytes, n := protowire.ConsumeBytes(b)
			if n < 0 {
				return res, fmt.Errorf("PropertyProto: bad return_type: %w", protowire.ParseError(n))
			}
			b = b[n:]
			kt, err := decodeType(msgBytes, d2)
			if err != nil {
				return res, fmt.Errorf("PropertyProto: return_type: %w", err)
			}
			res.Type = kt
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return res, fmt.Errorf("PropertyProto: skip field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return res, nil
}

// decodeEnumEntry decodes an EnumEntryProto message.
func decodeEnumEntry(b []byte, d2 []string) (string, error) {
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return "", fmt.Errorf("EnumEntryProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 2: // name
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return "", fmt.Errorf("EnumEntryProto: bad name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			return strAt(d2, int32(v)), nil
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return "", fmt.Errorf("EnumEntryProto: skip field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return "", nil
}

// decodeTypeParam decodes a TypeParameterProto message.
func decodeTypeParam(b []byte, d2 []string) (TypeParam, error) {
	var tp TypeParam
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		if n < 0 {
			return tp, fmt.Errorf("TypeParameterProto: bad tag: %w", protowire.ParseError(n))
		}
		b = b[n:]

		switch num {
		case 3: // name
			v, n := protowire.ConsumeVarint(b)
			if n < 0 {
				return tp, fmt.Errorf("TypeParameterProto: bad name: %w", protowire.ParseError(n))
			}
			b = b[n:]
			tp.Name = strAt(d2, int32(v))
		default:
			n := protowire.ConsumeFieldValue(num, typ, b)
			if n < 0 {
				return tp, fmt.Errorf("TypeParameterProto: skip field %d: %w", num, protowire.ParseError(n))
			}
			b = b[n:]
		}
	}
	return tp, nil
}

// classKindFromFlags extracts the ClassKind from ClassProto flags bits 6-8.
func classKindFromFlags(flags int32) ClassKind {
	switch (flags >> 6) & 0x7 {
	case 0:
		return ClassKindClass
	case 1:
		return ClassKindInterface
	case 2:
		return ClassKindEnumClass
	case 3:
		return ClassKindClass // enum entry treated as class
	case 4:
		return ClassKindAnnotationClass
	case 5:
		return ClassKindObject
	case 6:
		return ClassKindCompanionObject
	}
	return ClassKindClass
}

// isPublicVisible returns true if the visibility bits (3-5) of flags indicate
// public (3) or internal (0) visibility.
func isPublicVisible(flags int32) bool {
	v := (flags >> 3) & 0x7
	return v == 3 || v == 0 // public or internal
}

// strAt safely indexes the string table d2.
func strAt(d2 []string, idx int32) string {
	if idx < 0 || int(idx) >= len(d2) {
		return fmt.Sprintf("?idx%d", idx)
	}
	return d2[idx]
}

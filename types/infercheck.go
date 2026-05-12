package types

// ContainsAny reports whether t or any of its components is the Any type.
func ContainsAny(t Type) bool {
	switch tt := t.(type) {
	case AnyType:
		return true
	case ListType:
		return ContainsAny(tt.Elem)
	case MapType:
		return ContainsAny(tt.Key) || ContainsAny(tt.Value)
	case OptionType:
		return ContainsAny(tt.Elem)
	case GroupType:
		return ContainsAny(tt.Key) || ContainsAny(tt.Elem)
	case StructType:
		for _, f := range tt.Fields {
			if ContainsAny(f.Type) {
				return true
			}
		}
	case UnionType:
		for _, v := range tt.Variants {
			for _, f := range v.Fields {
				if ContainsAny(f.Type) {
					return true
				}
			}
		}
	}
	return false
}

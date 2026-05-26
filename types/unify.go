package types

func unify(a, b Type, subst Subst) bool {
	if _, ok := b.(AnyType); ok {
		return true
	}
	switch at := a.(type) {

	case AnyType:
		return true

	case *TypeVar:
		if subst != nil {
			if val, ok := subst[at.Name]; ok {
				return unify(val, b, subst)
			}
			subst[at.Name] = b
			return true
		}
		if bt, ok := b.(*TypeVar); ok {
			return at.Name == bt.Name
		}
		return false

	case ListType:
		switch bt := b.(type) {
		case ListType:
			return unify(at.Elem, bt.Elem, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case MapType:
		switch bt := b.(type) {
		case MapType:
			return unify(at.Key, bt.Key, subst) &&
				unify(at.Value, bt.Value, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case ChanType:
		switch bt := b.(type) {
		case ChanType:
			return unify(at.Elem, bt.Elem, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case StreamType:
		switch bt := b.(type) {
		case StreamType:
			return unify(at.Elem, bt.Elem, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case SubType:
		switch bt := b.(type) {
		case SubType:
			return unify(at.Elem, bt.Elem, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case FutureType:
		switch bt := b.(type) {
		case FutureType:
			return unify(at.Elem, bt.Elem, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case OptionType:
		switch bt := b.(type) {
		case OptionType:
			return unify(at.Elem, bt.Elem, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case GroupType:
		switch bt := b.(type) {
		case GroupType:
			return unify(at.Key, bt.Key, subst) && unify(at.Elem, bt.Elem, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case StructType:
		switch bt := b.(type) {
		case StructType:
			if at.Name != "" && bt.Name != "" && at.Name != bt.Name {
				return false
			}
			if len(at.Fields) != len(bt.Fields) {
				return false
			}
			for _, f := range at.Fields {
				bv, ok := bt.FieldType(f.Name)
				if !ok {
					return false
				}
				if !unify(f.Type, bv, subst) {
					return false
				}
			}
			return true
		case UnionType:
			if vt, ok := bt.Variants[at.Name]; ok {
				return unify(at, vt, subst)
			}
			return false
		default:
			return false
		}

	case UnionType:
		switch bt := b.(type) {
		case UnionType:
			return at.Name == bt.Name
		case StructType:
			if vt, ok := at.Variants[bt.Name]; ok {
				return unify(vt, bt, subst)
			}
			return false
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case FuncType:
		bt, ok := b.(FuncType)
		if !ok || len(at.Params) != len(bt.Params) {
			return false
		}
		if (at.Variadic == nil) != (bt.Variadic == nil) {
			return false
		}
		for i := range at.Params {
			if !unify(at.Params[i], bt.Params[i], subst) {
				return false
			}
		}
		if at.Variadic != nil && !unify(at.Variadic, bt.Variadic, subst) {
			return false
		}
		return unify(at.Return, bt.Return, subst)

	case IntType:
		switch b.(type) {
		case IntType, BigIntType:
			return true
		default:
			return false
		}

	case Int64Type:
		switch b.(type) {
		case Int64Type, IntType:
			return true
		default:
			return false
		}

	case BigIntType:
		switch b.(type) {
		case BigIntType, IntType, Int64Type:
			return true
		default:
			return false
		}

	case BigRatType:
		switch b.(type) {
		case BigRatType, FloatType, IntType, Int64Type, BigIntType:
			return true
		default:
			return false
		}

	case FloatType:
		_, ok := b.(FloatType)
		return ok

	case StringType:
		_, ok := b.(StringType)
		return ok

	case BoolType:
		_, ok := b.(BoolType)
		return ok

	case UnitType:
		_, ok := b.(UnitType)
		return ok

	case ValueType:
		_, ok := b.(ValueType)
		return ok

	default:
		// If a didn't match, maybe b is AnyType or a TypeVar
		switch bt := b.(type) {
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(a, val, subst)
				}
				subst[bt.Name] = a
				return true
			}
			if atv, ok := a.(*TypeVar); ok {
				return atv.Name == bt.Name
			}
			return false
		case OptionType:
			if ot, ok := a.(OptionType); ok {
				return unify(ot.Elem, bt.Elem, subst)
			}
			return false
		default:
			return false
		}
	}
}

// --- Entry Point ---

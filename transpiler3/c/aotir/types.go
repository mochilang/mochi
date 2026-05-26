package aotir

// Type is the monomorphic type of an aotir Value or function
// signature element. The set grows by phase. Phase 1 ships only
// TypeUnit (no value) and TypeString (read-only NUL-terminated
// C string). Phase 2.0 adds the three scalar primitives. Phase
// 3.0 adds TypeRecord; the record's identity (its source name)
// rides on a parallel RecordName field on the carrying IR node
// rather than inflating Type into a struct (avoids touching
// every Type compare site in the verifier / emit / lower).
// Phase 3.1 adds TypeList for the four scalar element types;
// the element type rides on a parallel ElemType field on every
// IR node that can carry a list value (mirrors the RecordName
// plumbing). Lists of records and nested lists are deferred to
// later sub-phases.
// Phase 3.2 adds TypeMap with key + value carried as parallel
// KeyType + ValueType fields. Eight (K,V) instantiations are
// reachable in 3.2 (K ∈ {int, string}, V ∈ {int, float, bool,
// string}); the runtime ships per-(K,V) helpers under
// mochi_map_<K>_<V>_* names. Map keys cannot themselves be lists
// or records in 3.2; values cannot either (same parallel-field
// rationale as 3.1: keeps the verifier's compare sites tractable
// until Phase 3.4 monomorphisation widens the predicate).
type Type int

const (
	TypeInvalid Type = iota
	TypeUnit
	TypeString
	TypeInt    // int64_t (signed, two's complement)
	TypeFloat  // double (IEEE 754 binary64)
	TypeBool   // int storing 0 or 1 (matches C runtime ABI)
	TypeRecord // struct mochi_<Name>; identity carried as RecordName beside the Type
	TypeList   // mochi_list_<T>; element type carried as ElemType beside the Type
	TypeMap    // mochi_map_<K>_<V>; key + value types carried as KeyType + ValueType beside the Type
	TypeSet    // OTP sets module v2 set; element type carried as ElemType beside the Type (Phase 3.3)
	TypeOMap   // OTP orddict ordered map; key + value types carried as KeyType + ValueType beside the Type (Phase 3.4)
	TypeUnion  // struct pkg_<S> with uint8_t tag + union; identity carried as UnionName beside the Type
	TypeFun    // function pointer; signature carried as FunSig beside the Type (Phase 5.0)
	TypeChan   // mochi_chan_t *; element type carried as ChanElemType beside the Type (Phase 9.1)
	TypeStream // mochi_stream_t *; element type carried as StreamElemType beside the Type (Phase 9.2)
	TypeSub    // mochi_sub_t *; element type carried as SubElemType beside the Type (Phase 9.2)
	TypeAgent  // mochi_agent_NAME_t; identity carried as AgentName beside the Type (Phase 9.3)
	TypeValue  // mochi_value_t; tagged union for FFI-crossing values (Phase 10.1)
	TypeFuture // Erlang reference returned by mochi_async:async/1 (Phase 11.0)
)

// String returns a stable identifier for the type, used in
// emit-time mangling and verifier diagnostics. The names must
// be deterministic; sort order on this name is the emit order
// for type-keyed lookup tables.
func (t Type) String() string {
	switch t {
	case TypeUnit:
		return "unit"
	case TypeString:
		return "string"
	case TypeInt:
		return "int"
	case TypeFloat:
		return "float"
	case TypeBool:
		return "bool"
	case TypeRecord:
		return "record"
	case TypeList:
		return "list"
	case TypeMap:
		return "map"
	case TypeSet:
		return "set"
	case TypeOMap:
		return "omap"
	case TypeUnion:
		return "union"
	case TypeFun:
		return "fun"
	case TypeChan:
		return "chan"
	case TypeStream:
		return "stream"
	case TypeSub:
		return "sub"
	case TypeAgent:
		return "agent"
	case TypeValue:
		return "value"
	case TypeFuture:
		return "future"
	default:
		return "invalid"
	}
}

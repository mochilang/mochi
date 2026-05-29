// Package typemap implements MEP-71 Phase 4: the closed Python-to-Mochi type
// translation table.
//
// The table is "closed" in the sense that every Python type expression either
// has a fixed Mochi rendering (e.g. `int -> int`, `list[T] -> list<T>` when T
// is in-table) or is refused with a SkipReason from package errors. The
// bridge does not invent dynamic interop types: aggressive "everything is
// MochiPyAny" rendering is rejected on principle. The Mochi side gets typed
// surfaces; out-of-table items become SkipReports the user can override with
// hand-written `extern python fun` declarations in the MEP-51 sidecar.
//
// The supported lowerings, drawn from MEP-71 §3 and research note 05:
//
//	int                      -> int
//	float                    -> float
//	bool                     -> bool
//	str                      -> string
//	bytes                    -> bytes
//	None                     -> None
//	list[T]                  -> list<T>           (T must be in-table)
//	tuple[A, B, ...]         -> tuple<A, B, ...>
//	tuple[T, ...]            -> list<T>           (homogeneous form)
//	dict[K, V]               -> map<K, V>         (K must be str or int)
//	set[T]                   -> set<T>
//	frozenset[T]             -> set<T>            (read-only)
//	Optional[T] / T | None   -> T?
//	X | Y                    -> X | Y             (PEP 604 sum type)
//	Callable[[A, B], R]      -> fun(A, B): R
//	Awaitable[T]             -> async T
//	AsyncIterator[T]         -> stream<T>
//	Iterator[T]              -> list<T>           (materialised at boundary)
//	TypedDict                -> record
//	@dataclass(frozen=True)  -> record
//	Protocol                 -> interface
//
// The refusal set, drawn from research note 05 §"The refusal table":
//
//	Any (unparameterised), object (non-Protocol position)
//	ParamSpec, Concatenate              -> SkipParamSpec
//	TypeVarTuple beyond declared mono   -> SkipTypeVarTuple
//	Unresolvable forward references     -> SkipForwardRef
//	Mutable @dataclass                  -> SkipUnsupportedTypingConstruct
//	complex                             -> SkipNoComplexType
//	Generator types other than Iterator / AsyncIterator
//	cast / assert_type / reveal_type    -> SkipUnsupportedTypingConstruct
//
// The Mapper drives the table. Callers feed it the raw type-expression strings
// the Phase 3 .pyi reader stored on `ModuleSurface`. Each call returns a
// Decision carrying either a *MochiType or a *errors.SkipReport.
package typemap

package typemap

import (
	"fmt"
	"strings"

	"mochi/package3/python/errors"
)

// Decision is the result of mapping one Python type expression.
type Decision struct {
	// Type is the lowered MochiType. Zero-value when Skip is set.
	Type MochiType
	// Skip is non-nil when the mapping was refused.
	Skip *errors.SkipReport
}

// OK reports whether the decision produced a usable type.
func (d Decision) OK() bool { return d.Skip == nil }

// Mapper applies the closed type table to Python type expressions.
type Mapper struct {
	// TypeVars is the set of in-scope type variables. Names found here lower
	// to KindTypeVar instead of refusing as an unknown identifier.
	TypeVars map[string]bool
	// Classes is the set of in-scope user-defined class names. Names found
	// here lower to KindRef.
	Classes map[string]bool
	// AllowPartial, when true, permits Any -> ref<Any> lowering for sources
	// marked Partial = true by Phase 3. When false (default), Any is refused.
	AllowPartial bool
	// ItemPath is the dotted qualified path of the item currently being
	// mapped, used to populate the SkipReport ItemPath field.
	ItemPath string
}

// Map parses and lowers a single Python type expression.
func (m *Mapper) Map(expr string) Decision {
	expr = strings.TrimSpace(expr)
	if expr == "" {
		return m.skip(errors.SkipUnsupportedTypingConstruct, "empty type expression", "")
	}
	pt, err := ParsePyType(expr)
	if err != nil {
		return m.skip(errors.SkipUnsupportedTypingConstruct, err.Error(), "")
	}
	return m.mapNode(pt, expr)
}

// MapParsed lowers an already-parsed PyType.
func (m *Mapper) MapParsed(pt PyType) Decision {
	return m.mapNode(pt, pt.String())
}

func (m *Mapper) skip(reason errors.SkipReason, detail, override string) Decision {
	return Decision{Skip: &errors.SkipReport{
		ItemPath: m.ItemPath,
		Reason:   reason,
		Detail:   detail,
		Override: override,
	}}
}

func (m *Mapper) mapNode(pt PyType, src string) Decision {
	switch pt.Kind {
	case PyName:
		return m.mapName(pt.Name)
	case PyAttr:
		// Strip typing / collections.abc / builtins module prefixes.
		qn := pt.QualifiedName()
		if qn == "" {
			return m.skip(errors.SkipUnsupportedTypingConstruct, "non-identifier attribute base in "+src, "")
		}
		return m.mapQualified(qn)
	case PySubscript:
		return m.mapSubscript(pt)
	case PyUnion:
		return m.mapUnion(pt.Args)
	case PyLiteral:
		// Bare literal at type position: forward reference (string) or None.
		if pt.Literal == "None" {
			return Decision{Type: NewScalar("None")}
		}
		if strings.HasPrefix(pt.Literal, "\"") || strings.HasPrefix(pt.Literal, "'") {
			// Forward reference: try to resolve against Classes / TypeVars.
			inner := pt.Literal[1 : len(pt.Literal)-1]
			return m.mapName(inner)
		}
		return m.skip(errors.SkipUnsupportedTypingConstruct, "bare literal "+pt.Literal+" at type position", "")
	case PyEllipsis:
		return m.skip(errors.SkipUnsupportedTypingConstruct, "bare ellipsis at type position", "")
	default:
		return m.skip(errors.SkipUnsupportedTypingConstruct, "unsupported type expression: "+src, "")
	}
}

// mapName resolves an unqualified identifier.
func (m *Mapper) mapName(name string) Decision {
	if t, ok := lookupScalar(name); ok {
		return Decision{Type: t}
	}
	if m.TypeVars[name] {
		return Decision{Type: NewTypeVar(name)}
	}
	if m.Classes[name] {
		return Decision{Type: NewRef(name)}
	}
	switch name {
	case "Any":
		if m.AllowPartial {
			return Decision{Type: NewRef("Any")}
		}
		return m.skip(errors.SkipUnsupportedTypingConstruct, "Any has no Mochi counterpart",
			"declare a concrete type via @extern python override")
	case "object":
		return m.skip(errors.SkipUnsupportedTypingConstruct, "object outside Protocol position", "")
	case "complex":
		return m.skip(errors.SkipNoComplexType, "Python complex has no Mochi counterpart", "")
	case "Self":
		return Decision{Type: NewRef("Self")}
	case "List", "Tuple", "Dict", "Set", "FrozenSet", "Optional", "Union",
		"Callable", "Awaitable", "AsyncIterator", "Iterator", "Iterable",
		"Type", "Literal", "Final", "ClassVar", "Annotated", "TypedDict",
		"Protocol", "TypeVar", "ParamSpec", "TypeVarTuple", "Generic",
		"Concatenate", "Unpack", "NotRequired", "Required":
		// Bare typing constructor without subscript: meaningless on its own.
		return m.skip(errors.SkipUnsupportedTypingConstruct, "bare typing constructor "+name+" without subscript", "")
	}
	return m.skip(errors.SkipForwardRef, "unresolved name "+name, "")
}

// mapQualified resolves a dotted identifier like `typing.List`.
func (m *Mapper) mapQualified(qn string) Decision {
	// Strip well-known module prefixes.
	for _, prefix := range []string{
		"typing.", "typing_extensions.", "collections.abc.", "builtins.",
	} {
		if strings.HasPrefix(qn, prefix) {
			return m.mapName(strings.TrimPrefix(qn, prefix))
		}
	}
	// User-defined classes with dotted access (e.g. mod.MyClass) lower to ref.
	parts := strings.Split(qn, ".")
	tail := parts[len(parts)-1]
	if m.Classes[tail] {
		return Decision{Type: NewRef(tail)}
	}
	return m.skip(errors.SkipForwardRef, "unresolved qualified name "+qn, "")
}

func (m *Mapper) mapSubscript(pt PyType) Decision {
	base := pt.Base.QualifiedName()
	if base == "" {
		return m.skip(errors.SkipUnsupportedTypingConstruct, "non-identifier subscript base", "")
	}
	// Normalise module prefix.
	for _, prefix := range []string{
		"typing.", "typing_extensions.", "collections.abc.", "builtins.",
	} {
		if strings.HasPrefix(base, prefix) {
			base = strings.TrimPrefix(base, prefix)
			break
		}
	}
	args := pt.Args
	switch base {
	case "list", "List":
		if len(args) != 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, fmt.Sprintf("list takes 1 type argument, got %d", len(args)), "")
		}
		inner := m.MapParsed(args[0])
		if !inner.OK() {
			return inner
		}
		return Decision{Type: NewList(inner.Type)}
	case "set", "Set":
		if len(args) != 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, fmt.Sprintf("set takes 1 type argument, got %d", len(args)), "")
		}
		inner := m.MapParsed(args[0])
		if !inner.OK() {
			return inner
		}
		return Decision{Type: NewSet(inner.Type)}
	case "frozenset", "FrozenSet":
		if len(args) != 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, fmt.Sprintf("frozenset takes 1 type argument, got %d", len(args)), "")
		}
		inner := m.MapParsed(args[0])
		if !inner.OK() {
			return inner
		}
		return Decision{Type: NewSet(inner.Type)}
	case "dict", "Dict", "Mapping", "MutableMapping":
		if len(args) != 2 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, fmt.Sprintf("dict takes 2 type arguments, got %d", len(args)), "")
		}
		key := m.MapParsed(args[0])
		if !key.OK() {
			return key
		}
		val := m.MapParsed(args[1])
		if !val.OK() {
			return val
		}
		if !isValidMapKey(key.Type) {
			return m.skip(errors.SkipUnsupportedTypingConstruct,
				"map keys must be str or an integer type, got "+key.Type.Render(), "")
		}
		return Decision{Type: NewMap(key.Type, val.Type)}
	case "tuple", "Tuple":
		// `tuple[T, ...]` is homogeneous list.
		if len(args) == 2 && args[1].Kind == PyEllipsis {
			inner := m.MapParsed(args[0])
			if !inner.OK() {
				return inner
			}
			return Decision{Type: NewList(inner.Type)}
		}
		var elems []MochiType
		for _, a := range args {
			d := m.MapParsed(a)
			if !d.OK() {
				return d
			}
			elems = append(elems, d.Type)
		}
		return Decision{Type: NewTuple(elems...)}
	case "Optional":
		if len(args) != 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, "Optional takes 1 type argument", "")
		}
		inner := m.MapParsed(args[0])
		if !inner.OK() {
			return inner
		}
		return Decision{Type: NewOptional(inner.Type)}
	case "Union":
		return m.mapUnion(args)
	case "Callable":
		return m.mapCallable(args)
	case "Awaitable", "Coroutine":
		// Coroutine[Y, S, R] -> Awaitable-ish: only R matters for our model.
		var inner Decision
		if base == "Awaitable" {
			if len(args) != 1 {
				return m.skip(errors.SkipUnsupportedTypingConstruct, "Awaitable takes 1 type argument", "")
			}
			inner = m.MapParsed(args[0])
		} else {
			if len(args) != 3 {
				return m.skip(errors.SkipUnsupportedTypingConstruct, "Coroutine takes 3 type arguments", "")
			}
			inner = m.MapParsed(args[2])
		}
		if !inner.OK() {
			return inner
		}
		return Decision{Type: NewAsync(inner.Type)}
	case "AsyncIterator", "AsyncIterable":
		if len(args) != 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, "AsyncIterator takes 1 type argument", "")
		}
		inner := m.MapParsed(args[0])
		if !inner.OK() {
			return inner
		}
		return Decision{Type: NewStream(inner.Type)}
	case "Iterator", "Iterable":
		if len(args) != 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, "Iterator takes 1 type argument", "")
		}
		inner := m.MapParsed(args[0])
		if !inner.OK() {
			return inner
		}
		return Decision{Type: NewList(inner.Type)}
	case "Type":
		return m.skip(errors.SkipUnsupportedTypingConstruct, "Type[X] has no Mochi counterpart", "")
	case "Literal":
		// Single-arm Literal of a known type collapses to that scalar.
		if len(args) == 0 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, "Literal[] requires at least one argument", "")
		}
		return m.mapLiteral(args)
	case "Final", "ClassVar":
		if len(args) != 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, base+" takes 1 type argument", "")
		}
		return m.MapParsed(args[0])
	case "Annotated":
		if len(args) < 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, "Annotated requires a type argument", "")
		}
		return m.MapParsed(args[0])
	case "ParamSpec", "Concatenate":
		return m.skip(errors.SkipParamSpec, "ParamSpec / Concatenate is out of scope", "")
	case "TypeVarTuple", "Unpack":
		return m.skip(errors.SkipTypeVarTuple, "TypeVarTuple / Unpack is out of scope", "")
	case "Generator", "AsyncGenerator":
		return m.skip(errors.SkipUnsupportedTypingConstruct,
			"Generator types other than Iterator / AsyncIterator are out of scope", "")
	case "NotRequired", "Required":
		if len(args) != 1 {
			return m.skip(errors.SkipUnsupportedTypingConstruct, base+" takes 1 type argument", "")
		}
		return m.MapParsed(args[0])
	}
	// User-defined generic class: lower as ref<T1, T2>.
	if m.Classes[base] {
		ref := MochiType{Kind: KindRef, Name: base}
		for _, a := range args {
			d := m.MapParsed(a)
			if !d.OK() {
				return d
			}
			ref.Params = append(ref.Params, d.Type)
		}
		return Decision{Type: ref}
	}
	return m.skip(errors.SkipForwardRef, "unresolved subscript base "+base, "")
}

func (m *Mapper) mapUnion(branches []PyType) Decision {
	if len(branches) == 0 {
		return m.skip(errors.SkipUnsupportedTypingConstruct, "empty union", "")
	}
	var out []MochiType
	for _, b := range branches {
		d := m.MapParsed(b)
		if !d.OK() {
			return d
		}
		if d.Type.Kind == KindRef && d.Type.Name == "Any" {
			return m.skip(errors.SkipOpenUnion, "union branch is Any", "")
		}
		out = append(out, d.Type)
	}
	return Decision{Type: NewSum(out...)}
}

func (m *Mapper) mapCallable(args []PyType) Decision {
	if len(args) != 2 {
		return m.skip(errors.SkipUnsupportedTypingConstruct,
			fmt.Sprintf("Callable takes 2 arguments, got %d", len(args)), "")
	}
	paramsNode := args[0]
	if paramsNode.Kind == PyEllipsis {
		return m.skip(errors.SkipParamSpec, "Callable[..., R] is out of scope (ParamSpec-shaped)", "")
	}
	if paramsNode.Kind != PyTuple {
		return m.skip(errors.SkipUnsupportedTypingConstruct, "Callable parameter list must be a bracketed sequence", "")
	}
	var params []MochiType
	for _, p := range paramsNode.Args {
		d := m.MapParsed(p)
		if !d.OK() {
			return d
		}
		params = append(params, d.Type)
	}
	ret := m.MapParsed(args[1])
	if !ret.OK() {
		return ret
	}
	return Decision{Type: NewFun(ret.Type, params...)}
}

func (m *Mapper) mapLiteral(args []PyType) Decision {
	// Single-shape Literal collapses to a known scalar; mixed shapes refuse.
	var kind MochiType
	for _, a := range args {
		var k MochiType
		switch {
		case a.Kind == PyLiteral && (strings.HasPrefix(a.Literal, "\"") || strings.HasPrefix(a.Literal, "'")):
			k = NewScalar("string")
		case a.Kind == PyLiteral && (a.Literal == "True" || a.Literal == "False"):
			k = NewScalar("bool")
		case a.Kind == PyLiteral && a.Literal == "None":
			k = NewScalar("None")
		case a.Kind == PyLiteral:
			k = NewScalar("int")
		default:
			return m.skip(errors.SkipUnsupportedTypingConstruct, "Literal argument must be a literal, got "+a.String(), "")
		}
		if kind.Kind == KindUnknown {
			kind = k
		} else if !kind.Equal(k) {
			return m.skip(errors.SkipUnsupportedTypingConstruct, "Literal arguments of mixed kinds are out of scope", "")
		}
	}
	return Decision{Type: kind}
}

// lookupScalar is the closed scalar table.
func lookupScalar(name string) (MochiType, bool) {
	switch name {
	case "int":
		return NewScalar("int"), true
	case "float":
		return NewScalar("float"), true
	case "bool":
		return NewScalar("bool"), true
	case "str":
		return NewScalar("string"), true
	case "bytes":
		return NewScalar("bytes"), true
	case "None", "NoneType":
		return NewScalar("None"), true
	}
	return MochiType{}, false
}

// isValidMapKey is the spec rule "map keys must be str or one of the integer
// types". `bool` is excluded because Mochi's map keys exclude bool by policy.
func isValidMapKey(t MochiType) bool {
	if t.Kind != KindScalar {
		return false
	}
	return t.Name == "string" || t.Name == "int"
}

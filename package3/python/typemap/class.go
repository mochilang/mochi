package typemap

import (
	"strings"

	"mochi/package3/python/errors"
	"mochi/package3/python/stubs"
)

// ClassDecision is the result of mapping a Python class declaration.
type ClassDecision struct {
	// Type is the lowered MochiType (KindRecord or KindInterface). Zero-value
	// when Skip is set.
	Type MochiType
	// Methods contains per-method decisions for KindRecord targets (interface
	// methods are inlined into Type.Methods). Methods without a successful
	// mapping appear in Skipped instead.
	Methods []MethodDecision
	// Skip is non-nil when the class was refused entirely.
	Skip *errors.SkipReport
	// Skipped is the per-member SkipReports for fields / methods that the
	// bridge refused while the class itself was mapped.
	Skipped []errors.SkipReport
}

// MethodDecision is the result of mapping one record / interface method.
type MethodDecision struct {
	Name   string
	Method MochiMethod
	Skip   *errors.SkipReport
}

// OK reports whether the decision produced a usable type.
func (d ClassDecision) OK() bool { return d.Skip == nil }

// MapClass lowers a stubs.ClassDecl into a Mochi record (TypedDict / frozen
// dataclass) or interface (Protocol). Other classes refuse with
// SkipUnsupportedTypingConstruct so the bridge does not invent surfaces for
// arbitrary Python classes.
func (m *Mapper) MapClass(cd stubs.ClassDecl) ClassDecision {
	saved := m.ItemPath
	defer func() { m.ItemPath = saved }()
	m.ItemPath = cd.Name

	switch {
	case cd.IsProtocol:
		return m.mapProtocol(cd)
	case cd.IsTypedDict:
		return m.mapTypedDict(cd)
	case cd.IsDataclass:
		return m.mapDataclass(cd)
	default:
		return ClassDecision{Skip: &errors.SkipReport{
			ItemPath: cd.Name,
			Reason:   errors.SkipUnsupportedTypingConstruct,
			Detail:   "class " + cd.Name + " is neither Protocol, TypedDict, nor @dataclass; supply a hand-written extern",
		}}
	}
}

func (m *Mapper) mapTypedDict(cd stubs.ClassDecl) ClassDecision {
	out := ClassDecision{Type: MochiType{Kind: KindRecord, Name: cd.Name}}
	for _, f := range cd.Fields {
		fd := m.mapField(cd.Name, f)
		if fd.Skip != nil {
			out.Skipped = append(out.Skipped, *fd.Skip)
			continue
		}
		out.Type.Fields = append(out.Type.Fields, fd.Field)
	}
	return out
}

func (m *Mapper) mapDataclass(cd stubs.ClassDecl) ClassDecision {
	if !classDecoratedAsFrozen(cd) {
		return ClassDecision{Skip: &errors.SkipReport{
			ItemPath: cd.Name,
			Reason:   errors.SkipUnsupportedTypingConstruct,
			Detail:   "mutable @dataclass: Mochi records are immutable; declare @dataclass(frozen=True) or provide a hand override",
			Override: "extern python type " + cd.Name + " { ... }",
		}}
	}
	out := ClassDecision{Type: MochiType{Kind: KindRecord, Name: cd.Name}}
	for _, f := range cd.Fields {
		fd := m.mapField(cd.Name, f)
		if fd.Skip != nil {
			out.Skipped = append(out.Skipped, *fd.Skip)
			continue
		}
		out.Type.Fields = append(out.Type.Fields, fd.Field)
	}
	return out
}

func (m *Mapper) mapProtocol(cd stubs.ClassDecl) ClassDecision {
	out := ClassDecision{Type: MochiType{Kind: KindInterface, Name: cd.Name}}
	for _, fn := range cd.Methods {
		md := m.mapMethod(cd.Name, fn)
		if md.Skip != nil {
			out.Skipped = append(out.Skipped, *md.Skip)
			continue
		}
		out.Type.Methods = append(out.Type.Methods, md.Method)
	}
	return out
}

type fieldDecision struct {
	Field MochiField
	Skip  *errors.SkipReport
}

func (m *Mapper) mapField(className string, f stubs.FieldDecl) fieldDecision {
	saved := m.ItemPath
	defer func() { m.ItemPath = saved }()
	m.ItemPath = className + "." + f.Name
	d := m.Map(f.Type)
	if !d.OK() {
		return fieldDecision{Skip: d.Skip}
	}
	return fieldDecision{Field: MochiField{Name: f.Name, Type: d.Type}}
}

func (m *Mapper) mapMethod(className string, fn stubs.FuncDecl) MethodDecision {
	saved := m.ItemPath
	defer func() { m.ItemPath = saved }()
	m.ItemPath = className + "." + fn.Name
	if strings.HasPrefix(fn.Name, "__") && strings.HasSuffix(fn.Name, "__") {
		// Dunders are not synthesised into the interface surface.
		return MethodDecision{Skip: &errors.SkipReport{
			ItemPath: m.ItemPath,
			Reason:   errors.SkipDunder,
			Detail:   "dunder methods are not exposed through the bridge",
		}}
	}
	method := MochiMethod{Name: fn.Name, IsAsync: fn.IsAsync}
	for _, p := range fn.Params {
		if p.Name == "self" || p.Name == "cls" {
			continue
		}
		switch p.Kind {
		case stubs.ParamVarArgs, stubs.ParamKwArgs:
			return MethodDecision{Skip: &errors.SkipReport{
				ItemPath: m.ItemPath,
				Reason:   errors.SkipParamSpec,
				Detail:   "*args / **kwargs in interface method " + fn.Name,
			}}
		}
		if p.Type == "" {
			return MethodDecision{Skip: &errors.SkipReport{
				ItemPath: m.ItemPath,
				Reason:   errors.SkipUnsupportedTypingConstruct,
				Detail:   "parameter " + p.Name + " has no annotation",
			}}
		}
		d := m.Map(p.Type)
		if !d.OK() {
			return MethodDecision{Skip: d.Skip}
		}
		method.Params = append(method.Params, d.Type)
	}
	if fn.ReturnType == "" {
		method.Return = NewScalar("None")
	} else {
		d := m.Map(fn.ReturnType)
		if !d.OK() {
			return MethodDecision{Skip: d.Skip}
		}
		method.Return = d.Type
	}
	return MethodDecision{Name: fn.Name, Method: method}
}

// MapFunction lowers a top-level FuncDecl. Returns a Decision whose Type, on
// success, is a KindFun MochiType. Methods on a class are handled via
// MapClass which dispatches to mapMethod above.
func (m *Mapper) MapFunction(fn stubs.FuncDecl) Decision {
	saved := m.ItemPath
	defer func() { m.ItemPath = saved }()
	m.ItemPath = fn.Name
	var params []MochiType
	for _, p := range fn.Params {
		switch p.Kind {
		case stubs.ParamVarArgs, stubs.ParamKwArgs:
			return m.skip(errors.SkipParamSpec, "*args / **kwargs in "+fn.Name, "")
		}
		if p.Type == "" {
			return m.skip(errors.SkipUnsupportedTypingConstruct,
				"parameter "+p.Name+" has no annotation in "+fn.Name, "")
		}
		d := m.Map(p.Type)
		if !d.OK() {
			return d
		}
		params = append(params, d.Type)
	}
	var ret MochiType
	if fn.ReturnType == "" {
		ret = NewScalar("None")
	} else {
		d := m.Map(fn.ReturnType)
		if !d.OK() {
			return d
		}
		ret = d.Type
	}
	t := NewFun(ret, params...)
	if fn.IsAsync {
		t = MochiType{Kind: KindFun, Params: append(params, NewAsync(ret))}
	}
	return Decision{Type: t}
}

// classDecoratedAsFrozen returns true when the class's decorator list contains
// `@dataclass(frozen=True)` (with whitespace tolerance).
func classDecoratedAsFrozen(cd stubs.ClassDecl) bool {
	for _, d := range cd.Decorators {
		d = strings.TrimSpace(d)
		// Match dataclass(... frozen=True ...) and dataclasses.dataclass(...).
		if !strings.Contains(d, "dataclass") {
			continue
		}
		paren := strings.Index(d, "(")
		if paren < 0 {
			continue
		}
		args := d[paren+1:]
		args = strings.TrimSuffix(args, ")")
		// Coarse but correct: any frozen=True in the args list wins.
		if strings.Contains(strings.ReplaceAll(args, " ", ""), "frozen=True") {
			return true
		}
	}
	return false
}

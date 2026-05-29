package wrapper

import (
	"fmt"

	javaerrors "mochi/package3/java/errors"
	javareflect "mochi/package3/java/reflect"
	"mochi/package3/java/typemap"
)

// Synth synthesises a WrapClass from a ClassInfo.
func Synth(cls javareflect.ClassInfo) *SynthResult {
	result := &SynthResult{
		WrapClass: &WrapClass{
			ClassName: "MochiWrap_" + classNameToIdent(cls.Name),
			OrigClass: cls.Name,
		},
	}

	for _, m := range cls.Methods {
		if !isPublicMethod(m.Modifiers) {
			continue
		}
		wm, skip := synthMethod(cls.Name, m)
		if skip != "" {
			result.Skips = append(result.Skips, SkipEntry{Name: m.Name, Reason: skip})
			continue
		}
		result.WrapClass.Methods = append(result.WrapClass.Methods, *wm)
	}

	for _, f := range cls.Fields {
		if !isPublicMethod(f.Modifiers) {
			continue
		}
		wf, skip := synthField(cls.Name, f)
		if skip != "" {
			result.Skips = append(result.Skips, SkipEntry{Name: f.Name, Reason: skip})
			continue
		}
		result.WrapClass.Fields = append(result.WrapClass.Fields, *wf)
	}
	return result
}

// synthMethod synthesises a single WrapMethod from a MethodInfo.
func synthMethod(className string, m javareflect.MethodInfo) (*WrapMethod, string) {
	// Skip varargs.
	if m.IsVarargs {
		return nil, javaerrors.SkipVarargs.String() + ": varargs method"
	}

	// Map return type.
	retM, sr, detail := typemap.Map(m.ReturnType)
	if sr != javaerrors.SkipUnknown {
		return nil, fmt.Sprintf("%v: return type %s: %s", sr, m.ReturnType, detail)
	}

	// Map parameter types.
	params := make([]*typemap.Mapping, 0, len(m.ParameterTypes))
	for i, pt := range m.ParameterTypes {
		pm, psr, pdetail := typemap.Map(pt)
		if psr != javaerrors.SkipUnknown {
			return nil, fmt.Sprintf("%v: param %d %s: %s", psr, i, pt, pdetail)
		}
		params = append(params, pm)
	}

	isStatic := isStaticMethod(m.Modifiers)
	wm := &WrapMethod{
		WrapName:      wrapMethodName(className, m.Name, isStatic),
		OrigName:      m.Name,
		IsStatic:      isStatic,
		ReturnMapping: retM,
		ParamMappings: params,
		ParamNames:    m.ParameterNames,
	}
	return wm, ""
}

// synthField synthesises a single WrapField from a FieldInfo.
func synthField(className string, f javareflect.FieldInfo) (*WrapField, string) {
	fm, sr, detail := typemap.Map(f.Type)
	if sr != javaerrors.SkipUnknown {
		return nil, fmt.Sprintf("%v: field type %s: %s", sr, f.Type, detail)
	}
	wf := &WrapField{
		WrapName: fmt.Sprintf("MochiWrap_%s_get_%s", classNameToIdent(className), f.Name),
		OrigName: f.Name,
		IsStatic: isStaticMethod(f.Modifiers),
		Mapping:  fm,
	}
	return wf, ""
}

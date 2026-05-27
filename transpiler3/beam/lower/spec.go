package lower

import (
	"strings"

	"mochi/transpiler3/beam/cerl"
	"mochi/transpiler3/c/aotir"
)

// addSpecAttrs emits a Dialyzer-compatible `-spec` module attribute for every
// function in fns. Phase 17.0: specs use Mochi's monomorphic aotir types
// mapped to the nearest Erlang built-in type. Unknown/complex types fall back
// to `term()` so specs are never wrong.
//
// Each attribute value is wrapped in c_literal as required by Core Erlang's
// module attribute format: [{c_literal(Key), c_literal(Value)}, ...].
func addSpecAttrs(mod *cerl.Module, fns []*aotir.Function) {
	for _, fn := range fns {
		if fn.IsLifted {
			continue
		}
		mod.Attrs = append(mod.Attrs, cerl.Attr{
			Key: "spec",
			Val: cerl.CLit(funcSpec(fn.Name, fn.Params, fn.ReturnType)),
		})
	}
	// main/1 always gets a spec: main([binary()]) -> ok.
	mod.Attrs = append(mod.Attrs, cerl.Attr{
		Key: "spec",
		Val: cerl.CLit(mainSpec()),
	})
}

// mainSpec builds the spec for main/1: main([binary()]) -> ok.
func mainSpec() cerl.Term {
	argvType := erlType("list", cerl.EList{erlType("binary")})
	retType := erlType("ok")
	return specEntry("main", 1, cerl.EList{argvType}, retType)
}

// funcSpec builds a spec attribute entry for a user function.
func funcSpec(name string, params []aotir.Param, ret aotir.Type) cerl.Term {
	argTypes := make(cerl.EList, len(params))
	for i, p := range params {
		argTypes[i] = mochTypeToErlType(p.Type)
	}
	retType := mochTypeToErlType(ret)
	return specEntry(name, len(params), argTypes, retType)
}

// specEntry builds: {spec, [{{Name, Arity}, [{type, 0, 'fun', [{type, 0, product, ArgTypes}, RetType]}]}]}
func specEntry(name string, arity int, argTypes cerl.EList, retType cerl.Term) cerl.Term {
	funType := cerl.ETuple{
		cerl.EAtom("type"), cerl.EInt(0), cerl.EAtom("fun"),
		cerl.EList{
			cerl.ETuple{cerl.EAtom("type"), cerl.EInt(0), cerl.EAtom("product"), argTypes},
			retType,
		},
	}
	entry := cerl.ETuple{
		cerl.ETuple{cerl.EAtom(name), cerl.EInt(int64(arity))},
		cerl.EList{funType},
	}
	return cerl.EList{entry}
}

// erlType constructs {type, 0, Name, []} or {type, 0, Name, [Arg]} etc.
func erlType(name string, args ...cerl.Term) cerl.Term {
	argList := make(cerl.EList, len(args))
	copy(argList, args)
	return cerl.ETuple{cerl.EAtom("type"), cerl.EInt(0), cerl.EAtom(name), argList}
}

// addOpaqueAttrs emits a -opaque module attribute for each agent type declared
// in the program. Agent state is a map on BEAM, so the opaque type is `map()`.
// Phase 17.1: Dialyzer sees each agent ref as a distinct opaque type.
func addOpaqueAttrs(mod *cerl.Module, agents []*aotir.AgentDecl) {
	for _, ag := range agents {
		typeName := strings.ToLower(ag.Name) + "_ref"
		entry := cerl.ETuple{
			cerl.EAtom(typeName), cerl.EInt(0),
			cerl.EList{erlType("map")},
		}
		mod.Attrs = append(mod.Attrs, cerl.Attr{
			Key: "opaque",
			Val: cerl.CLit(cerl.EList{entry}),
		})
	}
}

// mochTypeToErlType converts an aotir.Type to its nearest Erlang type term.
func mochTypeToErlType(t aotir.Type) cerl.Term {
	switch t {
	case aotir.TypeInt:
		return erlType("integer")
	case aotir.TypeFloat:
		return erlType("float")
	case aotir.TypeBool:
		return erlType("boolean")
	case aotir.TypeString:
		return erlType("binary")
	case aotir.TypeUnit:
		// unit return → ok atom
		return cerl.ETuple{cerl.EAtom("atom"), cerl.EInt(0), cerl.EAtom("ok")}
	case aotir.TypeList:
		return erlType("list")
	case aotir.TypeMap:
		return erlType("map")
	case aotir.TypeRecord:
		return erlType("map")
	case aotir.TypeUnion:
		return erlType("tuple")
	case aotir.TypeFun:
		return erlType("function")
	default:
		return erlType("term")
	}
}

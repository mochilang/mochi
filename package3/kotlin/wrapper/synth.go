package wrapper

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"text/template"

	"github.com/mochilang/mochi/package3/kotlin/metadata"
	"github.com/mochilang/mochi/package3/kotlin/typemap"
)

// BridgeData is the template context for MochiBridge.java.
type BridgeData struct {
	Artifact  string
	Functions []BridgeFunc
}

// BridgeFunc holds data for one @CEntryPoint method.
type BridgeFunc struct {
	CName      string // C entry point name (snake_case)
	JavaName   string // Java method name (camelCase)
	ReturnType string // Java return type string
	ParamList  string // ", Type name, Type name" (leading comma if non-empty)
	Body       string // method body
}

// ArtifactData is the common template context with just an artifact name.
type ArtifactData struct {
	Artifact string
}

// Synthesize generates the Java JNI wrapper source for one artifact.
// It writes three files into outputDir/java/com/mochi/bridge/<artifact>/:
//
//   - MochiBridge.java        — @CEntryPoint functions for each mapped function
//   - MochiHandleRegistry.java — handle registry for object references
//   - MochiJNI.java           — UTF-8/UTF-16 conversion helpers, list/map marshalling
func Synthesize(artifact string, classes []*metadata.APIObject, outputDir string) error {
	outPkg := filepath.Join(outputDir, "java", "com", "mochi", "bridge", artifact)
	if err := os.MkdirAll(outPkg, 0o755); err != nil {
		return fmt.Errorf("synth: mkdir %q: %w", outPkg, err)
	}

	reg := typemap.NewNameRegistry()

	// Collect all bridgeable functions across all classes.
	var funcs []BridgeFunc
	for _, obj := range classes {
		mc, reason := typemap.TranslateClass(obj, reg)
		if reason != typemap.NotRefused {
			continue
		}

		className := mc.ExternTypeName

		// Regular functions
		for _, mf := range mc.Functions {
			bf := buildBridgeFunc(className, mf)
			funcs = append(funcs, bf)
		}

		// Constructors
		for _, mf := range mc.Constructors {
			bf := buildBridgeFunc(className, mf)
			funcs = append(funcs, bf)
		}

		// Property getters
		for _, mf := range mc.Properties {
			bf := buildBridgeFunc(className, mf)
			funcs = append(funcs, bf)
		}
	}

	bridgeData := BridgeData{
		Artifact:  artifact,
		Functions: funcs,
	}
	artifactData := ArtifactData{Artifact: artifact}

	if err := renderTemplate(bridgeTemplate, bridgeData, filepath.Join(outPkg, "MochiBridge.java")); err != nil {
		return err
	}
	if err := renderTemplate(handleRegistryTemplate, artifactData, filepath.Join(outPkg, "MochiHandleRegistry.java")); err != nil {
		return err
	}
	if err := renderTemplate(jniHelpersTemplate, artifactData, filepath.Join(outPkg, "MochiJNI.java")); err != nil {
		return err
	}

	return nil
}

// buildBridgeFunc converts a MochiFunction into a BridgeFunc for template rendering.
func buildBridgeFunc(className string, mf typemap.MochiFunction) BridgeFunc {
	cName := typemap.ShimFnName(className, mf.OrigName)
	if mf.Name != "" {
		cName = mf.Name
	}

	javaName := mochiToJavaCamel(mf.Name)

	retJava := mochiTypeToJava(mf.ReturnType)

	var paramParts []string
	var callArgs []string
	for _, p := range mf.Params {
		javaType := mochiTypeToJava(p.Type)
		paramParts = append(paramParts, fmt.Sprintf("%s %s", javaType, p.Name))
		callArgs = append(callArgs, p.Name)
	}

	paramList := ""
	if len(paramParts) > 0 {
		paramList = ", " + strings.Join(paramParts, ", ")
	}

	body := buildBody(className, mf, retJava, callArgs)

	return BridgeFunc{
		CName:      cName,
		JavaName:   javaName,
		ReturnType: retJava,
		ParamList:  paramList,
		Body:       body,
	}
}

// buildBody generates the Java method body for a bridge function.
func buildBody(className string, mf typemap.MochiFunction, retJava string, callArgs []string) string {
	argsStr := strings.Join(callArgs, ", ")

	if mf.ReturnType.IsVoid {
		// void call
		return fmt.Sprintf("// TODO: invoke %s.%s(%s);", className, mf.OrigName, argsStr)
	}

	// Determine return expression
	switch retJava {
	case "void":
		return fmt.Sprintf("// TODO: invoke %s.%s(%s);", className, mf.OrigName, argsStr)
	case "String":
		return fmt.Sprintf("return MochiJNI.toMochiString(/* %s.%s(%s) */ null);", className, mf.OrigName, argsStr)
	case "boolean":
		return fmt.Sprintf("return false; // TODO: %s.%s(%s)", className, mf.OrigName, argsStr)
	case "double", "float":
		return fmt.Sprintf("return 0; // TODO: %s.%s(%s)", className, mf.OrigName, argsStr)
	default:
		// long (handles, ints, extern objects)
		return fmt.Sprintf("return 0L; // TODO: %s.%s(%s)", className, mf.OrigName, argsStr)
	}
}

// mochiTypeToJava maps a MochiType to its Java JNI equivalent type name.
func mochiTypeToJava(mt typemap.MochiType) string {
	if mt.IsVoid {
		return "void"
	}
	switch mt.Name {
	case "int", "long":
		return "long" // Mochi int is 64-bit; use long
	case "double":
		return "double"
	case "float":
		return "float"
	case "bool":
		return "boolean"
	case "string", "bytes":
		return "String"
	case "Option":
		if len(mt.TypeArgs) == 1 {
			inner := mt.TypeArgs[0]
			if inner.Name == "string" {
				return "String" // nullable String
			}
		}
		return "long" // handle; 0 = None
	case "List", "Set":
		return "String" // serialised as JSON string
	case "Map":
		return "String" // serialised as JSON string
	default:
		if mt.IsExtern {
			return "long" // opaque handle
		}
		return "long"
	}
}

// mochiToJavaCamel converts a snake_case Mochi name to lowerCamelCase Java name.
// "my_class_greet" -> "myClassGreet"
func mochiToJavaCamel(name string) string {
	parts := strings.Split(name, "_")
	if len(parts) == 0 {
		return name
	}
	var sb strings.Builder
	for i, p := range parts {
		if p == "" {
			continue
		}
		if i == 0 {
			sb.WriteString(p)
		} else {
			sb.WriteString(strings.ToUpper(p[:1]) + p[1:])
		}
	}
	return sb.String()
}

// renderTemplate executes a text/template and writes the result to path.
func renderTemplate(tmplStr string, data any, path string) error {
	tmpl, err := template.New("").Parse(tmplStr)
	if err != nil {
		return fmt.Errorf("synth: parse template: %w", err)
	}
	f, err := os.Create(path)
	if err != nil {
		return fmt.Errorf("synth: create %q: %w", path, err)
	}
	defer f.Close()
	if err := tmpl.Execute(f, data); err != nil {
		return fmt.Errorf("synth: render template to %q: %w", path, err)
	}
	return nil
}

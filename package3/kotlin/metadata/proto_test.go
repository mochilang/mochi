package metadata

import (
	"testing"

	"google.golang.org/protobuf/encoding/protowire"
)

// protoBuilder is a helper for building raw protobuf wire bytes.
type protoBuilder struct {
	buf []byte
}

func (b *protoBuilder) varint(field protowire.Number, v uint64) {
	b.buf = protowire.AppendTag(b.buf, field, protowire.VarintType)
	b.buf = protowire.AppendVarint(b.buf, v)
}

func (b *protoBuilder) bytes(field protowire.Number, data []byte) {
	b.buf = protowire.AppendTag(b.buf, field, protowire.BytesType)
	b.buf = protowire.AppendBytes(b.buf, data)
}

func (b *protoBuilder) build() []byte { return b.buf }

// buildFunctionProto encodes a FunctionProto with name idx, flags, and optional return type.
func buildFunctionProto(nameIdx int, flags int32, returnTypeBytes []byte, params [][]byte, receiverTypeBytes []byte) []byte {
	var pb protoBuilder
	pb.varint(1, uint64(flags))
	pb.varint(2, uint64(nameIdx))
	if returnTypeBytes != nil {
		pb.bytes(3, returnTypeBytes)
	}
	for _, p := range params {
		pb.bytes(5, p)
	}
	if receiverTypeBytes != nil {
		pb.bytes(6, receiverTypeBytes)
	}
	return pb.build()
}

// buildTypeProto encodes a TypeProto.
func buildTypeProto(classNameIdx int, nullable bool, args [][]byte, typeParamID int) []byte {
	var pb protoBuilder
	if nullable {
		pb.varint(1, 1)
	}
	if classNameIdx >= 0 {
		pb.varint(3, uint64(classNameIdx))
	}
	for _, arg := range args {
		pb.bytes(4, arg)
	}
	if typeParamID >= 0 {
		pb.varint(9, uint64(typeParamID))
	}
	return pb.build()
}

// buildTypeArgProto encodes a TypeArgumentProto.
func buildTypeArgProto(typeBytes []byte, projection int) []byte {
	var pb protoBuilder
	if typeBytes != nil {
		pb.bytes(2, typeBytes)
	}
	pb.varint(3, uint64(projection))
	return pb.build()
}

// buildValueParamProto encodes a ValueParameterProto.
func buildValueParamProto(nameIdx int, typeBytes []byte, hasDefault bool) []byte {
	var pb protoBuilder
	flags := uint64(0)
	if hasDefault {
		flags |= 1
	}
	pb.varint(1, flags)
	pb.varint(2, uint64(nameIdx))
	if typeBytes != nil {
		pb.bytes(3, typeBytes)
	}
	return pb.build()
}

// buildPropertyProto encodes a PropertyProto.
func buildPropertyProto(nameIdx int, flags int32, typeBytes []byte) []byte {
	var pb protoBuilder
	pb.varint(1, uint64(flags))
	pb.varint(2, uint64(nameIdx))
	if typeBytes != nil {
		pb.bytes(3, typeBytes)
	}
	return pb.build()
}

// buildConstructorProto encodes a ConstructorProto.
func buildConstructorProto(flags int32, params [][]byte) []byte {
	var pb protoBuilder
	pb.varint(1, uint64(flags))
	for _, p := range params {
		pb.bytes(5, p)
	}
	return pb.build()
}

// buildClassProto encodes a ClassProto.
func buildClassProto(flags int32, fqNameIdx int, fns [][]byte, props [][]byte, ctors [][]byte, enumEntries [][]byte, sealedSubs []int) []byte {
	var pb protoBuilder
	pb.varint(1, uint64(flags))
	pb.varint(3, uint64(fqNameIdx))
	for _, c := range ctors {
		pb.bytes(8, c)
	}
	for _, fn := range fns {
		pb.bytes(9, fn)
	}
	for _, prop := range props {
		pb.bytes(10, prop)
	}
	for _, e := range enumEntries {
		pb.bytes(12, e)
	}
	for _, idx := range sealedSubs {
		pb.varint(14, uint64(idx))
	}
	return pb.build()
}

// buildPackageProto encodes a PackageProto.
func buildPackageProto(fns [][]byte, props [][]byte) []byte {
	var pb protoBuilder
	for _, fn := range fns {
		pb.bytes(3, fn)
	}
	for _, prop := range props {
		pb.bytes(4, prop)
	}
	return pb.build()
}

// publicFunctionFlags returns function flags with visibility=public (bits 3-5 = 3).
func publicFunctionFlags(extra int32) int32 {
	return (3 << 3) | extra
}

// publicPropertyFlags returns property flags with visibility=public (bits 3-5 = 3).
func publicPropertyFlags(extra int32) int32 {
	return (3 << 3) | extra
}

// Test 1: Simple class with one public function greet(name: String): String
func TestDecodeClass_SimpleFunction(t *testing.T) {
	// String table: [0]="greet", [1]="name", [2]="kotlin/String", [3]="MyClass"
	d2 := []string{"greet", "name", "kotlin/String", "MyClass"}

	// return type: kotlin.String (idx=2), not nullable
	retType := buildTypeProto(2, false, nil, -1)
	// param: name: String
	paramType := buildTypeProto(2, false, nil, -1)
	param := buildValueParamProto(1, paramType, false)
	// function: greet(name: String): String, public, not suspend
	fnFlags := publicFunctionFlags(0)
	fn := buildFunctionProto(0, fnFlags, retType, [][]byte{param}, nil)

	// class flags: public class (visibility=3, kind=0)
	classFlags := int32(3 << 3) // public class
	classProto := buildClassProto(classFlags, 3, [][]byte{fn}, nil, nil, nil, nil)

	raw := &RawMetadata{
		Kind:    1,
		Version: [3]int32{1, 9, 0},
		D1:      classProto,
		D2:      d2,
	}

	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass returned error: %v", err)
	}
	if len(obj.Functions) != 1 {
		t.Fatalf("expected 1 function, got %d", len(obj.Functions))
	}
	fn0 := obj.Functions[0]
	if fn0.Name != "greet" {
		t.Errorf("function name: got %q, want %q", fn0.Name, "greet")
	}
	if len(fn0.Params) != 1 {
		t.Fatalf("expected 1 param, got %d", len(fn0.Params))
	}
	if fn0.Params[0].Name != "name" {
		t.Errorf("param name: got %q, want %q", fn0.Params[0].Name, "name")
	}
	if fn0.ReturnType.ClassName != "kotlin.String" {
		t.Errorf("return type: got %q, want %q", fn0.ReturnType.ClassName, "kotlin.String")
	}
	if fn0.Flags.IsSuspend {
		t.Error("expected IsSuspend=false")
	}
	if fn0.Flags.IsInline {
		t.Error("expected IsInline=false")
	}
}

// Test 2: Suspend function
func TestDecodeClass_SuspendFunction(t *testing.T) {
	d2 := []string{"fetch", "url", "kotlin/String", "MyClass"}

	retType := buildTypeProto(2, false, nil, -1)
	paramType := buildTypeProto(2, false, nil, -1)
	param := buildValueParamProto(1, paramType, false)
	// suspend flag: bit 18
	fnFlags := publicFunctionFlags(1 << 18)
	fn := buildFunctionProto(0, fnFlags, retType, [][]byte{param}, nil)

	classFlags := int32(3 << 3)
	classProto := buildClassProto(classFlags, 3, [][]byte{fn}, nil, nil, nil, nil)

	raw := &RawMetadata{Kind: 1, Version: [3]int32{1, 9, 0}, D1: classProto, D2: d2}
	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass returned error: %v", err)
	}
	if len(obj.Functions) == 0 {
		t.Fatal("expected at least one function")
	}
	if !obj.Functions[0].Flags.IsSuspend {
		t.Error("expected IsSuspend=true for suspend function")
	}
}

// Test 3: Data class with two properties
func TestDecodeClass_DataClassProperties(t *testing.T) {
	d2 := []string{"id", "name", "kotlin/Long", "kotlin/String", "User"}

	// bit 10 = is_data
	classFlags := int32((3 << 3) | (1 << 10))

	// property "id": Long (idx=2), val (bit 9 = 0), visibility=public
	propFlagsVal := publicPropertyFlags(0) // val
	idType := buildTypeProto(2, false, nil, -1)
	idProp := buildPropertyProto(0, propFlagsVal, idType)

	// property "name": String (idx=3), val
	nameType := buildTypeProto(3, false, nil, -1)
	nameProp := buildPropertyProto(1, propFlagsVal, nameType)

	classProto := buildClassProto(classFlags, 4, nil, [][]byte{idProp, nameProp}, nil, nil, nil)

	raw := &RawMetadata{Kind: 1, Version: [3]int32{1, 9, 0}, D1: classProto, D2: d2}
	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass returned error: %v", err)
	}
	if obj.Kind != ClassKindDataClass {
		t.Errorf("kind: got %v, want ClassKindDataClass", obj.Kind)
	}
	if len(obj.Properties) != 2 {
		t.Fatalf("expected 2 properties, got %d", len(obj.Properties))
	}
	if obj.Properties[0].Name != "id" {
		t.Errorf("prop[0].Name: got %q, want %q", obj.Properties[0].Name, "id")
	}
	if obj.Properties[0].Type.ClassName != "kotlin.Long" {
		t.Errorf("prop[0].Type: got %q, want %q", obj.Properties[0].Type.ClassName, "kotlin.Long")
	}
	if obj.Properties[1].Name != "name" {
		t.Errorf("prop[1].Name: got %q, want %q", obj.Properties[1].Name, "name")
	}
}

// Test 4: Class with companion object name
func TestDecodeClass_CompanionObjectName(t *testing.T) {
	d2 := []string{"com/example/Foo", "Companion"}
	// companion_object_name is field 4 in ClassProto
	var pb protoBuilder
	classFlags := int32(3 << 3) // public class
	pb.varint(1, uint64(classFlags))
	pb.varint(3, 0) // fq_name idx 0
	pb.varint(4, 1) // companion_object_name idx 1 = "Companion"

	classProto := pb.build()
	raw := &RawMetadata{Kind: 1, Version: [3]int32{1, 9, 0}, D1: classProto, D2: d2}
	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass error: %v", err)
	}
	if obj.ClassName != "com.example.Foo" {
		t.Errorf("class name: got %q, want %q", obj.ClassName, "com.example.Foo")
	}
}

// Test 5: Sealed class with two sealed subclass FQ names
func TestDecodeClass_SealedClass(t *testing.T) {
	d2 := []string{"com/example/Shape", "com/example/Circle", "com/example/Square"}
	// sealed_subclass_fq_name is field 14 (repeated varint)
	classFlags := int32(3 << 3)
	classProto := buildClassProto(classFlags, 0, nil, nil, nil, nil, []int{1, 2})

	raw := &RawMetadata{Kind: 1, Version: [3]int32{1, 9, 0}, D1: classProto, D2: d2}
	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass error: %v", err)
	}
	if obj.Kind != ClassKindSealedClass {
		t.Errorf("kind: got %v, want ClassKindSealedClass", obj.Kind)
	}
	if len(obj.SealedSubs) != 2 {
		t.Fatalf("sealed subs: got %d, want 2", len(obj.SealedSubs))
	}
	if obj.SealedSubs[0].ClassName != "com.example.Circle" {
		t.Errorf("sealedSubs[0]: got %q, want %q", obj.SealedSubs[0].ClassName, "com.example.Circle")
	}
	if obj.SealedSubs[1].ClassName != "com.example.Square" {
		t.Errorf("sealedSubs[1]: got %q, want %q", obj.SealedSubs[1].ClassName, "com.example.Square")
	}
}

// Test 6: PackageProto (k=2): top-level function in a file facade
func TestDecodePackage_TopLevelFunction(t *testing.T) {
	d2 := []string{"greet", "kotlin/String"}
	retType := buildTypeProto(1, false, nil, -1)
	fnFlags := publicFunctionFlags(0)
	fn := buildFunctionProto(0, fnFlags, retType, nil, nil)
	pkgProto := buildPackageProto([][]byte{fn}, nil)

	raw := &RawMetadata{Kind: 2, Version: [3]int32{1, 9, 0}, D1: pkgProto, D2: d2, XS: "com/example/Foo"}
	obj, err := DecodePackage(raw)
	if err != nil {
		t.Fatalf("DecodePackage error: %v", err)
	}
	if obj.Kind != ClassKindFileFacade {
		t.Errorf("kind: got %v, want ClassKindFileFacade", obj.Kind)
	}
	if len(obj.Functions) != 1 {
		t.Fatalf("expected 1 function, got %d", len(obj.Functions))
	}
	if obj.Functions[0].Name != "greet" {
		t.Errorf("function name: got %q, want %q", obj.Functions[0].Name, "greet")
	}
	if obj.ClassName != "com.example.Foo" {
		t.Errorf("class name: got %q, want %q", obj.ClassName, "com.example.Foo")
	}
}

// Test 7: Extension function (has receiver_type field 6)
func TestDecodeClass_ExtensionFunction(t *testing.T) {
	d2 := []string{"isEmpty", "kotlin/String", "kotlin/Boolean", "MyClass"}
	retType := buildTypeProto(2, false, nil, -1)
	receiverType := buildTypeProto(1, false, nil, -1) // String receiver
	fnFlags := publicFunctionFlags(0)

	var pb protoBuilder
	pb.varint(1, uint64(fnFlags))
	pb.varint(2, 0) // name "isEmpty"
	pb.bytes(3, retType)
	pb.bytes(6, receiverType) // receiver_type
	fn := pb.build()

	classFlags := int32(3 << 3)
	classProto := buildClassProto(classFlags, 3, [][]byte{fn}, nil, nil, nil, nil)
	raw := &RawMetadata{Kind: 1, Version: [3]int32{1, 9, 0}, D1: classProto, D2: d2}
	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass error: %v", err)
	}
	if len(obj.Functions) == 0 {
		t.Fatal("expected at least one function")
	}
	fn0 := obj.Functions[0]
	if fn0.Receiver == nil {
		t.Fatal("expected non-nil Receiver for extension function")
	}
	if fn0.Receiver.ClassName != "kotlin.String" {
		t.Errorf("receiver type: got %q, want %q", fn0.Receiver.ClassName, "kotlin.String")
	}
}

// Test 8: Nullable return type
func TestDecodeClass_NullableReturnType(t *testing.T) {
	d2 := []string{"find", "kotlin/String", "MyClass"}
	// nullable TypeProto: flags bit 0 = 1
	retType := buildTypeProto(1, true, nil, -1)
	fnFlags := publicFunctionFlags(0)
	fn := buildFunctionProto(0, fnFlags, retType, nil, nil)

	classFlags := int32(3 << 3)
	classProto := buildClassProto(classFlags, 2, [][]byte{fn}, nil, nil, nil, nil)
	raw := &RawMetadata{Kind: 1, Version: [3]int32{1, 9, 0}, D1: classProto, D2: d2}
	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass error: %v", err)
	}
	if len(obj.Functions) == 0 {
		t.Fatal("expected at least one function")
	}
	if !obj.Functions[0].ReturnType.Nullable {
		t.Error("expected return type to be nullable")
	}
}

// Test 9: Generic return type List<String>
func TestDecodeClass_GenericReturnType(t *testing.T) {
	d2 := []string{"getNames", "kotlin/collections/List", "kotlin/String", "MyClass"}
	// TypeProto for String argument
	stringType := buildTypeProto(2, false, nil, -1)
	// TypeArgumentProto wrapping String (projection=INV=2)
	typeArg := buildTypeArgProto(stringType, 2)
	// TypeProto for List<String>
	listType := buildTypeProto(1, false, [][]byte{typeArg}, -1)

	fnFlags := publicFunctionFlags(0)
	fn := buildFunctionProto(0, fnFlags, listType, nil, nil)

	classFlags := int32(3 << 3)
	classProto := buildClassProto(classFlags, 3, [][]byte{fn}, nil, nil, nil, nil)
	raw := &RawMetadata{Kind: 1, Version: [3]int32{1, 9, 0}, D1: classProto, D2: d2}
	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass error: %v", err)
	}
	if len(obj.Functions) == 0 {
		t.Fatal("expected at least one function")
	}
	retType := obj.Functions[0].ReturnType
	if retType.ClassName != "kotlin.collections.List" {
		t.Errorf("return type class: got %q, want %q", retType.ClassName, "kotlin.collections.List")
	}
	if len(retType.TypeArgs) != 1 {
		t.Fatalf("type args: got %d, want 1", len(retType.TypeArgs))
	}
	if retType.TypeArgs[0].ClassName != "kotlin.String" {
		t.Errorf("type arg[0]: got %q, want %q", retType.TypeArgs[0].ClassName, "kotlin.String")
	}
}

// Test: private function should be excluded
func TestDecodeClass_PrivateFunctionExcluded(t *testing.T) {
	d2 := []string{"internalHelper", "kotlin/Unit", "MyClass"}
	retType := buildTypeProto(1, false, nil, -1)
	// private: visibility bits 3-5 = 1
	fnFlags := int32(1 << 3)
	fn := buildFunctionProto(0, fnFlags, retType, nil, nil)

	classFlags := int32(3 << 3)
	classProto := buildClassProto(classFlags, 2, [][]byte{fn}, nil, nil, nil, nil)
	raw := &RawMetadata{Kind: 1, Version: [3]int32{1, 9, 0}, D1: classProto, D2: d2}
	obj, err := DecodeClass(raw)
	if err != nil {
		t.Fatalf("DecodeClass error: %v", err)
	}
	if len(obj.Functions) != 0 {
		t.Errorf("expected 0 functions (private excluded), got %d", len(obj.Functions))
	}
}

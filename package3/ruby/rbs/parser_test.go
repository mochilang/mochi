package rbs

import (
	"testing"
)

// ---- RBS fixtures -----------------------------------------------------------

var nokogiriHTMLFrag = `
class Nokogiri::HTML::DocumentFragment
  def css: (String selector) -> NodeSet
  def at_css: (String selector) -> Node?
  def to_html: () -> String
  attr_reader errors: Array[SyntaxError]
end
`

var redisFrag = `
class Redis
  def get: (String key) -> String?
  def set: (String key, String value) -> String
  def del: (*String keys) -> Integer
  def expire: (String key, Integer seconds) -> Integer
  def hgetall: (String key) -> Hash[String, String]
end
`

var jsonFrag = `
module JSON
  def self.parse: (String source) -> untyped
  def self.generate: (untyped obj) -> String
  def self.dump: (untyped obj) -> String
end
`

var attrFrag = `
class MyModel
  attr_reader name: String
  attr_accessor count: Integer
  attr_writer label: String
end
`

var optionalFrag = `
class Maybe
  def find: (String key) -> String?
  def lookup: (Integer id) -> Integer?
  def present?: () -> bool
  def valid!: () -> void
end
`

var unionNilFrag = `
class UnionNil
  def value: () -> (String | nil)
  def other: () -> (nil | Integer)
end
`

var complexTypesFrag = `
class ComplexTypes
  def array_method: (Array[String] items) -> Array[Integer]
  def hash_method: (Hash[String, Integer] data) -> Hash[Symbol, String]
  def tuple_method: () -> [String, Integer, bool]
  def proc_method: (^(Integer) -> String cb) -> void
end
`

var nestedClassFrag = `
class Outer
  def outer_method: () -> String
  class Inner
    def inner_method: () -> Integer
  end
end
`

var overloadFrag = `
class Overloaded
  def convert: (String s) -> Integer
               | (Integer i) -> String
               | (Float f) -> String
end
`

var untypedFrag = `
class WithUntyped
  def parse: (String src) -> untyped
  def dump: (untyped obj) -> String
end
`

var voidFrag = `
class WithVoid
  def perform: () -> void
  def initialize: (String name) -> void
end
`

// ---- helpers ----------------------------------------------------------------

func findClass(classes []ClassDecl, name string) *ClassDecl {
	for i := range classes {
		if classes[i].Name == name {
			return &classes[i]
		}
	}
	return nil
}

func findMethod(cd *ClassDecl, name string) *MethodDecl {
	for i := range cd.Methods {
		if cd.Methods[i].Name == name {
			return &cd.Methods[i]
		}
	}
	return nil
}

func findAttr(cd *ClassDecl, name string) *MethodDecl {
	for i := range cd.Attrs {
		if cd.Attrs[i].Name == name {
			return &cd.Attrs[i]
		}
	}
	return nil
}

// ---- tests ------------------------------------------------------------------

// TestParseNokogiriHTML verifies that a namespaced class name is correctly
// parsed and methods with node-set return types are recorded.
func TestParseNokogiriHTML(t *testing.T) {
	classes, err := parseRBSTexts([]string{nokogiriHTMLFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "Nokogiri::HTML::DocumentFragment")
	if cd == nil {
		t.Fatal("class Nokogiri::HTML::DocumentFragment not found")
	}

	t.Run("css method", func(t *testing.T) {
		m := findMethod(cd, "css")
		if m == nil {
			t.Fatal("method css not found")
		}
		if len(m.Types) == 0 {
			t.Fatal("css has no types")
		}
		if len(m.Types[0].Params) != 1 {
			t.Fatalf("expected 1 param, got %d", len(m.Types[0].Params))
		}
		if m.Types[0].Params[0].Type.Kind != TypeString {
			t.Errorf("expected String param, got %v", m.Types[0].Params[0].Type.Kind)
		}
	})

	t.Run("at_css returns optional", func(t *testing.T) {
		m := findMethod(cd, "at_css")
		if m == nil {
			t.Fatal("method at_css not found")
		}
		if m.Types[0].Return.Kind != TypeOptional {
			t.Errorf("expected TypeOptional return, got %v", m.Types[0].Return.Kind)
		}
		if m.Types[0].Return.Elem == nil {
			t.Error("optional elem is nil")
		}
	})

	t.Run("to_html returns string", func(t *testing.T) {
		m := findMethod(cd, "to_html")
		if m == nil {
			t.Fatal("method to_html not found")
		}
		if m.Types[0].Return.Kind != TypeString {
			t.Errorf("expected TypeString return, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("errors attr_reader", func(t *testing.T) {
		a := findAttr(cd, "errors")
		if a == nil {
			t.Fatal("attr errors not found")
		}
		if !a.IsAttr {
			t.Error("IsAttr should be true")
		}
		if a.Types[0].Return.Kind != TypeArray {
			t.Errorf("expected TypeArray, got %v", a.Types[0].Return.Kind)
		}
	})
}

// TestParseRedis verifies method signatures for the Redis class.
func TestParseRedis(t *testing.T) {
	classes, err := parseRBSTexts([]string{redisFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "Redis")
	if cd == nil {
		t.Fatal("class Redis not found")
	}

	t.Run("get returns optional string", func(t *testing.T) {
		m := findMethod(cd, "get")
		if m == nil {
			t.Fatal("method get not found")
		}
		if m.Types[0].Return.Kind != TypeOptional {
			t.Errorf("expected TypeOptional, got %v", m.Types[0].Return.Kind)
		}
		if m.Types[0].Return.Elem.Kind != TypeString {
			t.Errorf("optional elem should be String, got %v", m.Types[0].Return.Elem.Kind)
		}
	})

	t.Run("set returns string", func(t *testing.T) {
		m := findMethod(cd, "set")
		if m == nil {
			t.Fatal("method set not found")
		}
		if m.Types[0].Return.Kind != TypeString {
			t.Errorf("expected TypeString, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("del splat param returns integer", func(t *testing.T) {
		m := findMethod(cd, "del")
		if m == nil {
			t.Fatal("method del not found")
		}
		if m.Types[0].Return.Kind != TypeInteger {
			t.Errorf("expected TypeInteger return, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("hgetall returns hash", func(t *testing.T) {
		m := findMethod(cd, "hgetall")
		if m == nil {
			t.Fatal("method hgetall not found")
		}
		ret := m.Types[0].Return
		if ret.Kind != TypeHash {
			t.Fatalf("expected TypeHash, got %v", ret.Kind)
		}
		if ret.Key.Kind != TypeString {
			t.Errorf("expected String key, got %v", ret.Key.Kind)
		}
		if ret.Value.Kind != TypeString {
			t.Errorf("expected String value, got %v", ret.Value.Kind)
		}
	})
}

// TestParseJSON verifies singleton method parsing.
func TestParseJSON(t *testing.T) {
	classes, err := parseRBSTexts([]string{jsonFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "JSON")
	if cd == nil {
		t.Fatal("module JSON not found")
	}

	t.Run("self.parse is singleton", func(t *testing.T) {
		m := findMethod(cd, "parse")
		if m == nil {
			t.Fatal("method parse not found")
		}
		if m.Kind != MethodSingleton {
			t.Errorf("expected MethodSingleton, got %v", m.Kind)
		}
	})

	t.Run("self.parse returns untyped", func(t *testing.T) {
		m := findMethod(cd, "parse")
		if m == nil {
			t.Fatal("method parse not found")
		}
		if m.Types[0].Return.Kind != TypeUntyped {
			t.Errorf("expected TypeUntyped, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("self.generate is singleton", func(t *testing.T) {
		m := findMethod(cd, "generate")
		if m == nil {
			t.Fatal("method generate not found")
		}
		if m.Kind != MethodSingleton {
			t.Errorf("expected MethodSingleton, got %v", m.Kind)
		}
	})

	t.Run("self.dump is singleton", func(t *testing.T) {
		m := findMethod(cd, "dump")
		if m == nil {
			t.Fatal("method dump not found")
		}
		if m.Kind != MethodSingleton {
			t.Errorf("expected MethodSingleton, got %v", m.Kind)
		}
	})
}

// TestParseAttrDecls verifies attr_reader/accessor/writer parsing.
func TestParseAttrDecls(t *testing.T) {
	classes, err := parseRBSTexts([]string{attrFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "MyModel")
	if cd == nil {
		t.Fatal("class MyModel not found")
	}

	t.Run("attr_reader name is attr", func(t *testing.T) {
		a := findAttr(cd, "name")
		if a == nil {
			t.Fatal("attr name not found")
		}
		if !a.IsAttr {
			t.Error("IsAttr should be true")
		}
		if a.Types[0].Return.Kind != TypeString {
			t.Errorf("expected TypeString, got %v", a.Types[0].Return.Kind)
		}
	})

	t.Run("attr_accessor count is attr", func(t *testing.T) {
		a := findAttr(cd, "count")
		if a == nil {
			t.Fatal("attr count not found")
		}
		if !a.IsAttr {
			t.Error("IsAttr should be true")
		}
		if a.Types[0].Return.Kind != TypeInteger {
			t.Errorf("expected TypeInteger, got %v", a.Types[0].Return.Kind)
		}
	})

	t.Run("attr_writer label is attr", func(t *testing.T) {
		a := findAttr(cd, "label")
		if a == nil {
			t.Fatal("attr label not found")
		}
		if !a.IsAttr {
			t.Error("IsAttr should be true")
		}
	})
}

// TestParseOptionalMethods verifies ? and ! method name suffixes and optional types.
func TestParseOptionalMethods(t *testing.T) {
	classes, err := parseRBSTexts([]string{optionalFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "Maybe")
	if cd == nil {
		t.Fatal("class Maybe not found")
	}

	t.Run("find returns optional string", func(t *testing.T) {
		m := findMethod(cd, "find")
		if m == nil {
			t.Fatal("method find not found")
		}
		if m.Types[0].Return.Kind != TypeOptional {
			t.Errorf("expected TypeOptional, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("lookup returns optional integer", func(t *testing.T) {
		m := findMethod(cd, "lookup")
		if m == nil {
			t.Fatal("method lookup not found")
		}
		if m.Types[0].Return.Kind != TypeOptional {
			t.Errorf("expected TypeOptional, got %v", m.Types[0].Return.Kind)
		}
		if m.Types[0].Return.Elem.Kind != TypeInteger {
			t.Errorf("optional elem should be Integer, got %v", m.Types[0].Return.Elem.Kind)
		}
	})

	t.Run("present? method name includes question mark", func(t *testing.T) {
		m := findMethod(cd, "present?")
		if m == nil {
			t.Fatal("method present? not found")
		}
		if m.Types[0].Return.Kind != TypeBool {
			t.Errorf("expected TypeBool, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("valid! method name includes exclamation mark", func(t *testing.T) {
		m := findMethod(cd, "valid!")
		if m == nil {
			t.Fatal("method valid! not found")
		}
		if m.Types[0].Return.Kind != TypeVoid {
			t.Errorf("expected TypeVoid, got %v", m.Types[0].Return.Kind)
		}
	})
}

// TestParseUnionNilSugar verifies that (T | nil) and (nil | T) both produce TypeOptional.
func TestParseUnionNilSugar(t *testing.T) {
	classes, err := parseRBSTexts([]string{unionNilFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "UnionNil")
	if cd == nil {
		t.Fatal("class UnionNil not found")
	}

	t.Run("String | nil is optional", func(t *testing.T) {
		m := findMethod(cd, "value")
		if m == nil {
			t.Fatal("method value not found")
		}
		if m.Types[0].Return.Kind != TypeOptional {
			t.Errorf("expected TypeOptional, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("nil | Integer is optional", func(t *testing.T) {
		m := findMethod(cd, "other")
		if m == nil {
			t.Fatal("method other not found")
		}
		if m.Types[0].Return.Kind != TypeOptional {
			t.Errorf("expected TypeOptional, got %v", m.Types[0].Return.Kind)
		}
	})
}

// TestParseComplexTypes verifies Array, Hash, Tuple, and Proc type parsing.
func TestParseComplexTypes(t *testing.T) {
	classes, err := parseRBSTexts([]string{complexTypesFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "ComplexTypes")
	if cd == nil {
		t.Fatal("class ComplexTypes not found")
	}

	t.Run("array param and return", func(t *testing.T) {
		m := findMethod(cd, "array_method")
		if m == nil {
			t.Fatal("method array_method not found")
		}
		if m.Types[0].Params[0].Type.Kind != TypeArray {
			t.Errorf("expected TypeArray param, got %v", m.Types[0].Params[0].Type.Kind)
		}
		if m.Types[0].Return.Kind != TypeArray {
			t.Errorf("expected TypeArray return, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("hash param and return", func(t *testing.T) {
		m := findMethod(cd, "hash_method")
		if m == nil {
			t.Fatal("method hash_method not found")
		}
		if m.Types[0].Params[0].Type.Kind != TypeHash {
			t.Errorf("expected TypeHash param, got %v", m.Types[0].Params[0].Type.Kind)
		}
		if m.Types[0].Return.Kind != TypeHash {
			t.Errorf("expected TypeHash return, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("tuple return", func(t *testing.T) {
		m := findMethod(cd, "tuple_method")
		if m == nil {
			t.Fatal("method tuple_method not found")
		}
		if m.Types[0].Return.Kind != TypeTuple {
			t.Errorf("expected TypeTuple, got %v", m.Types[0].Return.Kind)
		}
		if len(m.Types[0].Return.Members) != 3 {
			t.Errorf("expected 3 tuple members, got %d", len(m.Types[0].Return.Members))
		}
	})

	t.Run("proc param", func(t *testing.T) {
		m := findMethod(cd, "proc_method")
		if m == nil {
			t.Fatal("method proc_method not found")
		}
		if m.Types[0].Params[0].Type.Kind != TypeProc {
			t.Errorf("expected TypeProc param, got %v", m.Types[0].Params[0].Type.Kind)
		}
	})
}

// TestParseNestedClasses verifies that nested classes are flattened with qualified names.
func TestParseNestedClasses(t *testing.T) {
	classes, err := parseRBSTexts([]string{nestedClassFrag})
	if err != nil {
		t.Fatal(err)
	}

	t.Run("outer class exists", func(t *testing.T) {
		cd := findClass(classes, "Outer")
		if cd == nil {
			t.Fatal("class Outer not found")
		}
		m := findMethod(cd, "outer_method")
		if m == nil {
			t.Fatal("outer_method not found on Outer")
		}
	})

	t.Run("nested class has qualified name", func(t *testing.T) {
		cd := findClass(classes, "Outer::Inner")
		if cd == nil {
			t.Fatal("class Outer::Inner not found; nested classes should be flattened")
		}
		m := findMethod(cd, "inner_method")
		if m == nil {
			t.Fatal("inner_method not found on Outer::Inner")
		}
	})
}

// TestParseMethodOverloads verifies that method overloads are all recorded.
func TestParseMethodOverloads(t *testing.T) {
	classes, err := parseRBSTexts([]string{overloadFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "Overloaded")
	if cd == nil {
		t.Fatal("class Overloaded not found")
	}
	m := findMethod(cd, "convert")
	if m == nil {
		t.Fatal("method convert not found")
	}
	if len(m.Types) < 3 {
		t.Errorf("expected at least 3 overloads, got %d", len(m.Types))
	}
}

// TestParseUntypedReturn verifies that `untyped` return types are represented as TypeUntyped.
func TestParseUntypedReturn(t *testing.T) {
	classes, err := parseRBSTexts([]string{untypedFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "WithUntyped")
	if cd == nil {
		t.Fatal("class WithUntyped not found")
	}

	t.Run("parse returns untyped", func(t *testing.T) {
		m := findMethod(cd, "parse")
		if m == nil {
			t.Fatal("method parse not found")
		}
		if m.Types[0].Return.Kind != TypeUntyped {
			t.Errorf("expected TypeUntyped, got %v", m.Types[0].Return.Kind)
		}
	})

	t.Run("dump takes untyped param", func(t *testing.T) {
		m := findMethod(cd, "dump")
		if m == nil {
			t.Fatal("method dump not found")
		}
		if m.Types[0].Params[0].Type.Kind != TypeUntyped {
			t.Errorf("expected TypeUntyped param, got %v", m.Types[0].Params[0].Type.Kind)
		}
	})
}

// TestParseVoidReturn verifies that `void` return types are TypeVoid.
func TestParseVoidReturn(t *testing.T) {
	classes, err := parseRBSTexts([]string{voidFrag})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "WithVoid")
	if cd == nil {
		t.Fatal("class WithVoid not found")
	}

	t.Run("perform returns void", func(t *testing.T) {
		m := findMethod(cd, "perform")
		if m == nil {
			t.Fatal("method perform not found")
		}
		if m.Types[0].Return.Kind != TypeVoid {
			t.Errorf("expected TypeVoid, got %v", m.Types[0].Return.Kind)
		}
	})
}

// TestParseMultipleTexts verifies that multiple RBS text inputs are merged.
func TestParseMultipleTexts(t *testing.T) {
	classes, err := parseRBSTexts([]string{redisFrag, jsonFrag})
	if err != nil {
		t.Fatal(err)
	}
	if findClass(classes, "Redis") == nil {
		t.Error("Redis class not found in multi-text parse")
	}
	if findClass(classes, "JSON") == nil {
		t.Error("JSON module not found in multi-text parse")
	}
}

// TestParseEmptyInput verifies that empty input returns no classes without error.
func TestParseEmptyInput(t *testing.T) {
	classes, err := parseRBSTexts(nil)
	if err != nil {
		t.Fatal(err)
	}
	if len(classes) != 0 {
		t.Errorf("expected 0 classes, got %d", len(classes))
	}
}

// TestParseGarbageInput verifies that unparseable content is silently skipped.
func TestParseGarbageInput(t *testing.T) {
	garbage := `this is not valid rbs at all !!! ???`
	_, err := parseRBSTexts([]string{garbage})
	if err != nil {
		t.Errorf("parser should not error on garbage input, got: %v", err)
	}
}

// TestParseTokenizer verifies basic tokenizer output.
func TestParseTokenizer(t *testing.T) {
	src := "def foo: (String x) -> Integer"
	tokens := tokenize(src)
	if len(tokens) == 0 {
		t.Fatal("expected tokens, got none")
	}
	// Last token should be EOF
	if tokens[len(tokens)-1].kind != tokEOF {
		t.Error("expected EOF as last token")
	}
}

// TestParseHashReturnTypes verifies Hash[K, V] with various key/value types.
func TestParseHashReturnTypes(t *testing.T) {
	src := `
class HashReturn
  def string_int: () -> Hash[String, Integer]
  def symbol_arr: () -> Hash[Symbol, Array[String]]
end
`
	classes, err := parseRBSTexts([]string{src})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "HashReturn")
	if cd == nil {
		t.Fatal("class HashReturn not found")
	}

	t.Run("string_int hash", func(t *testing.T) {
		m := findMethod(cd, "string_int")
		if m == nil {
			t.Fatal("method string_int not found")
		}
		ret := m.Types[0].Return
		if ret.Kind != TypeHash {
			t.Fatalf("expected TypeHash, got %v", ret.Kind)
		}
		if ret.Key.Kind != TypeString {
			t.Errorf("expected String key, got %v", ret.Key.Kind)
		}
		if ret.Value.Kind != TypeInteger {
			t.Errorf("expected Integer value, got %v", ret.Value.Kind)
		}
	})

	t.Run("symbol_arr hash", func(t *testing.T) {
		m := findMethod(cd, "symbol_arr")
		if m == nil {
			t.Fatal("method symbol_arr not found")
		}
		ret := m.Types[0].Return
		if ret.Kind != TypeHash {
			t.Fatalf("expected TypeHash, got %v", ret.Kind)
		}
		if ret.Key.Kind != TypeSymbol {
			t.Errorf("expected Symbol key, got %v", ret.Key.Kind)
		}
		if ret.Value.Kind != TypeArray {
			t.Errorf("expected Array value, got %v", ret.Value.Kind)
		}
	})
}

// TestParseSingletonMethodKind verifies that singleton methods are MethodSingleton.
func TestParseSingletonMethodKind(t *testing.T) {
	src := `
class Factory
  def self.create: (String name) -> Factory
  def instance_method: () -> String
end
`
	classes, err := parseRBSTexts([]string{src})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "Factory")
	if cd == nil {
		t.Fatal("class Factory not found")
	}

	t.Run("self.create is singleton", func(t *testing.T) {
		m := findMethod(cd, "create")
		if m == nil {
			t.Fatal("method create not found")
		}
		if m.Kind != MethodSingleton {
			t.Errorf("expected MethodSingleton, got %v", m.Kind)
		}
	})

	t.Run("instance_method is instance", func(t *testing.T) {
		m := findMethod(cd, "instance_method")
		if m == nil {
			t.Fatal("method instance_method not found")
		}
		if m.Kind != MethodInstance {
			t.Errorf("expected MethodInstance, got %v", m.Kind)
		}
	})
}

// TestParseInterfaceDecl verifies that interface declarations are parsed as ClassDecls.
func TestParseInterfaceDecl(t *testing.T) {
	src := `
interface _Stringable
  def to_s: () -> String
end
`
	classes, err := parseRBSTexts([]string{src})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "_Stringable")
	if cd == nil {
		t.Fatal("interface _Stringable not found")
	}
	m := findMethod(cd, "to_s")
	if m == nil {
		t.Fatal("method to_s not found on _Stringable")
	}
}

// TestParseComments verifies that RBS comments are stripped cleanly.
func TestParseComments(t *testing.T) {
	src := `
# This is a top-level comment
class Commented
  # This is a method comment
  def greet: (String name) -> String
end
`
	classes, err := parseRBSTexts([]string{src})
	if err != nil {
		t.Fatal(err)
	}
	cd := findClass(classes, "Commented")
	if cd == nil {
		t.Fatal("class Commented not found")
	}
	m := findMethod(cd, "greet")
	if m == nil {
		t.Fatal("method greet not found")
	}
	if m.Types[0].Return.Kind != TypeString {
		t.Errorf("expected TypeString, got %v", m.Types[0].Return.Kind)
	}
}

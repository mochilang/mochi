package swiftinterface_test

import (
	"testing"

	"mochi/package3/swift/swiftinterface"
)

const miniInterface = `// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 5.9
// swift-module-flags: -module-name MyModule -enable-library-evolution

import Swift

public struct Point {
  public var x: Double
  public var y: Double
}

public enum Direction {
  case north
  case south
}

@available(macOS 14.0, *)
public func greet(name: String) -> String

public func add(_ a: Int, _ b: Int) -> Int

public func fetchData(url: String) async throws -> String

extension Point {
  public func distance(to other: Point) -> Double
}
`

func TestParseString(t *testing.T) {
	surf, err := swiftinterface.ParseString(miniInterface)
	if err != nil {
		t.Fatalf("ParseString error: %v", err)
	}

	if surf.ModuleName != "MyModule" {
		t.Errorf("ModuleName = %q, want %q", surf.ModuleName, "MyModule")
	}

	// Check types
	if len(surf.Types) < 2 {
		t.Errorf("Types count = %d, want >= 2: %v", len(surf.Types), surf.Types)
	} else {
		found := map[string]string{}
		for _, ty := range surf.Types {
			found[ty.Name] = ty.Kind
		}
		if found["Point"] != "struct" {
			t.Errorf("Point kind = %q, want struct", found["Point"])
		}
		if found["Direction"] != "enum" {
			t.Errorf("Direction kind = %q, want enum", found["Direction"])
		}
	}

	// Check functions
	if len(surf.Functions) < 3 {
		t.Errorf("Functions count = %d, want >= 3", len(surf.Functions))
	}

	fnMap := map[string]*swiftinterface.FuncDecl{}
	for i := range surf.Functions {
		fnMap[surf.Functions[i].Name] = &surf.Functions[i]
	}

	greet, ok := fnMap["greet"]
	if !ok {
		t.Error("greet function not found")
	} else {
		if !greet.IsPublic {
			t.Error("greet should be public")
		}
		if greet.ReturnType != "String" {
			t.Errorf("greet return type = %q, want String", greet.ReturnType)
		}
		if len(greet.Attributes) == 0 {
			t.Error("greet should have @available attribute")
		}
	}

	add, ok := fnMap["add"]
	if !ok {
		t.Error("add function not found")
	} else {
		if len(add.Params) != 2 {
			t.Errorf("add params count = %d, want 2", len(add.Params))
		}
		if add.ReturnType != "Int" {
			t.Errorf("add return type = %q, want Int", add.ReturnType)
		}
	}

	fetch, ok := fnMap["fetchData"]
	if !ok {
		t.Error("fetchData function not found")
	} else {
		if !fetch.IsAsync {
			t.Error("fetchData should be async")
		}
		if !fetch.IsThrowing {
			t.Error("fetchData should be throwing")
		}
	}

	// Check extensions
	if len(surf.Extensions) < 1 {
		t.Errorf("Extensions count = %d, want >= 1", len(surf.Extensions))
	} else {
		ext := surf.Extensions[0]
		if ext.TypeName != "Point" {
			t.Errorf("Extension type = %q, want Point", ext.TypeName)
		}
		if len(ext.Functions) < 1 {
			t.Errorf("Extension functions count = %d, want >= 1", len(ext.Functions))
		}
	}
}

func TestParseStringEmptyModule(t *testing.T) {
	src := `// swift-interface-format-version: 1.0

public func simple() -> Int
`
	surf, err := swiftinterface.ParseString(src)
	if err != nil {
		t.Fatalf("ParseString error: %v", err)
	}
	if len(surf.Functions) != 1 {
		t.Errorf("Functions count = %d, want 1", len(surf.Functions))
	}
	if surf.Functions[0].Name != "simple" {
		t.Errorf("Function name = %q, want simple", surf.Functions[0].Name)
	}
	if surf.Functions[0].ReturnType != "Int" {
		t.Errorf("Return type = %q, want Int", surf.Functions[0].ReturnType)
	}
}

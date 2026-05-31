package apisurface_test

import (
	"testing"

	"mochi/package3/typescript/apisurface"
)

// TestPhase4ApiSurface is the MEP-72 Phase 4 gate for the apisurface package.
func TestPhase4ApiSurface(t *testing.T) {
	raw := `{
		"packageName": "lodash",
		"version": "4.17.21",
		"exports": {
			"./chunk": [
				{"kind": "function", "name": "chunk", "params": [{"name": "array", "type": "any[]"}, {"name": "size", "type": "number"}], "returnType": "any[][]"}
			],
			"./compact": [
				{"kind": "function", "name": "compact", "params": [{"name": "array", "type": "any[]"}], "returnType": "any[]"}
			]
		}
	}`

	surface, err := apisurface.Parse([]byte(raw))
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if surface.PackageName != "lodash" {
		t.Errorf("PackageName = %q, want lodash", surface.PackageName)
	}
	if surface.Version != "4.17.21" {
		t.Errorf("Version = %q, want 4.17.21", surface.Version)
	}
	if len(surface.Exports) != 2 {
		t.Errorf("len(Exports) = %d, want 2", len(surface.Exports))
	}
	all := surface.AllExports()
	if len(all) != 2 {
		t.Errorf("AllExports count = %d, want 2", len(all))
	}

	// Check chunk export.
	chunkExports, ok := surface.Exports["./chunk"]
	if !ok || len(chunkExports) == 0 {
		t.Fatal("./chunk export not found")
	}
	chunk := chunkExports[0]
	if chunk.Name != "chunk" {
		t.Errorf("chunk name = %q", chunk.Name)
	}
	if len(chunk.Params) != 2 {
		t.Errorf("chunk params count = %d, want 2", len(chunk.Params))
	}
}

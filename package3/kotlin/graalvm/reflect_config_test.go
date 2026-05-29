package graalvm

import (
	"encoding/json"
	"testing"

	"github.com/mochilang/mochi/package3/kotlin/metadata"
)

func TestGenerateReflectConfig_DataClass(t *testing.T) {
	classes := []*metadata.APIObject{
		{
			ClassName: "com.example.User",
			Kind:      metadata.ClassKindDataClass,
			Properties: []metadata.Property{
				{Name: "id", Type: metadata.KotlinType{ClassName: "kotlin.Long"}},
				{Name: "name", Type: metadata.KotlinType{ClassName: "kotlin.String"}},
			},
		},
	}

	data, err := GenerateReflectConfig(classes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var entries []ReflectConfigEntry
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("invalid JSON: %v\noutput: %s", err, data)
	}

	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	e := entries[0]
	if e.Name != "com.example.User" {
		t.Errorf("name: got %q, want %q", e.Name, "com.example.User")
	}
	if !e.AllDeclaredMethods {
		t.Errorf("expected allDeclaredMethods=true for data class")
	}
}

func TestGenerateReflectConfig_EnumClass(t *testing.T) {
	classes := []*metadata.APIObject{
		{
			ClassName:   "com.example.Color",
			Kind:        metadata.ClassKindEnumClass,
			EnumEntries: []string{"RED", "GREEN", "BLUE"},
		},
	}

	data, err := GenerateReflectConfig(classes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var entries []ReflectConfigEntry
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("invalid JSON: %v\noutput: %s", err, data)
	}

	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	e := entries[0]
	if e.Name != "com.example.Color" {
		t.Errorf("name: got %q, want %q", e.Name, "com.example.Color")
	}
	if !e.AllDeclaredMethods {
		t.Errorf("expected allDeclaredMethods=true for enum class")
	}
	if !e.AllDeclaredFields {
		t.Errorf("expected allDeclaredFields=true for enum class")
	}
}

func TestGenerateReflectConfig_SealedClass(t *testing.T) {
	successSub := &metadata.APIObject{
		ClassName: "com.example.Result$Success",
		Kind:      metadata.ClassKindDataClass,
	}
	failureSub := &metadata.APIObject{
		ClassName: "com.example.Result$Failure",
		Kind:      metadata.ClassKindDataClass,
	}
	classes := []*metadata.APIObject{
		{
			ClassName:  "com.example.Result",
			Kind:       metadata.ClassKindSealedClass,
			SealedSubs: []*metadata.APIObject{successSub, failureSub},
		},
	}

	data, err := GenerateReflectConfig(classes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var entries []ReflectConfigEntry
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("invalid JSON: %v\noutput: %s", err, data)
	}

	// Expect 3 entries: Result + Success + Failure
	if len(entries) != 3 {
		t.Fatalf("expected 3 entries (sealed + 2 subs), got %d", len(entries))
	}

	names := make(map[string]bool)
	for _, e := range entries {
		names[e.Name] = true
	}
	for _, want := range []string{"com.example.Result", "com.example.Result$Success", "com.example.Result$Failure"} {
		if !names[want] {
			t.Errorf("missing entry for %q", want)
		}
	}
}

func TestGenerateReflectConfig_Empty(t *testing.T) {
	data, err := GenerateReflectConfig(nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var entries []ReflectConfigEntry
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("invalid JSON: %v\noutput: %s", err, data)
	}

	if len(entries) != 0 {
		t.Errorf("expected empty array, got %d entries", len(entries))
	}
}

func TestGenerateReflectConfig_ValidJSON(t *testing.T) {
	classes := []*metadata.APIObject{
		{ClassName: "com.example.Foo", Kind: metadata.ClassKindClass},
		{ClassName: "com.example.Bar", Kind: metadata.ClassKindInterface},
	}

	data, err := GenerateReflectConfig(classes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var entries []ReflectConfigEntry
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("output is not valid JSON: %v\noutput: %s", err, data)
	}

	if len(entries) != 2 {
		t.Errorf("expected 2 entries, got %d", len(entries))
	}
	for _, e := range entries {
		if e.Name == "" {
			t.Errorf("entry has empty name: %+v", e)
		}
	}
}

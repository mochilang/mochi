package graalvm

import (
	"encoding/json"
	"fmt"

	"github.com/mochilang/mochi/package3/kotlin/metadata"
)

// ReflectConfigEntry is a single entry in a GraalVM reflect-config.json.
type ReflectConfigEntry struct {
	Name                    string `json:"name"`
	AllDeclaredConstructors bool   `json:"allDeclaredConstructors,omitempty"`
	AllDeclaredMethods      bool   `json:"allDeclaredMethods,omitempty"`
	AllDeclaredFields       bool   `json:"allDeclaredFields,omitempty"`
}

// GenerateReflectConfig generates a GraalVM reflect-config.json from the APIObject tree.
// For each data class, enum, sealed class hierarchy: adds the class + all its methods.
// For Kotlin companion objects: adds the INSTANCE field.
//
// Output is a JSON array of ReflectConfigEntry.
func GenerateReflectConfig(classes []*metadata.APIObject) ([]byte, error) {
	var entries []ReflectConfigEntry

	for _, obj := range classes {
		collectEntries(obj, &entries)
	}

	if len(entries) == 0 {
		return []byte("[]"), nil
	}

	data, err := json.MarshalIndent(entries, "", "  ")
	if err != nil {
		return nil, fmt.Errorf("graalvm: marshal reflect-config: %w", err)
	}
	return data, nil
}

// collectEntries recursively collects ReflectConfigEntry items from an APIObject.
func collectEntries(obj *metadata.APIObject, entries *[]ReflectConfigEntry) {
	entry := reflectEntryForKind(obj)
	*entries = append(*entries, entry)

	// For sealed classes, also add each subclass.
	if obj.Kind == metadata.ClassKindSealedClass {
		for _, sub := range obj.SealedSubs {
			collectEntries(sub, entries)
		}
	}

	// For nested objects, recurse.
	for _, nested := range obj.Nested {
		collectEntries(nested, entries)
	}
}

// reflectEntryForKind builds a ReflectConfigEntry for the given APIObject based on its kind.
func reflectEntryForKind(obj *metadata.APIObject) ReflectConfigEntry {
	switch obj.Kind {
	case metadata.ClassKindCompanionObject:
		// Companion objects need the INSTANCE field for reflection access.
		return ReflectConfigEntry{
			Name:               obj.ClassName,
			AllDeclaredFields:  true,
			AllDeclaredMethods: true,
		}
	case metadata.ClassKindEnumClass:
		// Enums need fields (for enum entries) and methods.
		return ReflectConfigEntry{
			Name:                    obj.ClassName,
			AllDeclaredConstructors: true,
			AllDeclaredMethods:      true,
			AllDeclaredFields:       true,
		}
	case metadata.ClassKindDataClass:
		// Data classes need constructors (for component functions), methods, and fields.
		return ReflectConfigEntry{
			Name:                    obj.ClassName,
			AllDeclaredConstructors: true,
			AllDeclaredMethods:      true,
			AllDeclaredFields:       true,
		}
	case metadata.ClassKindSealedClass:
		// Sealed classes and all subclasses need full reflection.
		return ReflectConfigEntry{
			Name:                    obj.ClassName,
			AllDeclaredConstructors: true,
			AllDeclaredMethods:      true,
			AllDeclaredFields:       true,
		}
	default:
		// All other classes: constructors + methods are sufficient.
		return ReflectConfigEntry{
			Name:                    obj.ClassName,
			AllDeclaredConstructors: true,
			AllDeclaredMethods:      true,
		}
	}
}

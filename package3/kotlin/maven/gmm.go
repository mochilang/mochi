package maven

import (
	"encoding/json"
	"fmt"
	"io"
)

// GradleModule represents the contents of a Gradle Module Metadata (.module) file.
type GradleModule struct {
	FormatVersion string    `json:"formatVersion"`
	Component     GMComponent `json:"component"`
	Variants      []Variant `json:"variants"`
}

// GMComponent holds component identity information.
type GMComponent struct {
	Group   string `json:"group"`
	Module  string `json:"module"`
	Version string `json:"version"`
}

// Variant represents a single variant in a Gradle Module Metadata file.
type Variant struct {
	Name         string            `json:"name"`
	Attributes   map[string]string `json:"attributes"`
	Files        []ModuleFile      `json:"files"`
	Dependencies []GMDependency    `json:"dependencies"`
	// DependencyConstraints captures version constraints.
	DependencyConstraints []GMDependencyConstraint `json:"dependencyConstraints"`
}

// ModuleFile describes a file artifact in a Gradle Module Metadata variant.
type ModuleFile struct {
	Name   string `json:"name"`
	URL    string `json:"url"`
	SHA256 string `json:"sha256"`
	MD5    string `json:"md5"`
	Size   int64  `json:"size"`
}

// GMDependency represents a dependency in a Gradle Module Metadata variant.
type GMDependency struct {
	Group     string              `json:"group"`
	Module    string              `json:"module"`
	Version   GMVersionConstraint `json:"version"`
	Exclusions []GMExclusion      `json:"excludes"`
}

// GMVersionConstraint holds Maven-style version range or exact version.
type GMVersionConstraint struct {
	Requires string `json:"requires"`
	Prefers  string `json:"prefers"`
	Strictly string `json:"strictly"`
}

// ResolvedVersion returns the most specific version from the constraint.
func (c GMVersionConstraint) ResolvedVersion() string {
	if c.Strictly != "" {
		return c.Strictly
	}
	if c.Requires != "" {
		return c.Requires
	}
	return c.Prefers
}

// GMExclusion represents an exclusion in a Gradle Module Metadata dependency.
type GMExclusion struct {
	Group  string `json:"group"`
	Module string `json:"module"`
}

// GMDependencyConstraint represents a dependency constraint.
type GMDependencyConstraint struct {
	Group   string              `json:"group"`
	Module  string              `json:"module"`
	Version GMVersionConstraint `json:"version"`
}

// ParseGradleModule parses a Gradle Module Metadata (.module) JSON file.
func ParseGradleModule(r io.Reader) (*GradleModule, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("gradle-module: read error: %w", err)
	}

	var m GradleModule
	if err := json.Unmarshal(data, &m); err != nil {
		return nil, fmt.Errorf("gradle-module: json parse error: %w", err)
	}

	return &m, nil
}

// SelectJVMVariant picks the best JVM variant from a Gradle Module Metadata file.
// It prefers variants with:
//   - org.jetbrains.kotlin.platform.type == "jvm"
//   - org.gradle.usage == "java-api" (preferred) or "java-runtime"
//
// Returns nil if no suitable variant is found.
func SelectJVMVariant(m *GradleModule) *Variant {
	if m == nil {
		return nil
	}

	var apiVariant *Variant
	var runtimeVariant *Variant

	for i := range m.Variants {
		v := &m.Variants[i]
		attrs := v.Attributes

		// Check Kotlin platform type
		platformType := attrs["org.jetbrains.kotlin.platform.type"]
		if platformType != "" && platformType != "jvm" {
			continue
		}

		usage := attrs["org.gradle.usage"]
		switch usage {
		case "java-api":
			if apiVariant == nil {
				apiVariant = v
			}
		case "java-runtime":
			if runtimeVariant == nil {
				runtimeVariant = v
			}
		}
	}

	if apiVariant != nil {
		return apiVariant
	}
	return runtimeVariant
}

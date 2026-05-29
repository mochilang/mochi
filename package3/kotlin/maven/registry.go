// Package maven provides a Maven Central metadata client, coordinate types,
// POM parsing, and transitive dependency resolution.
package maven

import "strings"

// Registry holds the base URL for an artifact repository.
type Registry struct {
	Name    string
	BaseURL string
	// Match is an optional function that returns true if this registry serves
	// the given coordinate. If nil, the registry is considered a match-all fallback.
	Match func(coord Coordinate) bool
}

// Predefined registries.
var (
	// RegistryMavenCentral is the default Maven Central repository.
	RegistryMavenCentral = Registry{
		Name:    "MavenCentral",
		BaseURL: "https://repo1.maven.org/maven2",
	}

	// RegistryJitPack serves artifacts from GitHub/Bitbucket/GitLab repositories.
	RegistryJitPack = Registry{
		Name:    "JitPack",
		BaseURL: "https://jitpack.io",
		Match: func(coord Coordinate) bool {
			return strings.HasPrefix(coord.GroupID, "com.github.") ||
				strings.HasPrefix(coord.GroupID, "com.gitlab.") ||
				strings.HasPrefix(coord.GroupID, "com.bitbucket.") ||
				coord.GroupID == "com.github" ||
				coord.GroupID == "com.gitlab" ||
				coord.GroupID == "com.bitbucket"
		},
	}

	// RegistryGoogleMaven is Google's Maven repository for Android artifacts.
	RegistryGoogleMaven = Registry{
		Name:    "GoogleMaven",
		BaseURL: "https://maven.google.com",
		Match: func(coord Coordinate) bool {
			return strings.HasPrefix(coord.GroupID, "com.google.") ||
				strings.HasPrefix(coord.GroupID, "com.android.") ||
				strings.HasPrefix(coord.GroupID, "androidx.") ||
				coord.GroupID == "android" ||
				coord.GroupID == "com.google.android"
		},
	}
)

// NewCustomRegistry creates a Registry with a custom base URL that matches all coordinates.
func NewCustomRegistry(name, baseURL string) Registry {
	return Registry{
		Name:    name,
		BaseURL: strings.TrimRight(baseURL, "/"),
	}
}

// RegistryFor returns the first registry in the list whose Match function returns true
// for the given coordinate. If no specific match is found, the first registry with a
// nil Match (catch-all) is returned. If none match, the default MavenCentral is returned.
func RegistryFor(registries []Registry, coord Coordinate) Registry {
	var fallback *Registry
	for i := range registries {
		r := &registries[i]
		if r.Match == nil {
			if fallback == nil {
				fallback = r
			}
			continue
		}
		if r.Match(coord) {
			return *r
		}
	}
	if fallback != nil {
		return *fallback
	}
	return RegistryMavenCentral
}

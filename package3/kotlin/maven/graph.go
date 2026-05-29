package maven

import (
	"context"
	"fmt"
	"strings"

	"github.com/mochilang/mochi/package3/kotlin/semver"
)

// excludeKey is a groupId:artifactId key for exclusion sets.
type excludeKey struct {
	GroupID    string
	ArtifactID string
}

// bfsEntry is a BFS queue entry during transitive resolution.
type bfsEntry struct {
	coord      Coordinate
	exclusions map[excludeKey]bool
}

// ResolveTransitive resolves the full transitive dependency graph starting from
// the given root coordinates.
//
// Rules:
//   - Only "compile" and "runtime" scoped dependencies are included.
//   - "test", "provided", "system", and "import" scoped deps are skipped.
//   - Exclusions are propagated transitively.
//   - If the same groupId:artifactId appears multiple times, the higher version wins.
//   - Returns dependencies in topological order (dependencies before dependents).
func ResolveTransitive(ctx context.Context, client *Client, rootCoords []Coordinate) ([]Coordinate, error) {
	// resolved maps groupId:artifactId -> best version found
	type resolvedEntry struct {
		coord Coordinate
		order int
	}
	resolved := make(map[string]*resolvedEntry)
	orderCounter := 0

	queue := make([]bfsEntry, 0, len(rootCoords))
	for _, c := range rootCoords {
		queue = append(queue, bfsEntry{coord: c, exclusions: nil})
	}

	// Track visited to avoid re-fetching the same coord
	visited := make(map[string]bool)

	for len(queue) > 0 {
		entry := queue[0]
		queue = queue[1:]

		coord := entry.coord
		gaKey := coord.GroupID + ":" + coord.ArtifactID

		// Check if we've already resolved this GA at a higher version
		if existing, ok := resolved[gaKey]; ok {
			existingVer, err1 := semver.Parse(existing.coord.Version)
			newVer, err2 := semver.Parse(coord.Version)
			if err1 == nil && err2 == nil {
				if semver.Compare(newVer, existingVer) <= 0 {
					// Existing version is >= new version, skip
					continue
				}
				// New version is higher, update and re-process dependencies
				existing.coord = coord
			}
		} else {
			resolved[gaKey] = &resolvedEntry{coord: coord, order: orderCounter}
			orderCounter++
		}

		// Avoid re-fetching if we already visited this exact version
		visitKey := coord.GroupID + ":" + coord.ArtifactID + "@" + coord.Version
		if visited[visitKey] {
			continue
		}
		visited[visitKey] = true

		// Fetch the POM
		pom, err := client.FetchPOM(ctx, coord)
		if err != nil {
			// Skip artifacts that can't be fetched (might be absent in registry)
			continue
		}

		// Apply dependency management to resolve any missing versions
		applyDepMgmt(pom)

		// Process each dependency
		for _, dep := range pom.Dependencies {
			scope := strings.ToLower(dep.EffectiveScope())
			// Only include compile and runtime scopes
			if scope != "compile" && scope != "runtime" {
				continue
			}
			if dep.IsOptional() {
				continue
			}
			if dep.GroupID == "" || dep.ArtifactID == "" {
				continue
			}

			// Check exclusions
			depKey := excludeKey{GroupID: dep.GroupID, ArtifactID: dep.ArtifactID}
			if entry.exclusions[depKey] {
				continue
			}
			// Also check wildcard exclusions
			if entry.exclusions[excludeKey{GroupID: dep.GroupID, ArtifactID: "*"}] {
				continue
			}
			if entry.exclusions[excludeKey{GroupID: "*", ArtifactID: dep.ArtifactID}] {
				continue
			}

			// Handle BOM imports
			if dep.Type == "pom" && scope == "import" {
				continue // handled separately by ResolveBOM
			}

			depVersion := dep.Version
			if depVersion == "" {
				// Try to find version in dependency management
				for _, dm := range pom.DependencyManagement {
					if dm.GroupID == dep.GroupID && dm.ArtifactID == dep.ArtifactID {
						depVersion = dm.Version
						break
					}
				}
			}
			if depVersion == "" {
				// Can't resolve version, skip
				continue
			}

			depCoord := Coordinate{
				GroupID:    dep.GroupID,
				ArtifactID: dep.ArtifactID,
				Version:    depVersion,
			}

			// Build exclusion set for this dependency
			childExclusions := make(map[excludeKey]bool)
			// Inherit parent exclusions
			for k, v := range entry.exclusions {
				childExclusions[k] = v
			}
			// Add this dep's own exclusions
			for _, excl := range dep.Exclusions {
				childExclusions[excludeKey{GroupID: excl.GroupID, ArtifactID: excl.ArtifactID}] = true
			}

			queue = append(queue, bfsEntry{coord: depCoord, exclusions: childExclusions})
		}
	}

	// Build result in topological order
	type orderedCoord struct {
		coord Coordinate
		order int
	}
	ordered := make([]orderedCoord, 0, len(resolved))
	for _, entry := range resolved {
		ordered = append(ordered, orderedCoord{coord: entry.coord, order: entry.order})
	}
	// Sort by order (insertion order = BFS order = approximate topological order)
	for i := 1; i < len(ordered); i++ {
		for j := i; j > 0 && ordered[j].order < ordered[j-1].order; j-- {
			ordered[j], ordered[j-1] = ordered[j-1], ordered[j]
		}
	}

	result := make([]Coordinate, len(ordered))
	for i, oc := range ordered {
		result[i] = oc.coord
	}
	return result, nil
}

// applyDepMgmt fills in missing versions from the POM's own dependencyManagement.
func applyDepMgmt(pom *POM) {
	dmVersions := make(map[string]string)
	for _, dm := range pom.DependencyManagement {
		if dm.Type != "pom" {
			key := dm.GroupID + ":" + dm.ArtifactID
			if dm.Version != "" {
				dmVersions[key] = dm.Version
			}
		}
	}
	for i := range pom.Dependencies {
		if pom.Dependencies[i].Version == "" {
			key := pom.Dependencies[i].GroupID + ":" + pom.Dependencies[i].ArtifactID
			if v, ok := dmVersions[key]; ok {
				pom.Dependencies[i].Version = v
			}
		}
	}
}

// ResolveBOM fetches a BOM POM (a POM with packaging=pom imported via scope=import)
// and returns it for use in dependencyManagement.
func ResolveBOM(ctx context.Context, client *Client, coord Coordinate) (*POM, error) {
	pom, err := client.FetchPOM(ctx, coord)
	if err != nil {
		return nil, fmt.Errorf("graph: fetch BOM %s: %w", coord, err)
	}
	return pom, nil
}

// Package extern generates and parses shim.mochi files for the Mochi↔Kotlin bridge.
package extern

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// ShimInfo holds parsed information from an existing shim.mochi file.
type ShimInfo struct {
	// CustomLines are lines that end with " custom" (trimmed).
	CustomLines []string
	// ArtifactCoord is from the header comment "// Artifact: <coord>".
	ArtifactCoord string
	// WrapperSHA256 is from the header comment "// Wrapper: ... (sha256: <hex>)".
	WrapperSHA256 string
}

// ParseShim reads an existing shim.mochi file and returns its parsed metadata.
func ParseShim(path string) (*ShimInfo, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("extern: open shim %q: %w", path, err)
	}
	defer f.Close()

	info := &ShimInfo{}
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()

		// Parse header comments.
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "// Artifact:") {
			info.ArtifactCoord = strings.TrimSpace(strings.TrimPrefix(trimmed, "// Artifact:"))
			continue
		}
		if strings.HasPrefix(trimmed, "// Wrapper:") {
			// Extract sha256 from "// Wrapper: ... (sha256: <hex>)"
			rest := strings.TrimSpace(strings.TrimPrefix(trimmed, "// Wrapper:"))
			if idx := strings.Index(rest, "(sha256:"); idx >= 0 {
				sha := rest[idx+len("(sha256:"):]
				sha = strings.TrimSuffix(strings.TrimSpace(sha), ")")
				info.WrapperSHA256 = strings.TrimSpace(sha)
			}
			continue
		}

		// Collect custom lines.
		if strings.HasSuffix(trimmed, " custom") {
			info.CustomLines = append(info.CustomLines, trimmed)
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("extern: scan shim %q: %w", path, err)
	}

	return info, nil
}

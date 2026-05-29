package reflect

import (
	"encoding/json"
	"fmt"
)

// ParseSurface parses the JSON output of the reflect-tool.jar into a Surface.
func ParseSurface(data []byte) (*Surface, error) {
	var s Surface
	if err := json.Unmarshal(data, &s); err != nil {
		return nil, fmt.Errorf("reflect: parse surface: %w", err)
	}
	if s.SchemaVersion != 1 {
		return nil, fmt.Errorf("reflect: unsupported schema version %d (want 1)", s.SchemaVersion)
	}
	return &s, nil
}

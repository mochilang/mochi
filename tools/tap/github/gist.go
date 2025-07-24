package github

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
)

// File represents a single file in a gist.
type File struct {
	Content string `json:"content"`
}

// Gist contains the minimal fields returned by the GitHub API when creating a gist.
type Gist struct {
	ID      string `json:"id"`
	HTMLURL string `json:"html_url"`
}

// Create creates a new GitHub gist with the given files.
// If token is empty, the GITHUB_TOKEN or GITHUB_GIST_TOKEN environment variable is used.
// The returned Gist contains the ID and HTMLURL of the created gist.
func Create(ctx context.Context, token, description string, public bool, files map[string][]byte) (*Gist, error) {
	if token == "" {
		token = os.Getenv("GITHUB_TOKEN")
	}
	if token == "" {
		token = os.Getenv("GITHUB_GIST_TOKEN")
	}
	if token == "" {
		return nil, fmt.Errorf("github gist: missing token")
	}

	gf := make(map[string]File, len(files))
	for name, data := range files {
		gf[name] = File{Content: string(data)}
	}
	payload := struct {
		Description string          `json:"description,omitempty"`
		Public      bool            `json:"public"`
		Files       map[string]File `json:"files"`
	}{description, public, gf}

	body, err := json.Marshal(payload)
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequestWithContext(ctx, "POST", "https://api.github.com/gists", bytes.NewReader(body))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Authorization", "token "+token)
	req.Header.Set("Content-Type", "application/json")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusCreated {
		data, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("create gist: %s: %s", resp.Status, data)
	}
	var g Gist
	if err := json.NewDecoder(resp.Body).Decode(&g); err != nil {
		return nil, err
	}
	return &g, nil
}

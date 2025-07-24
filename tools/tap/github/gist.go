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

// GistFile represents a single file in a gist.
type GistFile struct {
	Content string `json:"content"`
}

// CreateGistRequest is the payload used when creating a new gist.
type CreateGistRequest struct {
	Description string              `json:"description,omitempty"`
	Public      bool                `json:"public"`
	Files       map[string]GistFile `json:"files"`
}

// CreateGist creates a new GitHub gist. The token parameter may be empty, in
// which case the GITHUB_TOKEN environment variable is used.
// The returned string is the HTML URL of the created gist.
func CreateGist(ctx context.Context, token string, req CreateGistRequest) (string, error) {
	if token == "" {
		token = os.Getenv("GITHUB_TOKEN")
	}
	if token == "" {
		return "", fmt.Errorf("github token not provided")
	}

	body, err := json.Marshal(req)
	if err != nil {
		return "", err
	}

	httpReq, err := http.NewRequestWithContext(ctx, http.MethodPost, "https://api.github.com/gists", bytes.NewReader(body))
	if err != nil {
		return "", err
	}
	httpReq.Header.Set("Authorization", "token "+token)
	httpReq.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(httpReq)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusCreated {
		data, _ := io.ReadAll(io.LimitReader(resp.Body, 1<<16))
		return "", fmt.Errorf("create gist: %s: %s", resp.Status, string(data))
	}

	var out struct {
		HTMLURL string `json:"html_url"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		return "", err
	}
	return out.HTMLURL, nil
}

package thealgorithms

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

const (
	repoOwner = "TheAlgorithms"
	branch    = "master"
)

var extensions = map[string]string{
	"Python":     ".py",
	"Go":         ".go",
	"TypeScript": ".ts",
	"C":          ".c",
}

// List scans a TheAlgorithms repository for source files and writes an
// indexed list to tests/github/TheAlgorithms/<repoName>/index.txt. The list is
// alphabetically sorted.
func List(ctx context.Context, repoName string) ([]string, error) {
	if ctx == nil {
		ctx = context.Background()
	}

	ext, ok := extensions[repoName]
	if !ok {
		return nil, fmt.Errorf("unsupported repository %q", repoName)
	}

	url := fmt.Sprintf("https://api.github.com/repos/%s/%s/git/trees/%s?recursive=1", repoOwner, repoName, branch)
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("Accept", "application/vnd.github.v3+json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		data, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("github api: %s: %s", resp.Status, string(data))
	}

	var out struct {
		Tree []struct {
			Path string `json:"path"`
			Type string `json:"type"`
		} `json:"tree"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		return nil, err
	}

	var files []string
	for _, item := range out.Tree {
		if item.Type == "blob" && strings.HasSuffix(item.Path, ext) {
			if repoName == "Python" && filepath.Base(item.Path) == "__init__.py" {
				continue
			}
			files = append(files, item.Path)
		}
	}

	sort.Strings(files)

	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	outDir := filepath.Join(root, "tests", "github", repoOwner, repoName)
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return nil, err
	}

	indexPath := filepath.Join(outDir, "index.txt")
	f, err := os.Create(indexPath)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	w := bufio.NewWriter(f)
	for i, p := range files {
		fmt.Fprintf(w, "%d %s\n", i+1, p)
	}
	if err := w.Flush(); err != nil {
		return nil, err
	}
	return files, nil
}

// DownloadByNumber reads the index file produced by List and downloads the
// file associated with the given index number, preserving the folder structure
// under tests/github/TheAlgorithms/<repoName>.
func DownloadByNumber(ctx context.Context, repoName string, number int) (string, error) {
	if number < 1 {
		return "", fmt.Errorf("number must be >= 1")
	}
	if ctx == nil {
		ctx = context.Background()
	}

	root, err := repoRoot()
	if err != nil {
		return "", err
	}
	indexPath := filepath.Join(root, "tests", "github", repoOwner, repoName, "index.txt")
	data, err := os.ReadFile(indexPath)
	if err != nil {
		return "", err
	}

	var target string
	scanner := bufio.NewScanner(strings.NewReader(string(data)))
	for scanner.Scan() {
		var idx int
		var path string
		fmt.Sscanf(scanner.Text(), "%d %s", &idx, &path)
		if idx == number {
			target = path
			break
		}
	}
	if err := scanner.Err(); err != nil {
		return "", err
	}
	if target == "" {
		return "", fmt.Errorf("number %d not found", number)
	}

	url := fmt.Sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s", repoOwner, repoName, branch, target)
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return "", err
	}

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		data, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("download: %s: %s", resp.Status, string(data))
	}

	outPath := filepath.Join(root, "tests", "github", repoOwner, repoName, target)
	if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
		return "", err
	}
	outFile, err := os.Create(outPath)
	if err != nil {
		return "", err
	}
	defer outFile.Close()

	if _, err := io.Copy(outFile, resp.Body); err != nil {
		return "", err
	}

	return outPath, nil
}

// repoRoot returns the repository root directory by looking for go.mod.
func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", fmt.Errorf("repository root not found")
		}
		dir = parent
	}
}

//go:build slow

package rosetta

import (
	"bytes"
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
	treeURL  = "https://api.github.com/repos/acmeism/RosettaCodeData/git/trees/main?recursive=1"
	rawURL   = "https://raw.githubusercontent.com/acmeism/RosettaCodeData/main/"
	cacheRel = "tests/rosetta"
)

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func cacheDir() string { return filepath.Join(repoRoot(), cacheRel) }

type treeEntry struct {
	Path string `json:"path"`
	Type string `json:"type"`
}

func fetchTree() ([]treeEntry, error) {
	resp, err := http.Get(treeURL)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("unexpected status: %s", resp.Status)
	}
	var data struct {
		Tree []treeEntry `json:"tree"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&data); err != nil {
		return nil, err
	}
	return data.Tree, nil
}

// ListTasks returns all Rosetta Code tasks. Results are cached in tests/rosetta/tasks.json.
func ListTasks(refresh bool) ([]string, error) {
	path := filepath.Join(cacheDir(), "tasks.json")
	if !refresh {
		if b, err := os.ReadFile(path); err == nil {
			var tasks []string
			if err := json.Unmarshal(b, &tasks); err == nil {
				return tasks, nil
			}
		}
	}
	entries, err := fetchTree()
	if err != nil {
		return nil, err
	}
	m := make(map[string]struct{})
	for _, e := range entries {
		if e.Type != "tree" {
			continue
		}
		if !strings.HasPrefix(e.Path, "Task/") {
			continue
		}
		rest := strings.TrimPrefix(e.Path, "Task/")
		if strings.Contains(rest, "/") {
			// Only directories directly under Task
			if idx := strings.Index(rest, "/"); idx != -1 {
				if idx != len(rest)-1 {
					continue
				}
			}
			continue
		}
		m[rest] = struct{}{}
	}
	tasks := make([]string, 0, len(m))
	for k := range m {
		tasks = append(tasks, k)
	}
	sort.Strings(tasks)
	if err := os.MkdirAll(cacheDir(), 0755); err == nil {
		if b, err := json.MarshalIndent(tasks, "", "  "); err == nil {
			os.WriteFile(path, b, 0644)
		}
	}
	return tasks, nil
}

// ListLanguages returns all programming languages present in the dataset. Results are cached in tests/rosetta/languages.json.
func ListLanguages(refresh bool) ([]string, error) {
	path := filepath.Join(cacheDir(), "languages.json")
	if !refresh {
		if b, err := os.ReadFile(path); err == nil {
			var langs []string
			if err := json.Unmarshal(b, &langs); err == nil {
				return langs, nil
			}
		}
	}
	entries, err := fetchTree()
	if err != nil {
		return nil, err
	}
	m := make(map[string]struct{})
	for _, e := range entries {
		if e.Type != "tree" {
			continue
		}
		if !strings.HasPrefix(e.Path, "Task/") {
			continue
		}
		parts := strings.Split(e.Path, "/")
		if len(parts) != 3 {
			continue
		}
		lang := parts[2]
		m[lang] = struct{}{}
	}
	langs := make([]string, 0, len(m))
	for k := range m {
		langs = append(langs, k)
	}
	sort.Strings(langs)
	if err := os.MkdirAll(cacheDir(), 0755); err == nil {
		if b, err := json.MarshalIndent(langs, "", "  "); err == nil {
			os.WriteFile(path, b, 0644)
		}
	}
	return langs, nil
}

// ListSources lists source file names for a given task and language. It checks local cache first unless refresh is true.
func ListSources(task, language string, refresh bool) ([]string, error) {
	var localDir string
	if language == "Go" {
		localDir = filepath.Join(cacheDir(), "x", language)
	} else {
		localDir = filepath.Join(cacheDir(), "x", language, task)
	}
	if !refresh {
		pattern := filepath.Join(localDir, task+"*")
		if language != "Go" {
			pattern = filepath.Join(localDir, "*")
		}
		if matches, err := filepath.Glob(pattern); err == nil && len(matches) > 0 {
			names := make([]string, 0, len(matches))
			for _, m := range matches {
				if fi, err := os.Stat(m); err == nil && !fi.IsDir() {
					names = append(names, filepath.Base(m))
				}
			}
			if len(names) > 0 {
				sort.Strings(names)
				return names, nil
			}
		}
	}
	url := fmt.Sprintf("https://api.github.com/repos/acmeism/RosettaCodeData/contents/Task/%s/%s", task, language)
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("unexpected status: %s", resp.Status)
	}
	var items []struct {
		Name string `json:"name"`
		Type string `json:"type"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&items); err != nil {
		return nil, err
	}
	names := make([]string, 0, len(items))
	for _, it := range items {
		if it.Type == "file" {
			names = append(names, it.Name)
		}
	}
	sort.Strings(names)
	return names, nil
}

// Download retrieves the named source code for a task and language. Content is
// cached under tests/rosetta/x/LANGUAGE. Go files are stored directly as
// TASK.go (or TASK-1.go, etc.) and are prefixed with a build tag so that they
// are ignored by the Go toolchain.
func Download(task, language, name string, refresh bool) ([]byte, error) {
	var localPath string
	if language == "Go" {
		localPath = filepath.Join(cacheDir(), "x", language, name)
	} else {
		localPath = filepath.Join(cacheDir(), "x", language, task, name)
	}
	if !refresh {
		if b, err := os.ReadFile(localPath); err == nil {
			return b, nil
		}
	}
	url := rawURL + filepath.Join("Task", task, language, name)
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("unexpected status: %s", resp.Status)
	}
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	if language == "Go" && strings.HasSuffix(name, ".go") {
		if !bytes.HasPrefix(data, []byte("//go:build")) {
			prefix := []byte("//go:build ignore\n// +build ignore\n\n")
			data = append(prefix, data...)
		}
	}
	if err := os.MkdirAll(filepath.Dir(localPath), 0755); err == nil {
		os.WriteFile(localPath, data, 0644)
	}
	return data, nil
}

// DownloadTaskByNumber downloads all source files for the nth task in tasks.json
// for the specified language. It returns the task name.
func DownloadTaskByNumber(n int, language string, refresh bool) (string, error) {
	tasks, err := ListTasks(false)
	if err != nil {
		return "", err
	}
	if n <= 0 || n > len(tasks) {
		return "", fmt.Errorf("task number %d out of range", n)
	}
	task := tasks[n-1]
	names, err := ListSources(task, language, refresh)
	if err != nil {
		return "", err
	}
	for _, name := range names {
		if _, err := Download(task, language, name, refresh); err != nil {
			return "", err
		}
	}
	return task, nil
}

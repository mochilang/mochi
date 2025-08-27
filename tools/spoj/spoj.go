package spoj

import (
	"bufio"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	md "github.com/JohannesKaufmann/html-to-markdown"
	"github.com/PuerkitoBio/goquery"
)

const indexDirRel = "tests/spoj/index"

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

func indexDir() string { return filepath.Join(repoRoot(), indexDirRel) }

// Problem represents a SPOJ problem entry.
type Problem struct {
	ID       int     `json:"id"`
	Name     string  `json:"name"`
	URL      string  `json:"url"`
	Users    int     `json:"users"`
	Accepted float32 `json:"accepted"`
}

// List fetches the problem list for the given page (0-based) and writes
// results to tests/spoj/index/{page}.md and {page}.jsonl. It returns the
// parsed problems.
func List(page int) ([]Problem, error) {
	start := page * 50
	url := fmt.Sprintf("https://www.spoj.com/problems/classical/sort=0,start=%d", start)
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("User-Agent", "Mozilla/5.0")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("unexpected status: %s", resp.Status)
	}
	doc, err := goquery.NewDocumentFromReader(resp.Body)
	if err != nil {
		return nil, err
	}
	problems := make([]Problem, 0, 50)
	doc.Find("table.problems tbody tr").Each(func(i int, s *goquery.Selection) {
		cols := s.Find("td")
		if cols.Length() < 5 {
			return
		}
		idStr := strings.TrimSpace(cols.Eq(0).Text())
		id, _ := strconv.Atoi(idStr)
		a := cols.Eq(1).Find("a")
		name := strings.TrimSpace(a.Text())
		href, _ := a.Attr("href")
		usersStr := strings.TrimSpace(cols.Eq(3).Text())
		users, _ := strconv.Atoi(strings.ReplaceAll(usersStr, ",", ""))
		accStr := strings.TrimSpace(cols.Eq(4).Text())
		acc, _ := strconv.ParseFloat(strings.ReplaceAll(accStr, ",", ""), 32)
		problems = append(problems, Problem{
			ID:       id,
			Name:     name,
			URL:      "https://www.spoj.com" + href,
			Users:    users,
			Accepted: float32(acc),
		})
	})
	if len(problems) == 0 {
		return nil, fmt.Errorf("no problems parsed")
	}
	// write outputs
	dir := indexDir()
	if err := os.MkdirAll(dir, 0755); err != nil {
		return problems, err
	}
	// JSONL
	jpath := filepath.Join(dir, fmt.Sprintf("%d.jsonl", page))
	jf, err := os.Create(jpath)
	if err != nil {
		return problems, err
	}
	for _, p := range problems {
		b, _ := json.Marshal(p)
		jf.Write(append(b, '\n'))
	}
	jf.Close()
	// Markdown table
	mpath := filepath.Join(dir, fmt.Sprintf("%d.md", page))
	mf, err := os.Create(mpath)
	if err != nil {
		return problems, err
	}
	fmt.Fprintln(mf, "| ID | Name | Users | ACC% |")
	fmt.Fprintln(mf, "|---|---|---|---|")
	for _, p := range problems {
		fmt.Fprintf(mf, "| %d | [%s](%s) | %d | %.2f |\n", p.ID, p.Name, p.URL, p.Users, p.Accepted)
	}
	mf.Close()
	return problems, nil
}

// Download retrieves the problem statement for the given problem ID and
// returns it converted to Markdown. It uses the cached index files to resolve
// the problem URL.
func Download(id int) (string, error) {
	if id <= 0 {
		return "", fmt.Errorf("invalid id")
	}
	// search all cached index pages until the problem is found. The SPOJ
	// problem identifiers are not contiguous, so the simple formula used
	// previously could look in the wrong page. To robustly locate the
	// problem we scan existing pages (downloading them on demand) until the
	// target ID appears or a reasonable upper bound is reached.
	dir := indexDir()
	var target Problem
	const maxPages = 500 // generous upper bound for the number of pages
	for page := 0; page < maxPages && target.URL == ""; page++ {
		jpath := filepath.Join(dir, fmt.Sprintf("%d.jsonl", page))
		if _, err := os.Stat(jpath); os.IsNotExist(err) {
			if _, err := List(page); err != nil {
				return "", err
			}
		}
		f, err := os.Open(jpath)
		if err != nil {
			return "", err
		}
		scanner := bufio.NewScanner(f)
		for scanner.Scan() {
			var p Problem
			if err := json.Unmarshal(scanner.Bytes(), &p); err == nil && p.ID == id {
				target = p
				break
			}
		}
		f.Close()
	}
	if target.URL == "" {
		return "", fmt.Errorf("problem %d not found", id)
	}
	req, err := http.NewRequest("GET", target.URL, nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("User-Agent", "Mozilla/5.0")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("unexpected status: %s", resp.Status)
	}
	doc, err := goquery.NewDocumentFromReader(resp.Body)
	if err != nil {
		return "", err
	}
	html, err := doc.Find("#problem-body").Html()
	if err != nil {
		return "", err
	}
	conv := md.NewConverter("", true, nil)
	md, err := conv.ConvertString(html)
	if err != nil {
		return "", err
	}
	return md, nil
}

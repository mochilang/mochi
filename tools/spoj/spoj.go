package spoj

import (
	"bufio"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	md "github.com/JohannesKaufmann/html-to-markdown"
	"github.com/PuerkitoBio/goquery"
)

const indexDirRel = "tests/spoj/index"

// Section represents a SPOJ problem category.
type Section string

const (
	SectionClassical  Section = "classical"
	SectionTutorial   Section = "tutorial"
	SectionChallenge  Section = "challenge"
	SectionPartial    Section = "partial"
)

// AllSections lists all supported SPOJ sections in priority order.
var AllSections = []Section{
	SectionClassical,
	SectionTutorial,
	SectionChallenge,
	SectionPartial,
}

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

// SessionCookie can be set to a SPOJ session cookie value (SPOJ_SESS or similar)
// to authenticate requests. Obtain it from browser DevTools after logging in.
var SessionCookie string

func setHeaders(req *http.Request) {
	req.Header.Set("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
	req.Header.Set("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
	req.Header.Set("Accept-Language", "en-US,en;q=0.5")
	req.Header.Set("Accept-Encoding", "identity")
	req.Header.Set("Referer", "https://www.spoj.com/problems/classical/")
	if SessionCookie != "" {
		req.Header.Set("Cookie", SessionCookie)
	}
}

func sectionSubdir(sec Section) string { return string(sec) }

func pageFilePath(sec Section, page int, ext string) string {
	dir := indexDir()
	sub := sectionSubdir(sec)
	if sub != "" {
		dir = filepath.Join(dir, sub)
	}
	return filepath.Join(dir, fmt.Sprintf("%d.%s", page, ext))
}

// Problem represents a SPOJ problem entry.
type Problem struct {
	ID       int     `json:"id"`
	Name     string  `json:"name"`
	URL      string  `json:"url"`
	Users    int     `json:"users"`
	Accepted float32 `json:"accepted"`
	Section  Section `json:"section,omitempty"`
}

// List fetches the classical problem list for the given page (0-based).
// Kept for backwards compatibility — equivalent to ListSection(SectionClassical, page).
func List(page int) ([]Problem, error) {
	return ListSection(SectionClassical, page)
}

// ListSection fetches the problem list for a given section and page (0-based).
// Results are written to tests/spoj/index[/{section}]/{page}.md and .jsonl.
func ListSection(sec Section, page int) ([]Problem, error) {
	start := page * 50
	url := fmt.Sprintf("https://www.spoj.com/problems/%s/sort=0,start=%d", sec, start)
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, err
	}
	setHeaders(req)
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
			Section:  sec,
		})
	})
	if len(problems) == 0 {
		return nil, fmt.Errorf("no problems parsed from %s page %d", sec, page)
	}
	if err := writeIndex(sec, page, problems); err != nil {
		return problems, err
	}
	return problems, nil
}

// ListAllSections fetches pages from all sections until empty, deduplicates by
// URL, and writes a combined index at tests/spoj/index/all.md and all.jsonl.
// Classical problems take priority when the same problem appears in multiple sections.
func ListAllSections(maxPages int) ([]Problem, error) {
	seen := map[string]bool{} // keyed by problem URL
	var all []Problem

	for _, sec := range AllSections {
		for page := 0; page < maxPages; page++ {
			probs, err := ListSection(sec, page)
			if err != nil {
				// stop paging this section on error
				break
			}
			if len(probs) == 0 {
				break
			}
			for _, p := range probs {
				if !seen[p.URL] {
					seen[p.URL] = true
					all = append(all, p)
				}
			}
		}
	}

	// sort by users descending so combined list is ranked by popularity
	sort.Slice(all, func(i, j int) bool {
		return all[i].Users > all[j].Users
	})

	if err := writeCombinedIndex(all); err != nil {
		return all, err
	}
	return all, nil
}

func writeIndex(sec Section, page int, problems []Problem) error {
	dir := indexDir()
	sub := sectionSubdir(sec)
	if sub != "" {
		dir = filepath.Join(dir, sub)
	}
	if err := os.MkdirAll(dir, 0755); err != nil {
		return err
	}

	// JSONL
	jpath := filepath.Join(dir, fmt.Sprintf("%d.jsonl", page))
	jf, err := os.Create(jpath)
	if err != nil {
		return err
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
		return err
	}
	fmt.Fprintln(mf, "| ID | Name | Users | ACC% |")
	fmt.Fprintln(mf, "|---|---|---|---|")
	for _, p := range problems {
		fmt.Fprintf(mf, "| %d | [%s](%s) | %d | %.2f |\n", p.ID, p.Name, p.URL, p.Users, p.Accepted)
	}
	mf.Close()
	return nil
}

func writeCombinedIndex(problems []Problem) error {
	dir := indexDir()
	if err := os.MkdirAll(dir, 0755); err != nil {
		return err
	}

	jpath := filepath.Join(dir, "all.jsonl")
	jf, err := os.Create(jpath)
	if err != nil {
		return err
	}
	for _, p := range problems {
		b, _ := json.Marshal(p)
		jf.Write(append(b, '\n'))
	}
	jf.Close()

	mpath := filepath.Join(dir, "all.md")
	mf, err := os.Create(mpath)
	if err != nil {
		return err
	}
	fmt.Fprintln(mf, "| ID | Section | Name | Users | ACC% |")
	fmt.Fprintln(mf, "|---|---|---|---|---|")
	for _, p := range problems {
		fmt.Fprintf(mf, "| %d | %s | [%s](%s) | %d | %.2f |\n", p.ID, p.Section, p.Name, p.URL, p.Users, p.Accepted)
	}
	mf.Close()
	return nil
}

// Download retrieves the problem statement for the given problem ID and
// returns it converted to Markdown. Searches classical first, then other sections.
func Download(id int) (string, error) {
	return DownloadFromSection(id, "")
}

// DownloadFromSection retrieves a problem by ID, searching a specific section
// first (or all sections if sec is empty). Returns the statement as Markdown.
func DownloadFromSection(id int, preferSec Section) (string, error) {
	if id <= 0 {
		return "", fmt.Errorf("invalid id")
	}

	sections := AllSections
	if preferSec != "" {
		// put preferred section first
		ordered := []Section{preferSec}
		for _, s := range AllSections {
			if s != preferSec {
				ordered = append(ordered, s)
			}
		}
		sections = ordered
	}

	target, err := findProblem(id, sections)
	if err != nil {
		return "", err
	}

	req, err := http.NewRequest("GET", target.URL, nil)
	if err != nil {
		return "", err
	}
	setHeaders(req)
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
	result, err := conv.ConvertString(html)
	if err != nil {
		return "", err
	}
	return result, nil
}

// findProblem searches cached index pages (downloading on demand) across the
// given sections in order. Returns the first matching Problem.
func findProblem(id int, sections []Section) (Problem, error) {
	const maxPages = 500
	dir := indexDir()

	for _, sec := range sections {
		sub := sectionSubdir(sec)
		secDir := dir
		if sub != "" {
			secDir = filepath.Join(dir, sub)
		}

		for page := 0; page < maxPages; page++ {
			jpath := filepath.Join(secDir, fmt.Sprintf("%d.jsonl", page))
			if _, err := os.Stat(jpath); os.IsNotExist(err) {
				if _, err := ListSection(sec, page); err != nil {
					break // no more pages for this section
				}
			}
			f, err := os.Open(jpath)
			if err != nil {
				break
			}
			scanner := bufio.NewScanner(f)
			var found Problem
			for scanner.Scan() {
				var p Problem
				if err := json.Unmarshal(scanner.Bytes(), &p); err == nil && p.ID == id {
					found = p
					break
				}
			}
			f.Close()
			if found.URL != "" {
				return found, nil
			}
		}
	}

	return Problem{}, fmt.Errorf("problem %d not found in any section", id)
}

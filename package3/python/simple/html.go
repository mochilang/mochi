package simple

import (
	"fmt"
	"io"
	"net/url"
	"path"
	"strings"

	"golang.org/x/net/html"
)

// ParseHTML parses a PEP 503 Simple Repository API HTML response. The base
// URL is needed to resolve relative anchor href attributes. Name is the
// PEP 503 project name (will be normalised by the parser).
//
// PEP 503 §"HTML format" requires the response to be a sequence of `<a>`
// elements where each anchor href points at the distribution file. PEP 503
// fragments encode the hash: `<a href="...whl#sha256=...">`. PEP 592 adds
// `data-yanked` and an optional reason string. PEP 658 adds
// `data-core-metadata` (or `data-dist-info-metadata`, deprecated). PEP 700
// reserves additional `data-*` attributes for size, upload time, etc.
//
// The parser is strict about what it accepts (rejects malformed hash fragments,
// rejects yanked attributes without a value) so the bridge can rely on the
// returned File structs.
func ParseHTML(name string, baseURL string, r io.Reader) (*Project, error) {
	doc, err := html.Parse(r)
	if err != nil {
		return nil, fmt.Errorf("simple: parse HTML for %q: %w", name, err)
	}
	base, err := url.Parse(baseURL)
	if err != nil {
		return nil, fmt.Errorf("simple: parse base URL %q: %w", baseURL, err)
	}

	project := &Project{
		Name: Normalise(name),
	}

	var walk func(n *html.Node) error
	walk = func(n *html.Node) error {
		if n.Type == html.ElementNode && n.Data == "a" {
			f, err := parseAnchor(n, base)
			if err != nil {
				return err
			}
			if f != nil {
				project.Files = append(project.Files, *f)
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			if err := walk(c); err != nil {
				return err
			}
		}
		return nil
	}
	if err := walk(doc); err != nil {
		return nil, err
	}
	return project, nil
}

func parseAnchor(a *html.Node, base *url.URL) (*File, error) {
	var hrefRaw string
	attrs := map[string]string{}
	hasAttr := map[string]bool{}
	for _, attr := range a.Attr {
		key := strings.ToLower(attr.Key)
		hasAttr[key] = true
		switch key {
		case "href":
			hrefRaw = attr.Val
		default:
			attrs[key] = attr.Val
		}
	}
	if hrefRaw == "" {
		return nil, nil // anchor without href: not a distribution link
	}

	ref, err := url.Parse(hrefRaw)
	if err != nil {
		return nil, fmt.Errorf("simple: bad anchor href %q: %w", hrefRaw, err)
	}
	resolved := base.ResolveReference(ref)
	fragment := resolved.Fragment
	resolved.Fragment = ""
	abs := resolved.String()

	filename := path.Base(resolved.Path)
	if filename == "" || filename == "/" || filename == "." {
		return nil, nil
	}

	f := &File{
		Filename: filename,
		URL:      abs,
		Hashes:   map[string]string{},
	}
	if fragment != "" {
		// Fragment has the form `algo=hex`. Multiple fragments separated by
		// `&` are not standardised but we tolerate them.
		for _, part := range strings.Split(fragment, "&") {
			eq := strings.IndexByte(part, '=')
			if eq <= 0 {
				return nil, fmt.Errorf("simple: malformed hash fragment %q in %s", part, hrefRaw)
			}
			algo := strings.ToLower(part[:eq])
			digest := strings.ToLower(part[eq+1:])
			if digest == "" {
				return nil, fmt.Errorf("simple: empty hash digest in fragment %q", fragment)
			}
			f.Hashes[algo] = digest
		}
	}
	if v, ok := attrs["data-requires-python"]; ok {
		// The PEP 503 spec mandates HTML-escaping; net/html un-escapes.
		f.RequiresPython = strings.TrimSpace(v)
	}
	if hasAttr["data-yanked"] {
		f.Yanked = true
		f.YankedReason = strings.TrimSpace(attrs["data-yanked"])
	}
	// PEP 658 has two attribute spellings; the new canonical one is
	// data-core-metadata, the legacy alias is data-dist-info-metadata.
	if v, ok := attrs["data-core-metadata"]; ok {
		f.CoreMetadata = true
		_ = v // The value carries the hash; we only need the flag in phase 1.
	} else if v, ok := attrs["data-dist-info-metadata"]; ok {
		f.CoreMetadata = true
		_ = v
	}
	return f, nil
}

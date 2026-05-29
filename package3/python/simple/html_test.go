package simple

import (
	"strings"
	"testing"
)

const minimalHTML = `<!DOCTYPE html>
<html>
  <head><title>Links for httpx</title></head>
  <body>
    <a href="https://files.pythonhosted.org/packages/abc/httpx-0.27.0.tar.gz#sha256=deadbeef">httpx-0.27.0.tar.gz</a>
    <a href="https://files.pythonhosted.org/packages/def/httpx-0.27.0-py3-none-any.whl#sha256=cafebabe">httpx-0.27.0-py3-none-any.whl</a>
  </body>
</html>`

func TestParseHTMLMinimal(t *testing.T) {
	p, err := ParseHTML("HTTPX", "https://pypi.org/simple/httpx/", strings.NewReader(minimalHTML))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if p.Name != "httpx" {
		t.Errorf("Name = %q; want httpx (normalised)", p.Name)
	}
	if len(p.Files) != 2 {
		t.Fatalf("len(Files) = %d; want 2", len(p.Files))
	}
	if p.Files[0].Filename != "httpx-0.27.0.tar.gz" {
		t.Errorf("Files[0].Filename = %q", p.Files[0].Filename)
	}
	if p.Files[0].Hashes["sha256"] != "deadbeef" {
		t.Errorf("Files[0].Hashes[sha256] = %q; want deadbeef", p.Files[0].Hashes["sha256"])
	}
	if p.Files[1].Filename != "httpx-0.27.0-py3-none-any.whl" {
		t.Errorf("Files[1].Filename = %q", p.Files[1].Filename)
	}
}

const relativeHTML = `<a href="../../packages/x/httpx-0.27.0.tar.gz#sha256=abc">sdist</a>
<a href="httpx-0.27.0-py3-none-any.whl#sha256=def">wheel</a>`

func TestParseHTMLRelativeURLs(t *testing.T) {
	p, err := ParseHTML("httpx", "https://pypi.org/simple/httpx/", strings.NewReader(relativeHTML))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if len(p.Files) != 2 {
		t.Fatalf("len(Files) = %d; want 2", len(p.Files))
	}
	if p.Files[0].URL != "https://pypi.org/packages/x/httpx-0.27.0.tar.gz" {
		t.Errorf("Files[0].URL = %q; want resolved against base", p.Files[0].URL)
	}
	if p.Files[1].URL != "https://pypi.org/simple/httpx/httpx-0.27.0-py3-none-any.whl" {
		t.Errorf("Files[1].URL = %q; want resolved against base", p.Files[1].URL)
	}
}

const pep592HTML = `<a href="https://example.com/x-1.0.tar.gz#sha256=abc" data-yanked="security issue">x-1.0.tar.gz</a>
<a href="https://example.com/x-1.1.tar.gz#sha256=def" data-yanked="">x-1.1.tar.gz</a>
<a href="https://example.com/x-1.2.tar.gz#sha256=ghi">x-1.2.tar.gz</a>`

func TestParseHTMLYanked(t *testing.T) {
	p, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(pep592HTML))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if len(p.Files) != 3 {
		t.Fatalf("len(Files) = %d; want 3", len(p.Files))
	}
	if !p.Files[0].Yanked || p.Files[0].YankedReason != "security issue" {
		t.Errorf("Files[0] yanked=%v reason=%q", p.Files[0].Yanked, p.Files[0].YankedReason)
	}
	if !p.Files[1].Yanked || p.Files[1].YankedReason != "" {
		t.Errorf("Files[1] yanked=%v reason=%q", p.Files[1].Yanked, p.Files[1].YankedReason)
	}
	if p.Files[2].Yanked {
		t.Errorf("Files[2] yanked=%v; want false", p.Files[2].Yanked)
	}
}

const pep503DataRequiresPython = `<a href="https://example.com/x-1.0-py3-none-any.whl#sha256=a" data-requires-python="&gt;=3.8">x-1.0-py3-none-any.whl</a>`

func TestParseHTMLRequiresPython(t *testing.T) {
	p, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(pep503DataRequiresPython))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if len(p.Files) != 1 {
		t.Fatalf("len(Files) = %d", len(p.Files))
	}
	if p.Files[0].RequiresPython != ">=3.8" {
		t.Errorf("RequiresPython = %q; want >=3.8 (HTML-unescaped)", p.Files[0].RequiresPython)
	}
}

const pep658HTML = `<a href="https://example.com/a-1.whl#sha256=a" data-core-metadata="sha256=feedface">a</a>
<a href="https://example.com/b-1.whl#sha256=b" data-dist-info-metadata="sha256=cafe">b</a>
<a href="https://example.com/c-1.whl#sha256=c">c</a>`

func TestParseHTMLCoreMetadata(t *testing.T) {
	p, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(pep658HTML))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if len(p.Files) != 3 {
		t.Fatalf("len(Files) = %d", len(p.Files))
	}
	if !p.Files[0].CoreMetadata {
		t.Error("Files[0].CoreMetadata = false; want true (data-core-metadata)")
	}
	if !p.Files[1].CoreMetadata {
		t.Error("Files[1].CoreMetadata = false; want true (data-dist-info-metadata)")
	}
	if p.Files[2].CoreMetadata {
		t.Error("Files[2].CoreMetadata = true; want false")
	}
}

func TestParseHTMLMalformedFragment(t *testing.T) {
	bad := `<a href="https://example.com/x.whl#sha256">x</a>`
	_, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(bad))
	if err == nil {
		t.Fatal("ParseHTML err = nil; want error on malformed fragment")
	}
}

func TestParseHTMLEmptyDigest(t *testing.T) {
	bad := `<a href="https://example.com/x.whl#sha256=">x</a>`
	_, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(bad))
	if err == nil {
		t.Fatal("ParseHTML err = nil; want error on empty digest")
	}
}

func TestParseHTMLMultipleHashFragments(t *testing.T) {
	src := `<a href="https://example.com/x.whl#sha256=abc&blake3=def">x</a>`
	p, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(src))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if p.Files[0].Hashes["sha256"] != "abc" || p.Files[0].Hashes["blake3"] != "def" {
		t.Errorf("hashes = %v; want sha256=abc blake3=def", p.Files[0].Hashes)
	}
}

func TestParseHTMLAnchorWithoutHref(t *testing.T) {
	src := `<a>nothing</a><a href="https://example.com/x.whl#sha256=abc">x</a>`
	p, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(src))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if len(p.Files) != 1 {
		t.Fatalf("len(Files) = %d; want 1 (anchor without href skipped)", len(p.Files))
	}
}

func TestParseHTMLHashKeyLowercased(t *testing.T) {
	src := `<a href="https://example.com/x.whl#SHA256=DEADBEEF">x</a>`
	p, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(src))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if p.Files[0].Hashes["sha256"] != "deadbeef" {
		t.Errorf("hashes[sha256] = %q; want lowercased", p.Files[0].Hashes["sha256"])
	}
}

func TestParseHTMLPath(t *testing.T) {
	src := `<a href="https://example.com/path/to/file/x-1.tar.gz#sha256=abc">label</a>`
	p, err := ParseHTML("x", "https://example.com/simple/x/", strings.NewReader(src))
	if err != nil {
		t.Fatalf("ParseHTML err = %v", err)
	}
	if p.Files[0].Filename != "x-1.tar.gz" {
		t.Errorf("Filename = %q; want basename of URL path", p.Files[0].Filename)
	}
}

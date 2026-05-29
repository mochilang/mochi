package simple

import (
	"strings"
	"testing"
)

const minimalJSON = `{
  "meta": {"api-version": "1.1"},
  "name": "HTTPX",
  "files": [
    {
      "filename": "httpx-0.27.0.tar.gz",
      "url": "https://files.pythonhosted.org/packages/abc/httpx-0.27.0.tar.gz",
      "hashes": {"sha256": "DEADBEEF", "blake3": "FEEDFACE"},
      "requires-python": ">=3.8",
      "yanked": false,
      "core-metadata": false,
      "size": 12345,
      "upload-time": "2024-04-28T13:48:35.123Z"
    },
    {
      "filename": "httpx-0.27.0-py3-none-any.whl",
      "url": "https://files.pythonhosted.org/packages/def/httpx-0.27.0-py3-none-any.whl",
      "hashes": {"sha256": "cafebabe"},
      "yanked": false,
      "core-metadata": {"sha256": "abc123"}
    }
  ]
}`

func TestParseJSONMinimal(t *testing.T) {
	p, err := ParseJSON("https://pypi.org/simple/httpx/", strings.NewReader(minimalJSON))
	if err != nil {
		t.Fatalf("ParseJSON err = %v", err)
	}
	if p.Name != "httpx" {
		t.Errorf("Name = %q; want httpx (normalised)", p.Name)
	}
	if p.Meta.APIVersion != "1.1" {
		t.Errorf("Meta.APIVersion = %q; want 1.1", p.Meta.APIVersion)
	}
	if len(p.Files) != 2 {
		t.Fatalf("len(Files) = %d; want 2", len(p.Files))
	}
	f0 := p.Files[0]
	if f0.Filename != "httpx-0.27.0.tar.gz" {
		t.Errorf("Files[0].Filename = %q", f0.Filename)
	}
	if f0.Hashes["sha256"] != "deadbeef" {
		t.Errorf("Files[0].Hashes[sha256] = %q; want lowercased", f0.Hashes["sha256"])
	}
	if f0.Hashes["blake3"] != "feedface" {
		t.Errorf("Files[0].Hashes[blake3] = %q; want lowercased", f0.Hashes["blake3"])
	}
	if f0.RequiresPython != ">=3.8" {
		t.Errorf("Files[0].RequiresPython = %q", f0.RequiresPython)
	}
	if f0.Size != 12345 {
		t.Errorf("Files[0].Size = %d; want 12345", f0.Size)
	}
	if f0.UploadTime != "2024-04-28T13:48:35.123Z" {
		t.Errorf("Files[0].UploadTime = %q", f0.UploadTime)
	}
	if f0.CoreMetadata {
		t.Error("Files[0].CoreMetadata = true; want false")
	}
	if !p.Files[1].CoreMetadata {
		t.Error("Files[1].CoreMetadata = false; want true (object form)")
	}
}

const yankedJSON = `{
  "meta": {"api-version": "1.0"},
  "name": "x",
  "files": [
    {"filename": "x-1.tar.gz", "url": "https://example.com/x-1.tar.gz", "hashes": {"sha256":"a"}, "yanked": true},
    {"filename": "x-2.tar.gz", "url": "https://example.com/x-2.tar.gz", "hashes": {"sha256":"b"}, "yanked": "security issue"},
    {"filename": "x-3.tar.gz", "url": "https://example.com/x-3.tar.gz", "hashes": {"sha256":"c"}, "yanked": false}
  ]
}`

func TestParseJSONYankedVariants(t *testing.T) {
	p, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(yankedJSON))
	if err != nil {
		t.Fatalf("ParseJSON err = %v", err)
	}
	if !p.Files[0].Yanked || p.Files[0].YankedReason != "" {
		t.Errorf("Files[0] yanked=%v reason=%q; want yanked=true no reason", p.Files[0].Yanked, p.Files[0].YankedReason)
	}
	if !p.Files[1].Yanked || p.Files[1].YankedReason != "security issue" {
		t.Errorf("Files[1] yanked=%v reason=%q", p.Files[1].Yanked, p.Files[1].YankedReason)
	}
	if p.Files[2].Yanked {
		t.Errorf("Files[2] yanked=true; want false")
	}
}

const relJSON = `{
  "meta": {"api-version": "1.1"},
  "name": "x",
  "files": [
    {"filename": "x-1.tar.gz", "url": "../../packages/abc/x-1.tar.gz", "hashes": {"sha256":"a"}}
  ]
}`

func TestParseJSONRelativeURL(t *testing.T) {
	p, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(relJSON))
	if err != nil {
		t.Fatalf("ParseJSON err = %v", err)
	}
	if p.Files[0].URL != "https://example.com/packages/abc/x-1.tar.gz" {
		t.Errorf("URL = %q; want resolved against base", p.Files[0].URL)
	}
}

const unknownJSON = `{
  "meta": {"api-version": "1.5", "future-field": "ignored"},
  "name": "x",
  "files": [
    {"filename": "x-1.tar.gz", "url": "https://example.com/x-1.tar.gz", "hashes": {"sha256":"a"}, "future-attr": 42}
  ],
  "future-top": "tolerated"
}`

func TestParseJSONTolerantOfUnknownFields(t *testing.T) {
	p, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(unknownJSON))
	if err != nil {
		t.Fatalf("ParseJSON err = %v; want tolerant of unknown fields", err)
	}
	if p.Meta.APIVersion != "1.5" {
		t.Errorf("APIVersion = %q", p.Meta.APIVersion)
	}
}

func TestParseJSONMissingName(t *testing.T) {
	src := `{"meta": {"api-version": "1.0"}, "files": []}`
	_, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(src))
	if err == nil {
		t.Fatal("ParseJSON err = nil; want error on missing name")
	}
}

func TestParseJSONMissingFilename(t *testing.T) {
	src := `{"name": "x", "files": [{"url": "https://example.com/x.whl", "hashes": {"sha256":"a"}}]}`
	_, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(src))
	if err == nil {
		t.Fatal("ParseJSON err = nil; want error on missing filename")
	}
}

func TestParseJSONMissingURL(t *testing.T) {
	src := `{"name": "x", "files": [{"filename": "x.whl", "hashes": {"sha256":"a"}}]}`
	_, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(src))
	if err == nil {
		t.Fatal("ParseJSON err = nil; want error on missing url")
	}
}

func TestParseJSONBadJSON(t *testing.T) {
	src := `{"name": "x", "files":`
	_, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(src))
	if err == nil {
		t.Fatal("ParseJSON err = nil; want error on truncated JSON")
	}
}

func TestParseJSONHashKeysLowercased(t *testing.T) {
	src := `{"name":"x","meta":{"api-version":"1.0"},"files":[{"filename":"x.whl","url":"https://example.com/x.whl","hashes":{"SHA256":"DEADBEEF"}}]}`
	p, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(src))
	if err != nil {
		t.Fatalf("ParseJSON err = %v", err)
	}
	if p.Files[0].Hashes["sha256"] != "deadbeef" {
		t.Errorf("hashes[sha256] = %q; want lowercased key + value", p.Files[0].Hashes["sha256"])
	}
}

func TestParseJSONNoMeta(t *testing.T) {
	src := `{"name":"x","files":[{"filename":"x.whl","url":"https://example.com/x.whl","hashes":{"sha256":"a"}}]}`
	p, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(src))
	if err != nil {
		t.Fatalf("ParseJSON err = %v", err)
	}
	if p.Meta.APIVersion != "" {
		t.Errorf("Meta.APIVersion = %q; want empty", p.Meta.APIVersion)
	}
}

func TestParseYankedHelperRejectsNonsense(t *testing.T) {
	src := `{"name":"x","meta":{"api-version":"1.0"},"files":[{"filename":"x.whl","url":"https://example.com/x.whl","hashes":{"sha256":"a"},"yanked":123}]}`
	_, err := ParseJSON("https://example.com/simple/x/", strings.NewReader(src))
	if err == nil {
		t.Fatal("ParseJSON err = nil; want error on numeric yanked")
	}
}

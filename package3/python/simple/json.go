package simple

import (
	"encoding/json"
	"fmt"
	"io"
	"net/url"
)

// ParseJSON parses a PEP 691 / PEP 700 JSON simple-index response. The base
// URL resolves relative file URLs. The PEP 691 envelope is:
//
//	{
//	  "meta": {"api-version": "1.1"},
//	  "name": "httpx",
//	  "files": [
//	    {
//	      "filename": "httpx-0.27.0-py3-none-any.whl",
//	      "url": "https://files.pythonhosted.org/.../httpx-0.27.0-py3-none-any.whl",
//	      "hashes": {"sha256": "<hex>", "blake3": "<hex>"},
//	      "requires-python": ">=3.8",
//	      "yanked": false,
//	      "core-metadata": true,
//	      "size": 75317,
//	      "upload-time": "2024-04-28T13:48:35.123Z"
//	    }
//	  ]
//	}
//
// `yanked` may be `false`, `true`, or a string (the reason). `core-metadata`
// may be `false`, `true`, or an object `{"sha256": "<hex>"}`.
func ParseJSON(baseURL string, r io.Reader) (*Project, error) {
	base, err := url.Parse(baseURL)
	if err != nil {
		return nil, fmt.Errorf("simple: parse base URL %q: %w", baseURL, err)
	}

	var raw struct {
		Meta  *struct {
			APIVersion string `json:"api-version"`
		} `json:"meta"`
		Name  string `json:"name"`
		Files []struct {
			Filename       string            `json:"filename"`
			URL            string            `json:"url"`
			Hashes         map[string]string `json:"hashes"`
			RequiresPython string            `json:"requires-python"`
			Yanked         json.RawMessage   `json:"yanked"`
			CoreMetadata   json.RawMessage   `json:"core-metadata"`
			Size           int64             `json:"size"`
			UploadTime     string            `json:"upload-time"`
		} `json:"files"`
	}
	// PEP 691 §"future extensions" reserves the right to add new fields, so
	// the decoder is permissive about unknown keys at the top level and
	// inside each file entry. Strict golden tests should compare via the
	// parsed Project struct, not by byte-equality of the raw response.
	if err := json.NewDecoder(r).Decode(&raw); err != nil {
		return nil, fmt.Errorf("simple: parse JSON: %w", err)
	}

	if raw.Name == "" {
		return nil, fmt.Errorf("simple: JSON response missing name")
	}
	project := &Project{
		Name: Normalise(raw.Name),
	}
	if raw.Meta != nil {
		project.Meta.APIVersion = raw.Meta.APIVersion
	}
	for _, rf := range raw.Files {
		if rf.Filename == "" {
			return nil, fmt.Errorf("simple: file without filename")
		}
		if rf.URL == "" {
			return nil, fmt.Errorf("simple: file %q without url", rf.Filename)
		}
		ref, err := url.Parse(rf.URL)
		if err != nil {
			return nil, fmt.Errorf("simple: file %q bad URL %q: %w", rf.Filename, rf.URL, err)
		}
		f := File{
			Filename:       rf.Filename,
			URL:            base.ResolveReference(ref).String(),
			Hashes:         normaliseHashes(rf.Hashes),
			RequiresPython: rf.RequiresPython,
			Size:           rf.Size,
			UploadTime:     rf.UploadTime,
		}
		if len(rf.Yanked) > 0 {
			yanked, reason, err := parseYanked(rf.Yanked)
			if err != nil {
				return nil, fmt.Errorf("simple: file %q yanked field: %w", rf.Filename, err)
			}
			f.Yanked = yanked
			f.YankedReason = reason
		}
		if len(rf.CoreMetadata) > 0 {
			f.CoreMetadata = parseCoreMetadata(rf.CoreMetadata)
		}
		project.Files = append(project.Files, f)
	}
	return project, nil
}

func normaliseHashes(in map[string]string) map[string]string {
	if len(in) == 0 {
		return map[string]string{}
	}
	out := make(map[string]string, len(in))
	for k, v := range in {
		out[lower(k)] = lower(v)
	}
	return out
}

func parseYanked(raw json.RawMessage) (bool, string, error) {
	// yanked may be bool or string. A non-empty string is treated as yanked
	// with reason; an empty string is technically PEP 592 but ambiguous, so
	// we treat it as yanked-no-reason.
	var b bool
	if err := json.Unmarshal(raw, &b); err == nil {
		return b, "", nil
	}
	var s string
	if err := json.Unmarshal(raw, &s); err == nil {
		return true, s, nil
	}
	return false, "", fmt.Errorf("yanked field is neither bool nor string: %s", string(raw))
}

func parseCoreMetadata(raw json.RawMessage) bool {
	var b bool
	if err := json.Unmarshal(raw, &b); err == nil {
		return b
	}
	// Object form `{"sha256": "..."}`. The mere presence indicates the
	// METADATA sidecar is available.
	var obj map[string]string
	if err := json.Unmarshal(raw, &obj); err == nil {
		return len(obj) > 0
	}
	return false
}

func lower(s string) string {
	b := make([]byte, len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c >= 'A' && c <= 'Z' {
			c += 'a' - 'A'
		}
		b[i] = c
	}
	return string(b)
}


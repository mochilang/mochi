package importspec

import "testing"

// TestPhase7ImportGrammar is the umbrella sentinel for MEP-71 Phase 7. It
// walks every spec shape MEP-71 §3 enumerates and asserts the parsed Spec
// reflects the canonical decomposition.
func TestPhase7ImportGrammar(t *testing.T) {
	cases := []struct {
		name string
		in   string
		want Spec
	}{
		{
			name: "bare",
			in:   "requests",
			want: Spec{Name: "requests", RawName: "requests", Source: SourceRegistry},
		},
		{
			name: "semver-pinned",
			in:   "requests@>=2.0,<3.0",
			want: Spec{Name: "requests", RawName: "requests", Source: SourceRegistry},
		},
		{
			name: "non-default index",
			in:   "torch@torch+https://download.pytorch.org/whl/cu121",
			want: Spec{
				Name:     "torch",
				RawName:  "torch",
				Source:   SourceIndex,
				IndexURL: "https://download.pytorch.org/whl/cu121",
			},
		},
		{
			name: "git VCS with rev",
			in:   "mypkg@git+https://github.com/user/repo#commit",
			want: Spec{
				Name:    "mypkg",
				RawName: "mypkg",
				Source:  SourceGit,
				GitURL:  "https://github.com/user/repo",
				GitRev:  "commit",
			},
		},
		{
			name: "local path",
			in:   "mypkg@path+../sibling",
			want: Spec{
				Name:      "mypkg",
				RawName:   "mypkg",
				Source:    SourcePath,
				LocalPath: "../sibling",
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got, err := Parse(tc.in)
			if err != nil {
				t.Fatalf("Parse(%q): %v", tc.in, err)
			}
			if got.Name != tc.want.Name {
				t.Errorf("Name = %q, want %q", got.Name, tc.want.Name)
			}
			if got.RawName != tc.want.RawName {
				t.Errorf("RawName = %q, want %q", got.RawName, tc.want.RawName)
			}
			if got.Source != tc.want.Source {
				t.Errorf("Source = %v, want %v", got.Source, tc.want.Source)
			}
			if got.IndexURL != tc.want.IndexURL {
				t.Errorf("IndexURL = %q, want %q", got.IndexURL, tc.want.IndexURL)
			}
			if got.GitURL != tc.want.GitURL {
				t.Errorf("GitURL = %q, want %q", got.GitURL, tc.want.GitURL)
			}
			if got.GitRev != tc.want.GitRev {
				t.Errorf("GitRev = %q, want %q", got.GitRev, tc.want.GitRev)
			}
			if got.LocalPath != tc.want.LocalPath {
				t.Errorf("LocalPath = %q, want %q", got.LocalPath, tc.want.LocalPath)
			}
		})
	}
}

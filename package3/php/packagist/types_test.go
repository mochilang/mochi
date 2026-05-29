package packagist

import (
	"testing"
)

func TestNormaliseVersion(t *testing.T) {
	cases := []struct{ in, want string }{
		{"7.8.1", "7.8.1"},
		{"v7.8.1", "7.8.1"},
		{"v1.0.0", "1.0.0"},
		{"1.0.0", "1.0.0"},
		{"dev-main", "dev-main"},
	}
	for _, tc := range cases {
		if got := NormaliseVersion(tc.in); got != tc.want {
			t.Errorf("NormaliseVersion(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestParseName(t *testing.T) {
	vendor, name, err := ParseName("guzzlehttp/guzzle")
	if err != nil {
		t.Fatalf("ParseName: %v", err)
	}
	if vendor != "guzzlehttp" || name != "guzzle" {
		t.Errorf("ParseName = %q, %q; want guzzlehttp, guzzle", vendor, name)
	}
}

func TestParseNameErrors(t *testing.T) {
	bad := []string{"", "/", "noSlash", "double/slash/here", "vendor/"}
	for _, s := range bad {
		_, _, err := ParseName(s)
		if err == nil {
			t.Errorf("ParseName(%q) should return error", s)
		}
	}
}

func TestNormalisePHPConstraint(t *testing.T) {
	pv := &PackageVersion{Require: map[string]string{"php": ">=8.1"}}
	NormalisePHPConstraint(pv)
	if pv.PHPConstraint != ">=8.1" {
		t.Errorf("PHPConstraint = %q; want >=8.1", pv.PHPConstraint)
	}

	pv2 := &PackageVersion{}
	NormalisePHPConstraint(pv2)
	if pv2.PHPConstraint != "*" {
		t.Errorf("PHPConstraint = %q; want *", pv2.PHPConstraint)
	}
}

func TestSortVersions(t *testing.T) {
	versions := []PackageVersion{
		{Version: "6.3.0", VersionNormalized: "6.3.0.0"},
		{Version: "6.4.0", VersionNormalized: "6.4.0.0"},
		{Version: "dev-main"},
		{Version: "7.0.0", VersionNormalized: "7.0.0.0"},
	}
	SortVersions(versions)
	// dev-main should be last
	if versions[len(versions)-1].Version != "dev-main" {
		t.Errorf("dev-main should sort last; got %q", versions[len(versions)-1].Version)
	}
	// highest stable first
	if versions[0].Version != "7.0.0" {
		t.Errorf("highest stable should be first; got %q", versions[0].Version)
	}
}

func TestLatestStable(t *testing.T) {
	versions := []PackageVersion{
		{Version: "7.0.0", VersionNormalized: "7.0.0.0"},
		{Version: "6.4.0", VersionNormalized: "6.4.0.0"},
		{Version: "dev-main"},
	}
	SortVersions(versions)
	v, ok := LatestStable(versions)
	if !ok {
		t.Fatal("LatestStable returned false")
	}
	if v.Version != "7.0.0" {
		t.Errorf("LatestStable = %q; want 7.0.0", v.Version)
	}
}

func TestLatestStableAllDev(t *testing.T) {
	versions := []PackageVersion{
		{Version: "dev-main"},
		{Version: "dev-feature"},
	}
	_, ok := LatestStable(versions)
	if ok {
		t.Error("LatestStable should return false when all versions are dev")
	}
}

func TestMatchConstraintWildcard(t *testing.T) {
	versions := []PackageVersion{
		{Version: "7.0.0", VersionNormalized: "7.0.0.0"},
		{Version: "6.4.0", VersionNormalized: "6.4.0.0"},
	}
	SortVersions(versions)
	v, ok := MatchConstraint(versions, "*")
	if !ok {
		t.Fatal("MatchConstraint(*) returned false")
	}
	if v.Version != "7.0.0" {
		t.Errorf("MatchConstraint(*) = %q; want 7.0.0", v.Version)
	}
}

func TestMatchConstraintCaret(t *testing.T) {
	versions := []PackageVersion{
		{Version: "7.0.0", VersionNormalized: "7.0.0.0"},
		{Version: "6.4.0", VersionNormalized: "6.4.0.0"},
		{Version: "6.3.0", VersionNormalized: "6.3.0.0"},
	}
	SortVersions(versions)
	v, ok := MatchConstraint(versions, "^6.3")
	if !ok {
		t.Fatal("MatchConstraint(^6.3) returned false")
	}
	// Should pick the highest 6.x version, not 7.0.0
	if v.VersionNormalized != "6.4.0.0" {
		t.Errorf("MatchConstraint(^6.3) = %q; want 6.4.0.0", v.VersionNormalized)
	}
}

func TestMatchConstraintGTE(t *testing.T) {
	versions := []PackageVersion{
		{Version: "7.0.0", VersionNormalized: "7.0.0.0"},
		{Version: "6.4.0", VersionNormalized: "6.4.0.0"},
	}
	SortVersions(versions)
	v, ok := MatchConstraint(versions, ">=6.4")
	if !ok {
		t.Fatal("MatchConstraint(>=6.4) returned false")
	}
	if v.Version != "7.0.0" {
		t.Errorf("MatchConstraint(>=6.4) = %q; want 7.0.0", v.Version)
	}
}

func TestMatchConstraintNoMatch(t *testing.T) {
	versions := []PackageVersion{
		{Version: "6.4.0", VersionNormalized: "6.4.0.0"},
	}
	SortVersions(versions)
	_, ok := MatchConstraint(versions, "^7.0")
	if ok {
		t.Error("MatchConstraint(^7.0) should not match 6.4.0")
	}
}

func TestCompareVersions(t *testing.T) {
	cases := []struct {
		a, b string
		want int
	}{
		{"7.0.0", "6.4.0", 1},
		{"6.4.0", "7.0.0", -1},
		{"6.4.0", "6.4.0", 0},
		{"6.4.1", "6.4.0", 1},
		{"1.0.0", "1.0.0.0", 0},
	}
	for _, tc := range cases {
		got := compareVersions(tc.a, tc.b)
		if got != tc.want {
			t.Errorf("compareVersions(%q, %q) = %d; want %d", tc.a, tc.b, got, tc.want)
		}
	}
}

func TestAutoloadInfoPSR4(t *testing.T) {
	pv := &PackageVersion{
		Autoload: AutoloadInfo{
			PSR4: map[string]string{
				"GuzzleHttp\\": "src/",
			},
		},
	}
	if len(pv.Autoload.PSR4) != 1 {
		t.Errorf("expected 1 PSR4 entry; got %d", len(pv.Autoload.PSR4))
	}
	if dir, ok := pv.Autoload.PSR4["GuzzleHttp\\"]; !ok || dir != "src/" {
		t.Errorf("PSR4[GuzzleHttp\\] = %q; want src/", dir)
	}
}

func TestPhase1PackagistSentinel(t *testing.T) {
	// Sentinel: verify the packagist package compiles and the key types exist.
	var _ *Client = NewClient("")
	var _ *PackageResponse = &PackageResponse{}
	var _ PackageVersion = PackageVersion{}
	var _ DistInfo = DistInfo{}
	var _ AutoloadInfo = AutoloadInfo{}
}

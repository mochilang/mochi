package maven

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"
)

// Diamond dependency test:
//
//	A → B@1.0, C@1.0
//	B@1.0 → D@1.0
//	C@1.0 → D@2.0
//
// Expected: D@2.0 wins (higher version).
const (
	pomA = `<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>test</groupId><artifactId>A</artifactId><version>1.0</version>
  <dependencies>
    <dependency><groupId>test</groupId><artifactId>B</artifactId><version>1.0</version><scope>compile</scope></dependency>
    <dependency><groupId>test</groupId><artifactId>C</artifactId><version>1.0</version><scope>compile</scope></dependency>
  </dependencies>
</project>`

	pomB1 = `<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>test</groupId><artifactId>B</artifactId><version>1.0</version>
  <dependencies>
    <dependency><groupId>test</groupId><artifactId>D</artifactId><version>1.0</version><scope>compile</scope></dependency>
  </dependencies>
</project>`

	pomC1 = `<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>test</groupId><artifactId>C</artifactId><version>1.0</version>
  <dependencies>
    <dependency><groupId>test</groupId><artifactId>D</artifactId><version>2.0</version><scope>compile</scope></dependency>
  </dependencies>
</project>`

	pomD1 = `<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>test</groupId><artifactId>D</artifactId><version>1.0</version>
</project>`

	pomD2 = `<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>test</groupId><artifactId>D</artifactId><version>2.0</version>
</project>`
)

func setupGraphServer(t *testing.T) *httptest.Server {
	t.Helper()
	handlers := map[string]string{
		"/test/A/1.0/A-1.0.pom": pomA,
		"/test/B/1.0/B-1.0.pom": pomB1,
		"/test/C/1.0/C-1.0.pom": pomC1,
		"/test/D/1.0/D-1.0.pom": pomD1,
		"/test/D/2.0/D-2.0.pom": pomD2,
	}
	mux := http.NewServeMux()
	for path, body := range handlers {
		body := body
		mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "text/xml")
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(body))
		})
	}
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	})
	return httptest.NewServer(mux)
}

func TestDiamondDependencyHigherVersionWins(t *testing.T) {
	srv := setupGraphServer(t)
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	coords, err := ResolveTransitive(context.Background(), client, []Coordinate{
		{GroupID: "test", ArtifactID: "A", Version: "1.0"},
	})
	if err != nil {
		t.Fatalf("ResolveTransitive error: %v", err)
	}

	// Find D in the resolved set
	var dVersion string
	for _, c := range coords {
		if c.GroupID == "test" && c.ArtifactID == "D" {
			dVersion = c.Version
		}
	}
	if dVersion == "" {
		t.Fatal("D not found in resolved dependencies")
	}
	if dVersion != "2.0" {
		t.Errorf("D version = %q, want %q (higher version should win)", dVersion, "2.0")
	}
}

func TestResolveTransitiveExclusions(t *testing.T) {
	// A → B@1.0 (excluding D), B@1.0 → D@1.0
	// D should not appear in the result.
	const pomAWithExcl = `<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>test</groupId><artifactId>A</artifactId><version>1.0</version>
  <dependencies>
    <dependency>
      <groupId>test</groupId><artifactId>B</artifactId><version>1.0</version><scope>compile</scope>
      <exclusions>
        <exclusion><groupId>test</groupId><artifactId>D</artifactId></exclusion>
      </exclusions>
    </dependency>
  </dependencies>
</project>`

	handlers := map[string]string{
		"/test/A/1.0/A-1.0.pom": pomAWithExcl,
		"/test/B/1.0/B-1.0.pom": pomB1,
		"/test/D/1.0/D-1.0.pom": pomD1,
	}
	mux := http.NewServeMux()
	for path, body := range handlers {
		body := body
		mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "text/xml")
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(body))
		})
	}
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	})
	srv := httptest.NewServer(mux)
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	coords, err := ResolveTransitive(context.Background(), client, []Coordinate{
		{GroupID: "test", ArtifactID: "A", Version: "1.0"},
	})
	if err != nil {
		t.Fatalf("ResolveTransitive error: %v", err)
	}

	for _, c := range coords {
		if c.GroupID == "test" && c.ArtifactID == "D" {
			t.Errorf("D should have been excluded but was found: %s", c)
		}
	}
}

func TestResolveTransitiveTestScopeExcluded(t *testing.T) {
	// A → B@1.0 (scope=test): B should not be included
	const pomATestDep = `<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>test</groupId><artifactId>A</artifactId><version>1.0</version>
  <dependencies>
    <dependency><groupId>test</groupId><artifactId>B</artifactId><version>1.0</version><scope>test</scope></dependency>
  </dependencies>
</project>`

	handlers := map[string]string{
		"/test/A/1.0/A-1.0.pom": pomATestDep,
		"/test/B/1.0/B-1.0.pom": pomB1,
	}
	mux := http.NewServeMux()
	for path, body := range handlers {
		body := body
		mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "text/xml")
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(body))
		})
	}
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	})
	srv := httptest.NewServer(mux)
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	coords, err := ResolveTransitive(context.Background(), client, []Coordinate{
		{GroupID: "test", ArtifactID: "A", Version: "1.0"},
	})
	if err != nil {
		t.Fatalf("ResolveTransitive error: %v", err)
	}

	for _, c := range coords {
		if c.GroupID == "test" && c.ArtifactID == "B" {
			t.Errorf("B (test scope) should not be included but was found: %s", c)
		}
	}
}

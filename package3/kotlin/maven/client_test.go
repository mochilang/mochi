package maven

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// Fixture: simple POM XML
const simplePOMXML = `<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>org.example</groupId>
  <artifactId>mylib</artifactId>
  <version>1.2.3</version>
  <packaging>jar</packaging>
  <dependencies>
    <dependency>
      <groupId>org.slf4j</groupId>
      <artifactId>slf4j-api</artifactId>
      <version>2.0.0</version>
      <scope>compile</scope>
    </dependency>
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>4.13.2</version>
      <scope>test</scope>
    </dependency>
  </dependencies>
</project>`

// Fixture: parent POM XML
const parentPOMXML = `<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>org.example</groupId>
  <artifactId>parent</artifactId>
  <version>1.0.0</version>
  <packaging>pom</packaging>
  <dependencyManagement>
    <dependencies>
      <dependency>
        <groupId>org.slf4j</groupId>
        <artifactId>slf4j-api</artifactId>
        <version>2.0.9</version>
      </dependency>
    </dependencies>
  </dependencyManagement>
</project>`

// Fixture: POM with parent reference
const childWithParentPOMXML = `<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <parent>
    <groupId>org.example</groupId>
    <artifactId>parent</artifactId>
    <version>1.0.0</version>
  </parent>
  <artifactId>child</artifactId>
  <version>1.0.0</version>
  <dependencies>
    <dependency>
      <groupId>org.slf4j</groupId>
      <artifactId>slf4j-api</artifactId>
    </dependency>
  </dependencies>
</project>`

// Fixture: maven-metadata.xml
const mavenMetadataXML = `<?xml version="1.0" encoding="UTF-8"?>
<metadata>
  <groupId>org.example</groupId>
  <artifactId>mylib</artifactId>
  <versioning>
    <latest>2.0.0</latest>
    <release>1.9.0</release>
    <versions>
      <version>1.0.0</version>
      <version>1.5.0</version>
      <version>1.7.3</version>
      <version>1.9.0</version>
      <version>2.0.0</version>
    </versions>
    <lastUpdated>20240101120000</lastUpdated>
  </versioning>
</metadata>`

// Fixture: Gradle Module Metadata JSON
const gradleModuleJSON = `{
  "formatVersion": "1.1",
  "component": {
    "group": "org.example",
    "module": "mylib",
    "version": "1.2.3"
  },
  "variants": [
    {
      "name": "jvmApiElements",
      "attributes": {
        "org.gradle.usage": "java-api",
        "org.jetbrains.kotlin.platform.type": "jvm",
        "org.gradle.libraryelements": "jar"
      },
      "files": [
        {
          "name": "mylib-1.2.3.jar",
          "url": "mylib-1.2.3.jar",
          "sha256": "abc123",
          "size": 12345
        }
      ],
      "dependencies": []
    },
    {
      "name": "jvmRuntimeElements",
      "attributes": {
        "org.gradle.usage": "java-runtime",
        "org.jetbrains.kotlin.platform.type": "jvm",
        "org.gradle.libraryelements": "jar"
      },
      "files": [
        {
          "name": "mylib-1.2.3.jar",
          "url": "mylib-1.2.3.jar",
          "sha256": "abc123",
          "size": 12345
        }
      ],
      "dependencies": []
    },
    {
      "name": "jsApiElements",
      "attributes": {
        "org.gradle.usage": "kotlin-api",
        "org.jetbrains.kotlin.platform.type": "js"
      },
      "files": [],
      "dependencies": []
    }
  ]
}`

// Fixture: BOM POM
const bomPOMXML = `<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>org.example</groupId>
  <artifactId>bom</artifactId>
  <version>3.0.0</version>
  <packaging>pom</packaging>
  <dependencyManagement>
    <dependencies>
      <dependency>
        <groupId>org.slf4j</groupId>
        <artifactId>slf4j-api</artifactId>
        <version>2.0.9</version>
      </dependency>
      <dependency>
        <groupId>com.google.guava</groupId>
        <artifactId>guava</artifactId>
        <version>32.1.2-jre</version>
      </dependency>
    </dependencies>
  </dependencyManagement>
</project>`

// Fixture: POM with exclusions
const pomWithExclusionsXML = `<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>org.example</groupId>
  <artifactId>myapp</artifactId>
  <version>1.0.0</version>
  <dependencies>
    <dependency>
      <groupId>org.example</groupId>
      <artifactId>mylib</artifactId>
      <version>1.2.3</version>
      <exclusions>
        <exclusion>
          <groupId>org.slf4j</groupId>
          <artifactId>slf4j-api</artifactId>
        </exclusion>
      </exclusions>
    </dependency>
  </dependencies>
</project>`

// setupTestServer creates a test HTTP server that serves different responses based on URL path.
func setupTestServer(t *testing.T, handlers map[string]string) *httptest.Server {
	t.Helper()
	mux := http.NewServeMux()
	for path, body := range handlers {
		body := body // capture loop var
		mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "text/xml")
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(body))
		})
	}
	// 404 for anything else
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	})
	return httptest.NewServer(mux)
}

func TestFetchPOM(t *testing.T) {
	srv := setupTestServer(t, map[string]string{
		"/org/example/mylib/1.2.3/mylib-1.2.3.pom": simplePOMXML,
	})
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	pom, err := client.FetchPOM(context.Background(), Coordinate{
		GroupID: "org.example", ArtifactID: "mylib", Version: "1.2.3",
	})
	if err != nil {
		t.Fatalf("FetchPOM error: %v", err)
	}
	if pom.GroupID != "org.example" {
		t.Errorf("GroupID = %q, want %q", pom.GroupID, "org.example")
	}
	if pom.ArtifactID != "mylib" {
		t.Errorf("ArtifactID = %q, want %q", pom.ArtifactID, "mylib")
	}
	if pom.Version != "1.2.3" {
		t.Errorf("Version = %q, want %q", pom.Version, "1.2.3")
	}
	if len(pom.Dependencies) != 2 {
		t.Errorf("len(Dependencies) = %d, want 2", len(pom.Dependencies))
	}
}

func TestFetchMavenMetadata(t *testing.T) {
	srv := setupTestServer(t, map[string]string{
		"/org/example/mylib/maven-metadata.xml": mavenMetadataXML,
	})
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	meta, err := client.FetchMavenMetadata(context.Background(), "org.example", "mylib")
	if err != nil {
		t.Fatalf("FetchMavenMetadata error: %v", err)
	}
	if meta.Versioning.Latest != "2.0.0" {
		t.Errorf("Latest = %q, want %q", meta.Versioning.Latest, "2.0.0")
	}
	if meta.Versioning.Release != "1.9.0" {
		t.Errorf("Release = %q, want %q", meta.Versioning.Release, "1.9.0")
	}
	if len(meta.Versioning.Versions) != 5 {
		t.Errorf("len(Versions) = %d, want 5", len(meta.Versioning.Versions))
	}
}

func TestResolveVersionExact(t *testing.T) {
	srv := setupTestServer(t, map[string]string{
		"/org/example/mylib/maven-metadata.xml": mavenMetadataXML,
	})
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	v, err := client.ResolveVersion(context.Background(), "org.example", "mylib", "1.7.3")
	if err != nil {
		t.Fatalf("ResolveVersion error: %v", err)
	}
	if v != "1.7.3" {
		t.Errorf("got %q, want %q", v, "1.7.3")
	}
}

func TestResolveVersionLATEST(t *testing.T) {
	srv := setupTestServer(t, map[string]string{
		"/org/example/mylib/maven-metadata.xml": mavenMetadataXML,
	})
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	v, err := client.ResolveVersion(context.Background(), "org.example", "mylib", "LATEST")
	if err != nil {
		t.Fatalf("ResolveVersion error: %v", err)
	}
	if v != "2.0.0" {
		t.Errorf("got %q, want %q", v, "2.0.0")
	}
}

func TestResolveVersionRange(t *testing.T) {
	srv := setupTestServer(t, map[string]string{
		"/org/example/mylib/maven-metadata.xml": mavenMetadataXML,
	})
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	// [1.5.0,2.0.0) should pick 1.9.0 as the highest matching
	v, err := client.ResolveVersion(context.Background(), "org.example", "mylib", "[1.5.0,2.0.0)")
	if err != nil {
		t.Fatalf("ResolveVersion error: %v", err)
	}
	if v != "1.9.0" {
		t.Errorf("got %q, want %q", v, "1.9.0")
	}
}

func TestSelectJVMVariant(t *testing.T) {
	srv := setupTestServer(t, map[string]string{
		"/org/example/mylib/1.2.3/mylib-1.2.3.module": gradleModuleJSON,
	})
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	gm, err := client.FetchGradleModule(context.Background(), Coordinate{
		GroupID: "org.example", ArtifactID: "mylib", Version: "1.2.3",
	})
	if err != nil {
		t.Fatalf("FetchGradleModule error: %v", err)
	}

	v := SelectJVMVariant(gm)
	if v == nil {
		t.Fatal("SelectJVMVariant returned nil")
	}
	if v.Name != "jvmApiElements" {
		t.Errorf("variant name = %q, want %q", v.Name, "jvmApiElements")
	}
	if v.Attributes["org.gradle.usage"] != "java-api" {
		t.Errorf("usage = %q, want java-api", v.Attributes["org.gradle.usage"])
	}
}

func TestParentPOMResolution(t *testing.T) {
	// Parse parent POM
	parent, err := ParsePOM(strings.NewReader(parentPOMXML))
	if err != nil {
		t.Fatalf("ParsePOM(parent): %v", err)
	}
	// Parse child POM
	child, err := ParsePOM(strings.NewReader(childWithParentPOMXML))
	if err != nil {
		t.Fatalf("ParsePOM(child): %v", err)
	}
	// Merge parent into child
	child.MergeParent(parent)

	// Child should inherit group ID from parent
	if child.EffectiveGroupID() != "org.example" {
		t.Errorf("EffectiveGroupID = %q, want %q", child.EffectiveGroupID(), "org.example")
	}
	// Child should have inherited dependencyManagement
	if len(child.DependencyManagement) == 0 {
		t.Error("expected DependencyManagement to be inherited from parent")
	}
}

func TestBOMResolution(t *testing.T) {
	bom, err := ParsePOM(strings.NewReader(bomPOMXML))
	if err != nil {
		t.Fatalf("ParsePOM(bom): %v", err)
	}

	// A POM that imports the BOM
	const importingPOM = `<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <groupId>org.example</groupId>
  <artifactId>myapp</artifactId>
  <version>1.0.0</version>
  <dependencies>
    <dependency>
      <groupId>org.slf4j</groupId>
      <artifactId>slf4j-api</artifactId>
    </dependency>
    <dependency>
      <groupId>com.google.guava</groupId>
      <artifactId>guava</artifactId>
    </dependency>
  </dependencies>
</project>`

	pom, err := ParsePOM(strings.NewReader(importingPOM))
	if err != nil {
		t.Fatalf("ParsePOM(importing): %v", err)
	}
	pom.ApplyBOM(bom)

	var slf4jVersion, guavaVersion string
	for _, dep := range pom.Dependencies {
		switch dep.ArtifactID {
		case "slf4j-api":
			slf4jVersion = dep.Version
		case "guava":
			guavaVersion = dep.Version
		}
	}
	if slf4jVersion != "2.0.9" {
		t.Errorf("slf4j-api version = %q, want %q", slf4jVersion, "2.0.9")
	}
	if guavaVersion != "32.1.2-jre" {
		t.Errorf("guava version = %q, want %q", guavaVersion, "32.1.2-jre")
	}
}

func TestExclusionHandling(t *testing.T) {
	// Serves the app POM (with exclusions) and mylib POM (which depends on slf4j-api)
	handlers := map[string]string{
		"/org/example/myapp/1.0.0/myapp-1.0.0.pom":   pomWithExclusionsXML,
		"/org/example/mylib/1.2.3/mylib-1.2.3.pom":   simplePOMXML,
		"/org/slf4j/slf4j-api/2.0.0/slf4j-api-2.0.0.pom": `<?xml version="1.0"?>
<project><groupId>org.slf4j</groupId><artifactId>slf4j-api</artifactId><version>2.0.0</version></project>`,
	}
	srv := setupTestServer(t, handlers)
	defer srv.Close()

	reg := NewCustomRegistry("test", srv.URL)
	client := NewClient(WithRegistry(reg))

	coords, err := ResolveTransitive(context.Background(), client, []Coordinate{
		{GroupID: "org.example", ArtifactID: "myapp", Version: "1.0.0"},
	})
	if err != nil {
		t.Fatalf("ResolveTransitive error: %v", err)
	}

	// slf4j-api should be excluded
	for _, c := range coords {
		if c.GroupID == "org.slf4j" && c.ArtifactID == "slf4j-api" {
			t.Errorf("slf4j-api should have been excluded but was found: %s", c)
		}
	}
}

package maven

import (
	"encoding/xml"
	"fmt"
	"io"
	"strings"
)

// POM represents a parsed Maven POM file.
type POM struct {
	XMLName    xml.Name `xml:"project"`
	GroupID    string   `xml:"groupId"`
	ArtifactID string   `xml:"artifactId"`
	Version    string   `xml:"version"`
	Packaging  string   `xml:"packaging"`
	Name       string   `xml:"name"`
	Description string  `xml:"description"`
	URL        string   `xml:"url"`

	Parent     *Parent  `xml:"parent"`
	Licenses   []License   `xml:"licenses>license"`
	Developers []Developer `xml:"developers>developer"`
	SCM        *SCM        `xml:"scm"`

	Properties map[string]string

	Dependencies       []Dependency `xml:"dependencies>dependency"`
	DependencyManagement []Dependency `xml:"dependencyManagement>dependencies>dependency"`
}

// EffectiveGroupID returns the group ID, falling back to parent group ID.
func (p *POM) EffectiveGroupID() string {
	if p.GroupID != "" {
		return p.GroupID
	}
	if p.Parent != nil {
		return p.Parent.GroupID
	}
	return ""
}

// EffectiveVersion returns the version, falling back to parent version.
func (p *POM) EffectiveVersion() string {
	if p.Version != "" {
		return p.Version
	}
	if p.Parent != nil {
		return p.Parent.Version
	}
	return ""
}

// EffectivePackaging returns the packaging, defaulting to "jar".
func (p *POM) EffectivePackaging() string {
	if p.Packaging != "" {
		return p.Packaging
	}
	return "jar"
}

// Parent represents a POM parent reference.
type Parent struct {
	GroupID    string `xml:"groupId"`
	ArtifactID string `xml:"artifactId"`
	Version    string `xml:"version"`
	RelativePath string `xml:"relativePath"`
}

// Dependency represents a single Maven dependency.
type Dependency struct {
	GroupID    string      `xml:"groupId"`
	ArtifactID string      `xml:"artifactId"`
	Version    string      `xml:"version"`
	Scope      string      `xml:"scope"`
	Type       string      `xml:"type"`
	Classifier string      `xml:"classifier"`
	Optional   string      `xml:"optional"`
	Exclusions []Exclusion `xml:"exclusions>exclusion"`
}

// IsOptional returns true if the dependency is optional.
func (d Dependency) IsOptional() bool {
	return strings.EqualFold(d.Optional, "true")
}

// EffectiveScope returns the dependency scope, defaulting to "compile".
func (d Dependency) EffectiveScope() string {
	if d.Scope != "" {
		return d.Scope
	}
	return "compile"
}

// Exclusion represents a dependency exclusion.
type Exclusion struct {
	GroupID    string `xml:"groupId"`
	ArtifactID string `xml:"artifactId"`
}

// License represents a project license.
type License struct {
	Name         string `xml:"name"`
	URL          string `xml:"url"`
	Distribution string `xml:"distribution"`
}

// Developer represents a project developer.
type Developer struct {
	ID    string `xml:"id"`
	Name  string `xml:"name"`
	Email string `xml:"email"`
}

// SCM holds source control management information.
type SCM struct {
	Connection          string `xml:"connection"`
	DeveloperConnection string `xml:"developerConnection"`
	URL                 string `xml:"url"`
}

// rawPOM is used for XML parsing including properties.
type rawPOM struct {
	XMLName    xml.Name `xml:"project"`
	GroupID    string   `xml:"groupId"`
	ArtifactID string   `xml:"artifactId"`
	Version    string   `xml:"version"`
	Packaging  string   `xml:"packaging"`
	Name       string   `xml:"name"`
	Description string  `xml:"description"`
	URL        string   `xml:"url"`

	Parent     *Parent  `xml:"parent"`
	Licenses   []License   `xml:"licenses>license"`
	Developers []Developer `xml:"developers>developer"`
	SCM        *SCM        `xml:"scm"`

	Properties struct {
		Entries []xml.Token
		Raw     []byte `xml:",innerxml"`
	} `xml:"properties"`

	Dependencies         []Dependency `xml:"dependencies>dependency"`
	DependencyManagement []Dependency `xml:"dependencyManagement>dependencies>dependency"`
}

// ParsePOM parses a Maven POM XML from the given reader.
func ParsePOM(r io.Reader) (*POM, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("pom: read error: %w", err)
	}

	var raw rawPOM
	if err := xml.Unmarshal(data, &raw); err != nil {
		return nil, fmt.Errorf("pom: xml parse error: %w", err)
	}

	pom := &POM{
		GroupID:              raw.GroupID,
		ArtifactID:           raw.ArtifactID,
		Version:              raw.Version,
		Packaging:            raw.Packaging,
		Name:                 raw.Name,
		Description:          raw.Description,
		URL:                  raw.URL,
		Parent:               raw.Parent,
		Licenses:             raw.Licenses,
		Developers:           raw.Developers,
		SCM:                  raw.SCM,
		Dependencies:         raw.Dependencies,
		DependencyManagement: raw.DependencyManagement,
		Properties:           make(map[string]string),
	}

	// Parse properties from inner XML
	if len(raw.Properties.Raw) > 0 {
		parseProperties(raw.Properties.Raw, pom.Properties)
	}

	// Built-in property substitutions
	pom.Properties["project.groupId"] = pom.EffectiveGroupID()
	pom.Properties["project.artifactId"] = pom.ArtifactID
	pom.Properties["project.version"] = pom.EffectiveVersion()

	// Interpolate all fields
	pom.GroupID = interpolate(pom.GroupID, pom.Properties)
	pom.ArtifactID = interpolate(pom.ArtifactID, pom.Properties)
	pom.Version = interpolate(pom.Version, pom.Properties)

	for i := range pom.Dependencies {
		pom.Dependencies[i] = interpolateDep(pom.Dependencies[i], pom.Properties)
	}
	for i := range pom.DependencyManagement {
		pom.DependencyManagement[i] = interpolateDep(pom.DependencyManagement[i], pom.Properties)
	}

	return pom, nil
}

// parseProperties parses the inner XML of <properties> into a map.
func parseProperties(raw []byte, props map[string]string) {
	decoder := xml.NewDecoder(strings.NewReader("<properties>" + string(raw) + "</properties>"))
	for {
		tok, err := decoder.Token()
		if err != nil {
			break
		}
		start, ok := tok.(xml.StartElement)
		if !ok {
			continue
		}
		var val string
		if err := decoder.DecodeElement(&val, &start); err == nil {
			props[start.Name.Local] = val
		}
	}
}

// interpolate replaces ${property} references in s using props.
func interpolate(s string, props map[string]string) string {
	if !strings.Contains(s, "${") {
		return s
	}
	var b strings.Builder
	for {
		start := strings.Index(s, "${")
		if start < 0 {
			b.WriteString(s)
			break
		}
		end := strings.Index(s[start:], "}")
		if end < 0 {
			b.WriteString(s)
			break
		}
		end += start
		b.WriteString(s[:start])
		key := s[start+2 : end]
		if val, ok := props[key]; ok {
			b.WriteString(val)
		} else {
			// Leave unresolved references as-is
			b.WriteString(s[start : end+1])
		}
		s = s[end+1:]
	}
	return b.String()
}

// interpolateDep interpolates all string fields of a Dependency.
func interpolateDep(d Dependency, props map[string]string) Dependency {
	d.GroupID = interpolate(d.GroupID, props)
	d.ArtifactID = interpolate(d.ArtifactID, props)
	d.Version = interpolate(d.Version, props)
	d.Scope = interpolate(d.Scope, props)
	d.Classifier = interpolate(d.Classifier, props)
	return d
}

// ApplyBOM merges dependency management from a BOM POM into this POM.
// BOM entries fill in missing versions for existing dependencies.
func (p *POM) ApplyBOM(bom *POM) {
	bomVersions := make(map[string]string)
	for _, d := range bom.DependencyManagement {
		key := d.GroupID + ":" + d.ArtifactID
		if d.Version != "" {
			bomVersions[key] = d.Version
		}
	}
	for i := range p.Dependencies {
		if p.Dependencies[i].Version == "" {
			key := p.Dependencies[i].GroupID + ":" + p.Dependencies[i].ArtifactID
			if v, ok := bomVersions[key]; ok {
				p.Dependencies[i].Version = v
			}
		}
	}
}

// MergeParent merges parent POM fields into this POM (only fills in missing values).
func (p *POM) MergeParent(parent *POM) {
	if p.GroupID == "" {
		p.GroupID = parent.GroupID
	}
	if p.Version == "" {
		p.Version = parent.Version
	}
	// Merge parent properties (child overrides parent)
	for k, v := range parent.Properties {
		if _, exists := p.Properties[k]; !exists {
			p.Properties[k] = v
		}
	}
	// Merge dependency management from parent
	parentBOMVersions := make(map[string]string)
	for _, d := range parent.DependencyManagement {
		key := d.GroupID + ":" + d.ArtifactID
		parentBOMVersions[key] = d.Version
	}
	existingDM := make(map[string]bool)
	for _, d := range p.DependencyManagement {
		existingDM[d.GroupID+":"+d.ArtifactID] = true
	}
	for _, d := range parent.DependencyManagement {
		if !existingDM[d.GroupID+":"+d.ArtifactID] {
			p.DependencyManagement = append(p.DependencyManagement, d)
		}
	}
}

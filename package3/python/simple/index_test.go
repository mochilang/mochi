package simple

import (
	"strings"
	"testing"
)

func TestNormalise(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"httpx", "httpx"},
		{"HTTPX", "httpx"},
		{"Flask-SQLAlchemy", "flask-sqlalchemy"},
		{"flask_sqlalchemy", "flask-sqlalchemy"},
		{"flask.sqlalchemy", "flask-sqlalchemy"},
		{"Flask__SQL-Alchemy", "flask-sql-alchemy"},
		{"Flask--SQL--Alchemy", "flask-sql-alchemy"},
		{"Flask_.SQL.Alchemy", "flask-sql-alchemy"},
		{"a", "a"},
		{"A.B.C", "a-b-c"},
		{"zope.interface", "zope-interface"},
		{"oslo.config", "oslo-config"},
	}
	for _, tc := range cases {
		got := Normalise(tc.in)
		if got != tc.want {
			t.Errorf("Normalise(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestValidate(t *testing.T) {
	good := &Project{
		Name: "httpx",
		Files: []File{
			{Filename: "httpx-0.27.0-py3-none-any.whl", URL: "https://example.com/x.whl"},
		},
	}
	if err := good.Validate(); err != nil {
		t.Errorf("good.Validate() = %v; want nil", err)
	}

	bad := []*Project{
		{Name: "", Files: []File{{Filename: "x.whl", URL: "u"}}},
		{Name: "HTTPX", Files: []File{{Filename: "x.whl", URL: "u"}}},
		{Name: "httpx", Files: nil},
		{Name: "httpx", Files: []File{{Filename: "", URL: "u"}}},
		{Name: "httpx", Files: []File{{Filename: "x.whl", URL: ""}}},
	}
	for i, p := range bad {
		if err := p.Validate(); err == nil {
			t.Errorf("bad[%d].Validate() = nil; want error", i)
		}
	}
}

func TestValidateRejectsUnnormalisedName(t *testing.T) {
	p := &Project{
		Name: "Flask-SQLAlchemy",
		Files: []File{
			{Filename: "x.whl", URL: "https://example.com/x.whl"},
		},
	}
	err := p.Validate()
	if err == nil {
		t.Fatalf("Validate() = nil; want error about non-normalised name")
	}
	if !strings.Contains(err.Error(), "normalised") {
		t.Errorf("Validate() error = %v; want mention of 'normalised'", err)
	}
}

func TestFilesByFilename(t *testing.T) {
	p := &Project{
		Name: "httpx",
		Files: []File{
			{Filename: "httpx-0.27.0.tar.gz", URL: "https://example.com/sdist"},
			{Filename: "httpx-0.27.0-py3-none-any.whl", URL: "https://example.com/wheel"},
		},
	}
	m := p.FilesByFilename()
	if len(m) != 2 {
		t.Fatalf("FilesByFilename() len = %d; want 2", len(m))
	}
	if m["httpx-0.27.0.tar.gz"].URL != "https://example.com/sdist" {
		t.Errorf("sdist URL = %q; want sdist URL", m["httpx-0.27.0.tar.gz"].URL)
	}
	if m["httpx-0.27.0-py3-none-any.whl"].URL != "https://example.com/wheel" {
		t.Errorf("wheel URL = %q; want wheel URL", m["httpx-0.27.0-py3-none-any.whl"].URL)
	}
}

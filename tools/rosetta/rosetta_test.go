package rosetta

import "testing"

func TestListTasks(t *testing.T) {
	tasks, err := ListTasks(false)
	if err != nil {
		t.Fatalf("ListTasks error: %v", err)
	}
	if len(tasks) == 0 {
		t.Fatal("no tasks returned")
	}
	found := false
	for _, tname := range tasks {
		if tname == "MD5" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("MD5 not found in task list")
	}
}

func TestListLanguages(t *testing.T) {
	langs, err := ListLanguages(false)
	if err != nil {
		t.Fatalf("ListLanguages error: %v", err)
	}
	if len(langs) == 0 {
		t.Fatal("no languages returned")
	}
	found := false
	for _, l := range langs {
		if l == "Go" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("Go not found in languages list")
	}
}

func TestListSourcesCached(t *testing.T) {
	names, err := ListSources("MD5", "Go", false)
	if err != nil {
		t.Fatalf("ListSources error: %v", err)
	}
	if len(names) == 0 {
		t.Fatal("no source names")
	}
	hasMd5 := false
	for _, n := range names {
		if n == "md5.go" {
			hasMd5 = true
			break
		}
	}
	if !hasMd5 {
		t.Fatalf("md5.go not listed")
	}
}

func TestDownloadCached(t *testing.T) {
	data, err := Download("MD5", "Go", "md5.go", false)
	if err != nil {
		t.Fatalf("Download error: %v", err)
	}
	if len(data) == 0 {
		t.Fatal("empty download")
	}
	if string(data[:2]) != "pa" && string(data[:6]) != "package" {
		t.Fatalf("unexpected data prefix: %q", data[:10])
	}
}

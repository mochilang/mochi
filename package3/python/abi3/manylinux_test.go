package abi3

import (
	"sort"
	"testing"
)

func TestKnownProfilesSorted(t *testing.T) {
	for tag, p := range KnownProfiles {
		if p.Tag != tag {
			t.Errorf("profile %q has Tag %q", tag, p.Tag)
		}
		if !sort.StringsAreSorted(p.AllowedLibs) {
			t.Errorf("profile %q AllowedLibs is unsorted: %v", tag, p.AllowedLibs)
		}
	}
}

func TestLookupProfile(t *testing.T) {
	if _, ok := LookupProfile("manylinux_2_28_x86_64"); !ok {
		t.Fatal("manylinux_2_28_x86_64 must be known")
	}
	if _, ok := LookupProfile("manylinux_99_99_x86_64"); ok {
		t.Fatal("future-dated tag must be unknown")
	}
}

func TestProfileAllows(t *testing.T) {
	p := KnownProfiles["manylinux_2_28_x86_64"]
	if !p.Allows("libc.so.6") {
		t.Error("libc.so.6 should be allowed")
	}
	if p.Allows("libssl.so.3") {
		t.Error("libssl.so.3 should NOT be allowed (must be vendored)")
	}
}

func TestProfileTagsContainsAll(t *testing.T) {
	got := ProfileTags()
	if !sort.StringsAreSorted(got) {
		t.Fatalf("ProfileTags not sorted: %v", got)
	}
	if len(got) != len(KnownProfiles) {
		t.Fatalf("ProfileTags returned %d, want %d", len(got), len(KnownProfiles))
	}
}

func TestMacOSAllowsLibSystem(t *testing.T) {
	p := KnownProfiles["macosx_11_0_arm64"]
	if !p.Allows("/usr/lib/libSystem.B.dylib") {
		t.Error("macOS profile must allow libSystem.B.dylib")
	}
	if p.Allows("/usr/local/lib/libopenssl.dylib") {
		t.Error("macOS profile must NOT allow Homebrew libs")
	}
}

func TestWindowsAllowsKernel32(t *testing.T) {
	p := KnownProfiles["win_amd64"]
	if !p.Allows("KERNEL32.dll") {
		t.Error("win_amd64 must allow KERNEL32.dll")
	}
	if p.Allows("libssl-3-x64.dll") {
		t.Error("win_amd64 must NOT allow third-party OpenSSL")
	}
}

func TestMusllinuxAllowsMusl(t *testing.T) {
	p := KnownProfiles["musllinux_1_2_x86_64"]
	if !p.Allows("libc.musl-x86_64.so.1") {
		t.Error("musllinux must allow libc.musl-x86_64.so.1")
	}
	if p.Allows("libc.so.6") {
		t.Error("musllinux must NOT allow glibc libc")
	}
}

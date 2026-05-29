package abi3

import "sort"

// ManylinuxProfile is the policy entry for one platform tag: which
// shared libraries the wheel may legitimately link against without
// shipping them inside the wheel's .libs/ vendoring directory.
//
// The Tag matches the platform field of a WheelTag (e.g.
// "manylinux_2_28_x86_64"). MinGlibc is the minimum glibc version
// implied by the tag (empty for non-glibc tags like musllinux / macos /
// windows). AllowedLibs is the sorted soname list — every external lib
// outside this set is a violation.
type ManylinuxProfile struct {
	Tag         string
	MinGlibc    string
	AllowedLibs []string
}

// Allows reports whether soname is in the profile's allow-list.
func (p ManylinuxProfile) Allows(soname string) bool {
	i := sort.SearchStrings(p.AllowedLibs, soname)
	return i < len(p.AllowedLibs) && p.AllowedLibs[i] == soname
}

// KnownProfiles is the closed table of platform tags MEP-71 ships
// auditing rules for. The allow-lists track the upstream PEP 600
// manylinux + PEP 656 musllinux baselines plus the macOS / Windows
// system libraries that uv / pip / auditwheel treat as always-present.
// Lists must remain sorted (KnownProfiles is consumed via binary
// search through Allows).
var KnownProfiles = map[string]ManylinuxProfile{
	"manylinux_2_17_x86_64": {
		Tag:      "manylinux_2_17_x86_64",
		MinGlibc: "2.17",
		AllowedLibs: []string{
			"ld-linux-x86-64.so.2",
			"libc.so.6",
			"libdl.so.2",
			"libm.so.6",
			"libpthread.so.0",
			"libresolv.so.2",
			"librt.so.1",
			"libutil.so.1",
		},
	},
	"manylinux_2_28_x86_64": {
		Tag:      "manylinux_2_28_x86_64",
		MinGlibc: "2.28",
		AllowedLibs: []string{
			"ld-linux-x86-64.so.2",
			"libc.so.6",
			"libdl.so.2",
			"libgcc_s.so.1",
			"libm.so.6",
			"libpthread.so.0",
			"libresolv.so.2",
			"librt.so.1",
			"libstdc++.so.6",
			"libutil.so.1",
		},
	},
	"manylinux_2_34_x86_64": {
		Tag:      "manylinux_2_34_x86_64",
		MinGlibc: "2.34",
		AllowedLibs: []string{
			"ld-linux-x86-64.so.2",
			"libc.so.6",
			"libgcc_s.so.1",
			"libm.so.6",
			"libstdc++.so.6",
		},
	},
	"manylinux_2_17_aarch64": {
		Tag:      "manylinux_2_17_aarch64",
		MinGlibc: "2.17",
		AllowedLibs: []string{
			"ld-linux-aarch64.so.1",
			"libc.so.6",
			"libdl.so.2",
			"libm.so.6",
			"libpthread.so.0",
			"libresolv.so.2",
			"librt.so.1",
			"libutil.so.1",
		},
	},
	"manylinux_2_28_aarch64": {
		Tag:      "manylinux_2_28_aarch64",
		MinGlibc: "2.28",
		AllowedLibs: []string{
			"ld-linux-aarch64.so.1",
			"libc.so.6",
			"libdl.so.2",
			"libgcc_s.so.1",
			"libm.so.6",
			"libpthread.so.0",
			"libstdc++.so.6",
		},
	},
	"musllinux_1_2_x86_64": {
		Tag: "musllinux_1_2_x86_64",
		AllowedLibs: []string{
			"ld-musl-x86_64.so.1",
			"libc.musl-x86_64.so.1",
		},
	},
	"musllinux_1_2_aarch64": {
		Tag: "musllinux_1_2_aarch64",
		AllowedLibs: []string{
			"ld-musl-aarch64.so.1",
			"libc.musl-aarch64.so.1",
		},
	},
	"macosx_11_0_arm64": {
		Tag: "macosx_11_0_arm64",
		AllowedLibs: []string{
			"/usr/lib/libSystem.B.dylib",
			"/usr/lib/libc++.1.dylib",
			"/usr/lib/libobjc.A.dylib",
			"/usr/lib/libresolv.9.dylib",
		},
	},
	"macosx_10_15_x86_64": {
		Tag: "macosx_10_15_x86_64",
		AllowedLibs: []string{
			"/usr/lib/libSystem.B.dylib",
			"/usr/lib/libc++.1.dylib",
			"/usr/lib/libobjc.A.dylib",
			"/usr/lib/libresolv.9.dylib",
		},
	},
	"win_amd64": {
		Tag: "win_amd64",
		AllowedLibs: []string{
			"ADVAPI32.dll",
			"KERNEL32.dll",
			"MSVCP140.dll",
			"USER32.dll",
			"VCRUNTIME140.dll",
			"VCRUNTIME140_1.dll",
			"api-ms-win-crt-runtime-l1-1-0.dll",
		},
	},
	"win_arm64": {
		Tag: "win_arm64",
		AllowedLibs: []string{
			"ADVAPI32.dll",
			"KERNEL32.dll",
			"MSVCP140.dll",
			"USER32.dll",
			"VCRUNTIME140.dll",
			"api-ms-win-crt-runtime-l1-1-0.dll",
		},
	},
}

// LookupProfile returns the profile for tag, or ok=false when the tag
// is unknown to the bridge. Callers should treat unknown tags as audit
// failures rather than silently auditing against the wrong policy.
func LookupProfile(tag string) (ManylinuxProfile, bool) {
	p, ok := KnownProfiles[tag]
	return p, ok
}

// ProfileTags returns the sorted slice of known tag names. Useful for
// surfacing the supported-platform set in CLI help text.
func ProfileTags() []string {
	out := make([]string, 0, len(KnownProfiles))
	for k := range KnownProfiles {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

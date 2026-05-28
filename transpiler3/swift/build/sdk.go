package build

import (
	"os/exec"
	"strings"
)

// LinuxSDKTriple lists the known musl static SDK identifiers.
const (
	SDKTripleX64   = "x86_64-swift-linux-musl"
	SDKTripleArm64 = "aarch64-swift-linux-musl"
)

// StaticLinuxSDKAvailable returns true if the named SDK triple is installed.
// It runs `swift sdk list` and checks for the triple.
func StaticLinuxSDKAvailable(triple string) bool {
	out, err := exec.Command("swift", "sdk", "list").Output()
	if err != nil {
		return false
	}
	return strings.Contains(string(out), triple)
}

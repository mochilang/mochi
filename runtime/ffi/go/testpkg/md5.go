package testpkg

import (
	"crypto/md5"
	"fmt"
)

// MD5Hex returns the hexadecimal MD5 digest of s.
func MD5Hex(s string) string {
	sum := md5.Sum([]byte(s))
	return fmt.Sprintf("%x", sum[:])
}

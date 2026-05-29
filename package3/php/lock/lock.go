// Package lock handles the [[php-package]] table in mochi.lock, recording
// Composer package name, version, dist-sha256, reflection-sha256,
// capabilities, php-version-constraint, and transitive dependencies. Phase 0
// ships the package stub; the full implementation arrives in phase 8.
package lock

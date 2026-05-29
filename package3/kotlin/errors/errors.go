// Package errors defines bridge-specific error types for the Mochi↔Kotlin bridge.
package errors

import (
	"errors"
	"fmt"
)

// ErrNoKotlinMetadata is returned when a .class file does not contain a
// @kotlin.Metadata annotation (i.e. it is a plain Java class file).
var ErrNoKotlinMetadata = errors.New("no @kotlin.Metadata annotation found in class file")

type ErrUnsupportedMetadataVersion struct {
	Version [3]int32
}

func (e *ErrUnsupportedMetadataVersion) Error() string {
	return fmt.Sprintf("unsupported kotlin.Metadata schema version [%d, %d, %d]: minimum supported is [1, 4, 0]",
		e.Version[0], e.Version[1], e.Version[2])
}

type ErrArtifactNotFound struct {
	GroupID    string
	ArtifactID string
	Version    string
}

func (e *ErrArtifactNotFound) Error() string {
	if e.Version == "" {
		return fmt.Sprintf("artifact not found on Maven Central: %s:%s", e.GroupID, e.ArtifactID)
	}
	return fmt.Sprintf("artifact not found on Maven Central: %s:%s@%s", e.GroupID, e.ArtifactID, e.Version)
}

type ErrNativeImageBuildFailed struct {
	ArtifactID string
	ExitCode   int
	Stderr     string
}

func (e *ErrNativeImageBuildFailed) Error() string {
	return fmt.Sprintf("GraalVM native-image build failed for %s (exit %d): %s",
		e.ArtifactID, e.ExitCode, e.Stderr)
}

type ErrCapabilityViolation struct {
	ArtifactID         string
	Capability         string
	Detected           bool
	Declared           bool
}

func (e *ErrCapabilityViolation) Error() string {
	return fmt.Sprintf("capability violation for %s: capability %q detected in artifact but not declared in [kotlin.capabilities]",
		e.ArtifactID, e.Capability)
}

type ErrGraalVMNotFound struct{}

func (e *ErrGraalVMNotFound) Error() string {
	return "GraalVM native-image not found; install GraalVM CE 21+ and set GRAALVM_HOME or add native-image to PATH"
}

type ErrLockMismatch struct {
	ArtifactID string
	Field      string
	Expected   string
	Got        string
}

func (e *ErrLockMismatch) Error() string {
	return fmt.Sprintf("mochi.lock mismatch for %s: field %q expected %s got %s",
		e.ArtifactID, e.Field, e.Expected, e.Got)
}

type ErrInvalidCoordinate struct {
	Input string
}

func (e *ErrInvalidCoordinate) Error() string {
	return fmt.Sprintf("invalid Maven coordinate %q: expected <groupId>:<artifactId>[@<version>]", e.Input)
}

type ErrFetchFailed struct {
	URL        string
	StatusCode int
}

func (e *ErrFetchFailed) Error() string {
	return fmt.Sprintf("failed to fetch %s: HTTP %d", e.URL, e.StatusCode)
}

type ErrPublishFailed struct {
	DeploymentID string
	State        string
	Message      string
}

func (e *ErrPublishFailed) Error() string {
	return fmt.Sprintf("Maven Central deployment %s failed with state %s: %s",
		e.DeploymentID, e.State, e.Message)
}

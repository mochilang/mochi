// Package swift is the MEP-69 Swift package bridge for Mochi. It provides
// tooling to ingest Swift packages via the Swift Package Manager registry
// (SE-0292) or direct git sources, parse .swiftinterface API surfaces,
// map Swift types to Mochi types, and emit @_cdecl bridging wrappers that
// allow Mochi programs to call Swift library functions as extern fns.
//
// The bridge is organised into layered sub-packages:
//
//   - errors:         shared error types and skip-reason taxonomy
//   - semver:         SPM version requirement parsing and satisfaction
//   - swiftpm:        swift package dump-package manifest parsing
//   - gitregistry:    git-based Swift package resolution and download
//   - se0292:         SE-0292 package registry HTTP client
//   - cache:          content-addressed on-disk cache for downloaded archives
//   - swiftinterface: parser for .swiftinterface API surface files
//   - typemap:        Swift type to Mochi type mapping
//   - emit:           Mochi extern fn source emitter
//   - cdeclwrapper:   @_cdecl Swift bridge file emitter
//   - build:          swift build driver (produces .a static libraries)
//   - lock:           mochi.lock [[swift-package]] table management
//   - library:        Mochi-to-Swift publish direction emitter
package swift

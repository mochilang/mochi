---
title: "Phase 15. iOS project generation (XcodeGen)"
sidebar_position: 19
sidebar_label: "Phase 15. iOS"
description: "MEP-49 Phase 15 — IOSProjectConfig; GenerateIOSProject generates project.yml and Info.plist for XcodeGen; xcodebuild archive deferred."
---

# Phase 15. iOS project generation (XcodeGen)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 15](/docs/mep/mep-0049#phase-15-ios-bundle) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase15iOS`: 5 fixtures validate the generated `project.yml` and `Info.plist` structure. Actual `xcodegen generate` and `xcodebuild archive` are skipped when XcodeGen is not installed (the test calls `t.Skip`). Gate runs on macOS 15 in CI.

## Goal-alignment audit

Phase 15 ships the `project.yml` + `Info.plist` generation layer that is the prerequisite for iOS `.ipa` creation. Generating a correct XcodeGen manifest and Info.plist validates the full metadata pipeline: bundle ID, app name, iOS deployment target, Swift version. The actual `xcodebuild archive` and `.ipa` export are deferred until a macOS CI runner with Xcode and signing credentials is available.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 15.0 | `IOSProjectConfig`; `GenerateIOSProject` writes `project.yml` + `Info.plist` | LANDED | mep/0049-phase-15 |
| 15.1 | `XcodeGenAvailable()`, `IOSSimulatorAvailable()` helpers | LANDED | mep/0049-phase-15 |
| 15.2 | `xcodegen generate` → `project.xcodeproj` | DEFERRED | — |
| 15.3 | `xcodebuild archive -destination "generic/platform=iOS"` → `.xcarchive` | DEFERRED | — |
| 15.4 | `xcodebuild -exportArchive` → `.ipa` | DEFERRED | — |
| 15.5 | Code signing; provisioning profiles; TestFlight upload | DEFERRED | — |

## Sub-phase 15.0 -- Project file generation

### Decisions made (15.0)

**`IOSProjectConfig`**: struct holding `AppName`, `BundleID`, `Version`, `MinIOSVersion`, `SwiftVersion`.

**`GenerateIOSProject(cfg, srcDir, outDir)`**: creates `outDir/project.yml` and `outDir/Sources/<AppName>/Info.plist`.

**Generated `project.yml`**:

```yaml
name: MochiOut
options:
  bundleIdPrefix: com.mochi
  deploymentTarget:
    iOS: "18.0"
  xcodeVersion: "16.0"
  swiftVersion: "6.0"

targets:
  MochiOut:
    type: application
    platform: iOS
    sources: Sources/MochiOut
    settings:
      SWIFT_VERSION: 6.0
      IPHONEOS_DEPLOYMENT_TARGET: "18.0"
      INFOPLIST_FILE: Sources/MochiOut/Info.plist
```

Note: the MochiRuntime package dependency is not yet included in the generated `project.yml`; it will be added when the full `xcodebuild` pipeline is landed.

**Generated `Info.plist`**: includes `CFBundleName`, `CFBundleIdentifier`, `CFBundleVersion`, `CFBundleShortVersionString`, and `UILaunchScreen` (empty dict). Uses `<?xml version="1.0"?>` DTD format.

## Sub-phase 15.1 -- Availability helpers

### Decisions made (15.1)

**`XcodeGenAvailable() bool`**: checks for `xcodegen` on PATH via `exec.LookPath`. Returns `false` on Linux.

**`IOSSimulatorAvailable() bool`**: runs `xcrun simctl list devices available --json` and looks for `"iPhone"` in the output. Returns `false` on non-Darwin platforms.

Both helpers are used in the gate test to `t.Skip` when prerequisites are absent, so the test always passes in CI even without full Xcode + simulator setup.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/build/ios.go` | `IOSProjectConfig`, `GenerateIOSProject`, `generateProjectYML`, `generateInfoPlist`, `XcodeGenAvailable`, `IOSSimulatorAvailable` |
| `transpiler3/swift/build/phase15_test.go` | `TestPhase15iOS`: 5 fixtures; skips xcodebuild if XcodeGen absent |
| `tests/transpiler3/swift/fixtures/phase15-ios/` | 5 fixture directories |

## Test set

- `TestPhase15iOS` -- 5 fixtures: `ios_agent`, `ios_hello`, `ios_list`, `ios_print`, `ios_record`. Each test calls `GenerateIOSProject`, then verifies `project.yml` and `Info.plist` exist and contain expected bundle ID / app name.

## Deferred work

- `xcodegen generate` → `project.xcodeproj` (requires XcodeGen installed). Deferred to Phase 15.2.
- `xcodebuild archive` + `xcodebuild -exportArchive` → `.ipa`. Deferred to Phase 15.3/15.4.
- Code signing: `CODE_SIGN_STYLE`, provisioning profile, `--options runtime`, `--timestamp`. Deferred to Phase 15.5.
- `xcrun notarytool submit --wait` + `xcrun stapler staple`. Deferred to Phase 15.5.
- TestFlight upload via App Store Connect REST API. Deferred to Phase 15.5.
- MochiRuntime as SwiftPM package dependency in `project.yml`. Deferred to Phase 15.2.
- SwiftUI layer + `@main` SwiftUI `App` conformance. Deferred to a Phase 15 sub-MEP.

---
title: "Phase 15. iOS app bundle (.ipa via xcodebuild)"
sidebar_position: 19
sidebar_label: "Phase 15. iOS (.ipa)"
description: "MEP-49 Phase 15 — xcodebuild archive + exportArchive pipeline; XcodeGen project generation; codesign; notarytool; .ipa for TestFlight."
---

# Phase 15. iOS app bundle (.ipa via xcodebuild)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 15](/docs/mep/mep-0049#phase-15-ios-bundle) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase15iOS`: iOS App Bundle gate -- `xcodebuild archive` completes without error on `macos-15` runner; `.ipa` validates via `xcrun altool --validate-app --platform ios`. 20 fixtures green on iOS 18 Simulator (arm64).

## Goal-alignment audit

The iOS `.ipa` is the primary deliverable for mobile Mochi apps. Phase 15 ships the full pipeline from Mochi source to a distributable `.ipa` that can be uploaded to TestFlight. This requires generating an Xcode project (via XcodeGen), archiving, code signing, and package export -- all driven from the Mochi build driver.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 15.0 | `mochi build --target=swift-ios` → SwiftPM source + `XcodeGen` `.yml` → `xcodegen generate` → `project.xcodeproj` | NOT STARTED | — |
| 15.1 | `xcodebuild archive -scheme MochiOut -destination generic/platform=iOS` → `.xcarchive` | NOT STARTED | — |
| 15.2 | `xcodebuild -exportArchive` with `ExportOptions.plist` → `.ipa` | NOT STARTED | — |
| 15.3 | Code signing: `codesign --options runtime --timestamp --sign "Apple Development: ..."` | NOT STARTED | — |
| 15.4 | TestFlight upload: `xcrun notarytool submit --wait` + `stapler staple` | NOT STARTED | — |

## Sub-phase 15.0 -- XcodeGen project generation

### Decisions made (15.0)

**XcodeGen (Apache-2.0)**: generates `project.xcodeproj` from a `project.yml` specification. The Mochi build driver generates `project.yml` and calls `xcodegen generate`. XcodeGen is preferred over raw `xcodeproj` Ruby gem because it is actively maintained, supports Swift Package Manager integration, and produces reproducible Xcode projects (no UUIDs in YAML, UUIDs in `.xcodeproj` are deterministically derived from paths).

**Generated `project.yml`**:

```yaml
name: MochiOut
options:
  bundleIdPrefix: com.mochi
  deploymentTarget:
    iOS: "18.0"
  xcodeVersion: "16.0"
  swiftVersion: "6.0"

packages:
  MochiRuntime:
    url: https://github.com/mochilang/swift-runtime
    from: 0.1.0

targets:
  MochiOut:
    type: application
    platform: iOS
    sources: Sources/MochiOut
    dependencies:
      - package: MochiRuntime
        product: MochiRuntime
    settings:
      SWIFT_VERSION: 6.0
      SWIFT_STRICT_CONCURRENCY: complete
      IPHONEOS_DEPLOYMENT_TARGET: "18.0"
      INFOPLIST_FILE: Sources/MochiOut/Info.plist
```

**`Info.plist`**: the Mochi build driver generates a minimal `Info.plist` with `CFBundleName`, `CFBundleIdentifier`, `CFBundleVersion`, `CFBundleShortVersionString`, `UILaunchScreen`. The bundle ID is set via `mochi build --bundle-id com.example.myapp`.

**Core/App split**: for iOS compatibility, the SwiftPM package has two targets: `MochiOutCore` (library, the transpiled Mochi logic) and `MochiOut` (executable, the `@main` entry). XcodeGen links both into the iOS app target.

## Sub-phase 15.1 -- xcodebuild archive

### Decisions made (15.1)

**Archive command**:

```bash
xcodebuild archive \
  -project MochiOut.xcodeproj \
  -scheme MochiOut \
  -destination "generic/platform=iOS" \
  -archivePath build/MochiOut.xcarchive \
  -allowProvisioningUpdates \
  CODE_SIGN_STYLE=Automatic
```

**`-allowProvisioningUpdates`**: allows xcodebuild to automatically update provisioning profiles from the Apple Developer portal. Requires the machine to be authenticated with `xcodebuild -downloadAllPlatforms` or an existing provisioning profile in the keychain.

**iOS Simulator gate**: the `TestPhase15iOS` gate runs on iOS Simulator (not device) to avoid provisioning requirements in CI. The simulator build uses `-destination "platform=iOS Simulator,name=iPhone 16 Pro"` and does not produce a `.ipa` (simulators use unpackaged `.app`).

**`-destination "generic/platform=iOS"`**: device archive. Only runs in the "release" CI pipeline, not per-PR.

## Sub-phase 15.2 -- .ipa export

### Decisions made (15.2)

**`ExportOptions.plist`**: required by `xcodebuild -exportArchive`. The Mochi build driver generates it:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>method</key>
    <string>app-store-connect</string>
    <key>destination</key>
    <string>export</string>
    <key>signingStyle</key>
    <string>automatic</string>
    <key>stripSwiftSymbols</key>
    <true/>
    <key>uploadBitcode</key>
    <false/>
</dict>
</plist>
```

**Export command**:

```bash
xcodebuild -exportArchive \
  -archivePath build/MochiOut.xcarchive \
  -exportPath build/ \
  -exportOptionsPlist ExportOptions.plist
```

**Output**: `build/MochiOut.ipa` -- the distributable iOS app package.

## Sub-phase 15.3 -- Code signing

### Decisions made (15.3)

**Automatic signing in CI**: `CODE_SIGN_STYLE=Automatic` with a valid Apple Developer account. The xcodebuild manages provisioning profile selection.

**Manual signing for automation**: `CODE_SIGN_STYLE=Manual`, `PROVISIONING_PROFILE_SPECIFIER` set to the profile UUID, `CODE_SIGN_IDENTITY` set to the certificate common name. The Mochi build driver reads these from environment variables (`MOCHI_CODESIGN_IDENTITY`, `MOCHI_PROVISIONING_PROFILE`).

**`codesign` for libraries**: embedded frameworks within the `.ipa` must be individually signed before the app bundle is signed. xcodebuild handles this automatically.

**`--options runtime`**: required for notarization. Applied to all binaries in the archive.

**`--timestamp`**: required for notarization. Signs with a secure timestamp from Apple's timestamp server.

## Sub-phase 15.4 -- TestFlight upload

### Decisions made (15.4)

**`xcrun notarytool submit`**: replaced `altool` for notarization in November 2023. The Mochi build driver calls:

```bash
xcrun notarytool submit build/MochiOut.ipa \
  --apple-id "$APPLE_ID" \
  --password "$APP_SPECIFIC_PASSWORD" \
  --team-id "$TEAM_ID" \
  --wait
```

`--wait` blocks until notarization completes (or fails). Typical time: 30-120 seconds.

**`xcrun altool --validate-app`**: the test gate uses `altool --validate-app` (not `--upload-app`) to verify the `.ipa` structure without actually uploading. This is the gate for `TestPhase15iOS`.

**`xcrun stapler staple`**: for macOS distribution, staples the notarization ticket to the binary. Not required for iOS `.ipa` (App Store handles this).

**TestFlight automation**: after notarization, the `.ipa` is automatically distributed to internal testers via the App Store Connect API. The Mochi build driver supports `mochi publish --testflight` which calls the App Store Connect REST API.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/build/ios.go` | `xcodegen generate`, `xcodebuild archive`, `xcodebuild -exportArchive`, notarytool invocation |
| `transpiler3/swift/build/xcodegen.go` | `project.yml` + `Info.plist` generation |
| `transpiler3/swift/build/codesign.go` | `codesign` and `xcrun notarytool submit` wrappers |
| `transpiler3/swift/build/phase15_test.go` | `TestPhase15iOS`: 20 simulator fixtures + App Bundle gate |
| `tests/transpiler3/swift/fixtures/phase15-ios/` | 20 fixture directories |

## Test set

- `TestPhase15iOS` -- 20 fixtures on iOS 18 Simulator: `ios_hello`, `ios_print`, `ios_agent`, `ios_fetch`, `ios_stream`, `ios_record`, `ios_list`, `ios_map`, `ios_query`, `ios_match`, `ios_closure`, `ios_datalog`, `ios_llm_mock`, `ios_websocket_mock`, `ios_mainactor`, `ios_lifecycle`, `ios_backgroundtask`, `ios_notificationcenter`, `ios_userdefaults`, `ios_filesystemaccess`.
- App Bundle gate: `xcodebuild archive` + `xcrun altool --validate-app` on `macos-15`.

## Deferred work

- SwiftUI layer. Deferred to a Phase 15 sub-MEP.
- SwiftData integration. Deferred to a Phase 15 sub-MEP.
- iPadOS split-view / multi-window. Deferred.
- WatchKit / watchOS packaging. Deferred to Phase 15.1.
- App Clips. Out of v1 scope.

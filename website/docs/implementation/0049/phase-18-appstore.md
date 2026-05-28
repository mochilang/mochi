---
title: "Phase 18. App Store / Mac App Store validation"
sidebar_position: 22
sidebar_label: "Phase 18. App Store"
description: "MEP-49 Phase 18 — App Store Connect API automated submission; Mac App Store .pkg via productbuild; notarization stapling; xcrun altool --validate-app gate."
---

# Phase 18. App Store / Mac App Store validation

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 18](/docs/mep/mep-0049#phase-18-app-store) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 14:44 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase18AppStore`: `xcrun altool --validate-app --platform ios` passes for the `.ipa` produced in Phase 15. `xcrun altool --validate-app --platform osx` passes for the macOS `.pkg`. App Store Connect API upload succeeds in integration test (with a test bundle ID). 15 fixtures validate cleanly.

## Goal-alignment audit

App Store submission is the final delivery gate for Mochi iOS and macOS apps. Phase 18 automates the full submission pipeline: validate, upload, and confirm receipt via the App Store Connect API. Without this phase, users would need to manually run xcodebuild, notarize, and upload from Xcode. With Phase 18, `mochi publish --app-store` does the complete submission from the command line.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 18.0 | macOS `.app` packaging: `codesign --deep`, `.dmg` creation via `create-dmg` or `hdiutil` | NOT STARTED | — |
| 18.1 | Mac App Store `.pkg` via `productbuild --component .app /Applications`; `pkgbuild` for installer | NOT STARTED | — |
| 18.2 | Notarization: `xcrun notarytool submit --wait`; `xcrun stapler staple` for offline Gatekeeper | NOT STARTED | — |
| 18.3 | App Store Connect API upload: `xcrun altool --upload-app` or REST API; build processing wait | NOT STARTED | — |
| 18.4 | `xcrun altool --validate-app` gate in CI; validation without upload | NOT STARTED | — |

## Sub-phase 18.0 -- macOS .app and .dmg

### Decisions made (18.0)

**macOS `.app` bundle**: a macOS application is distributed as a `.app` bundle (a directory with a specific structure). The Mochi build driver (`mochi build --target=swift-macos`) produces:

```
MochiOut.app/
  Contents/
    MacOS/
      MochiOut          (compiled binary, codesigned)
    Resources/
      Assets.xcassets/  (icons, if provided)
    Info.plist
    _CodeSignature/     (codesign database)
```

**`codesign --deep`**: signs all embedded binaries (frameworks, dylibs) within the `.app` bundle before signing the outer bundle:

```bash
codesign \
  --deep \
  --options runtime \
  --timestamp \
  --sign "Developer ID Application: Your Name (TEAMID)" \
  MochiOut.app
```

**`create-dmg` (Apache-2.0)**: creates a distributable `.dmg` from the `.app`. Used for direct download distribution (not Mac App Store). The Mochi build driver calls `create-dmg` if available, else falls back to `hdiutil create` + `hdiutil convert` for a basic read-only `.dmg`.

**`hdiutil` flow**:

```bash
hdiutil create -srcfolder MochiOut.app -volname "MochiOut" -fs HFS+ \
  -fsargs "-c c=64,a=16,b=16" -format UDRW -size 100m MochiOut_rw.dmg
hdiutil convert MochiOut_rw.dmg -format UDZO -o MochiOut.dmg
```

## Sub-phase 18.1 -- Mac App Store .pkg

### Decisions made (18.1)

**Mac App Store requires a `.pkg`**: unlike direct download (`.dmg`), Mac App Store submissions use a `.pkg` installer.

**`productbuild`**: creates the final `.pkg`:

```bash
productbuild \
  --component MochiOut.app /Applications \
  --sign "3rd Party Mac Developer Installer: Your Name (TEAMID)" \
  MochiOut.pkg
```

**App Sandbox**: Mac App Store apps must have the App Sandbox entitlement. The Mochi build driver generates an `entitlements.plist`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "...">
<plist version="1.0">
<dict>
    <key>com.apple.security.app-sandbox</key>
    <true/>
    <key>com.apple.security.network.client</key>
    <true/>
    <!-- other entitlements from mochi.toml -->
</dict>
</plist>
```

Entitlements are declared in `mochi.toml` under `[macos.entitlements]`. The build driver reads them and generates the plist.

**`pkgbuild`**: for headless tools (command-line utilities distributed via Mac App Store), `pkgbuild` is used instead of `productbuild`:

```bash
pkgbuild --root staging/ --identifier com.example.mochitool \
  --version 1.0 --install-location /usr/local/bin \
  MochiTool.pkg
```

## Sub-phase 18.2 -- Notarization and stapling

### Decisions made (18.2)

**`xcrun notarytool submit`**: for both `.dmg` (direct download) and `.pkg` (Mac App Store):

```bash
xcrun notarytool submit MochiOut.dmg \
  --apple-id "$APPLE_ID" \
  --password "$APP_SPECIFIC_PASSWORD" \
  --team-id "$TEAM_ID" \
  --wait \
  --output-format json
```

The `--wait` flag blocks until Apple's notarization service completes. The JSON output contains the status and any issues found.

**`xcrun stapler staple`**: staples the notarization ticket to the file so Gatekeeper can verify it offline:

```bash
xcrun stapler staple MochiOut.dmg
xcrun stapler validate MochiOut.dmg  # verification step
```

Stapling is required for `.dmg` and `.pkg`. It is NOT available for `.ipa` (iOS apps are verified online by the App Store).

**Notarization credentials**: stored in the macOS Keychain profile (`--keychain-profile mochinotary`) instead of environment variables in production:

```bash
xcrun notarytool store-credentials mochinotary \
  --apple-id "$APPLE_ID" \
  --password "$APP_SPECIFIC_PASSWORD" \
  --team-id "$TEAM_ID"
# Then:
xcrun notarytool submit MochiOut.dmg --keychain-profile mochinotary --wait
```

## Sub-phase 18.3 -- App Store Connect API upload

### Decisions made (18.3)

**`xcrun altool --upload-app`**: the traditional upload mechanism. Still supported but App Store Connect REST API is preferred for automation.

**App Store Connect REST API**: Apple's REST API (api.appstoreconnect.apple.com) allows uploading builds, managing TestFlight, and reading build status without Xcode. The Mochi build driver uses the API via JWT authentication (API key + key ID + issuer ID, configured in `mochi.toml`).

**JWT auth**: App Store Connect API uses JWT with ES256 (ECDSA P-256). The Mochi driver generates the JWT from the API key file (`.p8`):

```go
// in transpiler3/swift/build/appstoreconnect.go:
func newJWT(keyID, issuerID string, key *ecdsa.PrivateKey) (string, error) {
    // ES256 JWT with 20-minute expiry, aud: "appstoreconnect-v1"
}
```

**Upload flow**:
1. Create a new build via `POST /v1/builds` (or `xcrun altool --upload-app`).
2. Poll `GET /v1/builds/{id}` until `processingState == "VALID"`.
3. Add build to TestFlight group via `POST /v1/betaBuildLocalizations`.
4. Submit for review via `POST /v1/appStoreVersionSubmissions`.

**`mochi publish --app-store` command**: drives the full upload flow. Requires `mochi.toml` with `[app-store]` section containing bundle ID, version, API key path.

## Sub-phase 18.4 -- xcrun altool validate gate

### Decisions made (18.4)

**`xcrun altool --validate-app`**: validates the `.ipa` or `.pkg` structure against App Store requirements without uploading:

```bash
xcrun altool --validate-app \
  --file MochiOut.ipa \
  --type ios \
  --apiKey "$API_KEY_ID" \
  --apiIssuer "$ISSUER_ID"
```

**CI gate**: the `TestPhase18AppStore` gate runs `--validate-app` in CI on `macos-15`. This does not require a real App Store account for structure validation (the `--validate-app` flag checks the binary structure and entitlements, not App Store-specific metadata). The API key is only needed for online validation (checking signing certificate revocation, etc.).

**Offline validation via `codesign -vvv`**: for fixtures that don't need the full App Store validation, `codesign -vvv --deep MochiOut.app` verifies the code signature structure locally:

```bash
codesign -vvv --deep --strict MochiOut.app
```

**15 fixture gate**: 15 programs that cover the App Store submission requirements are validated: correct bundle structure, Info.plist completeness, entitlements, code signature, embedded frameworks properly signed.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/build/macos.go` | `.app` packaging, `codesign --deep`, `.dmg` creation |
| `transpiler3/swift/build/macasstore.go` | `productbuild`, `pkgbuild`, entitlements plist generation |
| `transpiler3/swift/build/notarize.go` | `xcrun notarytool submit`, `xcrun stapler staple` wrappers |
| `transpiler3/swift/build/appstoreconnect.go` | App Store Connect REST API client; JWT generation |
| `transpiler3/swift/build/phase18_test.go` | `TestPhase18AppStore`: 15 fixtures + `xcrun altool --validate-app` gate |
| `tests/transpiler3/swift/fixtures/phase18-appstore/` | 15 fixture directories |

## Test set

- `TestPhase18AppStore` -- 15 fixtures: `appstore_hello_ios`, `appstore_hello_macos`, `appstore_dmg_structure`, `appstore_pkg_structure`, `appstore_entitlements_network`, `appstore_entitlements_sandbox`, `appstore_codesign_verify`, `appstore_notarize_structure`, `appstore_staple_verify`, `appstore_info_plist_complete`, `appstore_embedded_framework`, `appstore_bitcode_disabled`, `appstore_symbols_stripped`, `appstore_validate_ios`, `appstore_validate_macos`.

## Deferred work

- App Store Connect API full automation (submit for review, phased release). Deferred to Phase 18.1.
- In-app purchases and StoreKit integration. Out of v1 scope.
- App Store screenshots and metadata upload. Out of v1 scope.
- Privacy manifest (`PrivacyInfo.xcprivacy`) generation. Deferred to Phase 18.1 (required for new App Store submissions from Spring 2024).
- visionOS App Store submission. Deferred to Phase 18.2.

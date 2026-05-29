# MEP-75 Phase 10: Packagist Publish Flow (GPG tag + Sigstore + Update API)

**Status**: LANDED 2026-05-30 00:00 (GMT+7)

## Goal

Implement the Packagist publish flow under `package3/php/publish`. The flow:
1. Validate `Config` for required fields
2. Create a GPG-signed git tag (`git tag -s v<version>`)
3. Push the tag to the remote
4. Ping the Packagist Update API to trigger a crawl
5. (Optional) Wait for Packagist to index the new version

## Design

### Config

- `PackagistName`: Composer vendor/package name
- `Version`: semver version (no `v` prefix)
- `RepoURL`: VCS repository URL
- `PackagistUsername` + `PackagistToken`: Packagist API credentials
- `GPGKeyID`: optional signing key ID (empty = default key)
- `Remote`: git remote name
- `NoVerify`: skip index verification wait
- `PackagistBaseURL`: override for testing

### Plan()

Returns the ordered `[]Step` for the publish flow. Each step has a `Name` and `Description` for progress output. The `verify` step is omitted when `NoVerify=true`.

### TagVersion() / PushTag()

Shell out to `git tag -s` and `git push` respectively. GPG signing uses the system keychain.

### PingUpdateAPI()

POST to `https://packagist.org/api/update-package?username=<user>&apiToken=<token>` with `{"repository": {"url": "<repo-url>"}}`. Returns error on non-200/202 response.

### WaitForIndex()

Polls `GET /packages/<vendor>/<package>.json` every 5 seconds until the version appears, or until the timeout elapses.

## Files Landed

- `package3/php/publish/publish.go` -- Validate, Plan, TagVersion, PushTag, PingUpdateAPI, WaitForIndex
- `package3/php/publish/publish_test.go` -- 13 test functions (HTTP mock server tests)

## Test Coverage

- Validate: OK, missing name/version/URL, v-prefix version, invalid name format
- Plan: step names present, NoVerify omits verify step
- PingUpdateAPI: success (200), error (401) with message
- WaitForIndex: success (version in response), timeout

---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import kotlin \"<group>:<artifact>@<version>\" as <alias>` import form, the `[kotlin-dependencies]` / `[kotlin]` / `[kotlin.publish]` / `[kotlin.capabilities]` manifest tables, the CLI subcommands, and the per-import alias resolution rule."
---

# 01. Language surface

This note covers every user-visible surface MEP-70 introduces: the import syntax, the manifest tables, and the CLI subcommands. The user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production gains `kotlin` as the fifth `<lang>` token:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust" | "kotlin"
```

The string literal is a Maven coordinate in one of these forms:

| Form | Example | Meaning |
|------|---------|---------|
| `<group>:<artifact>` | `"org.jetbrains.kotlinx:kotlinx-coroutines-core"` | Version from `[kotlin-dependencies]` or `mochi.lock`. |
| `<group>:<artifact>@<version>` | `"org.jetbrains.kotlinx:kotlinx-coroutines-core@1.7.3"` | Explicit version pin. |
| `<group>:<artifact>@<version>@<classifier>` | `"org.jetbrains.kotlin:kotlin-stdlib@1.9.23@sources"` | With classifier (rare). |

### Minimal example

```mochi
import kotlin "org.jetbrains.kotlinx:kotlinx-serialization-json@1.6.3" as json
import kotlin "com.squareup.okhttp3:okhttp@4.12.0" as okhttp

struct User {
    name: string
    age: int
}

fn fetch_user(url: string): User {
    let client = okhttp.OkHttpClient()
    let req = okhttp.Request.Builder().url(url).build()
    let resp = client.newCall(req).execute()
    let body = resp.body().string()
    return json.Json.decodeFromString(body)
}
```

### `auto` modifier

`import kotlin "..." as k auto` binds every public top-level declaration of the artifact at file scope rather than under the `k` namespace. The default is namespaced.

```mochi
import kotlin "org.jetbrains.kotlinx:kotlinx-datetime@0.5.0" as dt auto

fn now(): LocalDate {
    return LocalDate.Companion.todayIn(TimeZone.UTC)
}
```

### Transitive imports

The bridge resolves transitive dependencies automatically. If artifact A declares a dependency on B, and B's API surface appears in A's public types, B's types are available through A's shim without a separate `import kotlin "B"` statement. The bridge emits `extern type` declarations for transitive-only types with no callable functions (opaque handles), so the type system remains sound.

## Manifest: `[kotlin-dependencies]`

This table is the user-facing declaration of Kotlin/JVM artifacts. It mirrors Maven's `<dependency>` model:

```toml
[kotlin-dependencies]
"org.jetbrains.kotlinx:kotlinx-coroutines-core" = "1.7.3"
"org.jetbrains.kotlinx:kotlinx-serialization-json" = { version = "1.6.3" }
"io.ktor:ktor-client-core" = { version = "2.3.9" }
"com.squareup.okhttp3:okhttp" = {
    version = "4.12.0",
    exclude = ["com.squareup.okio:okio"]
}
"org.jetbrains.kotlin:kotlin-stdlib" = {
    version = "1.9.23",
    scope = "provided"
}
"com.example:my-local-lib" = { path = "../my-lib/build/libs/my-lib-1.0.jar" }
```

Available keys:

| Key | Type | Meaning |
|-----|------|---------|
| `version` | string | Maven version or range. A bare string is shorthand for `{ version = "..." }`. |
| `scope` | string | `"compile"` (default), `"provided"` (JVM present at runtime but not linked into the native image), `"test"` (excluded from production). |
| `exclude` | string[] | Transitive dependencies to drop (Maven `<exclusion>` equivalent). |
| `repository` | string | Source override: `"maven-central"` (default), `"jitpack"`, `"google"`, `"local"`, or a URL. |
| `path` | string | Local JAR path, relative to the manifest. Bypasses Maven resolution. |
| `classifier` | string | Maven classifier (e.g., `"jdk8"`, `"android"`). |

The user does not write a Gradle build file or `pom.xml`. The bridge synthesises everything needed at lock time.

## Manifest: `[kotlin]`

```toml
[kotlin]
kotlin-version = "1.9.23"
jvm-target = "21"
runtime = "graalvm"
graalvm-version = "21.0.2"
coroutines-dispatcher = "blocking"
monomorphise = [
    { item = "kotlinx.serialization.json.Json.decodeFromString", T = "User" },
    { item = "kotlin.collections.listOf", T = "kotlin.String" },
]
```

| Key | Default | Meaning |
|-----|---------|---------|
| `kotlin-version` | `"1.9.23"` | Kotlin stdlib version pinned in the wrapper. |
| `jvm-target` | `"17"` | JVM bytecode target (`"8"`, `"11"`, `"17"`, `"21"`). |
| `runtime` | `"graalvm"` | Bridge runtime: `"graalvm"` (GraalVM Native Image AOT) or `"jvm-embed"` (libjvm.so at runtime). |
| `graalvm-version` | `"21.0.2"` | GraalVM CE version. Validated against `native-image --version` at lock time. |
| `coroutines-dispatcher` | `"blocking"` | `suspend` function calling convention: `"blocking"` (blocks caller thread) or `"event-loop"` (returns a future handle). |
| `monomorphise` | `[]` | Explicit generic instantiations. Each entry binds one `<item>` at one `<T>`. Required for `inline reified` functions and unbounded generic parameters. |

## Manifest: `[kotlin.publish]`

```toml
[kotlin.publish]
group-id = "com.example"
artifact-id = "mylib"
version = "1.0.0"
description = "My Mochi library published as a Kotlin JAR"
url = "https://github.com/example/mylib"
licenses = [{ name = "Apache-2.0", url = "https://www.apache.org/licenses/LICENSE-2.0" }]
developers = [{ id = "alice", name = "Alice", email = "alice@example.com" }]
scm-connection = "scm:git:https://github.com/example/mylib.git"
publish-to = "maven-central"
signing-key-id = ""
include-sources = true
include-javadoc = true
android-aar = false
```

All POM-required fields (`group-id`, `artifact-id`, `version`, `description`, `url`, `licenses`, `developers`, `scm-connection`) are enforced at publish time. Maven Central rejects bundles that omit any required POM field.

## Manifest: `[kotlin.capabilities]`

```toml
[kotlin.capabilities]
net = true
fs = false
reflection = false
classloading = false
```

The bridge computes the capability union of all transitive dependencies and asserts it is a subset of this declaration. `reflection` and `classloading` are Kotlin-bridge-specific additions to MEP-57's `[capabilities]` table. Declaring `classloading = true` forces `runtime = "jvm-embed"` since GraalVM Native Image cannot support fully dynamic class loading.

## CLI surface

### `mochi pkg add kotlin <group>:<artifact>[@<version>]`

```
$ mochi pkg add kotlin org.jetbrains.kotlinx:kotlinx-coroutines-core@1.7.3
Added "org.jetbrains.kotlinx:kotlinx-coroutines-core" = "1.7.3" to [kotlin-dependencies]
Running mochi pkg lock ...
Resolved 12 Kotlin packages (kotlinx-coroutines-core + 11 transitive)
Fetched 12 JARs to ~/.cache/mochi/kotlin-deps/
Ingested kotlinx-metadata v9 surface: 847 functions, 234 types
Synthesized wrapper: kotlin_wrap/kotlinx-coroutines-core/
Compiled GraalVM native image: kotlin_wrap/kotlinx-coroutines-core/libwrap.so (42 MB)
Wrote mochi.lock (+12 [[kotlin-package]] entries)
```

### `mochi pkg lock`

Full resolution and compilation pass. Walks `[kotlin-dependencies]`, resolves the transitive graph against Maven Central metadata, fetches JARs to the blob cache, ingests Kotlin metadata from each JAR, type-maps the public surface, synthesises and compiles the GraalVM wrapper per artifact, emits `shim.mochi`, and writes `[[kotlin-package]]` entries.

### `mochi pkg lock --check`

Reads `mochi.lock`, recomputes every `jar-sha256`, `jar-blake3`, `metadata-sha256`, `wrapper-sha256`, and `native-image-sha256`, and exits non-zero on any mismatch. This is the CI-enforced reproducibility gate.

### `mochi pkg publish --to=maven-central [--dry-run]`

Builds `TargetKotlinLibrary`, assembles the Maven Central deployment bundle (classes JAR, sources JAR, Javadoc JAR, POM, GPG `.asc` signatures, SHA-1/MD5/SHA-256 checksums), obtains a Sonatype Central Portal OIDC token, POSTs the bundle to the Central Portal API, and polls until PUBLISHED or FAILED. `--dry-run` exercises the full signing and bundle-validation path without uploading.

### `mochi pkg sync kotlin`

Re-runs wrapper synthesis and GraalVM native-image compilation from the existing `mochi.lock` (does not re-resolve versions). Used after a bridge upgrade that changes the wrapper format.

### `mochi pkg info kotlin <group>:<artifact>`

Queries Maven Central metadata and prints the available versions, the Kotlin metadata schema version, and the mapped API surface summary (function count, type count, suspended count, generic count).

## Per-import alias resolution

The alias `<alias>` introduced by `import kotlin "<coord>" as <alias>` participates in normal Mochi name resolution. The bridge generates a shim file at `<workdir>/kotlin_wrap/<artifact>/shim.mochi`:

```mochi
// kotlin_wrap/okhttp/shim.mochi (generated)
extern type OkHttpClient
extern type Request
extern type Response
extern type ResponseBody
extern fn okhttp_client_new(): OkHttpClient from kotlin "com.squareup.okhttp3.OkHttpClient"
extern fn okhttp_request_builder_new(): RequestBuilder from kotlin "com.squareup.okhttp3.Request.Builder"
extern fn okhttp_request_builder_url(b: RequestBuilder, url: string): RequestBuilder from kotlin "com.squareup.okhttp3.Request.Builder.url"
extern fn okhttp_request_builder_build(b: RequestBuilder): Request from kotlin "com.squareup.okhttp3.Request.Builder.build"
extern fn okhttp_client_new_call(c: OkHttpClient, req: Request): Call from kotlin "com.squareup.okhttp3.OkHttpClient.newCall"
extern fn okhttp_call_execute(call: Call): Response from kotlin "com.squareup.okhttp3.Call.execute"
extern fn okhttp_response_body(resp: Response): ResponseBody from kotlin "com.squareup.okhttp3.Response.body"
extern fn okhttp_response_body_string(body: ResponseBody): string from kotlin "com.squareup.okhttp3.ResponseBody.string"
```

`import kotlin "com.squareup.okhttp3:okhttp" as okhttp` becomes `import "./kotlin_wrap/okhttp/shim.mochi" as okhttp` post-resolution. The shim is regenerated on every `mochi pkg lock` and is gitignored by default.

## Cross-references

- [[02-design-philosophy]] for the rationale behind each surface decision.
- [[04-kotlin-metadata-ingest]] for how the public surface is discovered.
- [[05-type-mapping]] for the closed translation table the shim file uses.
- [[06-maven-central-publish]] for the `mochi pkg publish` path.
- [MEP-70 §4](/docs/mep/mep-0070#4-surface-syntax-import-kotlin) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the broader `mochi.toml` + `mochi.lock` model this extends.

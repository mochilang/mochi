# package3/java -- MEP-67 Java bridge

This package implements the bidirectional Java bridge for Mochi (MEP-67).

## Two directions

**Consume**: import Java libraries into Mochi.

```mochi
import java "com.google.guava:guava@33.4.8-jre" as guava
```

**Publish**: ship Mochi packages as Maven artifacts.

```
mochi pkg publish --to=maven-central
```

## Sub-packages

| Package | Description |
|---------|-------------|
| `errors` | Cross-cutting error types and skip-reason enum |
| `maven` | Maven Central HTTP client, coordinate parsing, JAR cache |
| `reflect` | Java reflection tool (runs a bundled JAR to extract class surfaces) |
| `typemap` | Java-to-Mochi type mapping table |
| `wrapper` | JNI wrapper class synthesiser and Java source emitter |
| `emit` | Mochi extern shim emitter and skip-report renderer |
| `jni` | JNI embedding (CGO, gated by build tag `java_jni`) |
| `build` | End-to-end build orchestration: Driver, Pipeline, javac, POM |
| `lock` | `mochi.lock` TOML integration for `[[java-package]]` tables |
| `publish` | Maven Central publish flow and Sigstore attestation |

## Spec reference

See `website/docs/mep/mep-0067.md` for the full MEP-67 specification.

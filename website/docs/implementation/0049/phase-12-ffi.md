---
title: "Phase 12. FFI (file I/O)"
sidebar_position: 16
sidebar_label: "Phase 12. FFI"
description: "MEP-49 Phase 12 — file I/O FFI: mochiReadFile, mochiWriteFile, mochiAppendFile, mochiLines via Foundation; ReadFileExpr/WriteFileStmt/AppendFileStmt/LinesExpr lowering."
---

# Phase 12. FFI (file I/O)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 12](/docs/mep/mep-0049#phase-12-ffi) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase12FFI`: 5 fixtures green on Swift 6.0+, macOS 15. Gate builds each fixture and compares stdout to `.expected`.

## Goal-alignment audit

Phase 12 ships the file I/O primitives that are the simplest and most universally needed FFI operations. `mochiReadFile`, `mochiWriteFile`, `mochiAppendFile`, and `mochiLines` cover the fixture suite. These are implemented as plain synchronous Foundation wrappers, keeping the generated code simple while matching the behaviour of the BEAM and C backends. C FFI (module maps, `@_silgen_name`, unsafe pointers) and Swift library FFI are deferred.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | `ReadFileExpr` → `mochiReadFile(path)` | LANDED | mep/0049-phase-12 |
| 12.1 | `WriteFileStmt` → `mochiWriteFile(path, content)` | LANDED | mep/0049-phase-12 |
| 12.2 | `AppendFileStmt` → `mochiAppendFile(path, content)` | LANDED | mep/0049-phase-12 |
| 12.3 | `LinesExpr` → `mochiLines(path)` | LANDED | mep/0049-phase-12 |
| 12.4 | C FFI: `module.modulemap`, `@_silgen_name`, `ClangImporter` | DEFERRED | — |
| 12.5 | Swift library FFI: `external module` → SwiftPM `.package` dependency | DEFERRED | — |
| 12.6 | Export to C: `@export fun foo()` → `@_cdecl("foo")` | DEFERRED | — |

## Sub-phase 12.0 -- Read file

### Decisions made (12.0)

**`mochiReadFile(_ path: String) -> String`**: reads the entire file at `path`, returning its content as a UTF-8 string. Returns `""` on error.

```swift
public func mochiReadFile(_ path: String) -> String {
    (try? String(contentsOfFile: path, encoding: .utf8)) ?? ""
}
```

The lowerer emits `mochiReadFile(pathExpr)` as a `RawSwiftExpr`.

## Sub-phase 12.1 -- Write file

### Decisions made (12.1)

**`mochiWriteFile(_ path: String, _ content: String)`**: writes `content` atomically to `path`, overwriting any existing file.

```swift
public func mochiWriteFile(_ path: String, _ content: String) {
    try? content.write(toFile: path, atomically: true, encoding: .utf8)
}
```

The lowerer emits `mochiWriteFile(path, content)` as a `RawSwiftStmt`.

## Sub-phase 12.2 -- Append file

### Decisions made (12.2)

**`mochiAppendFile(_ path: String, _ content: String)`**: opens the file for appending (creates it if it does not exist).

```swift
public func mochiAppendFile(_ path: String, _ content: String) {
    if let data = content.data(using: .utf8) {
        if FileManager.default.fileExists(atPath: path) {
            if let fh = FileHandle(forWritingAtPath: path) {
                fh.seekToEndOfFile()
                fh.write(data)
                fh.closeFile()
            }
        } else {
            try? content.write(toFile: path, atomically: true, encoding: .utf8)
        }
    }
}
```

## Sub-phase 12.3 -- Lines

### Decisions made (12.3)

**`mochiLines(_ path: String) -> [String]`**: reads the file and splits by `\n`. Strips the trailing empty element left by files ending with a newline.

```swift
public func mochiLines(_ path: String) -> [String] {
    guard let content = try? String(contentsOfFile: path, encoding: .utf8) else { return [] }
    let lines = content.components(separatedBy: "\n")
    if lines.last == "" { return Array(lines.dropLast()) }
    return lines
}
```

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `ReadFileExpr`, `WriteFileStmt`, `AppendFileStmt`, `LinesExpr` lowering |
| `transpiler3/swift/runtime/Sources/MochiRuntime/FileIO.swift` | `mochiReadFile`, `mochiWriteFile`, `mochiAppendFile`, `mochiLines` |
| `transpiler3/swift/build/phase12_test.go` | `TestPhase12FFI`: 5 fixtures |
| `tests/transpiler3/swift/fixtures/phase12-ffi/` | 5 fixture directories |

## Test set

- `TestPhase12FFI` -- 5 fixtures: `ffi_append`, `ffi_lines`, `ffi_newlines`, `ffi_overwrite`, `ffi_write_read`.

## Deferred work

- C FFI: `module.modulemap`, `@_silgen_name`, `ClangImporter`, unsafe pointer bridging. Deferred to Phase 12.4.
- Swift library FFI: `external module "url"` → SwiftPM `.package` + `.product` dependency. Deferred to Phase 12.5.
- Export to C: `@export fun foo()` → `@_cdecl("mochi_foo")` + generated `MochiExports.h`. Deferred to Phase 12.6.
- Objective-C bridging header. Deferred.
- `UnsafePointer<T>`, `UnsafeMutablePointer<T>`, `UnsafeRawPointer`. Deferred.

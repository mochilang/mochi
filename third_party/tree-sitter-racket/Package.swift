// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterRacket",
    products: [
        .library(name: "TreeSitterRacket", targets: ["TreeSitterRacket"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterRacket",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterRacketTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterRacket",
            ],
            path: "bindings/swift/TreeSitterRacketTests"
        )
    ],
    cLanguageStandard: .c11
)

// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "MochiRuntime",
    platforms: [.macOS(.v13)],
    products: [
        .library(name: "MochiRuntime", targets: ["MochiRuntime"]),
    ],
    targets: [
        .target(
            name: "MochiRuntime",
            path: "Sources/MochiRuntime"
        ),
    ]
)

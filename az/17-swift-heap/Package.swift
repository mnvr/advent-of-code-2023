// swift-tools-version: 5.9

import PackageDescription

let package = Package(
    name: "17",
    dependencies: [
      .package(url: "https://github.com/apple/swift-collections", branch: "release/1.1"),
    ],
    targets: [
        .executableTarget(
            name: "17",
            dependencies: [
                .product(name: "Collections", package: "swift-collections"),
            ]),
    ]
)

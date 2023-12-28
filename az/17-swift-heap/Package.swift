// swift-tools-version: 5.9

import PackageDescription

let package = Package(
    name: "17",
    dependencies: [
      .package(url: "https://github.com/apple/swift-collections", branch: "release/1.1"),
    ],
    targets: [
        // Targets are the basic building blocks of a package, defining a module or a test suite.
        // Targets can depend on other targets in this package and products from dependencies.
        .executableTarget(
            name: "17",
            dependencies: [
                .product(name: "Collections", package: "swift-collections"),
            ]),
    ]
)

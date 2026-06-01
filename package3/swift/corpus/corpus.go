// Package corpus defines the 24-package fixture corpus gate for the Swift bridge.
// Each entry describes one well-known Swift package and its expected bridge output.
package corpus

// Package describes one entry in the fixture corpus.
type Package struct {
	// Name is the package name.
	Name string
	// URL is the git repository URL.
	URL string
	// Version is the version to test against.
	Version string
	// MinFunctions is the minimum number of bridged functions expected.
	MinFunctions int
	// MaxSkips is the maximum allowed skip count (0 = no skips expected).
	MaxSkips int
}

// Packages is the 24-package fixture corpus defined in MEP-69 Phase 16.
var Packages = []Package{
	{Name: "swift-argument-parser", URL: "https://github.com/apple/swift-argument-parser", Version: "1.3.0", MinFunctions: 5, MaxSkips: 20},
	{Name: "Alamofire", URL: "https://github.com/Alamofire/Alamofire", Version: "5.9.0", MinFunctions: 10, MaxSkips: 50},
	{Name: "SwiftyJSON", URL: "https://github.com/SwiftyJSON/SwiftyJSON", Version: "5.0.2", MinFunctions: 5, MaxSkips: 10},
	{Name: "RxSwift", URL: "https://github.com/ReactiveX/RxSwift", Version: "6.7.0", MinFunctions: 5, MaxSkips: 100},
	{Name: "SnapKit", URL: "https://github.com/SnapKit/SnapKit", Version: "5.7.1", MinFunctions: 5, MaxSkips: 50},
	{Name: "Moya", URL: "https://github.com/Moya/Moya", Version: "15.0.3", MinFunctions: 5, MaxSkips: 30},
	{Name: "Kingfisher", URL: "https://github.com/onevcat/Kingfisher", Version: "7.11.0", MinFunctions: 5, MaxSkips: 50},
	{Name: "Then", URL: "https://github.com/devxoul/Then", Version: "3.0.0", MinFunctions: 2, MaxSkips: 5},
	{Name: "ObjectMapper", URL: "https://github.com/tristanhimmelman/ObjectMapper", Version: "4.2.0", MinFunctions: 5, MaxSkips: 20},
	{Name: "PromiseKit", URL: "https://github.com/mxcl/PromiseKit", Version: "6.22.1", MinFunctions: 5, MaxSkips: 30},
	{Name: "RealmSwift", URL: "https://github.com/realm/realm-swift", Version: "10.49.0", MinFunctions: 10, MaxSkips: 200},
	{Name: "Charts", URL: "https://github.com/ChartsOrg/Charts", Version: "5.0.0", MinFunctions: 5, MaxSkips: 100},
	{Name: "CryptoSwift", URL: "https://github.com/krzyzanowskim/CryptoSwift", Version: "1.8.2", MinFunctions: 10, MaxSkips: 20},
	{Name: "SwiftLint", URL: "https://github.com/realm/SwiftLint", Version: "0.55.0", MinFunctions: 2, MaxSkips: 100},
	{Name: "swift-log", URL: "https://github.com/apple/swift-log", Version: "1.6.1", MinFunctions: 5, MaxSkips: 20},
	{Name: "swift-nio", URL: "https://github.com/apple/swift-nio", Version: "2.67.0", MinFunctions: 10, MaxSkips: 200},
	{Name: "vapor", URL: "https://github.com/vapor/vapor", Version: "4.99.3", MinFunctions: 10, MaxSkips: 300},
	{Name: "leaf", URL: "https://github.com/vapor/leaf", Version: "4.3.0", MinFunctions: 5, MaxSkips: 50},
	{Name: "jwt-kit", URL: "https://github.com/vapor/jwt-kit", Version: "5.0.0", MinFunctions: 5, MaxSkips: 30},
	{Name: "postgres-nio", URL: "https://github.com/vapor/postgres-nio", Version: "1.21.0", MinFunctions: 5, MaxSkips: 100},
	{Name: "fluent", URL: "https://github.com/vapor/fluent", Version: "4.9.0", MinFunctions: 5, MaxSkips: 50},
	{Name: "fluent-postgres-driver", URL: "https://github.com/vapor/fluent-postgres-driver", Version: "2.9.2", MinFunctions: 5, MaxSkips: 50},
	{Name: "async-http-client", URL: "https://github.com/swift-server/async-http-client", Version: "1.21.2", MinFunctions: 5, MaxSkips: 100},
	{Name: "multipart-kit", URL: "https://github.com/vapor/multipart-kit", Version: "4.6.0", MinFunctions: 5, MaxSkips: 20},
}

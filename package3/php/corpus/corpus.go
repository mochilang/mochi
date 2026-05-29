// Package corpus defines the 24-package fixture surfaces used to gate
// MEP-75 Phase 14. Each fixture is a hand-authored ReflectionSurface that
// captures the key PHP patterns found in the named Composer package without
// requiring a live PHP installation or network access.
//
// The fixture corpus is drawn from the April 2026 top Packagist download
// rankings. Each surface exercises the type-mapping table, extern emitter,
// glue emitter, and mochi.lock round-trip for that package's characteristic
// patterns.
//
// Corpus packages (24):
//  1. guzzlehttp/guzzle         -- HTTP client, async promise returns
//  2. symfony/console           -- CLI command framework
//  3. symfony/http-foundation   -- HTTP request/response
//  4. laravel/framework         -- application container
//  5. phpunit/phpunit           -- test framework
//  6. monolog/monolog           -- PSR-3 logging
//  7. doctrine/orm              -- ORM / entity manager
//  8. psr/log                   -- PSR-3 interface
//  9. nesbot/carbon             -- datetime extension
// 10. vlucas/phpdotenv          -- env loading
// 11. league/flysystem          -- filesystem abstraction
// 12. paragonie/random_compat   -- secure random
// 13. ramsey/uuid               -- UUID generation
// 14. bacon/bacon-qr-code       -- QR code rendering
// 15. spatie/laravel-permission -- RBAC
// 16. barryvdh/laravel-debugbar -- debug toolbar
// 17. composer/composer         -- package manager API
// 18. phpmailer/phpmailer       -- email
// 19. symfony/mailer            -- Symfony mailer
// 20. league/oauth2-server      -- OAuth2 server
// 21. firebase/php-jwt          -- JWT
// 22. socialiteproviders/google -- OAuth2 social login
// 23. stripe/stripe-php         -- Stripe SDK
// 24. pestphp/pest              -- next-gen test runner
package corpus

import "mochi/package3/php/reflect"

// Fixture is a named corpus entry.
type Fixture struct {
	// Name is the Composer package name, e.g. "guzzlehttp/guzzle".
	Name string
	// Surface is the representative ReflectionSurface fixture.
	Surface *reflect.ReflectionSurface
}

// All returns all 24 fixture entries.
func All() []Fixture {
	return []Fixture{
		guzzlehttpGuzzle(),
		symfonyConsole(),
		symfonyHTTPFoundation(),
		laravelFramework(),
		phpunitPHPUnit(),
		monologMonolog(),
		doctrineORM(),
		psrLog(),
		nesbotCarbon(),
		vlucasPhpdotenv(),
		leagueFlysystem(),
		paragonieRandomCompat(),
		ramseyUUID(),
		baconBaconQRCode(),
		spatieLaravelPermission(),
		barryvdhLaravelDebugbar(),
		composerComposer(),
		phpmailerPHPMailer(),
		symfonyMailer(),
		leagueOAuth2Server(),
		firebasePHPJWT(),
		socialiteProvidersGoogle(),
		stripeStripePHP(),
		pestphpPest(),
	}
}

func guzzlehttpGuzzle() Fixture {
	return Fixture{
		Name: "guzzlehttp/guzzle",
		Surface: &reflect.ReflectionSurface{
			PackageName: "guzzlehttp/guzzle",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `GuzzleHttp\Client`,
					Methods: []reflect.MethodSurface{
						{Name: "send", ReturnType: `Psr\Http\Message\ResponseInterface`},
						{Name: "sendAsync", ReturnType: `GuzzleHttp\Promise\PromiseInterface`},
						{Name: "request", ReturnType: `Psr\Http\Message\ResponseInterface`, Parameters: []reflect.ParameterSurface{
							{Name: "method", Type: "string"},
							{Name: "uri", Type: "string"},
						}},
						{Name: "getConfig", ReturnType: "mixed"},
					},
				},
			},
			Interfaces: []reflect.InterfaceSurface{
				{
					FQCN: `GuzzleHttp\ClientInterface`,
					Methods: []reflect.MethodSurface{
						{Name: "send", ReturnType: `Psr\Http\Message\ResponseInterface`},
						{Name: "sendAsync", ReturnType: `GuzzleHttp\Promise\PromiseInterface`},
					},
				},
			},
		},
	}
}

func symfonyConsole() Fixture {
	return Fixture{
		Name: "symfony/console",
		Surface: &reflect.ReflectionSurface{
			PackageName: "symfony/console",
			Classes: []reflect.ClassSurface{
				{
					FQCN:     `Symfony\Component\Console\Command\Command`,
					Abstract: true,
					Methods: []reflect.MethodSurface{
						{Name: "execute", ReturnType: "int", Parameters: []reflect.ParameterSurface{
							{Name: "input", Type: `Symfony\Component\Console\Input\InputInterface`},
							{Name: "output", Type: `Symfony\Component\Console\Output\OutputInterface`},
						}},
						{Name: "getName", ReturnType: "string"},
						{Name: "setName", ReturnType: `Symfony\Component\Console\Command\Command`, Parameters: []reflect.ParameterSurface{
							{Name: "name", Type: "string"},
						}},
					},
				},
				{
					FQCN: `Symfony\Component\Console\Application`,
					Methods: []reflect.MethodSurface{
						{Name: "run", ReturnType: "int"},
						{Name: "add", ReturnType: `Symfony\Component\Console\Command\Command`},
					},
				},
			},
		},
	}
}

func symfonyHTTPFoundation() Fixture {
	return Fixture{
		Name: "symfony/http-foundation",
		Surface: &reflect.ReflectionSurface{
			PackageName: "symfony/http-foundation",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Symfony\Component\HttpFoundation\Request`,
					Methods: []reflect.MethodSurface{
						{Name: "getMethod", ReturnType: "string"},
						{Name: "getUri", ReturnType: "string"},
						{Name: "getContent", ReturnType: "string"},
						{Name: "isMethod", ReturnType: "bool", Parameters: []reflect.ParameterSurface{
							{Name: "method", Type: "string"},
						}},
					},
				},
				{
					FQCN: `Symfony\Component\HttpFoundation\Response`,
					Methods: []reflect.MethodSurface{
						{Name: "getStatusCode", ReturnType: "int"},
						{Name: "getContent", ReturnType: "string"},
						{Name: "send", ReturnType: `Symfony\Component\HttpFoundation\Response`},
					},
				},
			},
		},
	}
}

func laravelFramework() Fixture {
	return Fixture{
		Name: "laravel/framework",
		Surface: &reflect.ReflectionSurface{
			PackageName: "laravel/framework",
			Classes: []reflect.ClassSurface{
				{
					FQCN:     `Illuminate\Foundation\Application`,
					Abstract: false,
					Methods: []reflect.MethodSurface{
						{Name: "make", ReturnType: "mixed", Parameters: []reflect.ParameterSurface{
							{Name: "abstract", Type: "string"},
						}},
						{Name: "bind", ReturnType: "void"},
						{Name: "singleton", ReturnType: "void"},
						{Name: "version", ReturnType: "string"},
					},
				},
			},
		},
	}
}

func phpunitPHPUnit() Fixture {
	return Fixture{
		Name: "phpunit/phpunit",
		Surface: &reflect.ReflectionSurface{
			PackageName: "phpunit/phpunit",
			Classes: []reflect.ClassSurface{
				{
					FQCN:     `PHPUnit\Framework\TestCase`,
					Abstract: true,
					Methods: []reflect.MethodSurface{
						{Name: "assertEquals", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "expected", Type: "mixed"},
							{Name: "actual", Type: "mixed"},
						}},
						{Name: "assertTrue", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "condition", Type: "bool"},
						}},
						{Name: "assertFalse", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "condition", Type: "bool"},
						}},
						{Name: "setUp", ReturnType: "void"},
						{Name: "tearDown", ReturnType: "void"},
					},
				},
			},
		},
	}
}

func monologMonolog() Fixture {
	return Fixture{
		Name: "monolog/monolog",
		Surface: &reflect.ReflectionSurface{
			PackageName: "monolog/monolog",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Monolog\Logger`,
					InterfaceFQCNs: []string{`Psr\Log\LoggerInterface`},
					Methods: []reflect.MethodSurface{
						{Name: "info", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "message", Type: "string"},
						}},
						{Name: "error", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "message", Type: "string"},
						}},
						{Name: "debug", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "message", Type: "string"},
						}},
						{Name: "pushHandler", ReturnType: `Monolog\Logger`},
					},
				},
			},
		},
	}
}

func doctrineORM() Fixture {
	return Fixture{
		Name: "doctrine/orm",
		Surface: &reflect.ReflectionSurface{
			PackageName: "doctrine/orm",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Doctrine\ORM\EntityManager`,
					InterfaceFQCNs: []string{`Doctrine\ORM\EntityManagerInterface`},
					Methods: []reflect.MethodSurface{
						{Name: "persist", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "object", Type: "object"},
						}},
						{Name: "flush", ReturnType: "void"},
						{Name: "find", ReturnType: "object", Parameters: []reflect.ParameterSurface{
							{Name: "className", Type: "string"},
							{Name: "id", Type: "mixed"},
						}},
						{Name: "remove", ReturnType: "void"},
						{Name: "beginTransaction", ReturnType: "void"},
						{Name: "commit", ReturnType: "void"},
						{Name: "rollback", ReturnType: "void"},
					},
				},
			},
			Interfaces: []reflect.InterfaceSurface{
				{
					FQCN: `Doctrine\ORM\EntityManagerInterface`,
					Methods: []reflect.MethodSurface{
						{Name: "persist", ReturnType: "void"},
						{Name: "flush", ReturnType: "void"},
					},
				},
			},
		},
	}
}

func psrLog() Fixture {
	return Fixture{
		Name: "psr/log",
		Surface: &reflect.ReflectionSurface{
			PackageName: "psr/log",
			Interfaces: []reflect.InterfaceSurface{
				{
					FQCN: `Psr\Log\LoggerInterface`,
					Methods: []reflect.MethodSurface{
						{Name: "emergency", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "message", Type: "string"}}},
						{Name: "alert", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "message", Type: "string"}}},
						{Name: "critical", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "message", Type: "string"}}},
						{Name: "error", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "message", Type: "string"}}},
						{Name: "warning", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "message", Type: "string"}}},
						{Name: "info", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "message", Type: "string"}}},
						{Name: "debug", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "message", Type: "string"}}},
						{Name: "log", ReturnType: "void"},
					},
				},
			},
		},
	}
}

func nesbotCarbon() Fixture {
	return Fixture{
		Name: "nesbot/carbon",
		Surface: &reflect.ReflectionSurface{
			PackageName: "nesbot/carbon",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Carbon\Carbon`,
					Methods: []reflect.MethodSurface{
						{Name: "now", ReturnType: `Carbon\Carbon`, Static: true},
						{Name: "parse", ReturnType: `Carbon\Carbon`, Static: true, Parameters: []reflect.ParameterSurface{
							{Name: "time", Type: "string"},
						}},
						{Name: "format", ReturnType: "string", Parameters: []reflect.ParameterSurface{
							{Name: "format", Type: "string"},
						}},
						{Name: "diffInDays", ReturnType: "int", Parameters: []reflect.ParameterSurface{
							{Name: "dt", Type: `Carbon\Carbon`},
						}},
						{Name: "toDateString", ReturnType: "string"},
						{Name: "toIso8601String", ReturnType: "string"},
					},
				},
			},
		},
	}
}

func vlucasPhpdotenv() Fixture {
	return Fixture{
		Name: "vlucas/phpdotenv",
		Surface: &reflect.ReflectionSurface{
			PackageName: "vlucas/phpdotenv",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Dotenv\Dotenv`,
					Methods: []reflect.MethodSurface{
						{Name: "createImmutable", ReturnType: `Dotenv\Dotenv`, Static: true, Parameters: []reflect.ParameterSurface{
							{Name: "paths", Type: "string"},
						}},
						{Name: "load", ReturnType: "void"},
						{Name: "safeLoad", ReturnType: "void"},
					},
				},
			},
		},
	}
}

func leagueFlysystem() Fixture {
	return Fixture{
		Name: "league/flysystem",
		Surface: &reflect.ReflectionSurface{
			PackageName: "league/flysystem",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `League\Flysystem\Filesystem`,
					InterfaceFQCNs: []string{`League\Flysystem\FilesystemOperator`},
					Methods: []reflect.MethodSurface{
						{Name: "read", ReturnType: "string", Parameters: []reflect.ParameterSurface{{Name: "path", Type: "string"}}},
						{Name: "write", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "path", Type: "string"},
							{Name: "contents", Type: "string"},
						}},
						{Name: "delete", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "path", Type: "string"}}},
						{Name: "fileExists", ReturnType: "bool", Parameters: []reflect.ParameterSurface{{Name: "path", Type: "string"}}},
					},
				},
			},
			Interfaces: []reflect.InterfaceSurface{
				{
					FQCN: `League\Flysystem\FilesystemOperator`,
					Methods: []reflect.MethodSurface{
						{Name: "read", ReturnType: "string"},
						{Name: "write", ReturnType: "void"},
					},
				},
			},
		},
	}
}

func paragonieRandomCompat() Fixture {
	return Fixture{
		Name: "paragonie/random_compat",
		Surface: &reflect.ReflectionSurface{
			PackageName: "paragonie/random_compat",
			Functions: []reflect.FunctionSurface{
				{Name: "random_bytes", ReturnType: "string", Parameters: []reflect.ParameterSurface{{Name: "length", Type: "int"}}},
				{Name: "random_int", ReturnType: "int", Parameters: []reflect.ParameterSurface{
					{Name: "min", Type: "int"},
					{Name: "max", Type: "int"},
				}},
			},
		},
	}
}

func ramseyUUID() Fixture {
	return Fixture{
		Name: "ramsey/uuid",
		Surface: &reflect.ReflectionSurface{
			PackageName: "ramsey/uuid",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Ramsey\Uuid\Uuid`,
					Methods: []reflect.MethodSurface{
						{Name: "uuid4", ReturnType: `Ramsey\Uuid\UuidInterface`, Static: true},
						{Name: "uuid7", ReturnType: `Ramsey\Uuid\UuidInterface`, Static: true},
						{Name: "toString", ReturnType: "string"},
					},
				},
			},
			Interfaces: []reflect.InterfaceSurface{
				{
					FQCN: `Ramsey\Uuid\UuidInterface`,
					Methods: []reflect.MethodSurface{
						{Name: "toString", ReturnType: "string"},
						{Name: "getBytes", ReturnType: "string"},
					},
				},
			},
		},
	}
}

func baconBaconQRCode() Fixture {
	return Fixture{
		Name: "bacon/bacon-qr-code",
		Surface: &reflect.ReflectionSurface{
			PackageName: "bacon/bacon-qr-code",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `BaconQrCode\Writer`,
					Methods: []reflect.MethodSurface{
						{Name: "writeString", ReturnType: "string", Parameters: []reflect.ParameterSurface{{Name: "text", Type: "string"}}},
						{Name: "writeFile", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "text", Type: "string"},
							{Name: "filename", Type: "string"},
						}},
					},
				},
			},
		},
	}
}

func spatieLaravelPermission() Fixture {
	return Fixture{
		Name: "spatie/laravel-permission",
		Surface: &reflect.ReflectionSurface{
			PackageName: "spatie/laravel-permission",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Spatie\Permission\Models\Role`,
					Methods: []reflect.MethodSurface{
						{Name: "findByName", ReturnType: `Spatie\Permission\Models\Role`, Static: true, Parameters: []reflect.ParameterSurface{{Name: "name", Type: "string"}}},
						{Name: "givePermissionTo", ReturnType: `Spatie\Permission\Models\Role`},
					},
				},
			},
		},
	}
}

func barryvdhLaravelDebugbar() Fixture {
	return Fixture{
		Name: "barryvdh/laravel-debugbar",
		Surface: &reflect.ReflectionSurface{
			PackageName: "barryvdh/laravel-debugbar",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Barryvdh\Debugbar\LaravelDebugbar`,
					Methods: []reflect.MethodSurface{
						{Name: "enable", ReturnType: "void"},
						{Name: "disable", ReturnType: "void"},
						{Name: "isEnabled", ReturnType: "bool"},
						{Name: "addMessage", ReturnType: "void", Parameters: []reflect.ParameterSurface{{Name: "message", Type: "mixed"}}},
					},
				},
			},
		},
	}
}

func composerComposer() Fixture {
	return Fixture{
		Name: "composer/composer",
		Surface: &reflect.ReflectionSurface{
			PackageName: "composer/composer",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Composer\Factory`,
					Methods: []reflect.MethodSurface{
						{Name: "create", ReturnType: `Composer\Composer`, Static: true, Parameters: []reflect.ParameterSurface{
							{Name: "config", Type: `Composer\IO\IOInterface`},
						}},
					},
				},
				{
					FQCN: `Composer\Composer`,
					Methods: []reflect.MethodSurface{
						{Name: "getPackage", ReturnType: `Composer\Package\RootPackageInterface`},
						{Name: "getLocker", ReturnType: `Composer\Package\Locker`},
					},
				},
			},
		},
	}
}

func phpmailerPHPMailer() Fixture {
	return Fixture{
		Name: "phpmailer/phpmailer",
		Surface: &reflect.ReflectionSurface{
			PackageName: "phpmailer/phpmailer",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `PHPMailer\PHPMailer\PHPMailer`,
					Methods: []reflect.MethodSurface{
						{Name: "addAddress", ReturnType: "bool", Parameters: []reflect.ParameterSurface{
							{Name: "address", Type: "string"},
							{Name: "name", Type: "string"},
						}},
						{Name: "send", ReturnType: "bool"},
						{Name: "setFrom", ReturnType: "bool", Parameters: []reflect.ParameterSurface{
							{Name: "address", Type: "string"},
						}},
						{Name: "isSMTP", ReturnType: "void"},
					},
				},
			},
		},
	}
}

func symfonyMailer() Fixture {
	return Fixture{
		Name: "symfony/mailer",
		Surface: &reflect.ReflectionSurface{
			PackageName: "symfony/mailer",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Symfony\Component\Mailer\Mailer`,
					Methods: []reflect.MethodSurface{
						{Name: "send", ReturnType: "void", Parameters: []reflect.ParameterSurface{
							{Name: "message", Type: `Symfony\Component\Mime\RawMessage`},
						}},
					},
				},
			},
			Interfaces: []reflect.InterfaceSurface{
				{
					FQCN: `Symfony\Component\Mailer\MailerInterface`,
					Methods: []reflect.MethodSurface{
						{Name: "send", ReturnType: "void"},
					},
				},
			},
		},
	}
}

func leagueOAuth2Server() Fixture {
	return Fixture{
		Name: "league/oauth2-server",
		Surface: &reflect.ReflectionSurface{
			PackageName: "league/oauth2-server",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `League\OAuth2\Server\AuthorizationServer`,
					Methods: []reflect.MethodSurface{
						{Name: "respondToAccessTokenRequest", ReturnType: `Psr\Http\Message\ResponseInterface`},
						{Name: "enableGrantType", ReturnType: "void"},
					},
				},
			},
		},
	}
}

func firebasePHPJWT() Fixture {
	return Fixture{
		Name: "firebase/php-jwt",
		Surface: &reflect.ReflectionSurface{
			PackageName: "firebase/php-jwt",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Firebase\JWT\JWT`,
					Methods: []reflect.MethodSurface{
						{Name: "encode", ReturnType: "string", Static: true, Parameters: []reflect.ParameterSurface{
							{Name: "payload", Type: "array"},
							{Name: "key", Type: "string"},
							{Name: "alg", Type: "string"},
						}},
						{Name: "decode", ReturnType: "object", Static: true, Parameters: []reflect.ParameterSurface{
							{Name: "jwt", Type: "string"},
							{Name: "keyOrKeyArray", Type: `Firebase\JWT\Key`},
						}},
					},
				},
			},
		},
	}
}

func socialiteProvidersGoogle() Fixture {
	return Fixture{
		Name: "socialiteproviders/google",
		Surface: &reflect.ReflectionSurface{
			PackageName: "socialiteproviders/google",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `SocialiteProviders\Google\Provider`,
					Methods: []reflect.MethodSurface{
						{Name: "user", ReturnType: `Laravel\Socialite\Contracts\User`},
						{Name: "redirect", ReturnType: `Symfony\Component\HttpFoundation\RedirectResponse`},
					},
				},
			},
		},
	}
}

func stripeStripePHP() Fixture {
	return Fixture{
		Name: "stripe/stripe-php",
		Surface: &reflect.ReflectionSurface{
			PackageName: "stripe/stripe-php",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Stripe\StripeClient`,
					Methods: []reflect.MethodSurface{
						{Name: "__construct", Parameters: []reflect.ParameterSurface{{Name: "config", Type: "string"}}},
					},
				},
				{
					FQCN: `Stripe\PaymentIntent`,
					Methods: []reflect.MethodSurface{
						{Name: "create", ReturnType: `Stripe\PaymentIntent`, Static: true, Parameters: []reflect.ParameterSurface{
							{Name: "params", Type: "array"},
						}},
						{Name: "confirm", ReturnType: `Stripe\PaymentIntent`},
						{Name: "cancel", ReturnType: `Stripe\PaymentIntent`},
					},
				},
			},
		},
	}
}

func pestphpPest() Fixture {
	return Fixture{
		Name: "pestphp/pest",
		Surface: &reflect.ReflectionSurface{
			PackageName: "pestphp/pest",
			Classes: []reflect.ClassSurface{
				{
					FQCN: `Pest\TestSuite`,
					Methods: []reflect.MethodSurface{
						{Name: "getInstance", ReturnType: `Pest\TestSuite`, Static: true},
						{Name: "run", ReturnType: "int"},
					},
				},
			},
		},
	}
}

package reflect

// reflectPHPScript is the PHP CLI script invoked by Run to extract the
// reflection surface of a Composer package. It scans all .php files under the
// package directory, uses PHP's Reflection API to enumerate public classes,
// interfaces, enums (PHP 8.1+), and top-level functions, then emits a single
// ReflectionSurface JSON object on stdout.
//
// The script is designed to be run as:
//
//	php reflect.php <package-dir>
//
// Error handling: reflection errors are appended to the "errors" array and do
// not abort the scan. The script always exits 0 when it can emit valid JSON,
// and exits 1 on fatal errors (e.g. missing argument).
const reflectPHPScript = `<?php
declare(strict_types=1);

if ($argc < 2) {
    fwrite(STDERR, "Usage: reflect.php <package-dir>\n");
    exit(1);
}

$pkgDir = rtrim($argv[1], '/');
if (!is_dir($pkgDir)) {
    fwrite(STDERR, "reflect.php: not a directory: $pkgDir\n");
    exit(1);
}

$surface = [
    'package_name' => basename($pkgDir),
    'php_version'  => PHP_VERSION,
    'classes'      => [],
    'interfaces'   => [],
    'enums'        => [],
    'functions'    => [],
    'errors'       => [],
];

// Recursively collect .php files.
function collectPhpFiles(string $dir): array {
    $files = [];
    $it = new RecursiveIteratorIterator(new RecursiveDirectoryIterator($dir));
    foreach ($it as $file) {
        if ($file->isFile() && $file->getExtension() === 'php') {
            $files[] = $file->getPathname();
        }
    }
    sort($files);
    return $files;
}

// Serialize a ReflectionType to a string.
function typeString(?ReflectionType $t): string {
    if ($t === null) return '';
    if ($t instanceof ReflectionNamedType) return $t->getName();
    if ($t instanceof ReflectionUnionType) {
        return implode('|', array_map('typeString', $t->getTypes()));
    }
    if ($t instanceof ReflectionIntersectionType) {
        return implode('&', array_map('typeString', $t->getTypes()));
    }
    return (string)$t;
}

function isNullable(?ReflectionType $t): bool {
    if ($t === null) return false;
    if ($t instanceof ReflectionNamedType) return $t->allowsNull();
    return false;
}

function serializeParam(ReflectionParameter $p): array {
    $type = typeString($p->getType());
    $nullable = isNullable($p->getType());
    $default = '';
    if ($p->isOptional() && $p->isDefaultValueAvailable()) {
        try {
            $v = $p->getDefaultValue();
            $default = is_null($v) ? 'null' : var_export($v, true);
        } catch (Throwable $e) {
            $default = '?';
        }
    }
    return [
        'name'          => $p->getName(),
        'type'          => $type,
        'nullable'      => $nullable,
        'optional'      => $p->isOptional(),
        'variadic'      => $p->isVariadic(),
        'default_value' => $default,
    ];
}

function serializeMethod(ReflectionMethod $m): array {
    $params = [];
    foreach ($m->getParameters() as $p) {
        $params[] = serializeParam($p);
    }
    return [
        'name'        => $m->getName(),
        'static'      => $m->isStatic(),
        'abstract'    => $m->isAbstract(),
        'final'       => $m->isFinal(),
        'parameters'  => $params,
        'return_type' => typeString($m->getReturnType()),
        'nullable'    => isNullable($m->getReturnType()),
    ];
}

function serializeProperty(ReflectionProperty $p): array {
    $type = '';
    $nullable = false;
    if ($p->hasType()) {
        $type = typeString($p->getType());
        $nullable = isNullable($p->getType());
    }
    $default = '';
    try {
        $defaults = $p->getDeclaringClass()->getDefaultProperties();
        if (array_key_exists($p->getName(), $defaults)) {
            $v = $defaults[$p->getName()];
            $default = is_null($v) ? 'null' : var_export($v, true);
        }
    } catch (Throwable $e) {}
    return [
        'name'          => $p->getName(),
        'type'          => $type,
        'nullable'      => $nullable,
        'static'        => $p->isStatic(),
        'readonly'      => $p->isReadOnly(),
        'default_value' => $default,
    ];
}

function serializeConstant(ReflectionClassConstant $c): array {
    $type = '';
    if (method_exists($c, 'getType') && $c->getType() !== null) {
        $type = typeString($c->getType());
    }
    try {
        $val = var_export($c->getValue(), true);
    } catch (Throwable $e) {
        $val = '?';
    }
    return ['name' => $c->getName(), 'value' => $val, 'type' => $type];
}

$phpFiles = collectPhpFiles($pkgDir);

// Include each file; catch fatal-ish errors via Throwable.
foreach ($phpFiles as $file) {
    try {
        include_once $file;
    } catch (Throwable $e) {
        $surface['errors'][] = "include $file: " . $e->getMessage();
    }
}

// Enumerate classes, interfaces, enums.
$declared = get_declared_classes();
foreach ($declared as $className) {
    try {
        $rc = new ReflectionClass($className);
    } catch (Throwable $e) {
        $surface['errors'][] = "ReflectionClass($className): " . $e->getMessage();
        continue;
    }

    // Only process classes defined inside our package dir.
    $file = $rc->getFileName();
    if ($file === false || strpos(realpath($file), realpath($pkgDir)) !== 0) {
        continue;
    }
    if (!$rc->isPublic() && !($rc->getModifiers() & ReflectionClass::IS_EXPLICIT_ABSTRACT)) {
        // internal or non-public
    }
    // Skip anonymous classes.
    if ($rc->isAnonymous()) continue;

    // Public methods only.
    $methods = [];
    foreach ($rc->getMethods(ReflectionMethod::IS_PUBLIC) as $m) {
        if ($m->getDeclaringClass()->getName() !== $className) continue;
        $methods[] = serializeMethod($m);
    }

    // Public properties.
    $props = [];
    foreach ($rc->getProperties(ReflectionProperty::IS_PUBLIC) as $p) {
        if ($p->getDeclaringClass()->getName() !== $className) continue;
        $props[] = serializeProperty($p);
    }

    // Public constants.
    $consts = [];
    foreach ($rc->getReflectionConstants(ReflectionClassConstant::IS_PUBLIC) as $c) {
        if ($c->getDeclaringClass()->getName() !== $className) continue;
        $consts[] = serializeConstant($c);
    }

    $parentFqcn = $rc->getParentClass() ? $rc->getParentClass()->getName() : '';
    $ifaceNames = array_map(fn($i) => $i->getName(), $rc->getInterfaces());

    if ($rc->isInterface()) {
        $surface['interfaces'][] = [
            'fqcn'         => $className,
            'parent_fqcns' => $ifaceNames,
            'methods'      => $methods,
            'constants'    => $consts,
        ];
    } elseif (PHP_VERSION_ID >= 80100 && $rc->isEnum()) {
        $cases = [];
        foreach ($rc->getCases() as $case) {
            $val = '';
            try { $val = (string)$case->getValue()->value; } catch (Throwable $e) {}
            $cases[] = ['name' => $case->getName(), 'value' => $val];
        }
        $backingType = '';
        if (method_exists($rc, 'getBackingType') && $rc->getBackingType() !== null) {
            $backingType = typeString($rc->getBackingType());
        }
        $surface['enums'][] = [
            'fqcn'         => $className,
            'backing_type' => $backingType,
            'cases'        => $cases,
            'methods'      => $methods,
        ];
    } else {
        $surface['classes'][] = [
            'fqcn'            => $className,
            'abstract'        => $rc->isAbstract(),
            'final'           => $rc->isFinal(),
            'parent_fqcn'     => $parentFqcn,
            'interface_fqcns' => array_values($ifaceNames),
            'methods'         => $methods,
            'properties'      => $props,
            'constants'       => $consts,
        ];
    }
}

// Top-level functions.
$declared = get_defined_functions()['user'];
foreach ($declared as $fnName) {
    try {
        $rf = new ReflectionFunction($fnName);
    } catch (Throwable $e) {
        $surface['errors'][] = "ReflectionFunction($fnName): " . $e->getMessage();
        continue;
    }
    $file = $rf->getFileName();
    if ($file === false || strpos(realpath($file), realpath($pkgDir)) !== 0) {
        continue;
    }
    $params = [];
    foreach ($rf->getParameters() as $p) {
        $params[] = serializeParam($p);
    }
    $surface['functions'][] = [
        'name'        => $fnName,
        'parameters'  => $params,
        'return_type' => typeString($rf->getReturnType()),
        'nullable'    => isNullable($rf->getReturnType()),
    ];
}

echo json_encode($surface, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
`

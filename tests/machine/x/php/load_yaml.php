<?php
class Person {
    public $name;
    public $age;
    public $email;
    public function __construct($fields = []) {
        $this->name = $fields['name'] ?? null;
        $this->age = $fields['age'] ?? null;
        $this->email = $fields['email'] ?? null;
    }
}
$people = array_map(fn($it) => new Person($it), _load("../interpreter/valid/people.yaml", ["format" => "yaml"]));
$adults = (function() use ($people) {
    $result = [];
    foreach ($people as $p) {
        if ($p->age >= 18) {
            $result[] = [
    "name" => $p->name,
    "email" => $p->email
];
        }
    }
    return $result;
})();
foreach ($adults as $a) {
    _print($a['name'], $a['email']);
}
function _load($path = null, $opts = []) {
    $fmt = $opts['format'] ?? 'csv';
    if ($path !== null && $path !== '' && $path != '-' && $path[0] !== '/') {
        $path = __DIR__ . '/' . $path;
    }
    if ($fmt === 'yaml') {
        $lines = ($path === null || $path === '' || $path === '-') ?
            file('php://stdin', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) :
            file($path, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
        $rows = [];
        $curr = [];
        foreach ($lines as $line) {
            $line = trim($line);
            if (str_starts_with($line, '-')) {
                if ($curr) $rows[] = $curr;
                $curr = [];
                $line = trim(substr($line, 1));
                if ($line !== '') {
                    [$k, $v] = array_map('trim', explode(':', $line, 2));
                    $curr[$k] = is_numeric($v) ? (int)$v : $v;
                }
            } else {
                [$k, $v] = array_map('trim', explode(':', $line, 2));
                $curr[$k] = is_numeric($v) ? (int)$v : $v;
            }
        }
        if ($curr) $rows[] = $curr;
        return $rows;
    }
    return [];
}

function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>

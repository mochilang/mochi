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
    var_dump($a['name'], $a['email']);
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
?>

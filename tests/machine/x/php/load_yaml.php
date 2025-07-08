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

// people: [Person]
$people = _load_yaml("/workspace/mochi/tests/interpreter/valid/people.yaml");
// adults: [{string: string}]
$adults = (function() use ($people) {
	$res = [];
	foreach ((is_string($people) ? str_split($people) : $people) as $p) {
		if (!(($p['age'] >= 18))) { continue; }
		$res[] = ["name" => $p['name'], "email" => $p['email']];
	}
	return $res;
})();
foreach ((is_string($adults) ? str_split($adults) : $adults) as $a) {
	_print($a['name'], $a['email']);
}

function _load_yaml($path) {
    $f = ($path === '' || $path === '-') ? fopen('php://stdin', 'r') : fopen($path, 'r');
    if (!$f) { throw new Exception('cannot open ' . $path); }
    $data = stream_get_contents($f);
    if ($path !== '' && $path !== '-') fclose($f);
    $val = yaml_parse($data);
    if ($val === false || $val === null) return [];
    if (array_keys($val) !== range(0, count($val) - 1)) { return [$val]; }
    return $val;
}
function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}

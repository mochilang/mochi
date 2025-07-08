<?php
class Person {
	public $name;
	public $age;
	public $status;
	public function __construct($fields = []) {
		$this->name = $fields['name'] ?? null;
		$this->age = $fields['age'] ?? null;
		$this->status = $fields['status'] ?? null;
	}
}

function mochi_test_update_adult_status() {
	global $people;
	if (!(($people == [new Person(['name' => "Alice", 'age' => 17, 'status' => "minor"]), new Person(['name' => "Bob", 'age' => 26, 'status' => "adult"]), new Person(['name' => "Charlie", 'age' => 19, 'status' => "adult"]), new Person(['name' => "Diana", 'age' => 16, 'status' => "minor"])]))) { throw new Exception("expect failed: ($people == [new Person(['name' => 'Alice', 'age' => 17, 'status' => 'minor']), new Person(['name' => 'Bob', 'age' => 26, 'status' => 'adult']), new Person(['name' => 'Charlie', 'age' => 19, 'status' => 'adult']), new Person(['name' => 'Diana', 'age' => 16, 'status' => 'minor'])])"); }
}

// people: [Person]
$people = [new Person(['name' => "Alice", 'age' => 17, 'status' => "minor"]), new Person(['name' => "Bob", 'age' => 25, 'status' => "unknown"]), new Person(['name' => "Charlie", 'age' => 18, 'status' => "unknown"]), new Person(['name' => "Diana", 'age' => 16, 'status' => "minor"])];
for ($_i = 0; $_i < count($people); $_i++) {
	$_item = $people[$_i];
	$status = is_array($_item) ? ($_item['status'] ?? null) : $_item->status;
	$age = is_array($_item) ? ($_item['age'] ?? null) : $_item->age;
	if (($age >= 18)) {
		if (is_array($_item)) { $_item['status'] = "adult"; } else { $_item->status = "adult"; }
		if (is_array($_item)) { $_item['age'] = ((is_array($age) && is_array(1)) ? array_merge($age, 1) : ((is_string($age) || is_string(1)) ? ($age . 1) : ($age + 1))); } else { $_item->age = ((is_array($age) && is_array(1)) ? array_merge($age, 1) : ((is_string($age) || is_string(1)) ? ($age . 1) : ($age + 1))); }
	}
	$people[$_i] = $_item;
}
_print("ok");
mochi_test_update_adult_status();

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}

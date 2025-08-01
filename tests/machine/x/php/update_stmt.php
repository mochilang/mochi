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
$people = [
    new Person([
        'name' => "Alice",
        'age' => 17,
        'status' => "minor"
    ]),
    new Person([
        'name' => "Bob",
        'age' => 25,
        'status' => "unknown"
    ]),
    new Person([
        'name' => "Charlie",
        'age' => 18,
        'status' => "unknown"
    ]),
    new Person([
        'name' => "Diana",
        'age' => 16,
        'status' => "minor"
    ])
];
foreach ($people as $_tmp0 => $_tmp1) {
    $name = $_tmp1->name;
    $age = $_tmp1->age;
    $status = $_tmp1->status;
    if ($age >= 18) {
        $_tmp1->status = "adult";
        $_tmp1->age = $age + 1;
    }
    $people[$_tmp0] = $_tmp1;
}
echo "ok", PHP_EOL;
?>

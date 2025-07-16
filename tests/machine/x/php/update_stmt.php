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
_print("ok");
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

<?php
$people = [
    ["name" => "Alice", "age" => 30],
    ["name" => "Bob", "age" => 15],
    ["name" => "Charlie", "age" => 65],
    ["name" => "Diana", "age" => 45]
];
$adults = (function() use ($people) {
    $result = [];
    foreach ($people as $person) {
        if ($person['age'] >= 18) {
            $result[] = [
    "name" => $person['name'],
    "age" => $person['age'],
    "is_senior" => $person['age'] >= 60
];
        }
    }
    return $result;
})();
_print("--- Adults ---");
foreach ($adults as $person) {
    _print($person['name'], "is", $person['age'], ($person['is_senior'] ? " (senior)" : ""));
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

<?php
$people = [
    ["name" => "Alice", "age" => 30, "city" => "Paris"],
    ["name" => "Bob", "age" => 15, "city" => "Hanoi"],
    ["name" => "Charlie", "age" => 65, "city" => "Paris"],
    ["name" => "Diana", "age" => 45, "city" => "Hanoi"],
    ["name" => "Eve", "age" => 70, "city" => "Paris"],
    ["name" => "Frank", "age" => 22, "city" => "Hanoi"],
];
$groups = [];
foreach ($people as $p) {
    $groups[$p['city']][] = $p;
}
$stats = [];
foreach ($groups as $city => $persons) {
    $total = 0;
    foreach ($persons as $person) {
        $total += $person['age'];
    }
    $avg = $total / count($persons);
    $stats[] = ["city" => $city, "count" => count($persons), "avg_age" => $avg];
}
_print("--- People grouped by city ---");
foreach ($stats as $s) {
    _print($s['city'], ": count =", $s['count'], ", avg_age =", $s['avg_age']);
}

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) {
            $parts[] = json_encode($a);
        } else {
            $parts[] = strval($a);
        }
    }
    echo implode(' ', $parts), PHP_EOL;
}
?>

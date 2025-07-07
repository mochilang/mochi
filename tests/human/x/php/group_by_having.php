<?php
$people = [
    ["name" => "Alice", "city" => "Paris"],
    ["name" => "Bob", "city" => "Hanoi"],
    ["name" => "Charlie", "city" => "Paris"],
    ["name" => "Diana", "city" => "Hanoi"],
    ["name" => "Eve", "city" => "Paris"],
    ["name" => "Frank", "city" => "Hanoi"],
    ["name" => "George", "city" => "Paris"],
];
$groups = [];
foreach ($people as $p) {
    $groups[$p['city']][] = $p;
}
$big = [];
foreach ($groups as $city => $persons) {
    if (count($persons) >= 4) {
        $big[] = ["city" => $city, "num" => count($persons)];
    }
}
_print($big);

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

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
var_dump($big);
?>

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
var_dump("--- Adults ---");
foreach ($adults as $person) {
    var_dump($person['name'], "is", $person['age'], ($person['is_senior'] ? " (senior)" : ""));
}
?>

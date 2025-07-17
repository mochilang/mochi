<?php
$people = [
    [
        "name" => "Alice",
        "age" => 30,
        "city" => "Paris"
    ],
    [
        "name" => "Bob",
        "age" => 15,
        "city" => "Hanoi"
    ],
    [
        "name" => "Charlie",
        "age" => 65,
        "city" => "Paris"
    ],
    [
        "name" => "Diana",
        "age" => 45,
        "city" => "Hanoi"
    ],
    [
        "name" => "Eve",
        "age" => 70,
        "city" => "Paris"
    ],
    [
        "name" => "Frank",
        "age" => 22,
        "city" => "Hanoi"
    ]
];
$stats = (function() use ($people) {
    $groups = [];
    foreach ($people as $person) {
        $_k = json_encode($person['city']);
        $groups[$_k][] = $person;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [
    "city" => $g['key'],
    "count" => count($g['items']),
    "avg_age" => count((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $p) {
            $result[] = $p['age'];
        }
        return $result;
    })()) ? array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $p) {
            $result[] = $p['age'];
        }
        return $result;
    })()) / count((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $p) {
            $result[] = $p['age'];
        }
        return $result;
    })()) : 0
];
    }
    return $result;
})();
echo "--- People grouped by city ---", PHP_EOL;
foreach ($stats as $s) {
    var_dump($s['city'], ": count =", $s['count'], ", avg_age =", $s['avg_age']);
}
?>

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
        $g = ['key'=>json_decode($_k, true),'items'=> $__g];
        $result[] = [
    "city" => $g['key'],
    "count" => count($g['items']),
    "avg_age" => _avg((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $p) {
            $result[] = $p['age'];
        }
        return $result;
    })())
];
    }
    return $result;
})();
var_dump("--- People grouped by city ---");
foreach ($stats as $s) {
    var_dump($s['city'], ": count =", $s['count'], ", avg_age =", $s['avg_age']);
}
function _avg($v) {
    if (is_array($v) && array_key_exists('items', $v)) {
        $v = $v['items'];
    } elseif (is_object($v) && property_exists($v, 'items')) {
        $v = $v->items;
    }
    if (!is_array($v)) {
        throw new Exception('avg() expects list or group');
    }
    if (!$v) return 0;
    $sum = 0;
    foreach ($v as $it) {
        if (is_int($it) || is_float($it)) {
            $sum += $it;
        } else {
            throw new Exception('avg() expects numbers');
        }
    }
    return $sum / count($v);
}
?>

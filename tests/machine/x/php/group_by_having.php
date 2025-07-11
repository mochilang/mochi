<?php
$people = [
    [
        "name" => "Alice",
        "city" => "Paris"
    ],
    [
        "name" => "Bob",
        "city" => "Hanoi"
    ],
    [
        "name" => "Charlie",
        "city" => "Paris"
    ],
    [
        "name" => "Diana",
        "city" => "Hanoi"
    ],
    [
        "name" => "Eve",
        "city" => "Paris"
    ],
    [
        "name" => "Frank",
        "city" => "Hanoi"
    ],
    [
        "name" => "George",
        "city" => "Paris"
    ]
];
$big = (function() use ($people) {
    $groups = [];
    foreach ($people as $p) {
        $_k = json_encode($p['city']);
        $groups[$_k][] = $p;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $g = ['key'=>json_decode($_k, true),'items'=> $__g];
        if (count($g['items']) >= 4) {
        $result[] = [
    "city" => $g['key'],
    "num" => count($g['items'])
];
        }
    }
    return $result;
})();
json_encode($big);
?>

<?php
$items = [
    [
        "cat" => "a",
        "val" => 10,
        "flag" => true
    ],
    [
        "cat" => "a",
        "val" => 5,
        "flag" => false
    ],
    [
        "cat" => "b",
        "val" => 20,
        "flag" => true
    ]
];
$result = (function() use ($items) {
    $groups = [];
    foreach ($items as $i) {
        $_k = json_encode($i['cat']);
        $groups[$_k][] = $i;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [$g['key'], [
    "cat" => $g['key'],
    "share" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = ($x['flag'] ? $x['val'] : 0);
        }
        return $result;
    })()) / array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['val'];
        }
        return $result;
    })())
]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
var_dump($result);
?>

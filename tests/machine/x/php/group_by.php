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
echo "--- People grouped by city ---", PHP_EOL;
foreach ($stats as $s) {
    _print($s['city'], ": count =", $s['count'], ", avg_age =", $s['avg_age']);
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

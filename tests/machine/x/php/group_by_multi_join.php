<?php
$nations = [
    ["id" => 1, "name" => "A"],
    ["id" => 2, "name" => "B"]
];
$suppliers = [
    ["id" => 1, "nation" => 1],
    ["id" => 2, "nation" => 2]
];
$partsupp = [
    [
        "part" => 100,
        "supplier" => 1,
        "cost" => 10,
        "qty" => 2
    ],
    [
        "part" => 100,
        "supplier" => 2,
        "cost" => 20,
        "qty" => 1
    ],
    [
        "part" => 200,
        "supplier" => 1,
        "cost" => 5,
        "qty" => 3
    ]
];
$filtered = (function() use ($nations, $partsupp, $suppliers) {
    $result = [];
    foreach ($partsupp as $ps) {
        foreach ($suppliers as $s) {
            if ($s['id'] == $ps['supplier']) {
                foreach ($nations as $n) {
                    if ($n['id'] == $s['nation']) {
                        if ($n['name'] == "A") {
                            $result[] = [
    "part" => $ps['part'],
    "value" => $ps['cost'] * $ps['qty']
];
                        }
                    }
                }
            }
        }
    }
    return $result;
})();
$grouped = (function() use ($filtered) {
    $groups = [];
    foreach ($filtered as $x) {
        $_k = json_encode($x['part']);
        $groups[$_k][] = $x;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [
    "part" => $g['key'],
    "total" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $r) {
            $result[] = $r['value'];
        }
        return $result;
    })())
];
    }
    return $result;
})();
_print($grouped);
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

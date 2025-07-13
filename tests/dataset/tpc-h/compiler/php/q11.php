<?php
$nation = [
    [
        "n_nationkey" => 1,
        "n_name" => "GERMANY"
    ],
    [
        "n_nationkey" => 2,
        "n_name" => "FRANCE"
    ]
];
$supplier = [
    ["s_suppkey" => 100, "s_nationkey" => 1],
    ["s_suppkey" => 200, "s_nationkey" => 1],
    ["s_suppkey" => 300, "s_nationkey" => 2]
];
$partsupp = [
    [
        "ps_partkey" => 1000,
        "ps_suppkey" => 100,
        "ps_supplycost" => 10,
        "ps_availqty" => 100
    ],
    [
        "ps_partkey" => 1000,
        "ps_suppkey" => 200,
        "ps_supplycost" => 20,
        "ps_availqty" => 50
    ],
    [
        "ps_partkey" => 2000,
        "ps_suppkey" => 100,
        "ps_supplycost" => 5,
        "ps_availqty" => 10
    ],
    [
        "ps_partkey" => 3000,
        "ps_suppkey" => 300,
        "ps_supplycost" => 8,
        "ps_availqty" => 500
    ]
];
$target_nation = "GERMANY";
$filtered = (function() use ($nation, $partsupp, $supplier, $target_nation) {
    $result = [];
    foreach ($partsupp as $ps) {
        foreach ($supplier as $s) {
            if ($s['s_suppkey'] == $ps['ps_suppkey']) {
                foreach ($nation as $n) {
                    if ($n['n_nationkey'] == $s['s_nationkey']) {
                        if ($n['n_name'] == $target_nation) {
                            $result[] = [
    "ps_partkey" => $ps['ps_partkey'],
    "value" => $ps['ps_supplycost'] * $ps['ps_availqty']
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
        $_k = json_encode($x['ps_partkey']);
        $groups[$_k][] = $x;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [
    "ps_partkey" => $g['key'],
    "value" => array_sum((function() use ($g) {
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
$total = array_sum((function() use ($filtered) {
    $result = [];
    foreach ($filtered as $x) {
        $result[] = $x['value'];
    }
    return $result;
})());
$threshold = $total * 0.0001;
$result = (function() use ($grouped, $threshold) {
    $result = [];
    foreach ($grouped as $x) {
        if ($x['value'] > $threshold) {
            $result[] = [-$x['value'], $x];
        }
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($result), PHP_EOL;
?>

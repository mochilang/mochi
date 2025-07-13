<?php
$nation = [
    [
        "n_nationkey" => 1,
        "n_name" => "CANADA"
    ],
    [
        "n_nationkey" => 2,
        "n_name" => "GERMANY"
    ]
];
$supplier = [
    [
        "s_suppkey" => 100,
        "s_name" => "Maple Supply",
        "s_address" => "123 Forest Lane",
        "s_nationkey" => 1
    ],
    [
        "s_suppkey" => 200,
        "s_name" => "Berlin Metals",
        "s_address" => "456 Iron Str",
        "s_nationkey" => 2
    ]
];
$part = [
    [
        "p_partkey" => 10,
        "p_name" => "forest glade bricks"
    ],
    [
        "p_partkey" => 20,
        "p_name" => "desert sand paper"
    ]
];
$partsupp = [
    [
        "ps_partkey" => 10,
        "ps_suppkey" => 100,
        "ps_availqty" => 100
    ],
    [
        "ps_partkey" => 20,
        "ps_suppkey" => 200,
        "ps_availqty" => 30
    ]
];
$lineitem = [
    [
        "l_partkey" => 10,
        "l_suppkey" => 100,
        "l_quantity" => 100,
        "l_shipdate" => "1994-05-15"
    ],
    [
        "l_partkey" => 10,
        "l_suppkey" => 100,
        "l_quantity" => 50,
        "l_shipdate" => "1995-01-01"
    ]
];
$prefix = "forest";
$shipped_94 = (function() use ($lineitem) {
    $groups = [];
    foreach ($lineitem as $l) {
        if ($l['l_shipdate'] >= "1994-01-01" && $l['l_shipdate'] < "1995-01-01") {
            $_k = json_encode([
    "partkey" => $l['l_partkey'],
    "suppkey" => $l['l_suppkey']
]);
            $groups[$_k][] = $l;
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [
    "partkey" => $g['key']['partkey'],
    "suppkey" => $g['key']['suppkey'],
    "qty" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_quantity'];
        }
        return $result;
    })())
];
    }
    return $result;
})();
$target_partkeys = (function() use ($part, $partsupp, $prefix, $shipped_94) {
    $result = [];
    foreach ($partsupp as $ps) {
        foreach ($part as $p) {
            if ($ps['ps_partkey'] == $p['p_partkey']) {
                foreach ($shipped_94 as $s) {
                    if ($ps['ps_partkey'] == $s['partkey'] && $ps['ps_suppkey'] == $s['suppkey']) {
                        if (substr($p['p_name'], 0, strlen($prefix)) == $prefix && $ps['ps_availqty'] > (0.5 * $s['qty'])) {
                            $result[] = $ps['ps_suppkey'];
                        }
                    }
                }
            }
        }
    }
    return $result;
})();
$result = (function() use ($nation, $supplier, $target_partkeys) {
    $result = [];
    foreach ($supplier as $s) {
        foreach ($nation as $n) {
            if ($n['n_nationkey'] == $s['s_nationkey']) {
                if (in_array($s['s_suppkey'], $target_partkeys) && $n['n_name'] == "CANADA") {
                    $result[] = [$s['s_name'], [
    "s_name" => $s['s_name'],
    "s_address" => $s['s_address']
]];
                }
            }
        }
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($result), PHP_EOL;
?>

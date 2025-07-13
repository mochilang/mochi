<?php
$nation = [
    [
        "n_nationkey" => 1,
        "n_name" => "BRAZIL"
    ],
    [
        "n_nationkey" => 2,
        "n_name" => "CANADA"
    ]
];
$supplier = [
    ["s_suppkey" => 100, "s_nationkey" => 1],
    ["s_suppkey" => 200, "s_nationkey" => 2]
];
$part = [
    [
        "p_partkey" => 1000,
        "p_name" => "green metal box"
    ],
    [
        "p_partkey" => 2000,
        "p_name" => "red plastic crate"
    ]
];
$partsupp = [
    [
        "ps_partkey" => 1000,
        "ps_suppkey" => 100,
        "ps_supplycost" => 10
    ],
    [
        "ps_partkey" => 1000,
        "ps_suppkey" => 200,
        "ps_supplycost" => 8
    ]
];
$orders = [
    [
        "o_orderkey" => 1,
        "o_orderdate" => "1995-02-10"
    ],
    [
        "o_orderkey" => 2,
        "o_orderdate" => "1997-01-01"
    ]
];
$lineitem = [
    [
        "l_orderkey" => 1,
        "l_partkey" => 1000,
        "l_suppkey" => 100,
        "l_quantity" => 5,
        "l_extendedprice" => 1000,
        "l_discount" => 0.1
    ],
    [
        "l_orderkey" => 2,
        "l_partkey" => 1000,
        "l_suppkey" => 200,
        "l_quantity" => 10,
        "l_extendedprice" => 800,
        "l_discount" => 0.05
    ]
];
$prefix = "green";
$start_date = "1995-01-01";
$end_date = "1996-12-31";
$result = (function() use ($end_date, $lineitem, $nation, $orders, $part, $partsupp, $prefix, $start_date, $supplier) {
    $groups = [];
    foreach ($lineitem as $l) {
        foreach ($part as $p) {
            if ($p['p_partkey'] == $l['l_partkey']) {
                foreach ($supplier as $s) {
                    if ($s['s_suppkey'] == $l['l_suppkey']) {
                        foreach ($partsupp as $ps) {
                            if ($ps['ps_partkey'] == $l['l_partkey'] && $ps['ps_suppkey'] == $l['l_suppkey']) {
                                foreach ($orders as $o) {
                                    if ($o['o_orderkey'] == $l['l_orderkey']) {
                                        foreach ($nation as $n) {
                                            if ($n['n_nationkey'] == $s['s_nationkey']) {
                                                if (substr($p['p_name'], 0, strlen($prefix)) == $prefix && $o['o_orderdate'] >= $start_date && $o['o_orderdate'] <= $end_date) {
                                                    $_k = json_encode([
    "nation" => $n['n_name'],
    "o_year" => (int)(substr($o['o_orderdate'], 0, 4))
]);
                                                    $groups[$_k][] = ["l" => $l, "p" => $p, "s" => $s, "ps" => $ps, "o" => $o, "n" => $n];
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [[
    $g['key']['nation'],
    -$g['key']['o_year']
], [
    "nation" => $g['key']['nation'],
    "o_year" => strval($g['key']['o_year']),
    "profit" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = ($x['l']['l_extendedprice'] * (1 - $x['l']['l_discount'])) - ($x['ps']['ps_supplycost'] * $x['l']['l_quantity']);
        }
        return $result;
    })())
]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($result), PHP_EOL;
$revenue = 1000 * 0.9;
$cost = 5 * 10;
?>

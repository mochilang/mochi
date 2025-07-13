<?php
$region = [
    [
        "r_regionkey" => 0,
        "r_name" => "AMERICA"
    ]
];
$nation = [
    [
        "n_nationkey" => 10,
        "n_regionkey" => 0,
        "n_name" => "BRAZIL"
    ],
    [
        "n_nationkey" => 20,
        "n_regionkey" => 0,
        "n_name" => "CANADA"
    ]
];
$customer = [
    ["c_custkey" => 1, "c_nationkey" => 10],
    ["c_custkey" => 2, "c_nationkey" => 20]
];
$orders = [
    [
        "o_orderkey" => 100,
        "o_custkey" => 1,
        "o_orderdate" => "1995-04-10"
    ],
    [
        "o_orderkey" => 200,
        "o_custkey" => 2,
        "o_orderdate" => "1995-07-15"
    ]
];
$lineitem = [
    [
        "l_orderkey" => 100,
        "l_suppkey" => 1000,
        "l_partkey" => 5000,
        "l_extendedprice" => 1000,
        "l_discount" => 0.1
    ],
    [
        "l_orderkey" => 200,
        "l_suppkey" => 2000,
        "l_partkey" => 5000,
        "l_extendedprice" => 500,
        "l_discount" => 0.05
    ]
];
$supplier = [
    ["s_suppkey" => 1000],
    ["s_suppkey" => 2000]
];
$part = [
    [
        "p_partkey" => 5000,
        "p_type" => "ECONOMY ANODIZED STEEL"
    ],
    [
        "p_partkey" => 6000,
        "p_type" => "SMALL BRASS"
    ]
];
$start_date = "1995-01-01";
$end_date = "1996-12-31";
$target_type = "ECONOMY ANODIZED STEEL";
$target_nation = "BRAZIL";
$result = (function() use ($customer, $end_date, $lineitem, $nation, $orders, $part, $region, $start_date, $supplier, $target_nation, $target_type) {
    $groups = [];
    foreach ($lineitem as $l) {
        foreach ($part as $p) {
            if ($p['p_partkey'] == $l['l_partkey']) {
                foreach ($supplier as $s) {
                    if ($s['s_suppkey'] == $l['l_suppkey']) {
                        foreach ($orders as $o) {
                            if ($o['o_orderkey'] == $l['l_orderkey']) {
                                foreach ($customer as $c) {
                                    if ($c['c_custkey'] == $o['o_custkey']) {
                                        foreach ($nation as $n) {
                                            if ($n['n_nationkey'] == $c['c_nationkey']) {
                                                foreach ($region as $r) {
                                                    if ($r['r_regionkey'] == $n['n_regionkey']) {
                                                        if (($p['p_type'] == $target_type && $o['o_orderdate'] >= $start_date && $o['o_orderdate'] <= $end_date && $r['r_name'] == "AMERICA")) {
                                                            $_k = json_encode(substr($o['o_orderdate'], 0, 4));
                                                            $groups[$_k][] = ["l" => $l, "p" => $p, "s" => $s, "o" => $o, "c" => $c, "n" => $n, "r" => $r];
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
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $year = ['key'=>$_key,'items'=> $__g];
        $result[] = [$year['key'], [
    "o_year" => $year['key'],
    "mkt_share" => array_sum((function() use ($target_nation, $year) {
        $result = [];
        foreach ($year['items'] as $x) {
            $result[] = match($x['n']['n_name'] == $target_nation) {
        true => $x['l']['l_extendedprice'] * (1 - $x['l']['l_discount']),
        default => 0,
    };
        }
        return $result;
    })()) / array_sum((function() use ($year) {
        $result = [];
        foreach ($year['items'] as $x) {
            $result[] = $x['l']['l_extendedprice'] * (1 - $x['l']['l_discount']);
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
$numerator = 1000 * 0.9;
$denominator = $numerator + (500 * 0.95);
$share = $numerator / $denominator;
?>

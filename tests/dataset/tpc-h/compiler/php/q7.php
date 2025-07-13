<?php
$nation = [
    [
        "n_nationkey" => 1,
        "n_name" => "FRANCE"
    ],
    [
        "n_nationkey" => 2,
        "n_name" => "GERMANY"
    ]
];
$supplier = [
    ["s_suppkey" => 100, "s_nationkey" => 1]
];
$customer = [
    ["c_custkey" => 200, "c_nationkey" => 2]
];
$orders = [
    [
        "o_orderkey" => 1000,
        "o_custkey" => 200
    ]
];
$lineitem = [
    [
        "l_orderkey" => 1000,
        "l_suppkey" => 100,
        "l_extendedprice" => 1000,
        "l_discount" => 0.1,
        "l_shipdate" => "1995-06-15"
    ],
    [
        "l_orderkey" => 1000,
        "l_suppkey" => 100,
        "l_extendedprice" => 800,
        "l_discount" => 0.05,
        "l_shipdate" => "1997-01-01"
    ]
];
$start_date = "1995-01-01";
$end_date = "1996-12-31";
$nation1 = "FRANCE";
$nation2 = "GERMANY";
$result = (function() use ($customer, $end_date, $lineitem, $nation, $nation1, $nation2, $orders, $start_date, $supplier) {
    $groups = [];
    foreach ($lineitem as $l) {
        foreach ($orders as $o) {
            if ($o['o_orderkey'] == $l['l_orderkey']) {
                foreach ($customer as $c) {
                    if ($c['c_custkey'] == $o['o_custkey']) {
                        foreach ($supplier as $s) {
                            if ($s['s_suppkey'] == $l['l_suppkey']) {
                                foreach ($nation as $n1) {
                                    if ($n1['n_nationkey'] == $s['s_nationkey']) {
                                        foreach ($nation as $n2) {
                                            if ($n2['n_nationkey'] == $c['c_nationkey']) {
                                                if (($l['l_shipdate'] >= $start_date && $l['l_shipdate'] <= $end_date && ($n1['n_name'] == $nation1 && $n2['n_name'] == $nation2) || ($n1['n_name'] == $nation2 && $n2['n_name'] == $nation1))) {
                                                    $_k = json_encode([
    "supp_nation" => $n1['n_name'],
    "cust_nation" => $n2['n_name'],
    "l_year" => substr($l['l_shipdate'], 0, 4)
]);
                                                    $groups[$_k][] = ["l" => $l, "o" => $o, "c" => $c, "s" => $s, "n1" => $n1, "n2" => $n2];
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
    $g['key']['supp_nation'],
    $g['key']['cust_nation'],
    $g['key']['l_year']
], [
    "supp_nation" => $g['key']['supp_nation'],
    "cust_nation" => $g['key']['cust_nation'],
    "l_year" => $g['key']['l_year'],
    "revenue" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
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
?>

<?php
$nation = [
    [
        "n_nationkey" => 1,
        "n_name" => "SAUDI ARABIA"
    ],
    [
        "n_nationkey" => 2,
        "n_name" => "FRANCE"
    ]
];
$supplier = [
    [
        "s_suppkey" => 100,
        "s_name" => "Desert Trade",
        "s_nationkey" => 1
    ],
    [
        "s_suppkey" => 200,
        "s_name" => "Euro Goods",
        "s_nationkey" => 2
    ]
];
$orders = [
    [
        "o_orderkey" => 500,
        "o_orderstatus" => "F"
    ],
    [
        "o_orderkey" => 600,
        "o_orderstatus" => "O"
    ]
];
$lineitem = [
    [
        "l_orderkey" => 500,
        "l_suppkey" => 100,
        "l_receiptdate" => "1995-04-15",
        "l_commitdate" => "1995-04-10"
    ],
    [
        "l_orderkey" => 500,
        "l_suppkey" => 200,
        "l_receiptdate" => "1995-04-12",
        "l_commitdate" => "1995-04-12"
    ],
    [
        "l_orderkey" => 600,
        "l_suppkey" => 100,
        "l_receiptdate" => "1995-05-01",
        "l_commitdate" => "1995-04-25"
    ]
];
$result = (function() use ($lineitem, $nation, $orders, $supplier) {
    $groups = [];
    foreach ($supplier as $s) {
        foreach ($lineitem as $l1) {
            if ($s['s_suppkey'] == $l1['l_suppkey']) {
                foreach ($orders as $o) {
                    if ($o['o_orderkey'] == $l1['l_orderkey']) {
                        foreach ($nation as $n) {
                            if ($n['n_nationkey'] == $s['s_nationkey']) {
                                if ($o['o_orderstatus'] == "F" && $l1['l_receiptdate'] > $l1['l_commitdate'] && $n['n_name'] == "SAUDI ARABIA" && (!count((function() use ($l1, $lineitem) {
    $result = [];
    foreach ($lineitem as $x) {
        if ($x['l_orderkey'] == $l1['l_orderkey'] && $x['l_suppkey'] != $l1['l_suppkey'] && $x['l_receiptdate'] > $x['l_commitdate']) {
            $result[] = $x;
        }
    }
    return $result;
})()) > 0)) {
                                    $_k = json_encode($s['s_name']);
                                    $groups[$_k][] = ["s" => $s, "l1" => $l1, "o" => $o, "n" => $n];
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
        $result[] = [[-count($g['items']), $g['key']], [
    "s_name" => $g['key'],
    "numwait" => count($g['items'])
]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($result), PHP_EOL;
?>

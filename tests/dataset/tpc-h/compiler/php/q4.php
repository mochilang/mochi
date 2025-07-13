<?php
$orders = [
    [
        "o_orderkey" => 1,
        "o_orderdate" => "1993-07-01",
        "o_orderpriority" => "1-URGENT"
    ],
    [
        "o_orderkey" => 2,
        "o_orderdate" => "1993-07-15",
        "o_orderpriority" => "2-HIGH"
    ],
    [
        "o_orderkey" => 3,
        "o_orderdate" => "1993-08-01",
        "o_orderpriority" => "3-NORMAL"
    ]
];
$lineitem = [
    [
        "l_orderkey" => 1,
        "l_commitdate" => "1993-07-10",
        "l_receiptdate" => "1993-07-12"
    ],
    [
        "l_orderkey" => 1,
        "l_commitdate" => "1993-07-12",
        "l_receiptdate" => "1993-07-10"
    ],
    [
        "l_orderkey" => 2,
        "l_commitdate" => "1993-07-20",
        "l_receiptdate" => "1993-07-25"
    ],
    [
        "l_orderkey" => 3,
        "l_commitdate" => "1993-08-02",
        "l_receiptdate" => "1993-08-01"
    ],
    [
        "l_orderkey" => 3,
        "l_commitdate" => "1993-08-05",
        "l_receiptdate" => "1993-08-10"
    ]
];
$start_date = "1993-07-01";
$end_date = "1993-08-01";
$date_filtered_orders = (function() use ($end_date, $orders, $start_date) {
    $result = [];
    foreach ($orders as $o) {
        if ($o['o_orderdate'] >= $start_date && $o['o_orderdate'] < $end_date) {
            $result[] = $o;
        }
    }
    return $result;
})();
$late_orders = (function() use ($date_filtered_orders, $lineitem) {
    $result = [];
    foreach ($date_filtered_orders as $o) {
        if (count((function() use ($lineitem, $o) {
    $result = [];
    foreach ($lineitem as $l) {
        if ($l['l_orderkey'] == $o['o_orderkey'] && $l['l_commitdate'] < $l['l_receiptdate']) {
            $result[] = $l;
        }
    }
    return $result;
})()) > 0) {
            $result[] = $o;
        }
    }
    return $result;
})();
$result = (function() use ($late_orders) {
    $groups = [];
    foreach ($late_orders as $o) {
        $_k = json_encode($o['o_orderpriority']);
        $groups[$_k][] = $o;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [$g['key'], [
    "o_orderpriority" => $g['key'],
    "order_count" => count($g['items'])
]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($result), PHP_EOL;
?>

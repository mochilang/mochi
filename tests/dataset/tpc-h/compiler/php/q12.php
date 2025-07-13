<?php
$orders = [
    [
        "o_orderkey" => 1,
        "o_orderpriority" => "1-URGENT"
    ],
    [
        "o_orderkey" => 2,
        "o_orderpriority" => "3-MEDIUM"
    ]
];
$lineitem = [
    [
        "l_orderkey" => 1,
        "l_shipmode" => "MAIL",
        "l_commitdate" => "1994-02-10",
        "l_receiptdate" => "1994-02-15",
        "l_shipdate" => "1994-02-05"
    ],
    [
        "l_orderkey" => 2,
        "l_shipmode" => "SHIP",
        "l_commitdate" => "1994-03-01",
        "l_receiptdate" => "1994-02-28",
        "l_shipdate" => "1994-02-27"
    ]
];
$result = (function() use ($lineitem, $orders) {
    $groups = [];
    foreach ($lineitem as $l) {
        foreach ($orders as $o) {
            if ($o['o_orderkey'] == $l['l_orderkey']) {
                if ((in_array($l['l_shipmode'], ["MAIL", "SHIP"])) && ($l['l_commitdate'] < $l['l_receiptdate']) && ($l['l_shipdate'] < $l['l_commitdate']) && ($l['l_receiptdate'] >= "1994-01-01") && ($l['l_receiptdate'] < "1995-01-01")) {
                    $_k = json_encode($l['l_shipmode']);
                    $groups[$_k][] = ["l" => $l, "o" => $o];
                }
            }
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [$g['key'], [
    "l_shipmode" => $g['key'],
    "high_line_count" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = (in_array($x['o']['o_orderpriority'], ["1-URGENT", "2-HIGH"]) ? 1 : 0);
        }
        return $result;
    })()),
    "low_line_count" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = (!(in_array($x['o']['o_orderpriority'], ["1-URGENT", "2-HIGH"])) ? 1 : 0);
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

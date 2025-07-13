<?php
$customer = [
    [
        "c_custkey" => 1,
        "c_mktsegment" => "BUILDING"
    ],
    [
        "c_custkey" => 2,
        "c_mktsegment" => "AUTOMOBILE"
    ]
];
$orders = [
    [
        "o_orderkey" => 100,
        "o_custkey" => 1,
        "o_orderdate" => "1995-03-14",
        "o_shippriority" => 1
    ],
    [
        "o_orderkey" => 200,
        "o_custkey" => 2,
        "o_orderdate" => "1995-03-10",
        "o_shippriority" => 2
    ]
];
$lineitem = [
    [
        "l_orderkey" => 100,
        "l_extendedprice" => 1000,
        "l_discount" => 0.05,
        "l_shipdate" => "1995-03-16"
    ],
    [
        "l_orderkey" => 100,
        "l_extendedprice" => 500,
        "l_discount" => 0,
        "l_shipdate" => "1995-03-20"
    ],
    [
        "l_orderkey" => 200,
        "l_extendedprice" => 1000,
        "l_discount" => 0.1,
        "l_shipdate" => "1995-03-14"
    ]
];
$cutoff = "1995-03-15";
$segment = "BUILDING";
$building_customers = (function() use ($customer, $segment) {
    $result = [];
    foreach ($customer as $c) {
        if ($c['c_mktsegment'] == $segment) {
            $result[] = $c;
        }
    }
    return $result;
})();
$valid_orders = (function() use ($building_customers, $cutoff, $orders) {
    $result = [];
    foreach ($orders as $o) {
        foreach ($building_customers as $c) {
            if ($o['o_custkey'] == $c['c_custkey']) {
                if ($o['o_orderdate'] < $cutoff) {
                    $result[] = $o;
                }
            }
        }
    }
    return $result;
})();
$valid_lineitems = (function() use ($cutoff, $lineitem) {
    $result = [];
    foreach ($lineitem as $l) {
        if ($l['l_shipdate'] > $cutoff) {
            $result[] = $l;
        }
    }
    return $result;
})();
$order_line_join = (function() use ($valid_lineitems, $valid_orders) {
    $groups = [];
    foreach ($valid_orders as $o) {
        foreach ($valid_lineitems as $l) {
            if ($l['l_orderkey'] == $o['o_orderkey']) {
                $_k = json_encode([
    "o_orderkey" => $o['o_orderkey'],
    "o_orderdate" => $o['o_orderdate'],
    "o_shippriority" => $o['o_shippriority']
]);
                $groups[$_k][] = ["o" => $o, "l" => $l];
            }
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [[
    -array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $r) {
            $result[] = $r['l']['l_extendedprice'] * (1 - $r['l']['l_discount']);
        }
        return $result;
    })()),
    $g['key']['o_orderdate']
], [
    "l_orderkey" => $g['key']['o_orderkey'],
    "revenue" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $r) {
            $result[] = $r['l']['l_extendedprice'] * (1 - $r['l']['l_discount']);
        }
        return $result;
    })()),
    "o_orderdate" => $g['key']['o_orderdate'],
    "o_shippriority" => $g['key']['o_shippriority']
]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($order_line_join), PHP_EOL;
?>

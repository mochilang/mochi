<?php
$nation = [
    [
        "n_nationkey" => 1,
        "n_name" => "GERMANY"
    ]
];
$customer = [
    [
        "c_custkey" => 1,
        "c_name" => "Alice",
        "c_acctbal" => 1000,
        "c_nationkey" => 1,
        "c_address" => "123 Market St",
        "c_phone" => "123-456",
        "c_comment" => "Premium client"
    ],
    [
        "c_custkey" => 2,
        "c_name" => "Bob",
        "c_acctbal" => 200,
        "c_nationkey" => 1,
        "c_address" => "456 Side St",
        "c_phone" => "987-654",
        "c_comment" => "Frequent returns"
    ]
];
$orders = [
    ["o_orderkey" => 100, "o_custkey" => 1],
    ["o_orderkey" => 200, "o_custkey" => 1],
    ["o_orderkey" => 300, "o_custkey" => 2]
];
$lineitem = [
    [
        "l_orderkey" => 100,
        "l_quantity" => 150,
        "l_extendedprice" => 1000,
        "l_discount" => 0.1
    ],
    [
        "l_orderkey" => 200,
        "l_quantity" => 100,
        "l_extendedprice" => 800,
        "l_discount" => 0
    ],
    [
        "l_orderkey" => 300,
        "l_quantity" => 30,
        "l_extendedprice" => 300,
        "l_discount" => 0.05
    ]
];
$threshold = 200;
$result = (function() use ($customer, $lineitem, $nation, $orders, $threshold) {
    $groups = [];
    foreach ($customer as $c) {
        foreach ($orders as $o) {
            if ($o['o_custkey'] == $c['c_custkey']) {
                foreach ($lineitem as $l) {
                    if ($l['l_orderkey'] == $o['o_orderkey']) {
                        foreach ($nation as $n) {
                            if ($n['n_nationkey'] == $c['c_nationkey']) {
                                $_k = json_encode([
    "c_name" => $c['c_name'],
    "c_custkey" => $c['c_custkey'],
    "c_acctbal" => $c['c_acctbal'],
    "c_address" => $c['c_address'],
    "c_phone" => $c['c_phone'],
    "c_comment" => $c['c_comment'],
    "n_name" => $n['n_name']
]);
                                $groups[$_k][] = ["c" => $c, "o" => $o, "l" => $l, "n" => $n];
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
        if (array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['l']['l_quantity'];
    }
    return $result;
})()) > $threshold) {
        $result[] = [-array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['l']['l_extendedprice'] * (1 - $x['l']['l_discount']);
    }
    return $result;
})()), [
    "c_name" => $g['key']['c_name'],
    "c_custkey" => $g['key']['c_custkey'],
    "revenue" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l']['l_extendedprice'] * (1 - $x['l']['l_discount']);
        }
        return $result;
    })()),
    "c_acctbal" => $g['key']['c_acctbal'],
    "n_name" => $g['key']['n_name'],
    "c_address" => $g['key']['c_address'],
    "c_phone" => $g['key']['c_phone'],
    "c_comment" => $g['key']['c_comment']
]];
        }
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($result), PHP_EOL;
?>

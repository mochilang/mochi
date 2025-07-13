<?php
$region = [
    ["r_regionkey" => 0, "r_name" => "ASIA"],
    [
        "r_regionkey" => 1,
        "r_name" => "EUROPE"
    ]
];
$nation = [
    [
        "n_nationkey" => 10,
        "n_regionkey" => 0,
        "n_name" => "JAPAN"
    ],
    [
        "n_nationkey" => 20,
        "n_regionkey" => 0,
        "n_name" => "INDIA"
    ],
    [
        "n_nationkey" => 30,
        "n_regionkey" => 1,
        "n_name" => "FRANCE"
    ]
];
$customer = [
    ["c_custkey" => 1, "c_nationkey" => 10],
    ["c_custkey" => 2, "c_nationkey" => 20]
];
$supplier = [
    [
        "s_suppkey" => 100,
        "s_nationkey" => 10
    ],
    [
        "s_suppkey" => 200,
        "s_nationkey" => 20
    ]
];
$orders = [
    [
        "o_orderkey" => 1000,
        "o_custkey" => 1,
        "o_orderdate" => "1994-03-15"
    ],
    [
        "o_orderkey" => 2000,
        "o_custkey" => 2,
        "o_orderdate" => "1994-06-10"
    ],
    [
        "o_orderkey" => 3000,
        "o_custkey" => 2,
        "o_orderdate" => "1995-01-01"
    ]
];
$lineitem = [
    [
        "l_orderkey" => 1000,
        "l_suppkey" => 100,
        "l_extendedprice" => 1000,
        "l_discount" => 0.05
    ],
    [
        "l_orderkey" => 2000,
        "l_suppkey" => 200,
        "l_extendedprice" => 800,
        "l_discount" => 0.1
    ],
    [
        "l_orderkey" => 3000,
        "l_suppkey" => 200,
        "l_extendedprice" => 900,
        "l_discount" => 0.05
    ]
];
$asia_nations = (function() use ($nation, $region) {
    $result = [];
    foreach ($region as $r) {
        foreach ($nation as $n) {
            if ($n['n_regionkey'] == $r['r_regionkey']) {
                if ($r['r_name'] == "ASIA") {
                    $result[] = $n;
                }
            }
        }
    }
    return $result;
})();
$local_customer_supplier_orders = (function() use ($asia_nations, $customer, $lineitem, $orders, $supplier) {
    $result = [];
    foreach ($customer as $c) {
        foreach ($asia_nations as $n) {
            if ($c['c_nationkey'] == $n['n_nationkey']) {
                foreach ($orders as $o) {
                    if ($o['o_custkey'] == $c['c_custkey']) {
                        foreach ($lineitem as $l) {
                            if ($l['l_orderkey'] == $o['o_orderkey']) {
                                foreach ($supplier as $s) {
                                    if ($s['s_suppkey'] == $l['l_suppkey']) {
                                        if ($o['o_orderdate'] >= "1994-01-01" && $o['o_orderdate'] < "1995-01-01" && $s['s_nationkey'] == $c['c_nationkey']) {
                                            $result[] = [
    "nation" => $n['n_name'],
    "revenue" => $l['l_extendedprice'] * (1 - $l['l_discount'])
];
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
    return $result;
})();
$result = (function() use ($local_customer_supplier_orders) {
    $groups = [];
    foreach ($local_customer_supplier_orders as $r) {
        $_k = json_encode($r['nation']);
        $groups[$_k][] = $r;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [-array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['revenue'];
    }
    return $result;
})()), [
    "n_name" => $g['key'],
    "revenue" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['revenue'];
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

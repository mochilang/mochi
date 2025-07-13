<?php
$supplier = [
    [
        "s_suppkey" => 100,
        "s_name" => "Best Supplier",
        "s_address" => "123 Market St",
        "s_phone" => "123-456"
    ],
    [
        "s_suppkey" => 200,
        "s_name" => "Second Supplier",
        "s_address" => "456 Elm St",
        "s_phone" => "987-654"
    ]
];
$lineitem = [
    [
        "l_suppkey" => 100,
        "l_extendedprice" => 1000,
        "l_discount" => 0.1,
        "l_shipdate" => "1996-01-15"
    ],
    [
        "l_suppkey" => 100,
        "l_extendedprice" => 500,
        "l_discount" => 0,
        "l_shipdate" => "1996-03-20"
    ],
    [
        "l_suppkey" => 200,
        "l_extendedprice" => 800,
        "l_discount" => 0.05,
        "l_shipdate" => "1995-12-30"
    ]
];
$start_date = "1996-01-01";
$end_date = "1996-04-01";
$revenue0 = (function() use ($end_date, $lineitem, $start_date) {
    $groups = [];
    foreach ($lineitem as $l) {
        if ($l['l_shipdate'] >= $start_date && $l['l_shipdate'] < $end_date) {
            $_k = json_encode($l['l_suppkey']);
            $groups[$_k][] = $l;
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [
    "supplier_no" => $g['key'],
    "total_revenue" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_extendedprice'] * (1 - $x['l_discount']);
        }
        return $result;
    })())
];
    }
    return $result;
})();
$revenues = (function() use ($revenue0) {
    $result = [];
    foreach ($revenue0 as $x) {
        $result[] = $x['total_revenue'];
    }
    return $result;
})();
$max_revenue = max($revenues);
$result = (function() use ($max_revenue, $revenue0, $supplier) {
    $result = [];
    foreach ($supplier as $s) {
        foreach ($revenue0 as $r) {
            if ($s['s_suppkey'] == $r['supplier_no']) {
                if ($r['total_revenue'] == $max_revenue) {
                    $result[] = [$s['s_suppkey'], [
    "s_suppkey" => $s['s_suppkey'],
    "s_name" => $s['s_name'],
    "s_address" => $s['s_address'],
    "s_phone" => $s['s_phone'],
    "total_revenue" => $r['total_revenue']
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
$rev = 1000 * 0.9 + 500;
?>

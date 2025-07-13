<?php
$part = [
    [
        "p_partkey" => 1,
        "p_type" => "PROMO LUXURY"
    ],
    [
        "p_partkey" => 2,
        "p_type" => "STANDARD BRASS"
    ]
];
$lineitem = [
    [
        "l_partkey" => 1,
        "l_extendedprice" => 1000,
        "l_discount" => 0.1,
        "l_shipdate" => "1995-09-05"
    ],
    [
        "l_partkey" => 2,
        "l_extendedprice" => 800,
        "l_discount" => 0,
        "l_shipdate" => "1995-09-20"
    ],
    [
        "l_partkey" => 1,
        "l_extendedprice" => 500,
        "l_discount" => 0.2,
        "l_shipdate" => "1995-10-02"
    ]
];
$start_date = "1995-09-01";
$end_date = "1995-10-01";
$filtered = (function() use ($end_date, $lineitem, $part, $start_date) {
    $result = [];
    foreach ($lineitem as $l) {
        foreach ($part as $p) {
            if ($p['p_partkey'] == $l['l_partkey']) {
                if ($l['l_shipdate'] >= $start_date && $l['l_shipdate'] < $end_date) {
                    $result[] = [
    "is_promo" => (is_array($p['p_type']) ? in_array("PROMO", $p['p_type'], true) : (is_string($p['p_type']) ? strpos($p['p_type'], strval("PROMO")) !== false : false)),
    "revenue" => $l['l_extendedprice'] * (1 - $l['l_discount'])
];
                }
            }
        }
    }
    return $result;
})();
$promo_sum = array_sum((function() use ($filtered) {
    $result = [];
    foreach ($filtered as $x) {
        if ($x['is_promo']) {
            $result[] = $x['revenue'];
        }
    }
    return $result;
})());
$total_sum = array_sum((function() use ($filtered) {
    $result = [];
    foreach ($filtered as $x) {
        $result[] = $x['revenue'];
    }
    return $result;
})());
$result = 100 * $promo_sum / $total_sum;
echo json_encode($result), PHP_EOL;
$promo = 1000 * 0.9;
$total = 900 + 800;
$expected = 100 * $promo / $total;
?>

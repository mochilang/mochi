<?php
$part = [
    [
        "p_partkey" => 1,
        "p_brand" => "Brand#23",
        "p_container" => "MED BOX"
    ],
    [
        "p_partkey" => 2,
        "p_brand" => "Brand#77",
        "p_container" => "LG JAR"
    ]
];
$lineitem = [
    [
        "l_partkey" => 1,
        "l_quantity" => 1,
        "l_extendedprice" => 100
    ],
    [
        "l_partkey" => 1,
        "l_quantity" => 10,
        "l_extendedprice" => 1000
    ],
    [
        "l_partkey" => 1,
        "l_quantity" => 20,
        "l_extendedprice" => 2000
    ],
    [
        "l_partkey" => 2,
        "l_quantity" => 5,
        "l_extendedprice" => 500
    ]
];
$brand = "Brand#23";
$container = "MED BOX";
$filtered = (function() use ($brand, $container, $lineitem, $part) {
    $result = [];
    foreach ($lineitem as $l) {
        foreach ($part as $p) {
            if ($p['p_partkey'] == $l['l_partkey']) {
                if ((($p['p_brand'] == $brand) && ($p['p_container'] == $container) && ($l['l_quantity'] < (0.2 * _avg((function() use ($lineitem, $p) {
    $result = [];
    foreach ($lineitem as $x) {
        if ($x['l_partkey'] == $p['p_partkey']) {
            $result[] = $x['l_quantity'];
        }
    }
    return $result;
})()))))) {
                    $result[] = $l['l_extendedprice'];
                }
            }
        }
    }
    return $result;
})();
$result = array_sum($filtered) / 7;
echo json_encode($result), PHP_EOL;
$expected = 100 / 7;
function _avg($v) {
    if (is_array($v) && array_key_exists('items', $v)) {
        $v = $v['items'];
    } elseif (is_object($v) && property_exists($v, 'items')) {
        $v = $v->items;
    }
    if (!is_array($v)) {
        throw new Exception('avg() expects list or group');
    }
    if (!$v) return 0;
    $sum = 0;
    foreach ($v as $it) {
        if (is_int($it) || is_float($it)) {
            $sum += $it;
        } else {
            throw new Exception('avg() expects numbers');
        }
    }
    return $sum / count($v);
}
?>

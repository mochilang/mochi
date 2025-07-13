<?php
$lineitem = [
    [
        "l_quantity" => 17,
        "l_extendedprice" => 1000,
        "l_discount" => 0.05,
        "l_tax" => 0.07,
        "l_returnflag" => "N",
        "l_linestatus" => "O",
        "l_shipdate" => "1998-08-01"
    ],
    [
        "l_quantity" => 36,
        "l_extendedprice" => 2000,
        "l_discount" => 0.1,
        "l_tax" => 0.05,
        "l_returnflag" => "N",
        "l_linestatus" => "O",
        "l_shipdate" => "1998-09-01"
    ],
    [
        "l_quantity" => 25,
        "l_extendedprice" => 1500,
        "l_discount" => 0,
        "l_tax" => 0.08,
        "l_returnflag" => "R",
        "l_linestatus" => "F",
        "l_shipdate" => "1998-09-03"
    ]
];
$result = (function() use ($lineitem) {
    $groups = [];
    foreach ($lineitem as $row) {
        if ($row['l_shipdate'] <= "1998-09-02") {
            $_k = json_encode([
    "returnflag" => $row['l_returnflag'],
    "linestatus" => $row['l_linestatus']
]);
            $groups[$_k][] = $row;
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [
    "returnflag" => $g['key']['returnflag'],
    "linestatus" => $g['key']['linestatus'],
    "sum_qty" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_quantity'];
        }
        return $result;
    })()),
    "sum_base_price" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_extendedprice'];
        }
        return $result;
    })()),
    "sum_disc_price" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_extendedprice'] * (1 - $x['l_discount']);
        }
        return $result;
    })()),
    "sum_charge" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_extendedprice'] * (1 - $x['l_discount']) * (1 + $x['l_tax']);
        }
        return $result;
    })()),
    "avg_qty" => _avg((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_quantity'];
        }
        return $result;
    })()),
    "avg_price" => _avg((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_extendedprice'];
        }
        return $result;
    })()),
    "avg_disc" => _avg((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['l_discount'];
        }
        return $result;
    })()),
    "count_order" => count($g['items'])
];
    }
    return $result;
})();
echo json_encode($result), PHP_EOL;
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

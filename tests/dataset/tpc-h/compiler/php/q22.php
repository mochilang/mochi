<?php
$customer = [
    [
        "c_custkey" => 1,
        "c_phone" => "13-123-4567",
        "c_acctbal" => 600
    ],
    [
        "c_custkey" => 2,
        "c_phone" => "31-456-7890",
        "c_acctbal" => 100
    ],
    [
        "c_custkey" => 3,
        "c_phone" => "30-000-0000",
        "c_acctbal" => 700
    ]
];
$orders = [["o_orderkey" => 10, "o_custkey" => 2]];
$valid_codes = [
    "13",
    "31",
    "23",
    "29",
    "30",
    "18",
    "17"
];
$avg_balance = _avg((function() use ($customer, $valid_codes) {
    $result = [];
    foreach ($customer as $c) {
        if (in_array($c['c_acctbal'] > 0 && substr($c['c_phone'], 0, 2), $valid_codes)) {
            $result[] = $c['c_acctbal'];
        }
    }
    return $result;
})());
$eligible_customers = (function() use ($avg_balance, $customer, $orders, $valid_codes) {
    $result = [];
    foreach ($customer as $c) {
        if (in_array(substr($c['c_phone'], 0, 2), $valid_codes) && $c['c_acctbal'] > $avg_balance && (!count((function() use ($c, $orders) {
    $result = [];
    foreach ($orders as $o) {
        if ($o['o_custkey'] == $c['c_custkey']) {
            $result[] = $o;
        }
    }
    return $result;
})()) > 0)) {
            $result[] = [
    "cntrycode" => substr($c['c_phone'], 0, 2),
    "c_acctbal" => $c['c_acctbal']
];
        }
    }
    return $result;
})();
$groups = (function() use ($eligible_customers) {
    $groups = [];
    foreach ($eligible_customers as $c) {
        $_k = json_encode($c['cntrycode']);
        $groups[$_k][] = $c;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = $g;
    }
    return $result;
})();
$tmp = [];
foreach ($groups as $g) {
    $total = array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['c_acctbal'];
    }
    return $result;
})());
    $row = [
    "cntrycode" => $g['key'],
    "numcust" => count($g['items']),
    "totacctbal" => $total
];
    $tmp = array_merge($tmp, [$row]);
}
$result = (function() use ($tmp) {
    $result = [];
    foreach ($tmp as $r) {
        $result[] = [$r['cntrycode'], $r];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
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

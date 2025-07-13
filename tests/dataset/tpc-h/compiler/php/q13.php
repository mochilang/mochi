<?php
$customer = [
    ["c_custkey" => 1],
    ["c_custkey" => 2],
    ["c_custkey" => 3]
];
$orders = [
    [
        "o_orderkey" => 100,
        "o_custkey" => 1,
        "o_comment" => "fast delivery"
    ],
    [
        "o_orderkey" => 101,
        "o_custkey" => 1,
        "o_comment" => "no comment"
    ],
    [
        "o_orderkey" => 102,
        "o_custkey" => 2,
        "o_comment" => "special requests only"
    ]
];
$per_customer = (function() use ($customer, $orders) {
    $result = [];
    foreach ($customer as $c) {
        $result[] = [
    "c_count" => count((function() use ($c, $orders) {
        $result = [];
        foreach ($orders as $o) {
            if (($o['o_custkey'] == $c['c_custkey'] && (!((is_array($o['o_comment']) ? in_array("special", $o['o_comment'], true) : (is_string($o['o_comment']) ? strpos($o['o_comment'], strval("special")) !== false : false)))) && (!((is_array($o['o_comment']) ? in_array("requests", $o['o_comment'], true) : (is_string($o['o_comment']) ? strpos($o['o_comment'], strval("requests")) !== false : false)))))) {
                $result[] = $o;
            }
        }
        return $result;
    })())
];
    }
    return $result;
})();
$grouped = (function() use ($per_customer) {
    $groups = [];
    foreach ($per_customer as $x) {
        $_k = json_encode($x['c_count']);
        $groups[$_k][] = $x;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [-$g['key'], [
    "c_count" => $g['key'],
    "custdist" => count($g['items'])
]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($grouped), PHP_EOL;
?>

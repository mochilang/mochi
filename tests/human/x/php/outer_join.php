<?php
$customers = [
    ["id"=>1, "name"=>"Alice"],
    ["id"=>2, "name"=>"Bob"],
    ["id"=>3, "name"=>"Charlie"],
    ["id"=>4, "name"=>"Diana"],
];
$orders = [
    ["id"=>100, "customerId"=>1, "total"=>250],
    ["id"=>101, "customerId"=>2, "total"=>125],
    ["id"=>102, "customerId"=>1, "total"=>300],
    ["id"=>103, "customerId"=>5, "total"=>80],
];
$result = [];
foreach ($orders as $o) {
    $cust = null;
    foreach ($customers as $c) {
        if ($c['id'] == $o['customerId']) { $cust = $c; break; }
    }
    $result[] = ['order'=>$o, 'customer'=>$cust];
}
foreach ($customers as $c) {
    $found = false;
    foreach ($orders as $o) {
        if ($o['customerId'] == $c['id']) { $found = true; break; }
    }
    if (!$found) {
        $result[] = ['order'=>null, 'customer'=>$c];
    }
}
_print("--- Outer Join using syntax ---");
foreach ($result as $row) {
    if ($row['order']) {
        if ($row['customer']) {
            _print("Order", $row['order']['id'], "by", $row['customer']['name'], "- $", $row['order']['total']);
        } else {
            _print("Order", $row['order']['id'], "by", "Unknown", "- $", $row['order']['total']);
        }
    } else {
        _print("Customer", $row['customer']['name'], "has no orders");
    }
}

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) {
            $parts[] = json_encode($a);
        } else {
            $parts[] = strval($a);
        }
    }
    echo implode(' ', $parts), PHP_EOL;
}
?>

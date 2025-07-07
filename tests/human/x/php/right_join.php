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
];
$result = [];
foreach ($customers as $c) {
    $has = false;
    foreach ($orders as $o) {
        if ($o['customerId'] == $c['id']) {
            $result[] = ['customerName'=>$c['name'], 'order'=>$o];
            $has = true;
        }
    }
    if (!$has) {
        $result[] = ['customerName'=>$c['name'], 'order'=>null];
    }
}
_print("--- Right Join using syntax ---");
foreach ($result as $e) {
    if ($e['order']) {
        _print("Customer", $e['customerName'], "has order", $e['order']['id'], "- $", $e['order']['total']);
    } else {
        _print("Customer", $e['customerName'], "has no orders");
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

<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 1],
    ["id" => 102, "customerId" => 2],
];
$groups = [];
foreach ($orders as $o) {
    foreach ($customers as $c) {
        if ($o['customerId'] === $c['id']) {
            $groups[$c['name']][] = $o;
        }
    }
}
ksort($groups);
$stats = [];
foreach ($groups as $name => $ordersFor) {
    $stats[] = ['name'=>$name, 'count'=>count($ordersFor)];
}
_print("--- Orders per customer ---");
foreach ($stats as $s) {
    _print($s['name'], "orders:", $s['count']);
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

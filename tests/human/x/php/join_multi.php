<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 2],
];
$items = [
    ["orderId" => 100, "sku" => "a"],
    ["orderId" => 101, "sku" => "b"],
];
$result = [];
foreach ($orders as $o) {
    foreach ($customers as $c) {
        if ($c['id'] === $o['customerId']) {
            foreach ($items as $i) {
                if ($i['orderId'] === $o['id']) {
                    $result[] = ['name'=>$c['name'], 'sku'=>$i['sku']];
                }
            }
        }
    }
}
_print("--- Multi Join ---");
foreach ($result as $r) {
    _print($r['name'], "bought item", $r['sku']);
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

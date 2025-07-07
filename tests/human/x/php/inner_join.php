<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
    ["id" => 3, "name" => "Charlie"],
];
$orders = [
    ["id" => 100, "customerId" => 1, "total" => 250],
    ["id" => 101, "customerId" => 2, "total" => 125],
    ["id" => 102, "customerId" => 1, "total" => 300],
    ["id" => 103, "customerId" => 4, "total" => 80],
];
$result = [];
foreach ($orders as $o) {
    foreach ($customers as $c) {
        if ($o['customerId'] === $c['id']) {
            $result[] = [
                'orderId' => $o['id'],
                'customerName' => $c['name'],
                'total' => $o['total'],
            ];
        }
    }
}
_print("--- Orders with customer info ---");
foreach ($result as $entry) {
    _print("Order", $entry['orderId'], "by", $entry['customerName'], "- $", $entry['total']);
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

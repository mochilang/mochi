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
];
$result = [];
foreach ($orders as $o) {
    foreach ($customers as $c) {
        $result[] = [
            "orderId" => $o['id'],
            "orderCustomerId" => $o['customerId'],
            "pairedCustomerName" => $c['name'],
            "orderTotal" => $o['total'],
        ];
    }
}
_print("--- Cross Join: All order-customer pairs ---");
foreach ($result as $entry) {
    _print("Order", $entry['orderId'], "(customerId:", $entry['orderCustomerId'], ", total: $", $entry['orderTotal'], ") paired with", $entry['pairedCustomerName']);
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

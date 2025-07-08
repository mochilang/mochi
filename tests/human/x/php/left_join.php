<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
];
$orders = [
    ["id" => 100, "customerId" => 1, "total" => 250],
    ["id" => 101, "customerId" => 3, "total" => 80],
];
$result = [];
foreach ($orders as $o) {
    $matched = null;
    foreach ($customers as $c) {
        if ($o['customerId'] === $c['id']) {
            $matched = $c;
            break;
        }
    }
    $result[] = [
        'orderId' => $o['id'],
        'customer' => $matched,
        'total' => $o['total']
    ];
}
var_dump("--- Left Join ---");
foreach ($result as $entry) {
    var_dump("Order", $entry['orderId'], "customer", $entry['customer'], "total", $entry['total']);
}
?>

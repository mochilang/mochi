<?php
$customers = [
    [
        "id" => 1,
        "name" => "Alice"
    ],
    [
        "id" => 2,
        "name" => "Bob"
    ],
    [
        "id" => 3,
        "name" => "Charlie"
    ]
];
$orders = [
    [
        "id" => 100,
        "customerId" => 1,
        "total" => 250
    ],
    [
        "id" => 101,
        "customerId" => 2,
        "total" => 125
    ],
    [
        "id" => 102,
        "customerId" => 1,
        "total" => 300
    ],
    [
        "id" => 103,
        "customerId" => 4,
        "total" => 80
    ]
];
$result = (function() use ($customers, $orders) {
    $result = [];
    foreach ($orders as $o) {
        foreach ($customers as $c) {
            if ($o['customerId'] == $c['id']) {
                $result[] = [
    "orderId" => $o['id'],
    "customerName" => $c['name'],
    "total" => $o['total']
];
            }
        }
    }
    return $result;
})();
var_dump("--- Orders with customer info ---");
foreach ($result as $entry) {
    var_dump("Order", $entry['orderId'], "by", $entry['customerName'], "- $", $entry['total']);
}
?>

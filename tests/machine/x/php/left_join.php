<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"]
];
$orders = [
    [
        "id" => 100,
        "customerId" => 1,
        "total" => 250
    ],
    [
        "id" => 101,
        "customerId" => 3,
        "total" => 80
    ]
];
$result = (function() use ($customers, $orders) {
    $result = [];
    foreach ($orders as $o) {
        $_match = null;
        foreach ($customers as $c) {
            if ($o['customerId'] == $c['id']) { $_match = $c; break; }
        }
        $c = $_match;
        $result[] = [
    "orderId" => $o['id'],
    "customer" => $c,
    "total" => $o['total']
];
    }
    return $result;
})();
echo "--- Left Join ---", PHP_EOL;
foreach ($result as $entry) {
    var_dump("Order", $entry['orderId'], "customer", $entry['customer'], "total", $entry['total']);
}
?>

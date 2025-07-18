<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
    ["id" => 3, "name" => "Charlie"],
    ["id" => 4, "name" => "Diana"]
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
        "customerId" => 5,
        "total" => 80
    ]
];
$result = (function() use ($customers, $orders) {
    $result = [];
    foreach ($orders as $o) {
        $_cust = null;
        foreach ($customers as $c) {
            if ($o['customerId'] == $c['id']) { $_cust = $c; break; }
        }
        $c = $_cust;
        $result[] = ["order" => $o, "customer" => $c];
    }
    foreach ($customers as $c) {
        $_found = false;
        foreach ($orders as $o) {
            if ($o['customerId'] == $c['id']) { $_found = true; break; }
        }
        if (!$_found) {
            $o = null;
            $result[] = ["order" => $o, "customer" => $c];
        }
    }
    return $result;
})();
echo "--- Outer Join using syntax ---", PHP_EOL;
foreach ($result as $row) {
    if ($row['order']) {
        if ($row['customer']) {
            echo "Order", $row['order']['id'], "by", $row['customer']['name'], "- $", $row['order']['total'], PHP_EOL;
        } else {
            echo "Order", $row['order']['id'], "by", "Unknown", "- $", $row['order']['total'], PHP_EOL;
        }
    } else {
        echo "Customer", $row['customer']['name'], "has no orders", PHP_EOL;
    }
}
?>

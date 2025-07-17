<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
    ["id" => 3, "name" => "Charlie"]
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
echo "--- Orders with customer info ---", PHP_EOL;
foreach ($result as $entry) {
    _print("Order", $entry['orderId'], "by", $entry['customerName'], "- $", $entry['total']);
}
function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>

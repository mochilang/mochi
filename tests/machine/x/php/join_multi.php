<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"]
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 2]
];
$items = [
    ["orderId" => 100, "sku" => "a"],
    ["orderId" => 101, "sku" => "b"]
];
$result = (function() use ($customers, $items, $orders) {
    $result = [];
    foreach ($orders as $o) {
        foreach ($customers as $c) {
            if ($o['customerId'] == $c['id']) {
                foreach ($items as $i) {
                    if ($o['id'] == $i['orderId']) {
                        $result[] = [
    "name" => $c['name'],
    "sku" => $i['sku']
];
                    }
                }
            }
        }
    }
    return $result;
})();
echo "--- Multi Join ---", PHP_EOL;
foreach ($result as $r) {
    _print($r['name'], "bought item", $r['sku']);
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

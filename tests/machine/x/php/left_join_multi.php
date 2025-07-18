<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"]
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 2]
];
$items = [["orderId" => 100, "sku" => "a"]];
$result = (function() use ($customers, $items, $orders) {
    $result = [];
    foreach ($orders as $o) {
        foreach ($customers as $c) {
            if ($o['customerId'] == $c['id']) {
                $_found = false;
                foreach ($items as $i) {
                    if ($o['id'] == $i['orderId']) {
                        $_found = true;
                        $result[] = [
    "orderId" => $o['id'],
    "name" => $c['name'],
    "item" => $i
];
                    }
                }
                if (!$_found) {
                    $i = null;
                    $result[] = [
    "orderId" => $o['id'],
    "name" => $c['name'],
    "item" => $i
];
                }
            }
        }
    }
    return $result;
})();
echo "--- Left Join Multi ---", PHP_EOL;
foreach ($result as $r) {
    var_dump($r['orderId'], $r['name'], $r['item']);
}
?>

<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1], ["id" => 101, "customerId" => 2]];
$items = [["orderId" => 100, "sku" => "a"], ["orderId" => 101, "sku" => "b"]];
$result = (function() use ($customers, $items, $orders) {
    $result = [];
    foreach ($orders as $o) {
        foreach ($customers as $c) {
            if ($o['customerId'] == $c['id']) {
                foreach ($items as $i) {
                    if ($o['id'] == $i['orderId']) {
                        $result[] = ["name" => $c['name'], "sku" => $i['sku']];
                    }
                }
            }
        }
    }
    return $result;
})();
var_dump("--- Multi Join ---");
foreach ($result as $r) {
    var_dump($r['name'], "bought item", $r['sku']);
}
?>

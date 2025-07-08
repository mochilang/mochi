<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 2],
];
$items = [
    ["orderId" => 100, "sku" => "a"],
];
$result = [];
foreach ($orders as $o) {
    foreach ($customers as $c) {
        if ($c['id'] === $o['customerId']) {
            $found = false;
            foreach ($items as $i) {
                if ($i['orderId'] === $o['id']) {
                    $result[] = ['orderId'=>$o['id'], 'name'=>$c['name'], 'item'=>$i];
                    $found = true;
                }
            }
            if (!$found) {
                $result[] = ['orderId'=>$o['id'], 'name'=>$c['name'], 'item'=>null];
            }
        }
    }
}
var_dump("--- Left Join Multi ---");
foreach ($result as $r) {
    var_dump($r['orderId'], $r['name'], $r['item']);
}
?>

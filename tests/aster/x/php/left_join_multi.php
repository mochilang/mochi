<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1], ["id" => 101, "customerId" => 2]];
$items = [["orderId" => 100, "sku" => "a"]];
$result = [];
foreach ($orders as $o) {
    foreach ($customers as $c) {
        ;
        foreach ($items as $i) {
            if (!($o["id"] == $i["orderId"])) {
            }
            ;
            if ($o["customerId"] == $c["id"]) {
                $result[] = ["orderId" => $o["id"], "name" => $c["name"], "item" => $i];
            }
        }
        if (!$matched) {
            ;
            if ($o["customerId"] == $c["id"]) {
                $result[] = ["orderId" => $o["id"], "name" => $c["name"], "item" => $i];
            }
        }
    }
}
echo "--- Left Join Multi ---", PHP_EOL;
foreach ($result as $r) {
    echo  . (is_float($r["item"]) ? json_encode($r["item"], 1344) : $r["item"]), PHP_EOL;
}

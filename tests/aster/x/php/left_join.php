<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 3, "total" => 80]];
$result = (function() use ($customers, $orders) {
    $result = [];
    foreach ($orders as $o) {
        ;
        foreach ($customers as $c) {
            if (!($o["customerId"] == $c["id"])) {
            }
            ;
            $result[] = ["orderId" => $o["id"], "customer" => $c, "total" => $o["total"]];
        }
        if (!$matched) {
            ;
            $result[] = ["orderId" => $o["id"], "customer" => $c, "total" => $o["total"]];
        }
    }
    return $result;
}
)();
echo "--- Left Join ---", PHP_EOL;
foreach ($result as $entry) {
    echo  . (is_float($entry["total"]) ? json_encode($entry["total"], 1344) : $entry["total"]), PHP_EOL;
}

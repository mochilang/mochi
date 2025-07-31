<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 2, "total" => 125], ["id" => 102, "customerId" => 1, "total" => 300]];
;
foreach ($orders as $o) {
    foreach ($customers as $c) {
         = ["orderId" => $o["id"], "orderCustomerId" => $o["customerId"], "pairedCustomerName" => $c["name"], "orderTotal" => $o["total"]];
    }
}
echo "--- Cross Join: All order-customer pairs ---", PHP_EOL;
foreach ($result as $entry) {
    echo  . (is_float($entry["pairedCustomerName"]) ? json_encode($entry["pairedCustomerName"], 1344) : $entry["pairedCustomerName"]), PHP_EOL;
}

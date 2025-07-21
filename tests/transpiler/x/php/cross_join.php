<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 2, "total" => 125], ["id" => 102, "customerId" => 1, "total" => 300]];
$result = [];
foreach ($orders as $o) {
  foreach ($customers as $c) {
    $result[] = ["orderId" => $o["id"], "orderCustomerId" => $o["customerId"], "pairedCustomerName" => $c["name"], "orderTotal" => $o["total"]];
  }
}

echo "--- Cross Join: All order-customer pairs ---", PHP_EOL;
foreach ($result as $entry) {
  echo "Order" . " " . (is_float($entry["orderId"]) ? json_encode($entry["orderId"], 1344) : $entry["orderId"]) . " " . "(customerId:" . " " . (is_float($entry["orderCustomerId"]) ? json_encode($entry["orderCustomerId"], 1344) : $entry["orderCustomerId"]) . " " . ", total: $" . " " . (is_float($entry["orderTotal"]) ? json_encode($entry["orderTotal"], 1344) : $entry["orderTotal"]) . " " . ") paired with" . " " . (is_float($entry["pairedCustomerName"]) ? json_encode($entry["pairedCustomerName"], 1344) : $entry["pairedCustomerName"]), PHP_EOL;
}

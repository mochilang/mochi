<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 2, "total" => 125], ["id" => 102, "customerId" => 1, "total" => 300]];
$result = [];
foreach ($orders as $o) {
  foreach ($customers as $c) {
    $result[] = ["orderId" => $o["id"], "orderCustomerId" => $o["customerId"], "pairedCustomerName" => $c["name"], "orderTotal" => $o["total"]];
  }
}

echo rtrim("--- Cross Join: All order-customer pairs ---"), PHP_EOL;
foreach ($result as $entry) {
  echo rtrim("Order" . " " . (is_float($entry["orderId"]) ? sprintf("%.15f", $entry["orderId"]) : $entry["orderId"]) . " " . "(customerId:" . " " . (is_float($entry["orderCustomerId"]) ? sprintf("%.15f", $entry["orderCustomerId"]) : $entry["orderCustomerId"]) . " " . ", total: $" . " " . (is_float($entry["orderTotal"]) ? sprintf("%.15f", $entry["orderTotal"]) : $entry["orderTotal"]) . " " . ") paired with" . " " . (is_float($entry["pairedCustomerName"]) ? sprintf("%.15f", $entry["pairedCustomerName"]) : $entry["pairedCustomerName"])), PHP_EOL;
}
?>

<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 2, "total" => 125], ["id" => 102, "customerId" => 1, "total" => 300], ["id" => 103, "customerId" => 4, "total" => 80]];
$result = [];
foreach ($orders as $o) {
  foreach ($customers as $c) {
    if ($o["customerId"] == $c["id"]) {
      $result[] = ["orderId" => $o["id"], "customerName" => $c["name"], "total" => $o["total"]];
    }
  }
}

echo rtrim("--- Orders with customer info ---"), PHP_EOL;
foreach ($result as $entry) {
  echo rtrim("Order" . " " . (is_float($entry["orderId"]) ? sprintf("%.15f", $entry["orderId"]) : $entry["orderId"]) . " " . "by" . " " . (is_float($entry["customerName"]) ? sprintf("%.15f", $entry["customerName"]) : $entry["customerName"]) . " " . "- $" . " " . (is_float($entry["total"]) ? sprintf("%.15f", $entry["total"]) : $entry["total"])), PHP_EOL;
}
?>

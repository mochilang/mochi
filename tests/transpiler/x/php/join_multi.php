<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1], ["id" => 101, "customerId" => 2]];
$items = [["orderId" => 100, "sku" => "a"], ["orderId" => 101, "sku" => "b"]];
$result = [];
foreach ($orders as $o) {
  foreach ($customers as $c) {
    foreach ($items as $i) {
      if ($o["customerId"] == $c["id"] && $o["id"] == $i["orderId"]) {
        $result[] = ["name" => $c["name"], "sku" => $i["sku"]];
      }
    }
  }
}

echo rtrim("--- Multi Join ---"), PHP_EOL;
foreach ($result as $r) {
  echo rtrim($r["name"] . " " . "bought item" . " " . $r["sku"]), PHP_EOL;
}
?>

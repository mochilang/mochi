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

echo "--- Multi Join ---", PHP_EOL;
foreach ($result as $r) {
  echo (is_float($r["name"]) ? json_encode($r["name"], 1344) : $r["name"]) . " " . "bought item" . " " . (is_float($r["sku"]) ? json_encode($r["sku"], 1344) : $r["sku"]), PHP_EOL;
}

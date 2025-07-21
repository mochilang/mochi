<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1], ["id" => 101, "customerId" => 2]];
$items = [["orderId" => 100, "sku" => "a"]];
$result = [];
foreach ($orders as $o) {
  foreach ($customers as $c) {
    $matched = false;
    foreach ($items as $i) {
      if (!($o["id"] == $i["orderId"])) continue;
      $matched = true;
      if ($o["customerId"] == $c["id"]) {
        $result[] = ["orderId" => $o["id"], "name" => $c["name"], "item" => $i];
      }
    }
    if (!$matched) {
      $i = null;
      if ($o["customerId"] == $c["id"]) {
        $result[] = ["orderId" => $o["id"], "name" => $c["name"], "item" => $i];
      }
    }
  }
}

echo rtrim("--- Left Join Multi ---"), PHP_EOL;
foreach ($result as $r) {
  echo rtrim((is_float($r["orderId"]) ? sprintf("%.15f", $r["orderId"]) : $r["orderId"]) . " " . (is_float($r["name"]) ? sprintf("%.15f", $r["name"]) : $r["name"]) . " " . (is_float($r["item"]) ? sprintf("%.15f", $r["item"]) : $r["item"])), PHP_EOL;
}
?>

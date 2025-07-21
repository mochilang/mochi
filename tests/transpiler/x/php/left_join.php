<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 3, "total" => 80]];
$result = (function() use ($customers, $orders) {
  $result = [];
  foreach ($orders as $o) {
    $matched = false;
    foreach ($customers as $c) {
      if (!($o["customerId"] == $c["id"])) continue;
      $matched = true;
      $result[] = ["orderId" => $o["id"], "customer" => $c, "total" => $o["total"]];
    }
    if (!$matched) {
      $c = null;
      $result[] = ["orderId" => $o["id"], "customer" => $c, "total" => $o["total"]];
    }
  }
  return $result;
})();
echo rtrim("--- Left Join ---"), PHP_EOL;
foreach ($result as $entry) {
  echo rtrim("Order" . " " . $entry["orderId"] . " " . "customer" . " " . $entry["customer"] . " " . "total" . " " . $entry["total"]), PHP_EOL;
}
?>
